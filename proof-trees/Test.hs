-- Log STLC type checking and process as natural deduction proof tree.

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Test where

import Control.Applicative ((<*) , (*>) , (<$>) , (<*>) , pure)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List (intercalate)
import Data.Proxy
import System.IO (hPutStrLn , stderr)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Parsec hiding (Stream , between)
import qualified Text.Parsec

import Debug.Trace.LogTree
import Debug.Trace.LogTree.Process.ProofTree
import Debug.Trace.LogTree.Simple.Logger
import Debug.Trace.LogTree.Simple.Call

----------------------------------------------------------------
-- Grammar.

type TmVar = String
type TyVar = String
data Tm = Lam TmVar Ty Tm
        | TmVar TmVar
        | Tm :@: Tm
  deriving Show
data Ty = TyVar TyVar
        | Ty :->: Ty
  deriving (Eq , Show)
type Ctx = [(TmVar,Ty)]

----------------------------------------------------------------
-- Parser.

type P a = Parsec String () a

parens :: P a -> P a
parens = Text.Parsec.between (char '(') (char ')')

tmVar , tyVar :: P String
tmVar = (:[]) <$> lower
tyVar = (:[]) <$> upper

tyAtom , arrs , ty :: P Ty
tyAtom = parens ty
     <|> TyVar <$> tyVar
arrs = chainr1 tyAtom arrOp where
  arrOp = string "->" *> pure (:->:)
ty = arrs

tmAtom , apps , lam , tm :: P Tm
tmAtom = parens tm
     <|> TmVar <$> tmVar
apps = chainl1 tmAtom appOp where
  appOp = pure (:@:)
lam = uncurry Lam <$> (char '\\' *> typing)
                  <*> (char '.' *> tm)
tm = apps <|> lam

typing :: P (TmVar , Ty)
typing = (,) <$> tmVar
             <*> (char ':' *> ty)

ctx :: P Ctx
ctx = typing `sepBy` (char ',')

-- Here 'call*' is the opposite of the standard 'exec*': throw away
-- any non-result state and return the result.
callParser :: P a -> String -> a
callParser p = either (error . show) id
             . parse (p <* eof) "<no file>"

----------------------------------------------------------------
-- Type checker.

type Mode = Bool
type Stream = LogStream (ProofTree Mode)
type M a = ErrorT String (ReaderT Ctx (Writer Stream)) a
runM :: Ctx -> M a -> (Either String a , Stream)
runM ctx = runWriter . flip runReaderT ctx . runErrorT
execM :: Ctx -> M a -> Stream
execM ctx = snd . runM ctx

type InferTy = Tm -> M Ty
infer , infer' :: InferTy
infer = simpleLogger (Proxy::Proxy "infer") ask (return ()) infer'
infer' (Lam x t e) = (t :->:) <$> (local (++ [(x,t)]) . infer $ e)
infer' (TmVar x) = maybe err pure . lookup x =<< ask where
  err = throwError $ "Variable " ++ x ++ " not in context!"
infer' (e :@: e1) = do
  t <- infer e
  t1 <- infer e1
  case t of
    t1' :->: t2 | t1' == t1 -> pure t2
    _ -> throwError $ "Can't apply " ++ show t ++ " to " ++ show t1 ++ "!"

callInfer :: Ctx -> Tm -> Ty
callInfer ctx = either error id . fst . runM ctx . infer

----------------------------------------------------------------
-- Latex pretty printer.

-- Precedence: higher value means tighter binding.
type Prec = Double

between :: Prec -> Prec -> Prec
between x y = (x + y) / 2

lowest , highest , precLam , precApp , precArr :: Prec
highest = 1
lowest = 0
precLam = lowest
precApp = between precLam highest
precArr = lowest

-- Associativity: left, none, or right.
data Assoc = L | N | R deriving Eq

-- Wrap a pretty print when the context dictates.
wrap :: Pretty a => Assoc -> a -> a -> String
wrap side ctx x = if prec x `comp` prec ctx
                  then pp x
                  else parens . pp $ x
  where
    comp = if side == assoc x || assoc x == N
           then (>=)
           else (>)
    parens s = "(" ++ s ++ ")"

class Pretty t where
  pp :: t -> String
  prec :: t -> Prec
  prec _ = highest
  assoc :: t -> Assoc
  assoc _ = N

instance Pretty Ty where
  pp (TyVar v) = v
  pp t@(t1 :->: t2) = wrap L t t1 ++ " {\\to} " ++ wrap R t t2
  prec (_ :->: _) = precArr
  prec _ = highest
  assoc (_ :->: _) = R
  assoc _ = N

instance Pretty Tm where
  pp (TmVar v) = v
  pp (Lam x t e) = "\\lambda " ++ x ++ " {:} " ++ pp t ++ " . " ++ pp e
  pp e@(e1 :@: e2) = wrap L e e1 ++ " " ++ wrap R e e2
  prec (Lam {}) = precLam
  prec (_ :@: _) = precApp
  prec _ = highest
  assoc (_ :@: _) = L
  assoc _ = N

instance Pretty Ctx where
  pp [] = "\\cdot"
  pp ctx@(_:_) =
    intercalate " , " [ x ++ " {:} " ++ pp t  | (x,t) <- ctx ]

ppCtxOnlyTypes :: Ctx -> String
ppCtxOnlyTypes [] = "\\cdot"
ppCtxOnlyTypes ctx@(_:_) = intercalate " , " [ pp t  | (_,t) <- ctx ]

----------------------------------------------------------------
-- Proof tree processor.

instance ProofTree Mode (Proxy (SimpleCall "infer" Ctx InferTy ())) where
  callAndReturn mode t = conclusion mode ctx tm (Right ty)
    where
      (tm , ()) = _arg t
      ty = _ret t
      ctx = _before t
  callAndError mode t = conclusion mode ctx tm (Left error)
    where
      (tm , ()) = _arg' t
      how = _how t
      ctx = _before' t
      error = maybe "\\,!" (const "\\,\\uparrow") how

conclusion :: Mode -> Ctx -> Tm -> Either String Ty -> (String , String)
conclusion mode ctx tm e = (judgment mode , rule tm)
  where
    rule (TmVar _) = "\\textsc{Axiom}"
    rule (Lam {}) = "\\to \\text{I}"
    rule (_ :@: _) = "\\to \\text{E}"

    tyOrError = either id pp e

    judgment True = pp ctx ++ " \\vdash " ++ pp tm ++ " : " ++ tyOrError
    judgment False = ppCtxOnlyTypes ctx ++ " \\vdash " ++ tyOrError

----------------------------------------------------------------
-- Bring it all together.

pipeline :: Mode -> Ctx -> String -> String
pipeline mode ctx =
  proofTree mode
  . either (error . show) (!! 0)
  . stream2Forest
  . execM ctx
  . infer
  . callParser tm

makeDoc :: String -> String
makeDoc tex = unlines
  [ "\\documentclass[10pt]{article}"
  , "\\usepackage{proof}"
  , "\\usepackage{amsmath}"
  , "\\usepackage[landscape]{geometry}"
  , "\\usepackage[cm]{fullpage}"
  -- The most slender font I could find:
  -- http://www.tug.dk/FontCatalogue/iwonalc/
  , "\\usepackage[light,condensed,math]{iwona}"
  , "\\usepackage[T1]{fontenc}"
  , "\\begin{document}"
  , "\\tiny"
  , "\\[" ++ tex ++ "\\]"
  , "\\end{document}"
  ]

-- Reusable main.
mainWith :: (Mode -> Ctx -> String -> String) -> IO ()
mainWith pipeline = do
  args <- getArgs
  when (not $ length args == 3) $ do
    err "usage: $0 MODE CTX TERM"
    err ""
    err "The MODEs are 'True' for show proof terms and 'False' for only show types."
    exitWith (ExitFailure 2)
  let mode = read $ args !! 0
  let ctx' = callParser ctx $ args !! 1
  let tex = pipeline mode ctx' $ args !! 2
  putStr . makeDoc $ tex
  where
    err = hPutStrLn stderr

main :: IO ()
main = mainWith pipeline
