module Leombda.Parser where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Int (Int32)
import qualified Text.Parsec as P

--------------------------------------------------------------------------------

type Parser = P.Parsec String ()

--------------------------------------------------------------------------------

data VarName = VarName String
  deriving (Show)

pVarName :: Parser VarName
pVarName = VarName <$> P.many1 (P.oneOf "abcdefghijklmnopqrstuvwxyz")

--------------------------------------------------------------------------------

data Lam = Lam VarName Expr
  deriving (Show)

pLam :: Parser Lam
pLam = do
  _ <- P.char '\\'
  vn <- pVarName
  P.spaces
  _ <- P.string "->"
  P.spaces
  e <- pExpr
  return (Lam vn e)

--------------------------------------------------------------------------------

data Lit = LitInt32 Int32
  deriving (Show)

pLit :: Parser Lit
pLit = LitInt32 <$> pInt32

pInt32 :: Parser Int32
pInt32 = do
   mn <- P.optionMaybe (P.char '-')
   num <- P.many1 P.digit
   let integralNum = read num :: Integer
   guard $ integralNum <= fromIntegral (maxBound :: Int32)
   let int32Num = fromInteger integralNum
   return $ case mn of
     Nothing -> int32Num
     Just _  -> (-1) * int32Num

--------------------------------------------------------------------------------

data App = App Expr Expr
  deriving (Show)

pApp :: Parser App
pApp = do
    e1 <- pSubExpr
    _ <- P.many1 P.space
    e2 <- pSubExpr
    return (App e1 e2)
  where
    pSubExpr =
      fmap ExprVar (P.try pVarName) <|>
      fmap ExprLit (P.try pLit) <|>
      pInBraces pExpr

--------------------------------------------------------------------------------

data Expr
  = ExprLam Lam
  | ExprVar VarName
  | ExprApp App
  | ExprLit Lit
  deriving (Show)

pExpr :: Parser Expr
pExpr =
    fmap ExprApp (P.try pApp) <|>
    fmap ExprVar (P.try pVarName) <|>
    fmap ExprLit (P.try pLit) <|>
    fmap ExprLam (P.try pLam) <|>
    pInBraces pExpr

--------------------------------------------------------------------------------

pInBraces :: Parser a -> Parser a
pInBraces = P.between (P.char '(' >> P.spaces) (P.spaces >> P.char ')')
