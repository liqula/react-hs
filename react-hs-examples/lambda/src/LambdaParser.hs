module LambdaParser where


import           Control.Applicative
import qualified Data.Char as Char
import qualified Data.Text as Text
import           LambdaAST
import           Parser

-------------------------------------------------------------------------------
maxNameNumber :: Expr ann -> Int
maxNameNumber expr =
  case expr of
    Var _ n ->
      nameToNumber n
    Abs _ n b ->
      max (nameToNumber n) (maxNameNumber b)
    App _ a b ->
      max (maxNameNumber a) (maxNameNumber b)

-------------------------------------------------------------------------------
nameToNumber :: Text.Text -> Int
nameToNumber txt = maybe (-1) fst (unParser pNameToNumber $ Text.unpack txt)


pNameWithNumber :: Parser (String, String)
pNameWithNumber = do
  _ <- sat (=='_')
  cs <- many (sat Char.isAlphaNum)
  _ <- sat (=='_')
  ds <- many (sat Char.isDigit)
  return (cs, ds)


pNameToNumber :: Parser Int
pNameToNumber = do
  (_, ds) <- pNameWithNumber
  return (read ds)


pWhitespace :: Parser String
pWhitespace = many (sat Char.isSpace)


pName :: Parser Name
pName = pName1 <|> pName2


pName1 :: Parser Name
pName1 = do
  c <- sat Char.isLower
  cs <- many (sat Char.isAlphaNum)
  return (Text.pack (c:cs))


pName2 :: Parser Name
pName2 = do
  (cs, ds) <- pNameWithNumber
  return (Text.pack("_" ++ cs ++ "_" ++ ds))


pVar :: Parser (Expr ())
pVar = do
  name <- pName
  _ <- pWhitespace
  return (Var () name)


pAbs :: Parser (Expr ())
pAbs = do
  _ <- sat (=='\\')
  _ <- pWhitespace
  name <- pName
  _ <- pWhitespace
  _ <- sat (=='.')
  _ <- pWhitespace
  body <- pExpr
  _ <- pWhitespace
  return (Abs () name body)


pApp :: Parser (Expr ())
pApp = do
  _ <- sat (=='@')
  _ <- pWhitespace
  a <- pExpr
  _ <- pWhitespace
  b <- pExpr
  _ <- pWhitespace
  return (App () a b)


pLet :: Parser (Expr ())
pLet = do
  _ <- sat (=='l')
  _ <- sat (=='e')
  _ <- sat (=='t')
  _ <- sat Char.isSpace
  _ <- pWhitespace
  x <- pName
  _ <- pWhitespace
  _ <- sat (=='=')
  _ <- pWhitespace
  e1 <- pExpr
  e2 <- pExpr
  return (App () (Abs () x e2) e1)


pExpr :: Parser (Expr ())
pExpr = pLet <|> pVar <|> pAbs <|> pApp


parseExpr :: String -> Maybe (Expr ())
parseExpr s =
  case unParser (pWhitespace >> pExpr) (removeComments s) of
    Just (expr, "") -> Just expr
    _ -> Nothing


removeComments :: String -> String
removeComments s =
  unlines (removeComment <$> lines s)


removeComment :: String -> String
removeComment ('#' : _) = ""
removeComment (c : r) = c : removeComment r
removeComment [] = ""

