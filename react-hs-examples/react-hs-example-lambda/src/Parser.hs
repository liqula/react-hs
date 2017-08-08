module Parser where


import Control.Applicative


newtype Parser a = Parser { unParser :: String -> Maybe (a, String) }


instance Functor Parser where
  fmap f (Parser g) = Parser $ \s ->
    case g s of
      Nothing -> Nothing
      Just (a, s') -> Just (f a, s')


instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  Parser gf <*> Parser ga = Parser $ \s ->
    case gf s of
      Nothing -> Nothing
      Just (f, s') -> case ga s' of
        Nothing -> Nothing
        Just (a, s'') -> Just (f a, s'')


instance Monad Parser where
  return = pure
  Parser g >>= f = Parser $ \s ->
    case g s of
      Nothing -> Nothing
      Just (a, s') -> unParser (f a) s'


instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser f <|> Parser g = Parser $ \s ->
    case f s of
      Nothing -> g s
      r -> r


item :: Parser Char
item = Parser $ \s ->
  case s of
    x:xs -> Just (x, xs)
    _    -> Nothing


sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \a -> if p a then return a else empty

