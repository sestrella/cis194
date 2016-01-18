module CIS194.HW10.Applicative where

import           CIS194.HW10.AParser
import           Control.Applicative
import           Data.Char

instance Functor Parser where
  fmap f (Parser a) = Parser $ fmap (first f) . a

first f (x, y) = (f x, y)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (Parser f) <*> (Parser a) = Parser $ fmap (foo a) f

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser bar
    where
      bar s = p1 s <|> p2 s

foo :: (String -> Maybe (a, String)) -> Maybe (a -> b, String) -> Maybe (b, String)
foo _ Nothing = Nothing
foo a (Just (f, s)) = fmap (\(z, s1) -> (f z, s1)) (a s)

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = char 'a' *> char 'b' *> pure ()

intPair :: Parser [Integer]
intPair = pair <$> posInt <*> (char ' ' *> posInt)
  where
   pair x y = [x, y]

intOrUppercase :: Parser ()
intOrUppercase = int <|> uppercase
  where
    int = posInt *> pure ()
    uppercase = satisfy isUpper *> pure ()
