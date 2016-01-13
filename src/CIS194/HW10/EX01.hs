module CIS194.HW10.EX01 where

import           CIS194.HW10.AParser

instance Functor Parser where
  fmap f (Parser g) = Parser (fmap (first f) . g)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)
