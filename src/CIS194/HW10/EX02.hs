module CIS194.HW10.EX02 where

import           CIS194.HW10.AParser
import           CIS194.HW10.EX01

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
