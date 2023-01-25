module MyStuff2 where

import Control.Applicative (Alternative (..))
import Data.List (nub)
data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)


newtype Parser i e a = Parser
  {
    runParser2 :: [i] -> Either [Error i e] (a, [i])
  }

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left [Unexpected hd]

char :: Eq i => i -> Parser i e i
char i = satisfy (== i)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)
  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Monad (Parser i e) where
  return = pure
  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser2 (k output) rest

string :: Eq i => [i] -> Parser i e [i]
string = traverse char

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]
  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

parser :: (Parser Char String String)
parser = (string "hello") <|> (string "greetings")