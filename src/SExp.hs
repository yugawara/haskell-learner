module SExp where

import Data.Scientific
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Identifier = Identifier
  { getId :: String
  } deriving (Show)

data SExp
  = SSExp    SExp [SExp]  -- (foo 0 "hello" bar), (bar (baz 1)), (foo)
  | SInteger Integer  -- 42
  | SString  String  -- "hello, world"
  | SBool    Bool  -- false, true
  | SId      Identifier  -- foo
  | SDouble  Double  -- 42f, 3.1415f
  deriving (Show)

type Parser = Parsec
  Void  -- The type for custom error messages. We have none, so use `Void`.
  String  -- The input stream type. Let's use `String` for now, but for better performance, you might want to use `Text` or `ByteString`.

-- * Helpers

skipSpace :: Parser ()
skipSpace = L.space
  -- Like `space`, but skips 1 or more space characters.
  space1
  -- Skip from ;; until a newline.
  (L.skipLineComment ";;")
  -- Skip from /* until */. There is also `skipBlockComment`, but it doesn't handle nested comments.
  (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parseSExp :: String -> Either String SExp
parseSExp input =
  let
    outputE = parse
      -- Skip any whitespace at the beginning, expect the end of input after the atom.
      (between skipSpace eof atom)
      -- Name of the source file, you can give it any name you want. I leave it blank for now.
      ""
      -- The actual string to be parsed
      input
  in
  -- If we get Left, it will be an `ParseErrorBundle`, let's pretty print it for now.
  -- If we get Right, do nothing.
  case outputE of
    Left err -> Left $ errorBundlePretty err
    Right output -> Right output

-- * Parsers

bool :: Parser Bool
bool = label "boolean" $ lexeme $ False <$ string "false" <|> True <$ string "true"

str :: Parser String
str = label "string" $ lexeme $ char '"' *> manyTill L.charLiteral (char '"')

identifier :: Parser Identifier
identifier = label "identifier" $ lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_'
  pure $ Identifier $ first : rest

sexp :: Parser (SExp, [SExp])
sexp = label "S-expression" $ lexeme $
  between (lexeme (char '(')) (char ')') ((,) <$> atom <*> many atom)

numeric :: Parser SExp
numeric = label "number" $ lexeme $ do
  value <- L.signed skipSpace L.scientific
  case floatingOrInteger value of
    Left d -> SDouble d <$ char' 'f'
    Right i -> do
      f <- optional $ char' 'f'
      pure $ case f of
        -- Note: 1.0 is an integer, but 1.1 is a parser error.
        Nothing -> SInteger i
        Just _ -> SDouble $ fromIntegral i

atom :: Parser SExp
atom = choice
  [ SBool <$> bool
  , SString <$> str
  , SId <$> identifier
  , uncurry SSExp <$> sexp
  , numeric
  ]
