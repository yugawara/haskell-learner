-- start snippet imports
--https://abhinavsarkar.net/code/jsonparser.html
{-# LANGUAGE DeriveGeneric, TupleSections, LambdaCase #-}
module JSONParser where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)
import Data.Char (isDigit, isHexDigit, isSpace, chr, ord, digitToInt)
import Data.Functor (($>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Numeric (showHex)
import Test.QuickCheck hiding (Positive, Negative)
-- end snippet imports

-- start snippet type
data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { int :: Integer, frac :: [Int], exponent :: Integer }
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Generic)
-- end snippet type

-- start snippet show-instance
instance Show JValue where
  show value = case value of
    JNull          -> "null"
    JBool True     -> "true"
    JBool False    -> "false"
    JString s      -> showJSONString s
    JNumber s [] 0 -> show s
    JNumber s f 0  -> show s ++ "." ++ concatMap show f
    JNumber s [] e -> show s ++ "e" ++ show e
    JNumber s f e  -> show s ++ "." ++ concatMap show f ++ "e" ++ show e
    JArray a       -> "[" ++ intercalate ", " (map show a) ++ "]"
    JObject o      -> "{" ++ intercalate ", " (map showKV o) ++ "}"
    where
      showKV (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

showJSONChar :: Char -> String
showJSONChar c = case c of
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/'  -> "\\/"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c
  _ -> [c]
  where
    showJSONNonASCIIChar c =
      let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a
-- end snippet show-instance

-- start snippet scalargens
jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary

jNumberGen :: Gen JValue
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary
-- end snippet scalargens

-- start snippet stringgen
jsonStringGen :: Gen String
jsonStringGen =
  concat <$> listOf (oneof [ vectorOf 1 arbitraryUnicodeChar
                           , escapedUnicodeChar ])
  where
    escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)
    hexDigitLetters    = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

jStringGen :: Gen JValue
jStringGen = JString <$> jsonStringGen
-- end snippet stringgen

-- start snippet compositegens
jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)

jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale (`div` 2) . listOf . objKV . (`div` 2)
  where
    objKV n = (,) <$> jsonStringGen <*> jValueGen n
-- end snippet compositegens

-- start snippet valuegen
jValueGen :: Int -> Gen JValue
jValueGen n = if n < 5
  then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
  else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
  where
    scalarGens      = [jNullGen , jBoolGen , jNumberGen , jStringGen]
    compositeGens n = [jArrayGen n, jObjectGen n]
-- end snippet valuegen

-- start snippet value-arbitrary
instance Arbitrary JValue where
  arbitrary = sized jValueGen
  shrink    = genericShrink
-- end snippet value-arbitrary

-- start snippet stringify
jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
  scale (round . sqrt . fromIntegral)
  . listOf
  . elements
  $ [' ' , '\n' , '\r' , '\t']

stringify :: JValue -> Gen String
stringify = pad . go
  where
    surround l r j = l ++ j ++ r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . intercalate ","

    go value = case value of
      JArray elements ->
        mapM (pad . stringify) elements
          >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        mapM stringifyKV kvs >>= fmap (surround "{" "}") . commaSeparated
      _           -> return $ show value

    stringifyKV (k, v) =
      surround <$> pad (pure $ showJSONString k) <*> stringify v <*> pure ":"
-- end snippet stringify

-- start snippet parser
newtype Parser i o =
  Parser { runParser :: i -> Maybe (i, o) }
-- end snippet parser

-- start snippet char-parser-1
char1 :: Char -> Parser String Char
char1 c = Parser $ \case
  (x:xs) | x == c -> Just (xs, x)
  _               -> Nothing
-- end snippet char-parser-1

-- start snippet char-parser-2
satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \case
  (x:xs) | predicate x -> Just (xs, x)
  _                    -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)
-- end snippet char-parser-2

-- start snippet digit-parser1
digit1 :: Parser String Int
digit1 = Parser $ \i -> case runParser (satisfy isDigit) i of
  Nothing      -> Nothing
  Just (i', o) -> Just (i', digitToInt o)
-- end snippet digit-parser1

-- start snippet digit-parser2
digit2 :: Parser String Int
digit2 = Parser $ \i -> case runParser (satisfy isDigit) i of
  Nothing      -> Nothing
  Just (i', o) -> Just . fmap digitToInt $ (i', o)

digit3 :: Parser String Int
digit3 = Parser $ \i -> fmap (fmap digitToInt) . runParser (satisfy isDigit) $ i
-- end snippet digit-parser2

-- start snippet functor
instance Functor (Parser i) where
  fmap f parser = Parser $ fmap (fmap f) . runParser parser
-- end snippet functor

-- start snippet digit-parser3
digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit
-- end snippet digit-parser3

-- start snippet string-parser1
string1 :: String -> Parser String String
string1 s = case s of
  ""     -> Parser $ \i -> Just (i, "")
  (c:cs) -> Parser $ \i -> case runParser (char c) i of
    Nothing        -> Nothing
    Just (rest, _) -> case runParser (string1 cs) rest of
      Nothing         -> Nothing
      Just (rest', _) -> Just (rest', c:cs)
-- end snippet string-parser1

-- start snippet string-parser2
string2 :: String -> Parser String String
string2 s = case s of
  ""     -> Parser $ pure . (, "")
  (c:cs) -> Parser $ \i -> case runParser (char c) i of
    Nothing        -> Nothing
    Just (rest, c) -> fmap (c:) <$> runParser (string2 cs) rest
-- end snippet string-parser2

-- start snippet applicative
instance Applicative (Parser i) where
  pure x    = Parser $ pure . (, x)
  pf <*> po = Parser $ \input -> case runParser pf input of
    Nothing        -> Nothing
    Just (rest, f) -> fmap f <$> runParser po rest
-- end snippet applicative

-- start snippet string-parser3
string :: String -> Parser String String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs
-- end snippet string-parser3

-- start snippet null-parser
jNull :: Parser String JValue
jNull = string "null" $> JNull
-- end snippet null-parser

-- start snippet alternative
instance Alternative (Parser i) where
  empty = Parser $ const empty
  p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input
-- end snippet alternative

-- start snippet bool-parser
jBool :: Parser String JValue
jBool =   string "true"  $> JBool True
      <|> string "false" $> JBool False
-- end snippet bool-parser

-- start snippet bool-parser-alt
lookahead :: Parser String Char
lookahead = Parser $ \case
  i@(x:_) -> Just (i, x)
  _       -> Nothing

jBoolAlt :: Parser String JValue
jBoolAlt = do
  c <- lookahead
  JBool <$> case c of
    't' -> string "true"  $> True
    'f' -> string "false" $> False
    _   -> empty
-- end snippet bool-parser-alt

-- start snippet jchar-parser
jsonChar :: Parser String Char
jsonChar =   string "\\\"" $> '"'
         <|> string "\\\\" $> '\\'
         <|> string "\\/"  $> '/'
         <|> string "\\b"  $> '\b'
         <|> string "\\f"  $> '\f'
         <|> string "\\n"  $> '\n'
         <|> string "\\r"  $> '\r'
         <|> string "\\t"  $> '\t'
         <|> unicodeChar
         <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
  where
    unicodeChar =
      chr . fromIntegral . digitsToNumber 16 0
        <$> (string "\\u" *> replicateM 4 hexDigit)

    hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base =
  foldl (\num d -> num * fromIntegral base + fromIntegral d)
-- end snippet jchar-parser

-- start snippet monad
instance Monad (Parser i) where
  p >>= f = Parser $ \input -> case runParser p input of
    Nothing        -> Nothing
    Just (rest, o) -> runParser (f o) rest
-- end snippet monad

-- start snippet string-parser
jString :: Parser String JValue
jString = JString <$> (char '"' *> jString')                   -- 1
  where
    jString' = do
      optFirst <- optional jsonChar                            -- 2
      case optFirst of
        Nothing -> "" <$ char '"'                              -- 3
        Just first | not (isSurrogate first) ->                -- 4
          (first:) <$> jString'                                -- 5
        Just first -> do                                       -- 6
          second <- jsonChar                                   -- 7
          if isHighSurrogate first && isLowSurrogate second    -- 8
          then (combineSurrogates first second :) <$> jString' -- 9
          else empty                                           -- 10
-- end snippet string-parser

-- start snippet string-parser-helper
highSurrogateLowerBound, highSurrogateUpperBound :: Int
highSurrogateLowerBound = 0xD800
highSurrogateUpperBound = 0xDBFF

lowSurrogateLowerBound, lowSurrogateUpperBound :: Int
lowSurrogateLowerBound  = 0xDC00
lowSurrogateUpperBound  = 0xDFFF

isHighSurrogate, isLowSurrogate, isSurrogate :: Char -> Bool
isHighSurrogate a =
  ord a >= highSurrogateLowerBound && ord a <= highSurrogateUpperBound
isLowSurrogate a  =
  ord a >= lowSurrogateLowerBound && ord a <= lowSurrogateUpperBound
isSurrogate a     = isHighSurrogate a || isLowSurrogate a

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b = chr $
  ((ord a - highSurrogateLowerBound) `shiftL` 10)
  + (ord b - lowSurrogateLowerBound) + 0x10000
-- end snippet string-parser-helper

-- 1 consume the starting quote
-- 2 get first (optional) char
-- 3 no first char => empty string ending with quote
-- 4 first char is not surrogate
-- 5 process rest
-- 6 first char is surrogate
-- 7 get second char
-- 8 valid surrogate pair
-- 9 combine surrogates, process rest
-- 10 invalid surrogate, fail

-- start snippet string-parser-test
prop_genParseJString :: Property
prop_genParseJString =
  forAllShrink jStringGen shrink $ \js ->
    case runParser jString (show js) of
      Nothing     -> False
      Just (_, o) -> o == js
-- end snippet string-parser-test

-- start snippet int-parser
jUInt :: Parser String Integer
jUInt =   (\d ds -> digitsToNumber 10 0 (d:ds)) <$> digit19 <*> digits
      <|> fromIntegral <$> digit

digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

digits :: Parser String [Int]
digits = some digit

jInt' :: Parser String Integer
jInt' = signInt <$> optional (char '-') <*> jUInt

signInt :: Maybe Char -> Integer -> Integer
signInt (Just '-') i = negate i
signInt _          i = i
-- end snippet int-parser

-- start snippet fracexp-parser
jFrac :: Parser String [Int]
jFrac = char '.' *> digits

jExp :: Parser String Integer
jExp = (char 'e' <|> char 'E')
  *> (signInt <$> optional (char '+' <|> char '-') <*> jUInt)
-- end snippet fracexp-parser

-- start snippet intfracexp-parser
jInt :: Parser String JValue
jInt = JNumber <$> jInt' <*> pure [] <*> pure 0

jIntExp :: Parser String JValue
jIntExp = JNumber <$> jInt' <*> pure [] <*> jExp

jIntFrac :: Parser String JValue
jIntFrac = (\i f -> JNumber i f 0) <$> jInt' <*> jFrac

jIntFracExp :: Parser String JValue
jIntFracExp = (\ ~(JNumber i f _) e -> JNumber i f e) <$> jIntFrac <*> jExp
-- end snippet intfracexp-parser

-- start snippet number-parser
jNumber :: Parser String JValue
jNumber = jIntFracExp <|> jIntExp <|> jIntFrac <|> jInt
-- end snippet number-parser

-- start snippet number-parser-test
prop_genParseJNumber :: Property
prop_genParseJNumber =
  forAllShrink jNumberGen shrink $ \jn ->
    case runParser jNumber (show jn) of
      Nothing     -> False
      Just (_, o) -> o == jn
-- end snippet number-parser-test

-- start snippet array-parser-helper
surroundedBy ::
  Parser String a -> Parser String b -> Parser String a
surroundedBy p1 p2 = p2 *> p1 <* p2

separatedBy :: Parser i v -> Parser i s -> Parser i [v]
separatedBy v s =   (:) <$> v <*> many (s *> v)
                <|> pure []

spaces :: Parser String String
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t')
-- end snippet array-parser-helper

-- start snippet array-parser
jArray :: Parser String JValue
jArray = JArray <$>
  (char '['
   *> (jValue `separatedBy` char ',' `surroundedBy` spaces)
   <* char ']')
-- end snippet array-parser

-- start snippet array-parser-test
prop_genParseJArray :: Property
prop_genParseJArray =
  forAllShrink (sized jArrayGen) shrink $ \ja -> do
    jas <- dropWhile isSpace <$> stringify ja
    return . counterexample (show jas) $ case runParser jArray jas of
      Nothing     -> False
      Just (_, o) -> o == ja
-- end snippet array-parser-test

-- start snippet object-parser
jObject :: Parser String JValue
jObject = JObject <$>
  (char '{' *> pair `separatedBy` char ',' `surroundedBy` spaces <* char '}')
  where
    pair = (\ ~(JString s) j -> (s, j))
      <$> (jString `surroundedBy` spaces)
      <*  char ':'
      <*> jValue
-- end snippet object-parser

-- start snippet object-parser-test
prop_genParseJObject :: Property
prop_genParseJObject =
  forAllShrink (sized jObjectGen) shrink $ \jo -> do
    jos <- dropWhile isSpace <$> stringify jo
    return . counterexample (show jos) $ case runParser jObject jos of
      Nothing     -> False
      Just (_, o) -> o == jo
-- end snippet object-parser-test

-- start snippet value-parser
jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
  where
    jValue' =   jNull
            <|> jBool
            <|> jString
            <|> jNumber
            <|> jArray
            <|> jObject
-- end snippet value-parser

-- start snippet value-parser-alt
jValueAlt ::
  Parser String JValue
jValueAlt =
  jValue' `surroundedBy` spaces
  where
    jValue' = do
      c <- lookahead
      case c of
        'n'  -> jNull
        't'  -> jBool
        'f'  -> jBool
        '\"' -> jString
        '['  -> jArray
        '{'  -> jObject
        _    -> jNumber
-- end snippet value-parser-alt

-- start snippet parsejson
parseJSON :: String -> Maybe JValue
parseJSON s = case runParser jValue s of
  Just ("", j) -> Just j
  _            -> Nothing
-- end snippet parsejson

-- start snippet parsejson-test
prop_genParseJSON :: Property
prop_genParseJSON = forAllShrink (sized jValueGen) shrink $ \value -> do
  json <- stringify value
  return . counterexample (show json) . (== Just value) . parseJSON $ json
-- end snippet parsejson-test

-- start snippet run-test
runTests :: IO ()
runTests = do
  putStrLn "== prop_genParseJString =="
  quickCheck prop_genParseJString

  putStrLn "== prop_genParseJNumber =="
  quickCheck prop_genParseJNumber

  putStrLn "== prop_genParseJArray =="
  quickCheck prop_genParseJArray

  putStrLn "== prop_genParseJObject =="
  quickCheck prop_genParseJObject

  putStrLn "== prop_genParseJSON =="
  quickCheck prop_genParseJSON
-- end snippet run-test