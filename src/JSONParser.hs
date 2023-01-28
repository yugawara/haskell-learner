-- start snippet imports
-- https://abhinavsarkar.net/code/jsonparser.html
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module JSONParser where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)
import Data.Char (chr, digitToInt, isDigit, isHexDigit, isSpace, ord)
import Data.Functor (($>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Numeric (showHex)
import Test.QuickCheck hiding (Negative, Positive)

-- end snippet imports

-- start snippet type
data JValue
  = JNull
  | JBool Bool
  | JString String
  | JNumber {int :: Integer, frac :: [Int], exponent :: Integer}
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Eq, Generic, Show)

-- end snippet type

-- start snippet show-instance
-- instance Show JValue where
--   show value = case value of
--     JNull          -> "null"
--     JBool True     -> "true"
--     JBool False    -> "false"
--     JString s      -> showJSONString s
--     JNumber s [] 0 -> show s
--     JNumber s f 0  -> show s ++ "." ++ concatMap show f
--     JNumber s [] e -> show s ++ "e" ++ show e
--     JNumber s f e  -> show s ++ "." ++ concatMap show f ++ "e" ++ show e
--     JArray a       -> "[" ++ intercalate ", " (map show a) ++ "]"
--     JObject o      -> "{" ++ intercalate ", " (map showKV o) ++ "}"
--     where
--       showKV (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

showJSONChar :: Char -> String
showJSONChar c = case c of
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/' -> "\\/"
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
  concat
    <$> listOf
      ( oneof
          [ vectorOf 1 arbitraryUnicodeChar,
            escapedUnicodeChar
          ]
      )
  where
    escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)
    hexDigitLetters = ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']

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
jValueGen n =
  if n < 5
    then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
    else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
  where
    scalarGens = [jNullGen, jBoolGen, jNumberGen, jStringGen]
    compositeGens n = [jArrayGen n, jObjectGen n]

-- end snippet valuegen

-- start snippet value-arbitrary
instance Arbitrary JValue where
  arbitrary = sized jValueGen
  shrink = genericShrink

-- end snippet value-arbitrary

-- start snippet stringify
jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
  scale (round . sqrt . fromIntegral)
    . listOf
    . elements
    $ [' ', '\n', '\r', '\t']

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
      _ -> return $ show value

    stringifyKV (k, v) =
      surround <$> pad (pure $ showJSONString k) <*> stringify v <*> pure ":"

-- end snippet stringify

-- start snippet parser
newtype Parser i o = Parser {runParser :: i -> Maybe (i, o)}

-- end snippet parser

-- start snippet char-parser-1
char1 :: Char -> Parser String Char
char1 c = Parser $ \case
  (x : xs) | x == c -> Just (xs, x)
  _ -> Nothing

-- end snippet char-parser-1

-- start snippet char-parser-2
satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \case
  (x : xs) | predicate x -> Just (xs, x)
  _ -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

-- end snippet char-parser-2

-- start snippet digit-parser1
digit1 :: Parser String Int
digit1 = Parser $ \i -> case runParser (satisfy isDigit) i of
  Nothing -> Nothing
  Just (i', o) -> Just (i', digitToInt o)

-- end snippet digit-parser1

-- start snippet digit-parser2
digit2 :: Parser String Int
digit2 = Parser $ \i -> case runParser (satisfy isDigit) i of
  Nothing -> Nothing
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
  "" -> Parser $ \i -> Just (i, "")
  (c : cs) -> Parser $ \i -> case runParser (char c) i of
    Nothing -> Nothing
    Just (rest, _) -> case runParser (string1 cs) rest of
      Nothing -> Nothing
      Just (rest', _) -> Just (rest', c : cs)

-- end snippet string-parser1

-- start snippet string-parser2
string2 :: String -> Parser String String
string2 s = case s of
  "" -> Parser $ pure . (,"")
  (c : cs) -> Parser $ \i -> case runParser (char c) i of
    Nothing -> Nothing
    Just (rest, c) -> fmap (c :) <$> runParser (string2 cs) rest

-- end snippet string-parser2

-- start snippet applicative
instance Applicative (Parser i) where
  pure x = Parser $ pure . (,x)
  pf <*> po = Parser $ \input -> case runParser pf input of
    Nothing -> Nothing
    Just (rest, f) -> fmap f <$> runParser po rest

-- end snippet applicative

-- start snippet string-parser3
string :: String -> Parser String String
string "" = pure ""
string (c : cs) = (:) <$> char c <*> string cs

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
jBool =
  string "true" $> JBool True
    <|> string "false" $> JBool False

-- end snippet bool-parser

-- start snippet bool-parser-alt
lookahead :: Parser String Char
lookahead = Parser $ \case
  i@(x : _) -> Just (i, x)
  _ -> Nothing

jBoolAlt :: Parser String JValue
jBoolAlt = do
  c <- lookahead
  JBool <$> case c of
    't' -> string "true" $> True
    'f' -> string "false" $> False
    _ -> empty

-- end snippet bool-parser-alt

-- start snippet jchar-parser
jsonChar :: Parser String Char
jsonChar =
  string "\\\"" $> '"'
    <|> string "\\\\" $> '\\'
    <|> string "\\/" $> '/'
    <|> string "\\b" $> '\b'
    <|> string "\\f" $> '\f'
    <|> string "\\n" $> '\n'
    <|> string "\\r" $> '\r'
    <|> string "\\t" $> '\t'
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
    Nothing -> Nothing
    Just (rest, o) -> runParser (f o) rest

-- end snippet monad

-- start snippet string-parser
jString :: Parser String JValue
jString = JString <$> (char '"' *> jString') -- 1
  where
    jString' = do
      optFirst <- optional jsonChar -- 2
      case optFirst of
        Nothing -> "" <$ char '"' -- 3
        Just first
          | not (isSurrogate first) -> -- 4
              (first :) <$> jString' -- 5
        Just first -> do
          -- 6
          second <- jsonChar -- 7
          if isHighSurrogate first && isLowSurrogate second -- 8
            then (combineSurrogates first second :) <$> jString' -- 9
            else empty -- 10
            -- end snippet string-parser

-- start snippet string-parser-helper
highSurrogateLowerBound, highSurrogateUpperBound :: Int
highSurrogateLowerBound = 0xD800
highSurrogateUpperBound = 0xDBFF

lowSurrogateLowerBound, lowSurrogateUpperBound :: Int
lowSurrogateLowerBound = 0xDC00
lowSurrogateUpperBound = 0xDFFF

isHighSurrogate, isLowSurrogate, isSurrogate :: Char -> Bool
isHighSurrogate a =
  ord a >= highSurrogateLowerBound && ord a <= highSurrogateUpperBound
isLowSurrogate a =
  ord a >= lowSurrogateLowerBound && ord a <= lowSurrogateUpperBound
isSurrogate a = isHighSurrogate a || isLowSurrogate a

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b =
  chr $
    ((ord a - highSurrogateLowerBound) `shiftL` 10)
      + (ord b - lowSurrogateLowerBound)
      + 0x10000

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
      Nothing -> False
      Just (_, o) -> o == js

-- end snippet string-parser-test

-- start snippet int-parser
jUInt :: Parser String Integer
jUInt =
  (\d ds -> digitsToNumber 10 0 (d : ds)) <$> digit19 <*> digits
    <|> fromIntegral <$> digit

digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

digits :: Parser String [Int]
digits = some digit

jInt' :: Parser String Integer
jInt' = signInt <$> optional (char '-') <*> jUInt

signInt :: Maybe Char -> Integer -> Integer
signInt (Just '-') i = negate i
signInt _ i = i

-- end snippet int-parser

-- start snippet fracexp-parser
jFrac :: Parser String [Int]
jFrac = char '.' *> digits

jExp :: Parser String Integer
jExp =
  (char 'e' <|> char 'E')
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
      Nothing -> False
      Just (_, o) -> o == jn

-- end snippet number-parser-test

-- start snippet array-parser-helper
surroundedBy ::
  Parser String a -> Parser String b -> Parser String a
surroundedBy p1 p2 = p2 *> p1 <* p2

separatedBy :: Parser i v -> Parser i s -> Parser i [v]
separatedBy v s =
  (:) <$> v <*> many (s *> v)
    <|> pure []

spaces :: Parser String String
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t')

-- end snippet array-parser-helper

-- start snippet array-parser
jArray :: Parser String JValue
jArray =
  JArray
    <$> ( char '['
            *> (jValue `separatedBy` char ',' `surroundedBy` spaces)
            <* char ']'
        )

-- end snippet array-parser

-- start snippet array-parser-test
prop_genParseJArray :: Property
prop_genParseJArray =
  forAllShrink (sized jArrayGen) shrink $ \ja -> do
    jas <- dropWhile isSpace <$> stringify ja
    return . counterexample (show jas) $ case runParser jArray jas of
      Nothing -> False
      Just (_, o) -> o == ja

-- end snippet array-parser-test

-- start snippet object-parser
jObject :: Parser String JValue
jObject =
  JObject
    <$> (char '{' *> pair `separatedBy` char ',' `surroundedBy` spaces <* char '}')
  where
    pair =
      (\ ~(JString s) j -> (s, j))
        <$> (jString `surroundedBy` spaces)
        <* char ':'
        <*> jValue

-- end snippet object-parser

-- start snippet object-parser-test
prop_genParseJObject :: Property
prop_genParseJObject =
  forAllShrink (sized jObjectGen) shrink $ \jo -> do
    jos <- dropWhile isSpace <$> stringify jo
    return . counterexample (show jos) $ case runParser jObject jos of
      Nothing -> False
      Just (_, o) -> o == jo

-- end snippet object-parser-test

-- start snippet value-parser
jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
  where
    jValue' =
      jNull
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
        'n' -> jNull
        't' -> jBool
        'f' -> jBool
        '\"' -> jString
        '[' -> jArray
        '{' -> jObject
        _ -> jNumber

-- end snippet value-parser-alt

-- start snippet parsejson
parseJSON :: String -> Maybe JValue
parseJSON s = case runParser jValue s of
  Just ("", j) -> Just j
  _ -> Nothing

-- end snippet parsejson

-- start snippet parsejson-test
prop_genParseJSON :: Property
prop_genParseJSON = forAllShrink (sized jValueGen) shrink $ \value -> do
  json <- stringify value
  return . counterexample (show json) . (== Just value) . parseJSON $ json

-- end snippet parsejson-test

readPJ = do
  s <- readFile "next.js.package.json"
  return s

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

vvvv = Just (JObject [("name", JString "nextjs-project"), ("version", JString "0.0.0"), ("private", JBool True), ("workspaces", JArray [JString "packages/*"]), ("scripts", JObject [("new-error", JString "plop error"), ("new-test", JString "plop test"), ("clean", JString "pnpm lerna clean -y && pnpm lerna bootstrap && pnpm lerna run clean && pnpm lerna exec 'rm -rf ./dist'"), ("build", JString "turbo run build"), ("lerna", JString "lerna"), ("dev", JString "turbo run dev --parallel"), ("test-types", JString "pnpm tsc"), ("test-unit", JString "pnpm jest test/unit/"), ("test-dev", JString "cross-env NEXT_TEST_MODE=dev pnpm testheadless"), ("test-start", JString "cross-env NEXT_TEST_MODE=start pnpm testheadless"), ("test-deploy", JString "cross-env NEXT_TEST_MODE=deploy pnpm testheadless"), ("testonly-dev", JString "cross-env NEXT_TEST_MODE=dev pnpm testonly"), ("testonly-start", JString "cross-env NEXT_TEST_MODE=start pnpm testonly"), ("testonly-deploy", JString "cross-env NEXT_TEST_MODE=deploy pnpm testonly"), ("test", JString "pnpm testheadless"), ("testonly", JString "pnpm jest --runInBand"), ("testheadless", JString "cross-env HEADLESS=true pnpm testonly"), ("test-pack", JString "TS_NODE_TRANSPILE_ONLY=1 node --loader ts-node/esm scripts/test-pack-package.mts"), ("genstats", JString "cross-env LOCAL_STATS=true node .github/actions/next-stats-action/src/index.js"), ("git-reset", JString "git reset --hard HEAD"), ("git-clean", JString "git clean -d -x -e node_modules -e packages -f"), ("lint-typescript", JString "turbo run typescript"), ("lint-eslint", JString "eslint . --ext js,jsx,ts,tsx --max-warnings=0 --config .eslintrc.json --no-eslintrc"), ("lint-no-typescript", JString "run-p prettier-check lint-eslint"), ("lint", JString "run-p test-types lint-typescript prettier-check lint-eslint lint-language"), ("lint-fix", JString "pnpm prettier-fix && eslint . --ext js,jsx,ts,tsx --fix --max-warnings=0 --config .eslintrc.json --no-eslintrc"), ("lint-language", JString "alex ."), ("prettier-check", JString "prettier --check ."), ("prettier-fix", JString "prettier --write ."), ("types", JString "lerna run types --stream"), ("typescript", JString "turbo run typescript"), ("prepublishOnly", JString "turbo run build"), ("publish-canary", JString "git checkout canary && git pull && lerna version prerelease --preid canary --force-publish && release --pre --skip-questions"), ("publish-stable", JString "lerna version --force-publish"), ("lint-staged", JString "lint-staged"), ("next-with-deps", JString "./scripts/next-with-deps.sh"), ("next", JString "cross-env NEXT_TELEMETRY_DISABLED=1 node --trace-deprecation --enable-source-maps packages/next/dist/bin/next"), ("next-no-sourcemaps", JString "cross-env NEXT_TELEMETRY_DISABLED=1 node --trace-deprecation packages/next/dist/bin/next"), ("clean-trace-jaeger", JString "rm -rf test/integration/basic/.next && TRACE_TARGET=JAEGER pnpm next build test/integration/basic"), ("debug", JString "cross-env NEXT_TELEMETRY_DISABLED=1 node --inspect packages/next/dist/bin/next"), ("postinstall", JString "git config feature.manyFiles true && node scripts/install-native.mjs"), ("version", JString "npx pnpm@7.24.3 install && IS_PUBLISH=yes ./scripts/check-pre-compiled.sh && git add ."), ("prepare", JString "husky install")]), ("devDependencies", JObject [("@babel/core", JString "7.18.0"), ("@babel/eslint-parser", JString "7.18.2"), ("@babel/generator", JString "7.18.0"), ("@babel/parser", JString "7.12.11"), ("@babel/plugin-proposal-object-rest-spread", JString "7.14.7"), ("@babel/preset-flow", JString "7.14.5"), ("@babel/preset-react", JString "7.14.5"), ("@edge-runtime/jest-environment", JString "2.0.0"), ("@fullhuman/postcss-purgecss", JString "1.3.0"), ("@mdx-js/loader", JString "^1.5.1"), ("@mdx-js/react", JString "^1.6.18"), ("@next/bundle-analyzer", JString "workspace:*"), ("@next/env", JString "workspace:*"), ("@next/eslint-plugin-next", JString "workspace:*"), ("@next/font", JString "workspace:*"), ("@next/mdx", JString "workspace:*"), ("@next/plugin-storybook", JString "workspace:*"), ("@next/polyfill-module", JString "workspace:*"), ("@next/polyfill-nomodule", JString "workspace:*"), ("@next/swc", JString "workspace:*"), ("@svgr/webpack", JString "5.5.0"), ("@swc/cli", JString "0.1.55"), ("@swc/core", JString "1.2.203"), ("@swc/helpers", JString "0.4.14"), ("@testing-library/react", JString "13.0.0"), ("@types/cheerio", JString "0.22.16"), ("@types/fs-extra", JString "8.1.0"), ("@types/html-validator", JString "5.0.3"), ("@types/http-proxy", JString "1.17.3"), ("@types/jest", JString "24.0.13"), ("@types/node", JString "14.14.31"), ("@types/node-fetch", JString "2.6.1"), ("@types/react", JString "16.9.17"), ("@types/react-dom", JString "16.9.4"), ("@types/relay-runtime", JString "13.0.0"), ("@types/selenium-webdriver", JString "4.0.15"), ("@types/sharp", JString "0.29.3"), ("@types/string-hash", JString "1.1.1"), ("@types/trusted-types", JString "2.0.2"), ("@typescript-eslint/eslint-plugin", JString "4.29.1"), ("@typescript-eslint/parser", JString "4.29.1"), ("@vercel/fetch", JString "6.1.1"), ("@vercel/og", JString "0.0.20"), ("@webassemblyjs/ast", JString "1.11.1"), ("@webassemblyjs/floating-point-hex-parser", JString "1.11.1"), ("@webassemblyjs/helper-api-error", JString "1.11.1"), ("@webassemblyjs/helper-buffer", JString "1.11.1"), ("@webassemblyjs/helper-code-frame", JString "npm:empty-npm-package@1.0.0"), ("@webassemblyjs/helper-module-context", JString "npm:empty-npm-package@1.0.0"), ("@webassemblyjs/helper-numbers", JString "1.11.1"), ("@webassemblyjs/helper-wasm-bytecode", JString "1.11.1"), ("@webassemblyjs/helper-wasm-section", JString "1.11.1"), ("@webassemblyjs/ieee754", JString "1.11.1"), ("@webassemblyjs/leb128", JString "1.11.1"), ("@webassemblyjs/utf8", JString "1.11.1"), ("@webassemblyjs/wasm-edit", JString "1.11.1"), ("@webassemblyjs/wasm-gen", JString "1.11.1"), ("@webassemblyjs/wasm-opt", JString "1.11.1"), ("@webassemblyjs/wasm-parser", JString "1.11.1"), ("@webassemblyjs/wast-parser", JString "npm:empty-npm-package@1.0.0"), ("@webassemblyjs/wast-printer", JString "1.11.1"), ("@zeit/next-typescript", JString "1.1.2-canary.0"), ("abort-controller", JString "3.0.0"), ("alex", JString "9.1.0"), ("amphtml-validator", JString "1.0.35"), ("async-sema", JString "3.0.1"), ("body-parser", JString "1.20.1"), ("browserslist", JString "4.20.2"), ("buffer", JString "5.6.0"), ("chalk", JString "5.0.1"), ("cheerio", JString "0.22.0"), ("cookie", JString "0.4.1"), ("cors", JString "2.8.5"), ("coveralls", JString "3.0.3"), ("create-next-app", JString "workspace:*"), ("critters", JString "0.0.6"), ("cross-env", JString "6.0.3"), ("cross-spawn", JString "6.0.5"), ("dd-trace", JString "2.3.0"), ("es5-ext", JString "0.10.53"), ("escape-string-regexp", JString "2.0.0"), ("eslint", JString "7.24.0"), ("eslint-config-next", JString "workspace:*"), ("eslint-plugin-eslint-plugin", JString "4.3.0"), ("eslint-plugin-import", JString "2.22.1"), ("eslint-plugin-jest", JString "24.3.5"), ("eslint-plugin-jsdoc", JString "39.6.4"), ("eslint-plugin-react", JString "7.23.2"), ("eslint-plugin-react-hooks", JString "4.5.0"), ("event-stream", JString "4.0.1"), ("execa", JString "2.0.3"), ("expect-type", JString "0.14.2"), ("express", JString "4.17.0"), ("faker", JString "5.5.3"), ("faunadb", JString "2.6.1"), ("find-up", JString "4.1.0"), ("firebase", JString "7.14.5"), ("flat", JString "5.0.2"), ("form-data", JString "4.0.0"), ("fs-extra", JString "9.0.0"), ("get-port", JString "5.1.1"), ("glob", JString "7.1.6"), ("gzip-size", JString "5.1.1"), ("html-validator", JString "5.1.18"), ("http-proxy", JString "1.18.1"), ("husky", JString "8.0.0"), ("image-size", JString "0.9.3"), ("is-animated", JString "2.0.2"), ("isomorphic-unfetch", JString "3.0.0"), ("jest", JString "27.0.6"), ("jest-extended", JString "1.2.1"), ("json5", JString "2.2.3"), ("ky", JString "0.19.1"), ("ky-universal", JString "0.6.0"), ("lerna", JString "4.0.0"), ("lint-staged", JString "10.1.7"), ("lodash", JString "4.17.20"), ("lost", JString "8.3.1"), ("minimatch", JString "3.0.4"), ("moment", JString "^2.24.0"), ("nanoid", JString "3.1.30"), ("next", JString "workspace:*"), ("node-fetch", JString "2.6.7"), ("npm-run-all", JString "4.1.5"), ("nprogress", JString "0.2.0"), ("pixrem", JString "5.0.0"), ("playwright-chromium", JString "1.28.1"), ("plop", JString "3.0.5"), ("postcss-nested", JString "4.2.1"), ("postcss-pseudoelements", JString "5.0.0"), ("postcss-short-size", JString "4.0.0"), ("postcss-trolling", JString "0.1.7"), ("prettier", JString "2.5.1"), ("pretty-bytes", JString "5.3.0"), ("pretty-ms", JString "7.0.0"), ("random-seed", JString "0.3.0"), ("react", JString "18.2.0"), ("react-17", JString "npm:react@17.0.2"), ("react-builtin", JString "npm:react@18.3.0-next-3ba7add60-20221201"), ("react-dom", JString "18.2.0"), ("react-dom-17", JString "npm:react-dom@17.0.2"), ("react-dom-builtin", JString "npm:react-dom@18.3.0-next-3ba7add60-20221201"), ("react-server-dom-webpack", JString "18.3.0-next-3ba7add60-20221201"), ("react-ssr-prepass", JString "1.0.8"), ("react-virtualized", JString "9.22.3"), ("relay-compiler", JString "13.0.2"), ("relay-runtime", JString "13.0.2"), ("release", JString "6.3.1"), ("request-promise-core", JString "1.1.2"), ("resolve-from", JString "5.0.0"), ("rimraf", JString "3.0.2"), ("sass", JString "1.54.0"), ("seedrandom", JString "3.0.5"), ("selenium-webdriver", JString "4.0.0-beta.4"), ("semver", JString "7.3.7"), ("shell-quote", JString "1.7.3"), ("strip-ansi", JString "6.0.0"), ("styled-components", JString "6.0.0-beta.5"), ("styled-jsx", JString "5.1.1"), ("styled-jsx-plugin-postcss", JString "3.0.2"), ("swr", JString "^2.0.0"), ("tailwindcss", JString "1.1.3"), ("taskr", JString "1.1.0"), ("tree-kill", JString "1.2.2"), ("ts-node", JString "10.9.1"), ("tsec", JString "0.2.1"), ("turbo", JString "1.6.3"), ("typescript", JString "4.8.2"), ("unfetch", JString "4.2.0"), ("wait-port", JString "0.2.2"), ("webpack", JString "5.74.0"), ("webpack-bundle-analyzer", JString "4.7.0"), ("whatwg-fetch", JString "3.0.0"), ("ws", JString "8.2.3")]), ("resolutions", JObject [("browserslist", JString "4.20.2"), ("caniuse-lite", JString "1.0.30001406"), ("@babel/core", JString "7.18.0"), ("@babel/parser", JString "7.18.0"), ("@babel/types", JString "7.18.0"), ("@babel/traverse", JString "7.18.0")]), ("engines", JObject [("node", JString ">=14.6.0")]), ("packageManager", JString "pnpm@7.24.3")])