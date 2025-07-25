{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad.State
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Void
import Lexer.LexerRunner
import Lexer.Tokens (TokenInfo)
import Parser.Parser
import Parser.ParserBase
import Parser.Types (Problem, Section, Type, TypeEnvironment, getNameFromProblem)
import Suggestions.Suggestions
import Test.Hspec
import Text.Megaparsec
import TypeInference.TypeInference (MaybeError (..))
import TypeInference.TypeInferenceUtil
import Text.RawString.QQ
-- the fact that this is not efficient should not matter.
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

main :: IO ()
main = hspec $ do
  describeBasicSuggestions
  describeLambdaSuggestions
  describeLetExpressions

describeBasicSuggestions :: Spec
describeBasicSuggestions = describe "basic code suggestions" $ do
  it "removing extra parenthesis from simple addition" $ do
    runSuggestion "fun = plus 2 4)" `shouldBe` "fun = plus 2 4 , Int"

  it "respect unnecessary brackets" $ do
    runSuggestion "fun = (plus 2) 4)" `shouldBe` "fun = (plus 2) 4 , Int"

  it "brackets for partial application if required" $ do
    runSuggestion "fun = trice (plus 2 4" `shouldBe` "fun = trice (plus 2) 4 , Int"

  it "swap arguments" $ do
    runSuggestion "fun = invertNum 4 True" `shouldBe` "fun = invertNum True 4 , Int"

  it "nested brackets" $ do
    runSuggestion "fun = plus plus plus 4 5 plus plus plus 2 3 4 5 6" `shouldBe` "fun = plus (plus (plus 4 5) (plus (plus (plus 2 3) 4) 5)) 6 , Int"

  it "suggestion for partial application" $ do
    runSuggestion "fun = plus plus 4 4 " `shouldBe` "fun = plus (plus 4 4) , (Int -> Int)"

  it "swapping twice" $ do
    runSuggestion "fun = invertNum invertNum 6 True False" `shouldBe` "fun = invertNum False (invertNum True 6) , Int"

describeLambdaSuggestions :: Spec
describeLambdaSuggestions =
  describe "Lambda expressions code suggestions" $ do
    it "basic lambda parenthesis " $ do
      runSuggestion "fun = \\x -> plus x x)" `shouldBe` "fun = (\\x -> plus x x) , (Int -> Int)"

    it "basic lambda applied " $ do
      runSuggestion "fun = \\x -> plus x x 5" `shouldBe` "fun = (\\x -> plus x x) 5 , Int"

    it "multiple input lambda  " $ do
      runSuggestion "fun = \\x y -> plus x y) 5" `shouldBe` "fun = (\\x y -> plus x y) 5 , (Int -> Int)"

    it "respect subdividing lambda" $ do
      runSuggestion "fun = \\x -> (\\y -> plus x y 5" `shouldBe` "fun = (\\x -> (\\y -> plus x y) 5) , (Int -> Int)"

    it "no brackets at all" $ do
      runSuggestion "fun = \\x -> \\y -> plus x y 5" `shouldBe` "fun = (\\x -> (\\y -> plus x y) 5) , (Int -> Int)"

    it "lambda as argument " $ do
      runSuggestion "fun = trice \\x -> plus x x 5" `shouldBe` "fun = trice (\\x -> plus x x) 5 , Int"

    it "nested lambda " $ do
      runSuggestion "fun = trice \\x -> trice \\y -> plus y y x 5" `shouldBe` "fun = trice (\\x -> trice (\\y -> plus y y) x) 5 , Int"

    it " = instead of -> " $ do
      runSuggestion "fun = trice \\x = plus 5 x 8)" `shouldBe` "fun = trice (\\x -> plus 5 x) 8 , Int"

    it "missing -> with set goal " $ do
      runSuggestion "fun = iterate \\x plus 5 x 8 2" `shouldBe` "fun = iterate (\\x -> plus 5 x) 8 2 , Int"

    it "swap with set goal " $ do
      runSuggestion "fun = iterate \\x invertNum x True 8 2" `shouldBe` "fun = iterate (\\x -> invertNum True x) 8 2 , Int"

    it "partially applied expression in lambda body" $ do
      runSuggestion "combiner :: (Int -> Int -> Int) -> Int -> (Int -> Int)\nfun = combiner \\x -> plus 4 5 6)" `shouldBe` "fun = combiner (\\x -> plus 4) 5 6 , Int"

    it "lambda shadowing" $ do
      runSuggestion "applyIf :: (Int -> Int) -> Bool -> Int -> Int\nx :: Bool \nfun = applyIf \\x -> plus 4 x x) 5" `shouldBe` "fun = applyIf (\\x -> plus 4 x) x 5 , Int"

describeLetExpressions :: Spec
describeLetExpressions = describe "Let expressions code suggestions" $ do
  it "acknowledging lets " $ do
    runSuggestion [r|
fun = 
    let
        x = 5
    in  plus 7 7)
|] `shouldBe` "fun = \n    let \n        x = 5 \n    in plus 7 7 , Int"

  it "acknowledging lets and process types" $ do
    runSuggestion [r|
fun = 
    let
        x = 5
    in  plus x x)
|] `shouldBe` "fun = \n    let \n        x = 5 \n    in plus x x , Int"

  it "dependencies of lets" $ do
    runSuggestion [r|
fun x = 
    let
        fun2 = 5
        fun3 = plus x fun2
    in 
        fun3)
|] `shouldBe` "fun x = \n    let \n        fun2 = 5 \n        fun3 = plus x fun2 \n    in \n        fun3 , (Int -> Int)"
  
  it "dependencies of lets (other oder)" $ do
    runSuggestion [r|
fun x = 
    let
        fun3 = plus x fun2
        fun2 = 5
    in 
        fun3)
|] `shouldBe` "fun x = \n    let \n        fun3 = plus x fun2 \n        fun2 = 5 \n    in \n        fun3 , (Int -> Int)"


  it "getting type information from the other definitions" $ do
    runSuggestion [r|
fun x = 
    let
        fun2 = plus fun3 invertNum x True
        fun3 = plus x 5
    in 
        fun3
|] `shouldBe` "fun x = \n    let \n        fun2 = plus fun3 (invertNum True x) \n        fun3 = plus x 5 \n    in \n        fun3 , (Int -> Int)"


  it "getting type information from the other definitions (other order)" $ do
    runSuggestion [r|
fun x = 
    let
        fun3 = plus x 5
        fun2 = plus fun3 invertNum x True
    in 
        fun3
|] `shouldBe` "fun x = \n    let \n        fun3 = plus x 5 \n        fun2 = plus fun3 (invertNum True x) \n    in \n        fun3 , (Int -> Int)"


  it "type information is gotten top down in undecidable cases" $ do
    runSuggestion [r|
fun x = 
    let
        fun3 = plus x 5
        fun2 = invertNum x True
    in 
        fun3
|] `shouldBe` "fun x = \n    let \n        fun3 = plus x 5 \n        fun2 = invertNum True x \n    in \n        fun3 , (Int -> Int)"

  it "type information is gotten top down in undecidable cases (the one that doesn't work)" $ do
    take 51 (runSuggestion [r|
fun x = 
    let
        fun2 = invertNum x True
        fun3 = plus x 5
    in 
        fun3
|] )`shouldNotBe` "fun x = \n    let \n        fun2 = invertNum True x"


  it "weird indentation" $ do
    runSuggestion [r|
fun x = 
    let
        fun3 = 4
            fun2 = 5
              fun4 = True
    in 
        fun3
|] `shouldBe` "fun x = \n    let \n        fun3 = 4 \n        fun2 = 5 \n        fun4 = True \n    in \n        fun3 , (v8 -> Int)"

  it "weird indentation 2" $ do
    runSuggestion [r|
fun x = 
    let
        fun3 = 4
            fun2 = 5
          fun4 = True
    in 
        fun3
|] `shouldBe` "fun x = \n    let \n        fun3 = 4 \n        fun2 = 5 \n        fun4 = True \n    in \n        fun3 , (v8 -> Int)"

  it "nested lets" $ do
    runSuggestion [r|
fun x y =
    let
        fun a b = plus a b
            var = let
                plus x y = x in x
        vare = 5
    in
        5)
|] `shouldBe` "fun x y = \n    let \n        fun a b = plus a b \n        var = \n            let \n                plus x y = x \n            in x \n        vare = 5 \n    in \n        5 , (v24 -> (v12 -> Int))"

  it "weird trailing symbols" $ do -- note that there might be correct parenthesis that remain unconsumed 
    runSuggestion [r|
fun x y =
    let
        var3 = plus 5 (plus 5 5) -> (
    in 
        var3
|] `shouldBe` "fun x y = \n    let \n        var3 = plus 5 (plus 5 5) \n    in \n        var3 , (v10 -> (v12 -> Int))"

  it "branching and un-branching" $ do -- this refers to not loosing fun3 once we "jump"(/branch) to do fun4
    runSuggestion [r|
fun x y =
    let
        var3 = 
            let
                fun3 = plus
            in
                fun4 5 (fun3 5 5)
        fun4 a b = plus a b 
    in 
        var3)
|] `shouldBe` "fun x y = \n    let \n        var3 = \n            let \n                fun3 = plus \n            in \n                fun4 5 (fun3 5 5) \n        fun4 a b = plus a b \n    in \n        var3 , (v10 -> (v12 -> Int))"




standardTypesAsString :: String
standardTypesAsString =
  intercalate
    "\n"
    [ "plus :: Int -> Int -> Int",
      "trice f x = f (f (f x))",
      "invertNum :: Bool -> Int -> Int",
      "iterate :: (Int -> Int) -> Int -> (Int -> Int)",
      "\n" -- do not remove the \n this should remain last in the list
    ]

standardTypeEnv :: TypeEnvironment
standardTypeEnv = Map.empty

runSuggestion :: String -> String
runSuggestion sourceString = case runLexer (standardTypesAsString ++ sourceString) of
  Left errorMsg -> errorMsg
  Right parsedTokens ->
    let sections = sanitizeSections $ splitIntoSections $ processInternalTokens parsedTokens
        parsedMaybeSections = List.map (\section -> evalState (runParserT pSection "[filename was here]" (tokensToParsableString sourceString section)) ParserState {indentLevel = 0}) sections
        (parsedErrors, parsedSections) = partitionEithers parsedMaybeSections
        parseProblemsBundle = getParseProblems parsedMaybeSections sections
        (inferredTypes, inferenceProblems, state) = inferTypeEnvironment standardTypeEnv (map (\(WithSimplePos _ _ x) -> x) parsedSections)
        inferenceProblemsBundle = matchProblems sections inferenceProblems
     in trim $ concatMap (makeSuggestion state inferredTypes) (reverse $ parseProblemsBundle ++ inferenceProblemsBundle)

makeSuggestion :: Int -> TypeEnvironment -> ([TokenInfo], Problem) -> String
makeSuggestion state inferredTypes problembundle =
  let maybeErr = generateSuggestion state inferredTypes (fst problembundle)
   in case maybeErr of
        Justt (expectedTokens, fixString, diffString, typ, numberOfBranches) -> fixString ++ ", " ++ show typ
        Error str -> concat ["Problem generating suggestion for " ++ show (getNameFromProblem $ snd problembundle), str, "------------------------------------"]

