module Main (main) where

import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import qualified Data.List as List
import qualified Data.Map as Map
import Lexer.LexerRunner
import Lexer.Tokens (TokenInfo)
import Parser.Parser
import Parser.ParserBase
import Parser.Types (Problem, Type, TypeEnvironment, getNameFromProblem, Section)
import Suggestions.Suggestions
import Test.Hspec
import Text.Megaparsec
import TypeInference.TypeInference (MaybeError (..))
import TypeInference.TypeInferenceUtil
import Data.Void
import Control.Monad.State

-- the fact that this is not efficient should not matter.
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

main :: IO ()
main = hspec $ do
  describe "basic code suggestions" $ do
    it "removing extra parenthesis from simple addition" $ do
      runSuggestion "fun = plus 2 4)" `shouldBe` "fun = plus 2 4 , Int"

    it "respect unnecessary brackets" $ do
      runSuggestion "fun = (plus 2) 4)" `shouldBe` "fun = (plus 2) 4 , Int"

    it "brackets for partial application if required" $ do
      runSuggestion "fun = trice plus 2 4)" `shouldBe` "fun = trice (plus 2) 4 , Int"

    it "swap arguments" $ do
      runSuggestion "fun = invertNum 4 True" `shouldBe` "fun = invertNum True 4 , Int"

    it "nested brackets" $ do
      runSuggestion "fun = plus plus plus 4 5 plus plus plus 2 3 4 5 6" `shouldBe` "fun = plus (plus (plus 4 5) (plus (plus (plus 2 3) 4) 5)) 6 , Int"

    it "suggestion for partial application" $ do
      runSuggestion "fun = plus plus 4 4 " `shouldBe` "fun = plus (plus 4 4) , (Int -> Int)"

    it "swapping twice" $ do
      runSuggestion "fun = invertNum invertNum 6 True False" `shouldBe` "fun = invertNum False (invertNum True 6) , Int"

  describe "lambda expressions code suggestions" $ do
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
    let sections = sanitizeSections $ splitIntoSections parsedTokens
        parsedMaybeSections = List.map (\section -> evalState (runParserT pSection "[filename was here]" ( tokensToParsableString sourceString section)) ParserState { indentLevel = 0 } ) sections
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