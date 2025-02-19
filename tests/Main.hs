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
import Parser.Types (Problem, Type, TypeEnvironment, getNameFromProblem)
import Suggestions.Suggestions
import Test.Hspec
import Text.Megaparsec
import TypeInference.TypeInference (MaybeError (..))
import TypeInference.TypeInferenceUtil

-- the fact that this is not efficient should not matter.
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

main :: IO ()
main = hspec $ do
  describe "code suggestions" $ do
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



standardTypesAsString :: String
standardTypesAsString =
  intercalate
    "\n"
    [ "plus :: Int -> Int -> Int",
      "trice f x = f (f (f x))",
      "invertNum :: Bool -> Int -> Int",

      "\n" -- do not remove the \n this should remain last in the list
    ]

standardTypeEnv :: TypeEnvironment
standardTypeEnv = Map.empty

runSuggestion :: String -> String
runSuggestion sourceString = case runLexer (standardTypesAsString ++ sourceString) of
  Left errorMsg -> errorMsg
  Right parsedTokens ->
    let sections = sanitizeSections $ splitIntoSections parsedTokens
        parsedMaybeSections = List.map (parse pSection "[filename was here]" . tokensToParsableString sourceString) sections
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