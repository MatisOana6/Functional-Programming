module Test.Tests where

import qualified Args
import Bencode.Parser
import qualified Bencode.Serializer as Ser
import Bencode.Value (BencodeValue (..))
import Control.Monad.State (State, execState)
import Data.Maybe (listToMaybe)
import qualified Entry.DB as DB
import Entry.Entry
import Entry.Query
import qualified Main
import Parsec
import Result (Result (..), isError)
import System.Environment (getArgs)
import qualified Test.Data as Data
import Test.SimpleTest
import Test.SimpleTest.Expectation
import Test.SimpleTest.Mock (makeMockIOState)
import qualified Test.SimpleTest.Mock as Mock
import Test.SimpleTest.TestCase
import Text.Printf (printf)

shouldParseAs :: String -> BencodeValue -> EqualityAssertion
shouldParseAs str v = parse str `shouldBe` Success v

parseShouldFailWith :: String -> ParseError -> EqualityAssertion
parseShouldFailWith str err = parse str `shouldBe` Error err

parseShouldFail :: String -> String -> PredicateAssertion
parseShouldFail str msg = isError `shouldHold` parse str `withMessage` msg

shouldHaveEffects :: String -> Mock.MockIOState -> State Mock.MockIOState () -> (Mock.MockIOState -> Bool) -> PredicateAssertion
shouldHaveEffects msg init fn pred = pred `shouldHold` execState fn init `withMessage` msg

shouldReadFromFile :: FilePath -> Mock.MockIOState -> State Mock.MockIOState () -> PredicateAssertion
shouldReadFromFile file init fn = Mock.fileWasRead file `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" was read" file)

shouldNotWriteFile :: FilePath -> Mock.MockIOState -> State Mock.MockIOState () -> PredicateAssertion
shouldNotWriteFile file init fn = (not . Mock.fileWasWritten file) `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" wasn't written" file)

shouldNotReadFile :: FilePath -> Mock.MockIOState -> State Mock.MockIOState () -> PredicateAssertion
shouldNotReadFile file init fn = (not . Mock.fileWasRead file) `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" wasn't read" file)

shouldWriteToStdout :: String -> Mock.MockIOState -> State Mock.MockIOState () -> PredicateAssertion
shouldWriteToStdout msg init fn = Mock.stdoutContains msg `shouldHold` (execState fn init) `withMessage` (printf "stdout contains %s" (show msg))

shouldWriteAllToStdout :: [String] -> Mock.MockIOState -> State Mock.MockIOState () -> PredicateAssertion
shouldWriteAllToStdout msgs init fn = (\a -> and $ (map Mock.stdoutContains msgs) <*> pure a) `shouldHold` (execState fn init) `withMessage` (printf "stdout contains %s" (unlines $ map show msgs))

shouldWriteFile :: FilePath -> Mock.MockIOState -> State Mock.MockIOState () -> PredicateAssertion
shouldWriteFile file init fn = Mock.fileWasWritten file `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" was written" file)

fileShouldContain :: FilePath -> String -> Mock.MockIOState -> State Mock.MockIOState () -> PredicateAssertion
fileShouldContain file contents init fn = Mock.fileContentsContains contents file `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" contains \"%s\"" file contents)

tests = [("parser", parserTests), ("handlers", handlerTests)]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> evalTestGroup False testSuite
    args' ->
      let tg' = do
            name <- listToMaybe $ dropWhile (`elem` ["-d", "--detailed"]) args'
            lookup name tests
          detailed = any (`elem` ["-d", "--detailed"]) args'
       in case tg' of
            Nothing -> evalTestGroup detailed testSuite
            Just tg -> evalTestGroup detailed tg

testSuite :: TestTree TestCase
testSuite =
  group
    "test suite"
    [ parserTests,
      handlerTests
    ]

parserTests :: TestTree TestCase
parserTests =
  group
    "bencode parser tests"
    [ group
        "integer parsing tests"
        [ testCase "parses 10" 50 ("i10e" `shouldParseAs` BencodeInt 10),
          testCase "parses 0" 50 ("i0e" `shouldParseAs` BencodeInt 0),
          testCase "fails to parse invalid number" 25 ("i0ae" `parseShouldFail` "Result is Error"),
          testCase "fails to parse number without end" 25 ("i0" `parseShouldFail` "Result is Error")
        ],
      group
        "string parsing tests"
        [ testCase "parses the string \"abc\"" 50 ("3:abc" `shouldParseAs` BencodeString "abc"),
          testCase "parses the empty string" 50 ("0:" `shouldParseAs` BencodeString ""),
          testCase "fails to parse string without ':'" 50 ("1a" `parseShouldFail` "Result is Error")
        ],
      group
        "list parsing tests"
        [ testCase "parses a list of strings" 25 ("l3:abc4:abcde" `shouldParseAs` BencodeList [BencodeString "abc", BencodeString "abcd"]),
          testCase "parses a list of numbers" 25 ("li1ei2ee" `shouldParseAs` BencodeList [BencodeInt 1, BencodeInt 2]),
          testCase "parses the empty list" 50 ("le" `shouldParseAs` BencodeList []),
          testCase "parses a list of lists" 25 ("lli1eee" `shouldParseAs` BencodeList [BencodeList [BencodeInt 1]]),
          testCase "fails to parse list without end" 25 ("li1e" `parseShouldFail` "Result is Error")
        ],
      group
        "dict parsing tests"
        [ testCase "parses dict with int values" 25 ("d3:abci10ee" `shouldParseAs` BencodeDict [("abc", BencodeInt 10)]),
          testCase "parses dict with string values" 25 ("d3:abc3:123e" `shouldParseAs` BencodeDict [("abc", BencodeString "123")]),
          testCase "parses empty dicts" 25 ("de" `shouldParseAs` BencodeDict []),
          testCase "parses nested dicts" 25 ("d1:ad1:bi0eee" `shouldParseAs` BencodeDict [("a", BencodeDict [("b", BencodeInt 0)])]),
          testCase "fails to parse dict with int keys" 50 ("di10ee" `parseShouldFail` "Result is Error")
        ]
    ]

handlerTests :: TestTree TestCase
handlerTests =
  group
    "command handler tests"
    [ group
        "handleInit tests"
        [ testCase
            "creates a file named \"snippets.ben\""
            50
            (shouldHaveEffects "\"snippets.ben\" was created" emptyFS Main.handleInit (Mock.fileWasCreated "snippets.ben")),
          testCase
            "\"snippets.ben\" only contains an empty list"
            100
            (fileShouldContain "snippets.ben" "le" emptyFS Main.handleInit)
        ],
      group
        "handleGet tests"
        [ testCase
            "reads from \"snippets.ben\""
            50
            (shouldReadFromFile "snippets.ben" testFSOneEntry (Main.handleGet (Args.GetOptions 1))),
          testCase
            "prints the correct result"
            50
            (shouldWriteToStdout "main = putStrLn \"Hello world!\"" testFSOneEntry (Main.handleGet (Args.GetOptions 0))),
          testCase
            "prints an error if \"snippets.ben\" is corrupted"
            50
            (shouldWriteToStdout "Failed to load DB" testFSCorrupted (Main.handleGet (Args.GetOptions 0)))
        ],
      group
        "handleSearch tests"
        [ testCase
            "reads from \"snippets.ben\""
            25
            (shouldReadFromFile "snippets.ben" testFSOneEntry (Main.handleSearch (Args.SearchOptions [Code "main"]))),
          testCase
            "prints the correct result for one code match"
            25
            (shouldWriteToStdout "[0] Hello.hs:" testFSMultipleEntries (Main.handleSearch (Args.SearchOptions [Code "main"]))),
          testCase
            "prints the correct result for one language match"
            25
            (shouldWriteToStdout "[1] hello.py:" testFSMultipleEntries (Main.handleSearch (Args.SearchOptions [Language "python"]))),
          testCase
            "prints the correct result for one description match"
            25
            (shouldWriteToStdout "[2] file_exists.sh:" testFSMultipleEntries (Main.handleSearch (Args.SearchOptions [Description "exists"]))),
          testCase
            "prints the correct results for multiple matches 1"
            50
            (shouldWriteAllToStdout ["[0] Hello.hs:", "[1] hello.py:"] testFSMultipleEntries (Main.handleSearch (Args.SearchOptions [Tag "main"]))),
          testCase
            "prints the correct results for multiple matches 2"
            50
            (shouldWriteAllToStdout ["[2] file_exists.sh:", "[3] File-Exists.ps1:"] testFSMultipleEntries (Main.handleSearch (Args.SearchOptions [Tag "file", Tag "exists"]))),
          testCase
            "handles the not found case"
            50
            (shouldWriteToStdout "No entries found" testFSOneEntry (Main.handleSearch (Args.SearchOptions [Code "not found"]))),
          testCase
            "prints an error if \"snippets.ben\" is corrupted"
            50
            (shouldWriteToStdout "Failed to load DB" testFSCorrupted (Main.handleSearch (Args.SearchOptions [Code "main"])))
        ],
      group
        "handleAdd tests"
        [ testCase
            "reads from the source file"
            50
            (shouldReadFromFile "Hello.hs" testFSNoEntries (Main.handleAdd (Data.entryToAddOpts $ Data.hsEntry 0))),
          testCase
            "reads from \"snippets.ben\""
            50
            (shouldReadFromFile "snippets.ben" testFSNoEntries (Main.handleAdd (Data.entryToAddOpts $ Data.hsEntry 0))),
          testCase
            "writes to \"snippets.ben\""
            50
            (shouldWriteFile "snippets.ben" testFSNoEntries (Main.handleAdd (Data.entryToAddOpts $ Data.hsEntry 0))),
          testCase
            "initially empty \"snippets.ben\" will contain the added snippet"
            50
            (fileShouldContain "snippets.ben" (Data.bashEntryBen 0) testFSNoEntries (Main.handleAdd (Data.entryToAddOpts $ Data.bashEntry 0))),
          testCase
            "\"snippets.ben\" will contain the added snippet 1"
            50
            (fileShouldContain "snippets.ben" (Data.pyEntryBen 1) testFSOneEntry (Main.handleAdd (Data.entryToAddOpts $ Data.pyEntry 1))),
          testCase
            "\"snippets.ben\" will contain the added snippet 2"
            50
            (fileShouldContain "snippets.ben" (Data.pwshEntryBen 1) testFSOneEntry (Main.handleAdd (Data.entryToAddOpts $ Data.pwshEntry 1))),
          testCase
            "\"snippets.ben\" will contain the added snippet 3"
            50
            (fileShouldContain "snippets.ben" (Data.rsEntryBen 4) testFSMultipleEntries (Main.handleAdd (Data.entryToAddOpts $ Data.rsEntry 4))),
          testCase
            "prints an error if \"snippets.ben\" is corrupted"
            50
            (shouldWriteToStdout "Failed to load DB" testFSCorrupted (Main.handleAdd (Data.entryToAddOpts $ Data.hsEntry 0))),
          testCase
            "prints failure message if \"snippets.ben\" already contains the snippet"
            100
            (shouldWriteAllToStdout ["Entry with this content already exists: ", "[0] Hello.hs:"] testFSOneEntry (Main.handleAdd (Data.entryToAddOpts $ Data.hsEntry 1))),
          testCase
            "doesn't modify \"snippets.ben\" if it already contains the snippet"
            100
            (shouldNotWriteFile "snippets.ben" testFSOneEntry (Main.handleAdd (Data.entryToAddOpts $ Data.hsEntry 1)))
        ]
    ]

emptyFS :: Mock.MockIOState
emptyFS = makeMockIOState []

snippetsBenFromFiles :: [Entry] -> (String, String)
snippetsBenFromFiles files = ("snippets.ben", DB.serialize (DB.fromEntries files))

setupMockFS :: [Data.TestFile] -> [Entry] -> Mock.MockIOState
setupMockFS filesInFs filesInSnippets =
  makeMockIOState $
    snippetsBen :
    (map Data.fileToFsTuple filesInFs)
  where
    snippetsBen = snippetsBenFromFiles filesInSnippets

testFSNoEntries :: Mock.MockIOState
testFSNoEntries =
  setupMockFS
    [Data.hsFile, Data.pyFile, Data.bashFile]
    []

testFSOneEntry :: Mock.MockIOState
testFSOneEntry =
  setupMockFS
    [Data.bashFile, Data.hsFile, Data.pyFile, Data.pwshFile]
    [Data.hsEntry 0]

testFSMultipleEntries :: Mock.MockIOState
testFSMultipleEntries =
  setupMockFS
    [Data.bashFile, Data.hsFile, Data.pyFile, Data.pwshFile, Data.rsFile]
    [Data.hsEntry 0, Data.pyEntry 1, Data.bashEntry 2, Data.pwshEntry 3]

testFSCorrupted :: Mock.MockIOState
testFSCorrupted =
  makeMockIOState
    [ ("snippets.ben", "i0e"),
      ("Hello.hs", "main = putStrLn \"Hello world!\"")
    ]
