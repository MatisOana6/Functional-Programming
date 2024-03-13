module Test.Data where

import qualified Args
import qualified Bencode.Serializer as Ser
import Entry.Entry

data TestFile = TestFile
  { testFileName :: String,
    testFileContents :: String,
    testFileLanguage :: String
  }

fileToFsTuple TestFile {testFileName = filename, testFileContents = contents} = (filename, contents)

entryFromFile :: TestFile -> Int -> String -> [String] -> Entry
entryFromFile TestFile {testFileName = filename, testFileContents = contents, testFileLanguage = language} id description tags =
  Entry
    { entryId = id,
      entrySnippet = contents,
      entryFilename = filename,
      entryLanguage = language,
      entryDescription = description,
      entryTags = tags
    }

entryToAddOpts :: Entry -> Args.AddOptions
entryToAddOpts
  Entry
    { entryFilename = filename,
      entryLanguage = language,
      entryDescription = description,
      entryTags = tags
    } =
    Args.AddOptions
      { Args.addOptFilename = filename,
        Args.addOptLanguage = language,
        Args.addOptDescription = description,
        Args.addOptTags = tags
      }

serializeEntry :: Entry -> String
serializeEntry = Ser.runSerializer Ser.serialize "" . entryEncoder

bashFile :: TestFile
bashFile =
  TestFile
    { testFileName = "file_exists.sh",
      testFileContents = "FILE=Test.hs\nif [ -f \"$FILE\" ]; then\n    echo \"$FILE exists.\"\nelse \n    echo \"$FILE does not exist.\"\nfi",
      testFileLanguage = "Bash"
    }

bashEntry :: Int -> Entry
bashEntry id =
  entryFromFile
    bashFile
    id
    "Check if a file exists in Bash"
    ["file", "exists"]

bashEntryBen id = serializeEntry $ bashEntry id

pyFile =
  TestFile
    { testFileName = "hello.py",
      testFileContents = "def main():\n    print(\"Hello world\")\n\nif __name__ == '__main__':\n    main()",
      testFileLanguage = "Python"
    }

pyEntry id =
  entryFromFile
    pyFile
    id
    "Hello world in Python"
    ["hello", "world", "main"]

pyEntryBen id = serializeEntry $ pyEntry id

hsFile =
  TestFile
    { testFileName = "Hello.hs",
      testFileContents = "main = putStrLn \"Hello world!\"",
      testFileLanguage = "Haskell"
    }

hsEntry id = entryFromFile hsFile id "Hello world in Haskell" ["io", "hello", "world", "main"]

hsEntryBen id = serializeEntry $ hsEntry id

pwshFile = TestFile {testFileName = "File-Exists.ps1", testFileContents = "Test-Path -PathType Leaf $File", testFileLanguage = "Powershell"}

pwshEntry id =
  entryFromFile
    pwshFile
    id
    "Check if a file exists in Powershell"
    ["file", "exists"]

pwshEntryBen id = serializeEntry $ pwshEntry id

rsFile =
  TestFile
    { testFileName = "hello.rs",
      testFileContents = "fn main()\n    println!(\"Hello world\");\n}",
      testFileLanguage = "Rust"
    }

rsEntry id = entryFromFile rsFile id "Hello world in Rust" ["hello", "world", "main"]

rsEntryBen id = serializeEntry $ rsEntry id