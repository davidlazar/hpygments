{-# LANGUAGE TemplateHaskell #-}
module Text.Highlighting.Pygments.Lexers
    (
      LexerAlias
    , Lexer
    , getAllLexers
    , getLexerByName

    -- ** Accessors
    , lexerName
    , lexerAliases
    , lexerFileTypes
    , lexerMimeTypes

    -- ** Common lexers
    , haskellLexer
    , literateHaskellLexer
    , textLexer
    ) where

import Data.Aeson.TH (deriveJSON)
import Data.Maybe (listToMaybe)

import Text.Highlighting.Pygments.JSON

type LexerAlias = String

data Lexer = Lexer
    { _lexerName      :: String
    , _lexerAliases   :: [LexerAlias]
    , _lexerFileTypes :: [String]
    , _lexerMimeTypes :: [String]
    } deriving (Eq, Ord, Show)

$(deriveJSON id ''Lexer)

getAllLexers :: IO [Lexer]
getAllLexers = getPygmentsJSON "lexers"

-- | Similar to the @get_lexer_by_name()@ function in Pygments
getLexerByName :: LexerAlias -> IO (Maybe Lexer)
getLexerByName name = do
    lexers <- getAllLexers
    let ls = filter (\l -> name `elem` _lexerAliases l) lexers
    return $ listToMaybe ls

lexerName :: Lexer -> String
lexerName = _lexerName

lexerAliases :: Lexer -> [LexerAlias]
lexerAliases = _lexerAliases

lexerFileTypes :: Lexer -> [String]
lexerFileTypes = _lexerFileTypes

lexerMimeTypes :: Lexer -> [String]
lexerMimeTypes = _lexerMimeTypes

haskellLexer :: Lexer
haskellLexer = Lexer
    { _lexerName = "Haskell"
    , _lexerAliases = ["haskell","hs"]
    , _lexerFileTypes = ["*.hs"]
    , _lexerMimeTypes = ["text/x-haskell"]
    }

literateHaskellLexer :: Lexer
literateHaskellLexer = Lexer 
    { _lexerName = "Literate Haskell"
    , _lexerAliases = ["lhs","literate-haskell"]
    , _lexerFileTypes = ["*.lhs"]
    , _lexerMimeTypes = ["text/x-literate-haskell"]
    }

-- | No highlighting
textLexer :: Lexer
textLexer = Lexer
    { _lexerName = "Text only"
    , _lexerAliases = ["text"]
    , _lexerFileTypes = ["*.txt"]
    , _lexerMimeTypes = ["text/plain"]
    }
