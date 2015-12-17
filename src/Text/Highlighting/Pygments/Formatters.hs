{-# LANGUAGE TemplateHaskell #-}
module Text.Highlighting.Pygments.Formatters
    (
      FormatterAlias
    , Formatter
    , getAllFormatters
    , getFormatterByName

    -- ** Accessors
    , formatterName
    , formatterAliases

    -- ** Common formatters
    , htmlFormatter
    , terminalFormatter
    ) where

import           Data.Aeson.TH                   (defaultOptions, deriveJSON)
import           Data.Maybe                      (listToMaybe)
import           Text.Highlighting.Pygments.JSON

type FormatterAlias = String

data Formatter = Formatter
    { _formatterName    :: String
    , _formatterAliases :: [FormatterAlias]
    } deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''Formatter)

getAllFormatters :: IO [Formatter]
getAllFormatters = getPygmentsJSON "formatters"

-- | Similar to the @get_formatter_by_name()@ function in Pygments
getFormatterByName :: FormatterAlias -> IO (Maybe Formatter)
getFormatterByName name = do
    formatters <- getAllFormatters
    let fs = filter (\f -> name `elem` _formatterAliases f) formatters
    return $ listToMaybe fs

formatterName :: Formatter -> String
formatterName = _formatterName

formatterAliases :: Formatter -> [FormatterAlias]
formatterAliases = _formatterAliases

htmlFormatter :: Formatter
htmlFormatter = Formatter
    { _formatterName = "HTML"
    , _formatterAliases = ["html"]
    }

terminalFormatter :: Formatter
terminalFormatter = Formatter
    { _formatterName = "Terminal"
    , _formatterAliases = ["terminal", "console"]
    }
