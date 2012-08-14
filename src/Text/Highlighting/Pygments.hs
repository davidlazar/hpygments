-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Highlighting.Pygments
-- Copyright   :  (c) David Lazar, 2012
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- This library uses the @pygmentize@ script that comes with Pygments to
-- highlight code in many languages. For documentation on the various lexers,
-- formatters, and options, see the Pygments documentation
-- <http://pygments.org/docs/>.
-----------------------------------------------------------------------------

module Text.Highlighting.Pygments
    (
      highlight
    , pygmentize

    , module Text.Highlighting.Pygments.Lexers
    , module Text.Highlighting.Pygments.Formatters

    -- * Options
    , Option
    , Options

    -- * Examples
    -- $examples
    ) where

import System.Process (readProcess)

import Text.Highlighting.Pygments.Lexers
import Text.Highlighting.Pygments.Formatters

-- | Highlight code robustly. This function is more robust than the
-- lower-level 'pygmentize' function since this library forbids the 
-- construction of invalid 'Lexer' and 'Formatter' values. Invalid
-- 'Options' may still cause this function to raise an exception.
highlight :: Lexer -> Formatter -> Options -> String -> IO String
highlight lexer formatter options code = do
    let (lexerAlias : _) = lexerAliases lexer
    let (formatterAlias : _) = formatterAliases formatter
    pygmentize lexerAlias formatterAlias options code

-- | Highlight code (less robustly) using the @pygmentize@ script that comes
-- with Pygments. Invalid values for 'LexerAlias', 'FormatterAlias', or
-- 'Options' will cause this function to raise an exception.
pygmentize :: LexerAlias -> FormatterAlias -> Options -> String -> IO String
pygmentize lexer formatter options code = do
    let args = ["-l", lexer, "-f", formatter] ++ optionsToArgs options
    readProcess "pygmentize" args code

-- | The lexer/formatter option @(key, value)@ is passed to the @pygmentize@ 
-- script via the command-line flag @-P key=value@.
--
-- Examples:
--
-- > [("hl_lines", "16,23,42"), ("encoding", "utf-8"), ("anchorlines", "True")]
--
type Option = (String, String)
type Options = [Option]

optionsToArgs :: Options -> [String]
optionsToArgs options = concatMap optionToArg options

optionToArg :: Option -> [String]
optionToArg (name, value) = ["-P", name ++ "=" ++ value]

{- $examples

Highlight a proposition:

>>> Just coqLexer <- getLexerByName "coq"
>>> highlight coqLexer terminalFormatter [("encoding", "utf-8")] "∀ x y : Z, x * y = 0 -> x = 0 \\/ y = 0" >>= putStr
∀ x y : Z, x * y = 0 -> x = 0 \/ y = 0

Output a complete HTML document:

>>> highlight haskellLexer htmlFormatter [("full", "True"), ("linenos", "table"), ("style", "emacs")] "fix f = let x = f x in x" >>= writeFile "fix.html"

Self-highlighting quine:

> quine = pygmentize "hs" "terminal" [] (s ++ show s) >>= putStr
>   where s = "quine = pygmentize \"hs\" \"terminal\" [] (s ++ show s) >>= putStr\n  where s = "

Highlight the code \"answer = 42\" using every language Pygments knows about:

>>> lexers <- getAllLexers
>>> forM_ lexers $ \l -> highlight l terminalFormatter [] "answer = 42" >>= printf "(%s) %s" (lexerName l)
...
(Prolog) answer = 42
(CSS+Django/Jinja) answer = 42
(Smalltalk) answer = 42
...

-}
