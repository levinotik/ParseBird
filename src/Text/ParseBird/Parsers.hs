module Text.ParseBird.Parsers where

import Data.Char
import qualified Text.Parsec as Parsec

import Text.Parsec ((<?>))

import Control.Applicative

import Control.Monad

import Control.Monad.Identity (Identity)

parse rule text = Parsec.parse rule "(source)" text

-- | Parse hashtags
--
-- Examples:
--
-- >>> parse hashtag "#1"
-- Left "(source)" (line 1, column 3):
-- unexpected end of input
-- expecting letter or digit
--
-- >>> parse hashtag "#1#"
-- Left "(source)" (line 1, column 3):
-- unexpected "#"
-- expecting letter or digit
--
-- >>> parse hashtag "#"
-- Left "(source)" (line 1, column 2):
-- unexpected end of input
-- expecting letter or digit
--
-- >>> parse hashtag "a"
-- Left "(source)" (line 1, column 1):
-- unexpected "a"
-- expecting "#"
--
-- >>> parse hashtag "#a@"
-- Right "a"
--
-- >>> parse hashtag "#a"
-- Right "a"
--
-- parse hashtag "#1a"
-- Right "#1a"
hashtag :: Parsec.Parsec String () String
hashtag  = do
      Parsec.char '#'
      h <- Parsec.many1 Parsec.alphaNum
      unless (any isLetter h || any (== '#') h) $
        Parsec.parserZero
      return h

mentionedScreenname = do
  Parsec.char '@'
  name <- Parsec.manyTill (Parsec.alphaNum <|> (Parsec.char '_')) spaceOrEnd
  return name

spaceOrEnd :: Parsec.Parsec String () ()
spaceOrEnd = Parsec.try ((Parsec.space >> return ()) <|> Parsec.eof)

totalResults :: Either Parsec.ParseError [a] -> Int
totalResults = either (const 0) length
