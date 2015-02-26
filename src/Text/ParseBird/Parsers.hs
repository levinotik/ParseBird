module Text.ParseBird.Parsers where

import Data.Char
import qualified Text.Parsec as Parsec

import Text.Parsec ((<?>))

import Control.Applicative

import Control.Monad

import Control.Monad.Identity (Identity)

parse rule text = Parsec.parse rule "(source)" text

hashtag :: Parsec.Parsec String () String
hashtag  = do
      Parsec.char '#'
      h <- Parsec.many1 Parsec.alphaNum
      unless (any isLetter h) $
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
