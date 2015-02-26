module Text.ParseBird.Parsers where

import Data.Char
import qualified Text.Parsec as Parsec

import Text.Parsec ((<?>))

import Control.Applicative

import Control.Monad.Identity (Identity)

parse rule text = Parsec.parse rule "(source)" text

hashtag :: Parsec.Parsec String () String
hashtag = (do
      Parsec.char '#'
      digits <- Parsec.many Parsec.digit
      letters <- Parsec.many1 Parsec.letter
      rest <- Parsec.many Parsec.alphaNum <|> (Parsec.try ((Parsec.space >> return []) <|> (Parsec.eof >> return [])))
      return (digits ++ letters ++ rest)) <|> do
      Parsec.char '#'
      letter <- Parsec.letter
      rest <- (Parsec.manyTill Parsec.alphaNum (Parsec.try ((Parsec.space >> return []) <|> (Parsec.eof >> return []))))
      return (letter:rest)

mentionedScreenname = do
  Parsec.char '@'
  name <- Parsec.manyTill (Parsec.alphaNum <|> (Parsec.char '_')) spaceOrEnd
  return name

spaceOrEnd :: Parsec.Parsec String () ()
spaceOrEnd = Parsec.try ((Parsec.space >> return ()) <|> Parsec.eof)

totalResults :: Either Parsec.ParseError [a] -> Int
totalResults = either (const 0) length