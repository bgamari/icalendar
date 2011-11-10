module Data.ICalendar.Parse ( parseICal
                            , readDateTime
                            , parseDateTimeProp
                            ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.List (foldl')
import Data.Char (isSpace)
import Text.Parsec
import Text.Parsec.Text
import Data.Maybe (isJust)
import Data.Either (partitionEithers)
import Control.Monad (liftM, ap)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime
import Data.Time.CalendarTime
import Data.Time.LocalTime.TimeZone.Utils
import Data.ICalendar.Types

crLfLines = map T.init . T.lines

unfoldLines :: Text -> [Text]
unfoldLines a = reverse $ foldl' f [] (crLfLines a)
        where f folded line
                | isSpace (T.head line) = (T.append (head folded) (T.tail line)):tail folded
                | otherwise = line : folded


wspChars = " \t\n\r\f\v\xa0"
controlChars = ['\x00'..'\x08'] ++ ['\x0a'..'\x1f'] ++ "\x7f"
--qSafeChar,safeChar,quotedString :: (Monad m, Stream Text m Char) => 
qSafeChar = noneOf (controlChars ++ "\"")
safeChar = noneOf (controlChars ++ "\";:,")
quotedString = do char '"'
                  s <- many qSafeChar
                  char '"'
                  return s
ianaToken = many (alphaNum <|> char '-')
value = many (noneOf controlChars)

contentLine = do name <- ianaToken
                 let param = do name <- ianaToken
                                char '='
                                values <- sepBy1 (many safeChar <|> quotedString) (char ',')
                                return $ (name, values)
                 params <- many (char ';' >> param)
                 char ':'
                 value <- value
                 char '\n'
                 case name of
                      "BEGIN"   -> do cls <- manyTill contentLine (try $ string "END:" >> string value >> char '\n')
                                      let (objs, props) = partitionEithers cls
                                      return $ Left (ICalObject value props objs)
                      otherwise -> return $ Right (ICalProperty name params value)
                 
ical = do cls <- many contentLine
          let (objs, props) = partitionEithers cls
          return $ ICalObject "ROOT" props objs

parseICal :: Text -> Either ParseError ICalObject
parseICal t = let t' = T.unlines $ unfoldLines t
              in runParser ical () "iCal" t'

--parseDateTime :: Maybe TimeZone -> Parsec s m CalendarTime
parseDateTime localTz = 
        do year <- readInt 4
           month <- readInt 2
           day <- readInt 2
           a <- char 'T'
           hour <- readInt 2
           minute <- readInt 2
           second <- readInt 2
           zulu <- optionMaybe (char 'Z')
           let lt = LocalTime (fromGregorian year month day) (TimeOfDay hour minute (fromIntegral second))
           return $ case isJust zulu of
                         True         -> toCalendarTime $ ZonedTime lt utc
                         False        -> toCalendarTime $ ZonedTime lt localTz
        where readInt n = (liftM read) $ count n digit

readDateTime :: TimeZone -> String -> Maybe CalendarTime
readDateTime localTz = either (const Nothing) Just . runParser (parseDateTime localTz) () "date"

parseDateTimeProp :: ICalProperty -> IO (Maybe CalendarTime)
parseDateTimeProp prop =
        do let tzName = lookup "TZID" $ icpParams prop
           case tzName of
                Just (tzName':_)  ->
                        do tz <- findTimeZone tzName'
                           case tz of
                                Nothing  -> return Nothing
                                Just tz' -> return $ readDateTime tz' (icpValue prop)
                otherwise  ->
                        do tz <- getCurrentTimeZone
                           return $ readDateTime tz (icpValue prop)

