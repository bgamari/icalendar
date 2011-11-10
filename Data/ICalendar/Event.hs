module Data.ICalendar.Event ( Event(..)
                            , parseEvent
                            , readEvents
                            , occurrences
                            ) where

import Data.Text (Text)
import Control.Monad (liftM, when, (<=<))
import Data.Maybe (isNothing, fromJust, catMaybes)
import Data.Time.CalendarTime
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime
import Data.ICalendar.Types
import Data.ICalendar.Parse
import Data.ICalendar.RRule

data Event = Event { iceStart :: CalendarTime
                   , iceEnd :: Maybe CalendarTime
                   , iceRRule :: Maybe RRule
                   , iceSummary :: Maybe String
                   , iceObject :: ICalObject
                   } deriving (Show, Eq)

parseEvent :: TimeZone -> ICalObject -> IO (Maybe Event)
parseEvent localTz obj | icoName obj == "VEVENT" =
                do let dtstart = lookupProp obj "DTSTART"
                   when (isNothing dtstart) (error "DTSTART property not found")
                   dtstart' <- parseDateTimeProp $ fromJust dtstart
                   case dtstart of
                        Nothing -> return Nothing
                        Just d -> do let dtend = lookupProp obj "DTEND"
                                         summary = lookupPropValue obj "SUMMARY"
                                     dtend' <- if isNothing dtend then return Nothing
                                                                  else parseDateTimeProp $ fromJust dtend
                                     rrule' <- parseRRule localTz obj
                                     return $ Just $ Event { iceStart=fromJust dtstart'
                                                           , iceEnd=dtend'
                                                           , iceRRule=rrule'
                                                           , iceSummary=summary
                                                           , iceObject=obj }
parseEvent _ _ = return Nothing

readEvents :: Text -> IO [Event]
readEvents content =
        do localTz <- getCurrentTimeZone
           let Right ic = parseICal content
               Just vcal = lookupObject ic "VCALENDAR"
           events <- mapM (parseEvent localTz) $ lookupObjects vcal "VEVENT"
           return $ catMaybes events

occurrences :: Event -> [UTCTime]
occurrences ev@(Event {iceRRule=Just rrule}) = recurrences rrule
occurrences ev = [maybe (error "Failed to convert start time") id $ fromCalendarTime $ iceStart ev]

