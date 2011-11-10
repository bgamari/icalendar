module Data.ICalendar.Event where

import qualified Data.Text as T
import Control.Monad (liftM, when, (<=<))
import Data.Maybe (isNothing, fromJust, catMaybes)
import Data.Time.CalendarTime
import Data.Time.LocalTime
import Data.ICalendar.Types
import Data.ICalendar.Parse
import Data.ICalendar.RRule

data Event = Event { iceStart :: CalendarTime
                   , iceEnd :: Maybe CalendarTime
                   , iceRRule :: Maybe RRule
                   } deriving (Show, Eq)

parseEvent :: TimeZone -> ICalObject -> IO (Maybe Event)
parseEvent localTz obj | icoName obj == "VEVENT" =
                do let dtstart = lookupProp obj "DTSTART"
                   when (isNothing dtstart) (error "DTSTART property not found")
                   dtstart' <- parseDateTimeProp $ fromJust dtstart
                   case dtstart of
                        Nothing -> return Nothing
                        Just d -> do let dtend = lookupProp obj "DTEND"
                                     dtend' <- if isNothing dtend then return Nothing
                                                                  else parseDateTimeProp $ fromJust dtend
                                     rrule' <- parseRRule localTz obj
                                     return $ Just $ Event { iceStart=fromJust dtstart'
                                                           , iceEnd=dtend'
                                                           , iceRRule=rrule' }
parseEvent _ _ = return Nothing

readEvents :: FilePath -> IO [Event]
readEvents fname =
        do f <- readFile "basic.ics"
           localTz <- getCurrentTimeZone
           let Right ic = parseICal $ T.pack f
               Just vcal = lookupObject ic "VCALENDAR"
           events <- mapM (parseEvent localTz) $ lookupObjects vcal "VEVENT"
           return $ catMaybes events

