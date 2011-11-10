{-# LANGUAGE PackageImports, PatternGuards #-}
module Data.ICalendar.RRule ( RRule(..)
                            , RFreq(..)
                            , parseRRule
                            , recurrences
                            ) where

import Data.Char (isDigit)
import Control.Monad (ap, (>=>), liftM)
import "mtl" Control.Monad.Writer (Writer, writer, tell, execWriter)
import Data.Maybe (listToMaybe, isJust, isNothing)
import Control.Arrow
import Data.List.Split (splitOn)
import Data.Time.LocalTime
import Data.Time.Clock (UTCTime)
import Data.Time.Recurrence
import Data.Time.CalendarTime

import Data.ICalendar.Parse
import Data.ICalendar.Types

data RFreq = Yearly | Monthly | Weekly | Daily | Hourly | Minutely | Secondly 
           deriving (Show, Eq)

data RRule = RRule { rFreq :: RFreq
                   , rInterval :: Integer
                   , rDateStart :: CalendarTime
                   , rCount :: Maybe Int
                   , rUntil :: Maybe CalendarTime
                   , rBySecond :: Maybe [Int]
                   , rByMinute :: Maybe [Int]
                   , rByHour :: Maybe [Int]
                   , rByWeekDay :: Maybe [(WeekDay, Maybe Int)]
                   , rByMonthDay :: Maybe [Int]
                   , rByYearDay :: Maybe [Int]
                   , rByWeekNo :: Maybe [Int]
                   , rByMonth :: Maybe [Month]
                   , rBySetPos :: Maybe [Int]
                   , rWeekStart :: WeekDay }
                   deriving (Show, Eq)

findDay :: String -> WeekDay
findDay "MO" = Monday
findDay "TU" = Tuesday
findDay "WE" = Wednesday
findDay "TH" = Thursday
findDay "FR" = Friday
findDay "SA" = Saturday
findDay "SU" = Sunday
findDay _    = error "Unknown day"

findFreq :: String -> RFreq
findFreq "YEARLY" = Yearly
findFreq "MONTHLY" = Monthly
findFreq "WEEKLY" = Weekly
findFreq "HOURLY" = Hourly
findFreq "MINUTELY" = Minutely
findFreq "SECONDLY" = Secondly

parseRRule :: TimeZone -> ICalObject -> IO (Maybe RRule)
parseRRule localTz obj =
        let dtstart = maybe (error "No DTSTART found") id $ lookupProp obj "DTSTART"
            dtend = return parseDateTimeProp `ap` lookupProp obj "DTEND"
            rrule = lookupPropValue obj "RRULE"
        in do dtstart' <- parseDateTimeProp dtstart 
              case (rrule, dtstart') of
                   (Nothing, _)          -> return Nothing
                   (Just rrule, Nothing) -> error "Failed to parse DTSTART"
                   (Just rrule, Just d)  -> return $ Just $ parseRRule' localTz d rrule
            
-- | Parse an ICal formatted RRULE with the given STARTDT
parseRRule' :: TimeZone -> CalendarTime -> String -> RRule
parseRRule' localTz dtstart rrule
        = let terms = map (break (=='=') >>> second (arr $ drop 1)) $ splitOn ";" rrule
              splitList = splitOn ","
              parseList :: Read a => String -> [a]
              parseList = map read . splitList
              parseDate :: String -> CalendarTime
              parseDate = maybe (error "DTSTART parse error") id . readDateTime localTz
              parseByDay :: String -> (WeekDay, Maybe Int)
              parseByDay a = let (n,day) = span isDigit a 
                                 n' = case n of "" -> Nothing
                                                a  -> Just $ read a
                             in (findDay day, n')
          in RRule { rFreq = maybe (error "No recurrence frequency given") findFreq $ lookup "FREQ" terms
                   , rInterval = maybe 1 read $ lookup "INTERVAL" terms
                   , rDateStart = maybe dtstart parseDate $ lookup "DTSTART" terms
                   , rCount = return read `ap` lookup "COUNT" terms
                   , rUntil = return parseDate `ap` lookup "UNTIL" terms
                   , rBySecond = return parseList `ap` lookup "BYSECOND" terms
                   , rByMinute = return parseList `ap` lookup "BYMINUTE" terms
                   , rByHour = return parseList `ap` lookup "BYHOUR" terms
                   , rByWeekDay = return (map parseByDay . splitList) `ap` lookup "BYDAY" terms
                   , rByMonthDay = return parseList `ap` lookup "BYMONTHDAY" terms
                   , rByYearDay = return parseList `ap` lookup "BYYEARDAY" terms
                   , rByWeekNo = return parseList `ap` lookup "BYWEEKNO" terms
                   , rByMonth = return (map toEnum . parseList) `ap` lookup "BYMONTH" terms
                   , rBySetPos = return parseList `ap` lookup "BYSETPOS" terms
                   , rWeekStart = maybe Monday findDay $ lookup "WKST" terms }

validateRRule :: RRule -> Maybe String
validateRRule = listToMaybe . execWriter . validateRRule'

validateRRule' :: RRule -> Writer [String] ()
validateRRule' rr = 
        do testMaybe (rBySecond rr) (and . map (\v->v >= 0 && v <= 60))         "Invalid BYSECOND value"
           testMaybe (rByMinute rr) (and . map (\v->v >= 0 && v <= 60))         "Invalid BYMINUTE value"
           testMaybe (rByHour rr) (and . map (\v->v >= 0 && v <= 23))           "Invalid BYHOUR value"
           testMaybe (rByMonthDay rr) (and . map (\v->v >= 1 && v <= 31))       "Invalid BYMONTHDAY value"
           testMaybe (rByMonthDay rr) (and . map (\v->v <= -1 && v >= -31))     "Invalid BYMONTHDAY value"
           testMaybe (rByYearDay rr) (and . map (\v->v >= 1 && v <= 366))       "Invalid BYYEARDAY value"
           testMaybe (rByYearDay rr) (and . map (\v->v <= -1 && v >= -366))     "Invalid BYYEARDAY value"
           testMaybe (rByWeekNo rr) (and . map (\v->v >= 1 && v <= 53))         "Invalid BYWEEKNO value"
           testMaybe (rByWeekNo rr) (and . map (\v->v <= -1 && v >= -53))       "Invalid BYWEEKNO value"
           test (isNothing $ rBySetPos rr)                                      "BYSETPOS not implemented"
           testMaybe (rByWeekDay rr) (and . map (\(_,v)->isNothing v))          "Numberic BYDAY not implemented"
        where test :: Bool -> String -> Writer [String] ()
              test True _     = return ()
              test False msg  = tell [msg]

              testMaybe :: Maybe a -> (a -> Bool) -> String -> Writer [String] ()
              testMaybe (Just a) condf b = test (condf a) b
              testMaybe Nothing _ _      = return ()

buildSched :: (CalendarTimeConvertible a, Moment a) => RRule -> [a] -> Schedule a
buildSched rr
        | Yearly <- rFreq rr =
                f enumMonths (rByMonth rr)
            -- >=> f enumWeekNumbers (rByWeekNo rr) -- TODO
            >=> f enumYearDays (rByYearDay rr)
            >=> f enumDays (rByMonthDay rr)
            >=> f enumWeekDaysInMonth (liftM (map fst) $ rByWeekDay rr)
            >=> f enumHours (rByHour rr)
            >=> f enumMinutes (rByMinute rr)
            >=> f enumSeconds (rBySecond rr)

        | Monthly <- rFreq rr =
                f filterMonths (rByMonth rr)
            >=> f enumDays (rByMonthDay rr)
            >=> f enumWeekDaysInMonth (liftM (map fst) $ rByWeekDay rr)
            >=> f enumHours (rByHour rr)
            >=> f enumMinutes (rByMinute rr)
            >=> f enumSeconds (rBySecond rr)

        | Weekly <- rFreq rr =
                f filterMonths (rByMonth rr)
            >=> f enumWeekDaysInMonth (liftM (map fst) $ rByWeekDay rr)
            >=> f enumHours (rByHour rr)
            >=> f enumMinutes (rByMinute rr)
            >=> f enumSeconds (rBySecond rr)

        | Daily <- rFreq rr =
                f filterMonths (rByMonth rr)
            >=> f filterDays (rByMonthDay rr)
            >=> f filterWeekDays (liftM (map fst) $ rByWeekDay rr)
            >=> f enumHours (rByHour rr)
            >=> f enumMinutes (rByMinute rr)
            >=> f enumSeconds (rBySecond rr)

        | Hourly <- rFreq rr =
                f filterMonths (rByMonth rr)
            >=> f filterYearDays (rByYearDay rr)
            >=> f filterDays (rByMonthDay rr)
            >=> f filterWeekDays (liftM (map fst) $ rByWeekDay rr)
            >=> f filterHours (rByHour rr)
            >=> f enumMinutes (rByMinute rr)
            >=> f enumSeconds (rBySecond rr)

        | Minutely <- rFreq rr =
                f filterMonths (rByMonth rr)
            >=> f filterYearDays (rByYearDay rr)
            >=> f filterDays (rByMonthDay rr)
            >=> f filterWeekDays (liftM (map fst) $ rByWeekDay rr)
            >=> f filterHours (rByHour rr)
            >=> f filterMinutes (rByMinute rr)
            >=> f enumSeconds (rBySecond rr)

        | Secondly <- rFreq rr =
                f filterMonths (rByMonth rr)
            >=> f filterYearDays (rByYearDay rr)
            >=> f filterDays (rByMonthDay rr)
            >=> f filterWeekDays (liftM (map fst) $ rByWeekDay rr)
            >=> f filterHours (rByHour rr)
            >=> f filterMinutes (rByMinute rr)
            >=> f filterSeconds (rBySecond rr)

        where f :: (b -> [a] -> Schedule a) -> Maybe b -> ([a] -> Schedule a)
              f g (Just a) = g a
              f _ Nothing  = return

recurrences :: RRule -> [UTCTime]
recurrences rr = recurrencesStarting rr (rDateStart rr)

recurrencesStarting :: RRule -> CalendarTime -> [UTCTime]
recurrencesStarting rr from = filterUntil $ filterCount $ r
        where period = case rFreq rr of 
                            Yearly      -> yearly
                            Monthly     -> monthly
                            Weekly      -> weekly
                            Daily       -> daily
                            Hourly      -> hourly
                            Minutely    -> minutely
                            Secondly    -> secondly

              start = maybe (error "Failed to convert start time") id $ fromCalendarTime from
              r = recur period `withStartOfWeek` rWeekStart rr
                               `by` rInterval rr
                               `starting` start
                               $ buildSched rr

              filterUntil, filterCount :: [UTCTime] -> [UTCTime]
              filterUntil = case rUntil rr of
                        Just d  -> takeWhile (<= ( maybe (error "Failed to convert until time") id
                                                 $ fromCalendarTime d))
                        Nothing -> id
              filterCount = case rCount rr of
                         Just c  -> take c
                         Nothing -> id
