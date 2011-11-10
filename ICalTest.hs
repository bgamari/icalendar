import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.ICalendar.Parse
import Data.ICalendar.RRule
import Data.ICalendar.Pretty
import Data.ICalendar.Types
import Data.ICalendar.Event
import Data.Time.LocalTime

main = do f <- readFile "basic.ics"
          events <- readEvents $ T.pack f
          let event = events !! 2
          print $ events
          print $ take 50 $ recurrences $ fromJust $ iceRRule event

