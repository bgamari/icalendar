module Data.ICalendar.Types ( ICalObject(..)
                            , ICalProperty(..)
                            -- * Property lookups
                            , lookupProps
                            , lookupProp
                            , lookupPropValue
                            , lookupPropParam
                            -- * Sub-object lookups
                            , lookupObjects
                            , lookupObject
                            ) where

import Data.Maybe (listToMaybe)

data ICalObject = ICalObject { icoName :: String
                             , icoProperties :: [ICalProperty]
                             , icoObjects :: [ICalObject]
                             } deriving (Show, Eq)

data ICalProperty = ICalProperty { icpName :: String
                                 , icpParams :: [(String, [String])]
                                 , icpValue :: String
                                 } deriving (Show, Eq)

-- | Convenience function to find all named properties of an 'ICalObject'
lookupProps :: ICalObject -> String -> [ICalProperty]
lookupProps obj propName = filter (\p -> icpName p == propName) (icoProperties obj)

-- | Convenience function to find a property of an 'ICalObject'
lookupProp :: ICalObject -> String -> Maybe ICalProperty
lookupProp obj propName = listToMaybe $ lookupProps obj propName

lookupPropValue :: ICalObject -> String -> Maybe String
lookupPropValue o propName =
        do (ICalProperty _ _ v) <- lookupProp o propName
           return v

lookupPropParam :: ICalObject -> String -> String -> Maybe [String]
lookupPropParam o propName paramName =
        do (ICalProperty _ params _) <- lookupProp o propName
           lookup paramName params

lookupObjects :: ICalObject -> String -> [ICalObject]
lookupObjects obj objName = filter (\o -> icoName o == objName) (icoObjects obj)

lookupObject :: ICalObject -> String -> Maybe ICalObject
lookupObject obj objName = listToMaybe $ lookupObjects obj objName

