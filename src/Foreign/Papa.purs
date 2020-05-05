
module Foreign.Papa
  ( parseCSV
  ) where

foreign import parseCSV :: String -> { data :: Array (Array String) }
