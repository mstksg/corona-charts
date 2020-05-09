
module Data.BSum where

data BSum f a b =
      BLeft  (f a)
    | BRight (f b)

