
module Type.Handler where

import Prelude
import Data.Maybe
import Data.Boolean
import Data.Either

class Handle a b | a -> b, b -> a where
    handle   :: a -> b
    unHandle :: b -> a
class Handle1 a b | a -> b, b -> a where
    handle1   :: forall x. a x -> b x
    unHandle1 :: forall x. b x -> a x
class Handle2 a b | a -> b, b -> a where
    handle2   :: forall x y. a x y -> b x y
    unHandle2 :: forall x y. b x y -> a x y

type HandleFunc a b = a -> b
type UnHandleFunc a b = b -> a

type HandleFunc1 a b = forall x. a x -> b x
type UnHandleFunc1 a b = forall x. b x -> a x

type HandleFunc2 a b = forall x y. a x y -> b x y
type UnHandleFunc2 a b = forall x y. b x y -> a x y

newtype OnBoolean = OnBoolean (forall r. { false:: Unit -> r, true:: Unit -> r } -> r)

instance handleBoolean :: Handle Boolean OnBoolean where
    handle b
      | b         = OnBoolean (\f -> f.true unit)
      | otherwise = OnBoolean (\f -> f.false unit)
    unHandle (OnBoolean f) = f { false: const false, true: const true }

newtype OnMaybe a = OnMaybe
    (forall r. { nothing :: Unit -> r, just :: a -> r } -> r)

instance handleMaybe1 :: Handle1 Maybe OnMaybe where
    handle1 = case _ of
      Nothing -> OnMaybe (\f -> f.nothing unit)
      Just  x -> OnMaybe (\f -> f.just x)
    unHandle1 (OnMaybe f) = f { nothing: const Nothing, just: Just }
instance handleMaybe :: Handle (Maybe a) (OnMaybe a) where
    handle   = handle1
    unHandle = unHandle1


newtype OnEither a b = OnEither
    (forall r. { left :: a -> r, right :: b -> r } -> r)

instance handleEither2 :: Handle2 Either OnEither where
    handle2 = case _ of
      Left  x -> OnEither (\f -> f.left  x)
      Right y -> OnEither (\f -> f.right y)
    unHandle2 (OnEither f) = f { left: Left, right: Right }
instance handleEither1 :: Handle1 (Either a) (OnEither a) where
    handle1   = handle2
    unHandle1 = unHandle2
instance handleEither :: Handle (Either a b) (OnEither a b) where
    handle   = handle1
    unHandle = unHandle1

