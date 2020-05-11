
module Halogen.Util where

import Prelude

import Data.Coyoneda
import Data.NaturalTransformation
import Data.Newtype
import Data.Profunctor
import Halogen as H
import Halogen.Component.Profunctor
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenQ

hoistQuery :: forall h f f' i o m. (f' ~> f) -> H.Component h f i o m -> H.Component h f' i o m
hoistQuery f = H.unComponent (\c -> H.mkComponent
    { eval: c.eval <<< reHalogenQ f
    , initialState: c.initialState
    , render: c.render
    }
  )

reHalogenQ :: forall f f' a i r. (f ~> f') -> HalogenQ f a i r -> HalogenQ f' a i r
reHalogenQ f = case _ of
    Initialize x -> Initialize x
    Finalize x -> Finalize x
    Receive i x -> Receive i x
    Action a x -> Action a x
    Query q g -> Query (hoistCoyoneda f q) g

dimapComponent
    :: forall h f i i' o o' m. Functor f
    => (i -> i')
    -> (o -> o')
    -> H.Component h f i' o  m
    -> H.Component h f i o' m
dimapComponent f g = unwrap <<< dimap f g <<< ProComponent

trimapComponent
    :: forall h f f' i i' o o' m. Functor f
    => (f ~> f')
    -> (i -> i')
    -> (o -> o')
    -> H.Component h f' i' o  m
    -> H.Component h f  i o' m
trimapComponent f g h = unwrap <<< dimap g h <<< ProComponent <<< hoistQuery f

    

classProp :: forall r a. String -> HP.IProp (class :: String | r) a
classProp cl = HP.class_ (HH.ClassName cl)
