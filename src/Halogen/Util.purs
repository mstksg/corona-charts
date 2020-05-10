
module Halogen.Util where

import Prelude

import Data.Coyoneda
import Data.NaturalTransformation
import Halogen as H
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

classProp :: forall r a. String -> HP.IProp (class :: String | r) a
classProp cl = HP.class_ (HH.ClassName cl)
