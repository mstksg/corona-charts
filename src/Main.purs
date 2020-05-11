module Main where

import Prelude

import Corona.Chart.UI as UI
import Corona.JHU (fetchCoronaData)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console
import Effect.Exception (throwException, error)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  dat <- fetchCoronaData >>= case _ of
    Right x -> pure x
    Left e  -> liftEffect (throwException (error e))
  body <- HA.awaitBody
  -- log "\xb1"

  runUI (UI.component dat) unit body

foreign import logMe :: forall a. a -> Effect Unit
