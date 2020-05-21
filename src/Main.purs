module Main where

import Prelude

import Corona.Chart.UI as UI
import Web.DOM.ParentNode as DOM
import Corona.Data.JHU (fetchCoronaData)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console
import Effect.Exception (throwException, error)
import Halogen.Aff as HA
import Data.Maybe
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  dat <- fetchCoronaData >>= case _ of
    Right x -> pure x
    Left e  -> liftEffect $ throwException (error e)
  _         <- HA.awaitBody
  container <- HA.selectElement (DOM.QuerySelector "#ui")
  case container of
    Nothing   -> liftEffect $ throwException (error "#ui not found")
    Just cont -> runUI (UI.component dat) unit cont

foreign import logMe :: forall a. a -> Effect Unit
