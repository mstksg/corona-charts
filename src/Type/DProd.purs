
module Type.DProd where

newtype DProd tag f = DProd (forall a. tag a -> f a)

runDProd :: forall tag f a. DProd tag f -> tag a -> f a
runDProd (DProd f) = f
