
module Data.Point where


type Point a b c d = { x :: a, y :: b, z :: c, t :: d }

type PointF f a b c d = { x :: f a, y :: f b, z :: f c, t :: f d }

hoistPointF
    :: forall f g a b c d.
       (forall x. f x -> g x)
    -> PointF f a b c d
    -> PointF g a b c d
hoistPointF f { x, y, z, t } =
    { x: f x
    , y: f y
    , z: f z
    , t: f t
    }


type Point2D a b = { x :: a, y :: b }
-- type NoZ a b d = { x :: a, y :: b, t :: d }

