
module D3.Scatter.Type where

import Prelude

import Data.Either
import Data.Enum
import Data.Int
import Data.Newtype
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Maybe
import Data.ModifiedJulianDay (Day)
import Data.Semiring
import Data.Tuple
import Effect
import Foreign.Object as O
import Type.DProd
import Type.DSum
import Type.Equality
import Type.Equiv
import Type.GCompare
import Type.Handler
import Data.Exists
import Web.DOM.Element (Element)

-- | interval, days since
newtype Days = Days Int

unDays :: Days -> Int
unDays (Days n) = n

derive instance eqDays :: Eq Days
derive instance ordDays :: Ord Days
instance showDays :: Show Days where
    show (Days n) = "Days " <> show n

instance daySemiring :: Semiring Days where
    add (Days x) (Days y) = Days (x + y)
    zero = Days zero
    mul (Days x) (Days y) = Days (x * y)
    one = Days one
instance dayRing :: Ring Days where
    sub (Days x) (Days y) = Days (x - y)

newtype Percent  = Percent Number

unPercent :: Percent -> Number
unPercent (Percent n) = n

instance eqPercent :: Eq Percent where
    eq (Percent x) (Percent y) = x == y
instance ordPercent :: Ord Percent where
    compare (Percent x) (Percent y) = compare x y
instance percentSemiring :: Semiring Percent where
    add (Percent x) (Percent y) = Percent (x + y)
    zero = Percent zero
    mul (Percent x) (Percent y) = Percent (x * y)
    one = Percent one
instance percentRing :: Ring Percent where
    sub (Percent x) (Percent y) = Percent (x - y)
instance percentCRing :: CommutativeRing Percent
instance percentERing :: EuclideanRing Percent where
    degree (Percent x) = degree x
    div (Percent x) (Percent y) = Percent (div x y)
    mod (Percent x) (Percent y) = Percent (mod x y)
instance percentShow :: Show Percent where
    -- show (Percent n) = show (n * toNumber 100) <> "%"
    show (Percent n) = "Percent " <> show n


data SType a =
        SDay     (a ~ Day)
      | SDays    (a ~ Days)
      | SInt     (a ~ Int)
      | SNumber  (a ~ Number)
      | SPercent (a ~ Percent)

sDay :: SType Day
sDay = SDay refl
sDays :: SType Days
sDays = SDays refl
sInt :: SType Int
sInt = SInt refl
sNumber :: SType Number
sNumber = SNumber refl
sPercent :: SType Percent
sPercent = SPercent refl

class STypeable a where
    sType :: SType a
instance stDay :: STypeable Day where
    sType = sDay
instance stDays :: STypeable Days where
    sType = sDays
instance stInt :: STypeable Int where
    sType = sInt
instance stNumber :: STypeable Number where
    sType = sNumber
instance stPercent :: STypeable Percent where
    sType = sPercent

instance decideSType :: Decide SType where
    decide = case _ of
      SDay rX -> case _ of
        SDay rY -> Just (equivFromF rY rX)
        _       -> Nothing
      SDays rX -> case _ of
        SDays rY -> Just (equivFromF rY rX)
        _        -> Nothing
      SInt rX -> case _ of
        SInt rY -> Just (equivFromF rY rX)
        _        -> Nothing
      SNumber rX -> case _ of
        SNumber rY -> Just (equivFromF rY rX)
        _        -> Nothing
      SPercent rX -> case _ of
        SPercent rY -> Just (equivFromF rY rX)
        _        -> Nothing

instance geqSType :: GEq SType where
    geq = decide
instance gordSType :: GOrd SType where
    gcompare x y = case geq x y of
      Just refl -> GEQ refl
      Nothing   -> case compare (sTypeIx x) (sTypeIx y) of
        LT -> GLT
        EQ -> GLT   -- ???
        GT -> GGT

instance eqSType :: Eq (SType a) where
    eq x y = isJust (geq x y)
instance ordSType :: Ord (SType a) where
    compare x y = toOrdering (gcompare x y)


instance showSType :: Show (SType a) where
    show = case _ of
      SDay _ -> "SDay"
      SDays _ -> "SDays"
      SInt _ -> "SInt"
      SNumber _ -> "SNumber"
      SPercent _ -> "SPercent"
instance gshowSType :: GShow SType where
    gshow = show

sTypeIx :: forall a. SType a -> Int
sTypeIx = case _ of
    SDay     _ -> 0
    SDays    _ -> 1
    SInt     _ -> 2
    SNumber  _ -> 3
    SPercent _ -> 4


sTypeShow :: forall a. SType a -> a -> String
sTypeShow = case _ of
    SDay     r -> show <<< equivTo r
    SDays    r -> show <<< equivTo r
    SInt     r -> show <<< equivTo r
    SNumber  r -> show <<< equivTo r
    SPercent r -> show <<< equivTo r

sTypeCompare :: forall a. SType a -> a -> a -> Ordering
sTypeCompare = case _ of
    SDay     r -> \x y -> compare (equivTo r x) (equivTo r y)
    SDays    r -> \x y -> compare (equivTo r x) (equivTo r y)
    SInt     r -> \x y -> compare (equivTo r x) (equivTo r y)
    SNumber  r -> \x y -> compare (equivTo r x) (equivTo r y)
    SPercent r -> \x y -> compare (equivTo r x) (equivTo r y)


-- | subset of numeric stypes
data NType a =
        -- NDays    (a ~ Days)
        NInt     (a ~ Int)
      | NNumber  (a ~ Number)
      | NPercent (a ~ Percent)

instance showNType :: Show (NType a) where
    show = case _ of
      -- NDays _ -> "NDays"
      NInt _ -> "NInt"
      NNumber _ -> "NNumber"
      NPercent _ -> "NPercent"
instance gshowNType :: GShow NType where
    gshow = show

fromNType :: forall a. NType a -> SType a
fromNType = case _ of
    NInt     r -> SInt r
    -- NDays    r -> SDays r
    NNumber  r -> SNumber r
    NPercent r -> SPercent r

toNType :: forall a. SType a -> Either (a ~ Day || a ~ Days) (NType a)
toNType = case _ of
    SDay     r -> Left (Left r)
    SDays    r -> Left (Right r)
    -- SDays    r -> Right $ NDays r
    SInt     r -> Right $ NInt r
    SNumber  r -> Right $ NNumber r
    SPercent r -> Right $ NPercent r

nTypeNumber :: forall a. NType a -> a -> Number
nTypeNumber = case _ of
    NInt   r -> toNumber <<< equivTo r
    NNumber r -> equivTo r
    NPercent r -> unPercent <<< equivTo r

numberNType :: forall a. NType a -> Number -> a
numberNType = case _ of
    NInt r -> equivFrom r <<< round
    NNumber r -> equivFrom r
    NPercent r -> equivFrom r <<< Percent

nTypeSubtract :: forall a. NType a -> a -> a -> a
nTypeSubtract = case _ of
    NInt     r -> \x y -> equivFrom r (equivTo r x - equivTo r y)
    NNumber  r -> \x y -> equivFrom r (equivTo r x - equivTo r y)
    NPercent r -> \x y -> equivFrom r (equivTo r x - equivTo r y)

-- nDays :: NType Days
-- nDays = NDays refl
nInt :: NType Int
nInt = NInt refl
nNumber :: NType Number
nNumber = NNumber refl
nPercent :: NType Percent
nPercent = NPercent refl

class NTypeable a where
    nType :: NType a
-- instance ntDays :: NTypeable Days where
--     nType = nDays
instance ntInt :: NTypeable Int where
    nType = nInt
instance ntNumber :: NTypeable Number where
    nType = nNumber
instance ntPercent :: NTypeable Percent where
    nType = nPercent

-- numericRing :: forall a. NType a -> (forall r. (Ring a => r) -> r)
-- numericRing = case _ of
--     NInt     refl -> \x -> withEquiv refl x
--     NNumber  refl -> \x -> withEquiv refl x
--     NPercent refl -> \x -> withEquiv refl x

type Point a b c = { x :: a, y :: b, z :: c }

type SeriesData a b c =
    { name   :: String
    , values :: Array (Point a b c)
    }

infixr 1 type Either as ||

data Scale a = Date   (a ~ Day)
             | Linear (a ~ Days || NType a)
             | Log    (NType a)

instance gshowScale :: GShow Scale where
    gshow = case _ of
      Date _ -> "Date"
      Linear _ -> "Linear"
      Log _    -> "Log"
instance showScale :: Show (Scale a) where
    show = gshow

validScales :: forall a. SType a -> Array (Scale a)
validScales = case _ of
    SDay r -> [Date r]
    SDays r -> [Linear (Left r)]
    SInt r -> [Linear (Right (NInt r)), Log (NInt r)]
    SNumber r -> [Linear (Right (NNumber r)), Log (NNumber r)]
    SPercent r -> [Linear (Right (NPercent r)), Log (NPercent r)]

defaultScale :: forall a. SType a -> Scale a
defaultScale = case _ of
    SDay  r    -> Date r
    SDays r    -> Linear (Left r)
    -- SDays r    -> Linear (NDays r)
    SInt  r    -> Log (NInt  r)  -- maybe log?
    SNumber r  -> Log (NNumber r)
    SPercent r -> Linear (Right (NPercent r))

sDate :: Scale Day
sDate = Date refl
sLinear :: forall a. NTypeable a => Scale a
sLinear = Linear (Right nType)
sLog :: forall a. NTypeable a => Scale a
sLog = Log nType

newtype NScale = NScale (DProd NType Scale)

derive instance newtypeNScale :: Newtype NScale _

instance showNScale :: Show NScale where
    show (NScale x) = gshow (runDProd x nInt)

toNScale :: forall a. Scale a -> Either (a ~ Day || a ~ Days) NScale
toNScale = case _ of
    Date   r  -> Left  (Left r)
    Linear dn -> case dn of
      Left  d -> Left (Right d)
      Right n -> Right (NScale (DProd (Linear <<< Right)))
    Log    n  -> Right (NScale (DProd Log))

runNScale :: forall a. NScale -> NType a -> Scale a
runNScale (NScale x) = runDProd x

type AxisConf a = { scale :: Scale a, label :: String }

type ScatterPlot a b c =
        { xAxis  :: AxisConf a
        , yAxis  :: AxisConf b
        , zAxis  :: AxisConf c
        , series :: Array (SeriesData a b c)
        }

type SomeScatterPlot =
        forall r.
          (forall a b c. SType a
                  -> SType b
                  -> SType c
                  -> ScatterPlot a b c
                  -> r
          )
          -> r

newtype OnScale a = OnScale
    (forall r.
        { date   :: a ~ Day -> r
        , linear :: (a ~ Days || NType a) -> r
        , log    :: NType a -> r
        } -> r
    )

instance handleScale :: Handle (Scale a) (OnScale a) where
    handle   = handle1
    unHandle = unHandle1
instance handle1Scale :: Handle1 Scale OnScale where
    handle1 = case _ of
      Date   refl -> OnScale (\h -> h.date   refl)
      Linear nt   -> OnScale (\h -> h.linear nt  )
      Log    nt   -> OnScale (\h -> h.log    nt  )
    unHandle1 (OnScale f) = f { date: Date, linear: Linear, log: Log }

newtype OnSType a = OnSType (forall r.
      { day     :: a ~ Day    -> r
      , days    :: a ~ Days   -> r
      , int     :: a ~ Int    -> r
      , number  :: a ~ Number -> r
      , percent :: a ~ Percent -> r
      } -> r
    )

instance handleSType :: Handle (SType a) (OnSType a) where
    handle   = handle1
    unHandle = unHandle1
instance handle1SType :: Handle1 SType OnSType where
    handle1 = case _ of
      SDay     refl -> OnSType (\h -> h.day    refl)
      SDays    refl -> OnSType (\h -> h.days   refl)
      SInt     refl -> OnSType (\h -> h.int   refl)
      SNumber  refl -> OnSType (\h -> h.number refl)
      SPercent refl -> OnSType (\h -> h.percent refl)
    unHandle1 (OnSType f) = f
      { day:     SDay
      , days:    SDays
      , int:    SInt
      , number:  SNumber
      , percent: SPercent
      }

