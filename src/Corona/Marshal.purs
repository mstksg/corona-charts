
module Corona.Marshal where

import Prelude

import Corona.Chart
import D3.Scatter.Type
import Data.Array as A
import Data.Either
import Data.Exists
import Type.DProd
import Data.Int
import Data.Int.Parse
import Data.Maybe
import Data.ModifiedJulianDay as MJD
import Data.Newtype
import Data.String as String
import Global as G
import Text.Parsing.StringParser as P
import Text.Parsing.StringParser.CodeUnits as P
import Text.Parsing.StringParser.Combinators as P
import Type.Ap
import Type.Chain as C
import Type.DSum
import Type.Equiv
import Type.GCompare
import Undefined

class Marshal a where
    serialize :: a -> String
    parse :: P.Parser a

class Marshal1 f where
    serialize1 :: forall a. f a -> String
    parse1 :: P.Parser (DSum SType f)

class Marshal2 f where
    serialize2 :: forall a b. f a b -> String
    parse2 :: P.Parser (DSum2 SType f)

-- | when you know what type you need to parse
class MarshalS f where
    serializeS :: forall a. SType a -> f a -> String
    parseS :: forall a. SType a -> P.Parser (f a)

instance marInt :: Marshal Int where
    serialize = show
    parse = do
      neg <- P.option identity (negate <$ P.char '-')
      neg <$> digits

instance marDay :: Marshal MJD.Day where
    serialize (MJD.Day x) = serialize x
    parse = MJD.Day <$> parse

instance marDays :: Marshal Days where
    serialize (Days x) = serialize x
    parse = Days <$> parse

digits :: P.Parser Int
digits = do
    ds <- String.fromCodePointArray
      <<< map String.codePointFromChar
      <<< A.fromFoldable
      <$> P.many1 P.anyDigit
    case parseInt ds (toRadix 10) of
      Nothing -> P.fail $ "Bad digits: " <> ds
      Just i  -> pure i

-- | store in fixed-precision with 3 decimal places
instance marNumber :: Marshal Number where
    serialize = fromMaybe "0" <<< G.toFixed 3
    parse = do
      neg <- P.option identity (negate <$ P.char '-')
      ds  <- digits
      _   <- P.char '.'
      ns  <- digits
      pure $ neg (toNumber ds + toNumber ns / 1000.0)

-- | store the hundred-times version
instance marPercent :: Marshal Percent where
    serialize (Percent d) = serialize (d * 100.0)
    parse = Percent <<< (_ / 100.0) <$> parse

instance marCutoffType :: Marshal CutoffType where
    serialize = case _ of
      After  -> "A"
      Before -> "B"
    parse = do
      c <- P.anyChar
      case c of
        'A' -> pure After
        'B' -> pure Before
        _   -> P.fail $ "invalid cutoff type: " <> show c

sTypeSerialize :: forall a. SType a -> a -> String
sTypeSerialize = case _ of
    SDay r -> serialize <<< equivTo r
    SDays r -> serialize <<< equivTo r
    SInt r -> serialize <<< equivTo r
    SNumber r -> serialize <<< equivTo r
    SPercent r -> serialize <<< equivTo r

sTypeParse :: forall a. SType a -> P.Parser a
sTypeParse = case _ of
    SDay r -> equivFromF r parse
    SDays r -> equivFromF r parse
    SInt r -> equivFromF r parse
    SNumber r -> equivFromF r parse
    SPercent r -> equivFromF r parse

instance mar1Base :: Marshal1 BaseProjection where
    serialize1 = case _ of
      Time _      -> "T"
      Confirmed _ -> "C"
      Deaths _    -> "D"
      Recovered _ -> "R"
    parse1 = do
      c <- P.anyChar
      case c of
        'T' -> pure $ sDay :=> Time refl
        'C' -> pure $ sInt :=> Confirmed refl
        'D' -> pure $ sInt :=> Deaths refl
        'R' -> pure $ sInt :=> Recovered refl
        _   -> P.fail $ "invalid base projection: " <> show c

serializeSType :: forall a. SType a -> String
serializeSType = case _ of
    SDay  _ -> "D"
    SDays _ -> "S"
    SInt  _ -> "I"
    SNumber _ -> "N"
    SPercent _ -> "P"
parseSType :: P.Parser (Exists SType)
parseSType = do
    c <- P.anyChar
    case c of
      'D' -> pure $ mkExists sDay
      'S' -> pure $ mkExists sDays
      'I' -> pure $ mkExists sInt
      'N' -> pure $ mkExists sNumber
      'P' -> pure $ mkExists sPercent
      _   -> P.fail $ "invalid stype: " <> show c

instance marCond :: STypeable a => Marshal (Condition a) where
    serialize = case _ of
      AtLeast x -> "L" <> sTypeSerialize sType x
      AtMost  x -> "M" <> sTypeSerialize sType x
    parse = do
      c <- P.anyChar
      x <- sTypeParse sType
      case c of
        'L' -> pure $ AtLeast x
        'M' -> pure $ AtMost  x
        _   -> P.fail $ "invalid condition: " <> show c

instance mar1Op :: STypeable a => Marshal1 (Operation a) where
    serialize1 = case _ of
      Delta _ _ -> "C"
      PGrowth _ _ -> "G"
      Window _ n -> "W" <> serialize n
      PMax _ _ -> "M"
      Restrict t _ ct co -> "R" <> serialize ct <> serialize co
      Take _ n ct -> "T" <> serialize n <> serialize ct
      DayNumber _ ct -> "N" <> serialize ct
      PointDate  _ -> "D"
    parse1 = do
        c <- P.anyChar
        case c of
          'C' -> case toNType t of
            Right nt -> pure $ t :=> Delta nt refl
            Left _   -> P.fail $ "delta does not support " <> gshow t
          'G' -> case toNType t of
            Right nt -> pure $ sPercent :=> PGrowth nt refl
            Left _   -> P.fail $ "pgrowth does not support " <> gshow t
          'W' -> case toNType t of
            Right nt -> runExists (\tf -> do
                n <- parse
                pure $ fromNType (toFractionalOut tf) :=> Window tf n
              ) (toFractional nt)
            Left _   -> P.fail $ "window does not support " <> gshow t
          'M' -> case toNType t of
            Right nt -> pure $ sPercent :=> PMax nt refl
            Left _   -> P.fail $ "pmax does not support " <> gshow t
          'R' -> do
             ct   <- parse
             cond <- parse
             pure $ t :=> Restrict t refl ct cond
          'T' -> do
             n  <- parse
             ct <- parse
             pure $ t :=> Take refl n ct
          'N' -> do
             ct <- parse
             pure $ sDays :=> DayNumber refl ct
          'D' -> pure $ sDay :=> PointDate refl
          _   -> P.fail $ "invalid operation: " <> show c
      where
        t :: SType a
        t = sType

instance mar1ChainOp :: STypeable a => Marshal1 (C.Chain Operation a) where
    serialize1 = case _ of
      C.Nil  _   -> "N"
      C.Cons cap -> withAp cap (\x xs ->
        let tNext = operationType x sType
        in  "C" <> serialize1 x <> case tNext of
              SDay r -> serialize1 (equivToF2 r xs)
              SDays r -> serialize1 (equivToF2 r xs)
              SInt r -> serialize1 (equivToF2 r xs)
              SNumber r -> serialize1 (equivToF2 r xs)
              SPercent r -> serialize1 (equivToF2 r xs)
      )
    parse1 = do
       c <- P.anyChar
       case c of
         'N' -> pure (t :=> C.nil)
         'C' -> do
           dx <- parse1
           withDSum dx (\tIn x -> do
             dxs <- case tIn of
               SDay r -> equivFromFF r <$> parse1
               SDays r -> equivFromFF r <$> parse1
               SInt r -> equivFromFF r <$> parse1
               SNumber r -> equivFromFF r <$> parse1
               SPercent r -> equivFromFF r <$> parse1
               _ -> undefined
             withDSum dxs (\tOut xs ->
               pure $ tOut :=> C.cons x xs
             )
           )
         _   -> P.fail $ "invalid chain: " <> show c
      where
        t :: SType a
        t = sType

instance mar1Projection :: Marshal1 Projection where
    serialize1 p = withProjection p (\bo ->
      serialize1 bo.base <>
        case baseType bo.base of
          SDay r -> serialize1 (equivToF2 r bo.operations)
          SDays r -> serialize1 (equivToF2 r bo.operations)
          SInt r -> serialize1 (equivToF2 r bo.operations)
          SNumber r -> serialize1 (equivToF2 r bo.operations)
          SPercent r -> serialize1 (equivToF2 r bo.operations)
      )
    parse1 = do
      db <- parse1
      withDSum db (\tIn b -> do
        dops <- case tIn of
          SDay r -> equivFromFF r <$> parse1
          SDays r -> equivFromFF r <$> parse1
          SInt r -> equivFromFF r <$> parse1
          SNumber r -> equivFromFF r <$> parse1
          SPercent r -> equivFromFF r <$> parse1
        withDSum dops (\tOut ops ->
          pure $ tOut :=> projection { base: b, operations: ops }
        )
      )

instance marScale :: STypeable a => Marshal (Scale a) where
    serialize = case _ of
      Date   _ -> "D"
      Linear _ -> "I"
      Log    _ -> "O"
    parse = do
        c <- P.anyChar
        case c of
          'D' -> case t of
            SDay r -> pure $ Date r
            _      -> P.fail $ "invalid type for date scale: " <> gshow t
          'I' -> case toNType t of
            Left (Left  _) -> P.fail "cannot use SDay for linear scale"
            Left (Right r) -> pure $ Linear (Left r)
            Right nt       -> pure $ Linear (Right nt)
          'O' -> case toNType t of
            Right nt       -> pure $ Log nt
            _      -> P.fail $ "invalid type for log scale: " <> gshow t
          _ -> P.fail $ "invalid scale: " <> show c
      where
        t :: SType a
        t = sType

instance marNScale :: Marshal NScale where
    serialize ns = case runNScale ns nInt of
      Date   _ -> "?"   -- umm...wish we could eliminate this (Int ~ Day) somehow
      Linear _ -> "I"
      Log    _ -> "O"
    parse = do
      c <- P.anyChar
      case c of
        'I' -> pure $ NScale (DProd (Linear <<< Right))
        'O' -> pure $ NScale (DProd Log)
        _   -> P.fail $ "invalid nscale: " <> show c

instance marDSum :: Marshal1 f => Marshal (DSum SType f) where
    serialize ds = withDSum ds (\_ -> serialize1)
    parse = parse1
