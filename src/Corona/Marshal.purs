
module Corona.Marshal where

import Prelude

import Corona.Chart
import Corona.Data as Corona
import D3.Scatter.Type
import Data.Array as A
import Data.Either
import Data.Exists
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
import Type.DProd
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

instance marBool :: Marshal Boolean where
    serialize b
      | b         = "t"
      | otherwise = "f"
    parse = do
      c <- P.anyChar
      case c of
        't' -> pure true
        'f' -> pure false
        _   -> P.fail $ "invalid boolean type: " <> show c

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

instance marStr :: Marshal String where
    serialize = identity
    parse = P.Parser $ \ps -> Right
      { result: String.drop ps.pos ps.str
      , suffix: ps { pos = String.length ps.str }
      }

instance marCutoffType :: Marshal CutoffType where
    serialize = case _ of
      After  -> "a"
      Before -> "b"
    parse = do
      c <- P.anyChar
      case c of
        'a' -> pure After
        'b' -> pure Before
        _   -> P.fail $ "invalid cutoff type: " <> show c

instance marDataset :: Marshal Corona.Dataset where
    serialize = case _ of
      Corona.WorldData -> "w"
      Corona.USData    -> "u"
    parse = do
      c <- P.anyChar
      case c of
        'w' -> pure Corona.WorldData
        'u' -> pure Corona.USData
        _   -> P.fail $ "invalid dataset: " <> show c


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
      Time _      -> "t"
      Confirmed _ -> "c"
      Deaths _    -> "d"
      Recovered _ -> "r"
      Active    _ -> "a"
    parse1 = do
      c <- P.anyChar
      case c of
        't' -> pure $ sDay :=> Time refl
        'c' -> pure $ sInt :=> Confirmed refl
        'd' -> pure $ sInt :=> Deaths refl
        'r' -> pure $ sInt :=> Recovered refl
        'a' -> pure $ sInt :=> Active refl
        _   -> P.fail $ "invalid base projection: " <> show c

serializeSType :: forall a. SType a -> String
serializeSType = case _ of
    SDay  _ -> "d"
    SDays _ -> "s"
    SInt  _ -> "i"
    SNumber _ -> "n"
    SPercent _ -> "p"
parseSType :: P.Parser (Exists SType)
parseSType = do
    c <- P.anyChar
    case c of
      'd' -> pure $ mkExists sDay
      's' -> pure $ mkExists sDays
      'i' -> pure $ mkExists sInt
      'n' -> pure $ mkExists sNumber
      'p' -> pure $ mkExists sPercent
      _   -> P.fail $ "invalid stype: " <> show c

instance marCond :: STypeable a => Marshal (Condition a) where
    serialize = case _ of
      AtLeast x -> "l" <> sTypeSerialize sType x
      AtMost  x -> "m" <> sTypeSerialize sType x
    parse = do
      c <- P.anyChar
      x <- sTypeParse sType
      case c of
        'l' -> pure $ AtLeast x
        'm' -> pure $ AtMost  x
        _   -> P.fail $ "invalid condition: " <> show c

instance mar1Op :: STypeable a => Marshal1 (Operation a) where
    serialize1 = case _ of
      Delta _ _ -> "c"
      PGrowth _ _ -> "g"
      Window _ n -> "w" <> serialize n
      PMax _ _ -> "m"
      Restrict t _ ct co -> "r" <> serialize ct <> serialize co
      Take _ n ct -> "t" <> serialize n <> serialize ct
      Lag _ n     -> "l" <> serialize n
      DayNumber _ ct -> "n" <> serialize ct
      PerCapita _ -> "p"
      PointDate  _ -> "d"
    parse1 = do
        c <- P.anyChar
        case c of
          'c' -> case toNType t of
            Right nt -> pure $ t :=> Delta nt refl
            Left _   -> P.fail $ "delta does not support " <> gshow t
          'g' -> case toNType t of
            Right nt -> pure $ sPercent :=> PGrowth nt refl
            Left _   -> P.fail $ "pgrowth does not support " <> gshow t
          'w' -> case toNType t of
            Right nt -> runExists (\tf -> do
                n <- parse
                pure $ fromNType (toFractionalOut tf) :=> Window tf n
              ) (toFractional nt)
            Left _   -> P.fail $ "window does not support " <> gshow t
          'm' -> case toNType t of
            Right nt -> pure $ sPercent :=> PMax nt refl
            Left _   -> P.fail $ "pmax does not support " <> gshow t
          'r' -> do
             ct   <- parse
             cond <- parse
             pure $ t :=> Restrict t refl ct cond
          't' -> do
             n  <- parse
             ct <- parse
             pure $ t :=> Take refl n ct
          'l' -> do
             n  <- parse
             pure $ t :=> Lag refl n
          'n' -> do
             ct <- parse
             pure $ sDays :=> DayNumber refl ct
          'p' -> case toNType t of
            Right nt -> pure $
              runExists (\tf -> fromNType (toFractionalOut tf) :=> PerCapita tf)
                        (toFractional nt)
            Left _   -> P.fail $ "percapita does not support " <> gshow t
          'd' -> pure $ sDay :=> PointDate refl
          _   -> P.fail $ "invalid operation: " <> show c
      where
        t :: SType a
        t = sType

instance mar1ChainOp :: STypeable a => Marshal1 (C.Chain Operation a) where
    serialize1 = case _ of
      C.Nil  _   -> "n"
      C.Cons cap -> withAp cap (\x xs ->
        let tNext = operationType x sType
        in  "c" <> serialize1 x <> case tNext of
              SDay r -> serialize1 (equivToF2 r xs)
              SDays r -> serialize1 (equivToF2 r xs)
              SInt r -> serialize1 (equivToF2 r xs)
              SNumber r -> serialize1 (equivToF2 r xs)
              SPercent r -> serialize1 (equivToF2 r xs)
      )
    parse1 = do
       c <- P.anyChar
       case c of
         'n' -> pure (t :=> C.nil)
         'c' -> do
           dx <- parse1
           withDSum dx (\tIn x -> do
             dxs <- case tIn of
               SDay r -> equivFromFF r <$> parse1
               SDays r -> equivFromFF r <$> parse1
               SInt r -> equivFromFF r <$> parse1
               SNumber r -> equivFromFF r <$> parse1
               SPercent r -> equivFromFF r <$> parse1
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
      Date   _ -> "d"
      Linear _ b -> "i" <> serialize b
      Log    _ -> "o"
      SymLog _ -> "s"
    parse = do
        c <- P.anyChar
        case c of
          'd' -> case t of
            SDay r -> pure $ Date r
            _      -> P.fail $ "invalid type for date scale: " <> gshow t
          'i' -> do
            b <- parse
            case toNType t of
              Left (Left  _) -> P.fail "cannot use SDay for linear scale"
              Left (Right r) -> pure $ Linear (Left r) b
              Right nt       -> pure $ Linear (Right nt) b
          'o' -> case toNType t of
            Right nt       -> pure $ Log nt
            _      -> P.fail $ "invalid type for log scale: " <> gshow t
          's' -> case toNType t of
            Right nt       -> pure $ SymLog nt
            _      -> P.fail $ "invalid type for symlog scale: " <> gshow t
          _ -> P.fail $ "invalid scale: " <> show c
      where
        t :: SType a
        t = sType

instance marNScale :: Marshal NScale where
    serialize ns = case runNScale ns nInt of
      Date   _   -> "?"   -- umm...wish we could eliminate this (Int ~ Day) somehow
      Linear _ b -> "i" <> serialize b
      Log    _  -> "o"
      SymLog _  -> "s"
    parse = do
      c <- P.anyChar
      case c of
        'i' -> do
           b <- parse
           pure $ NScale (DProd (flip Linear b <<< Right))
        'o' -> pure $ NScale (DProd Log)
        's' -> pure $ NScale (DProd SymLog)
        _   -> P.fail $ "invalid nscale: " <> show c

instance marDSum :: Marshal1 f => Marshal (DSum SType f) where
    serialize ds = withDSum ds (\_ -> serialize1)
    parse = parse1
