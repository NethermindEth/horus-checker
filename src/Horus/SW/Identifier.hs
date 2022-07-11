module Horus.SW.Identifier
  ( Identifier (..)
  , Member (..)
  , Struct (..)
  , Function (..)
  , getFunctionPc
  , getLabelPc
  )
where

import Data.Aeson (FromJSON (..), Value (Object), withObject, (.:))
import Data.Map (Map)
import Data.Text (Text, unpack)

import Horus.Label (Label)
import Horus.SW.CairoType (CairoType)
import Horus.SW.CairoType.JSON ()
import Horus.SW.ScopedName (ScopedName)

data Member = Member {me_cairoType :: CairoType, me_offset :: Int}
data Struct = Struct
  { st_fullName :: ScopedName
  , st_members :: Map Text Member
  , st_size :: Int
  }
data Function = Function {fu_pc :: Label, fu_decorators :: [Text]}

data Identifier
  = IAlias ScopedName
  | IConst Integer
  | IMember Member
  | IStruct Struct
  | IType CairoType
  | ILabel Label
  | IFunction Function
  | INamespace
  | IReference
  | IScope

getFunctionPc :: Identifier -> Maybe Label
getFunctionPc (IFunction f) = pure (fu_pc f)
getFunctionPc _ = Nothing

getLabelPc :: Identifier -> Maybe Label
getLabelPc (ILabel pc) = pure pc
getLabelPc _ = Nothing

instance FromJSON Identifier where
  parseJSON = withObject "Identifier" $ \v -> do
    typ <- v .: "type"
    case typ :: Text of
      "alias" -> IAlias <$> v .: "destination"
      "const" -> IConst <$> v .: "value"
      "member" -> IMember <$> parseJSON (Object v)
      "struct" -> IStruct <$> parseJSON (Object v)
      "label" -> ILabel <$> v .: "pc"
      "function" -> IFunction <$> parseJSON (Object v)
      "namespace" -> pure INamespace
      "reference" -> pure IReference
      "scope" -> pure IScope
      _ -> fail ("Unknown type: '" <> unpack typ <> "'")

instance FromJSON Member where
  parseJSON = withObject "Member" $ \v ->
    Member <$> v .: "cairo_type" <*> v .: "offset"

instance FromJSON Struct where
  parseJSON = withObject "Struct" $ \v ->
    Struct <$> v .: "full_name" <*> v .: "members" <*> v .: "size"

instance FromJSON Function where
  parseJSON = withObject "Function" $ \v ->
    Function <$> v .: "pc" <*> v .: "decorators"
