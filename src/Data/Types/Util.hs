module Data.Types.Util where

import Data.OpenApi (NamedSchema, Schema, Definitions)
import Data.OpenApi.Schema
import Data.OpenApi.Declare
import Data.Data
import GHC.Generics
import Data.OpenApi.Internal.Schema
import Data.List
import Data.Char

schemaOpts :: (Generic a, GToSchema (Rep a), Typeable a) => String -> Proxy a -> Declare (Definitions Schema) NamedSchema
schemaOpts prefix = genericDeclareNamedSchema $ defaultSchemaOptions
  { fieldLabelModifier = \name ->
      let toLowerCase [] = []
          toLowerCase (c:cs) = toLower c : cs
      in maybe name toLowerCase $ stripPrefix prefix name
  }