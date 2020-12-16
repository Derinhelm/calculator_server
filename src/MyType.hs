{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module MyType
  ( Res(..)
  ) where
  
import Data.Aeson
import Data.Maybe
import Data.Swagger
import GHC.Generics

data Res = Res
  { resOperator :: !String
  , resArguments :: ![Float]
  , resResult :: !(Maybe Float)
  , resError :: !(Maybe String)
  } 
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving ToSchema
  