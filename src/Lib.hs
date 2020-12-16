{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Lib
  ( webAppEntry
  , app
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import MyType
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Servant.Swagger
import Servant.Swagger.UI

createErrorText
  :: String
  -> String
createErrorText "div" = "Division by zero"
createErrorText "pow" = "Raising zero to a negative power"
createErrorText "sqrt" = "Extracting the root from the negative"
createErrorText e = "An unexpected error occurred: " ++ e ++ ". Please inform the developers"

createResult
  :: String
  -> [Float]
  -> Float
  -> Res
createResult oper args res
    | ((isInfinite res) || (isNaN res)) = Res oper args Nothing $ Just $ createErrorText oper
    | otherwise = Res oper args (Just res) Nothing

data Routes route = Routes
  { _add 
    :: route 
    :- "calc" 
    :> "add"
    :> Capture "a" Float
    :> Capture "b" Float 
    :> Get '[JSON] Res
  ,  _sub 
    :: route 
    :- "calc" 
    :> "sub"
    :> Capture "a" Float
    :> Capture "b" Float 
    :> Get '[JSON] Res
  ,  _mul 
    :: route 
    :- "calc" 
    :> "mul"
    :> Capture "a" Float
    :> Capture "b" Float 
    :> Get '[JSON] Res
  ,  _div 
    :: route 
    :- "calc" 
    :> "div"
    :> Capture "a" Float
    :> Capture "b" Float 
    :> Get '[JSON] Res
  ,  _sqrt 
    :: route 
    :- "calc" 
    :> "sqrt"
    :> Capture "a" Float
    :> Get '[JSON] Res
  ,  _pow 
    :: route 
    :- "calc" 
    :> "pow"
    :> Capture "a" Float
    :> Capture "b" Float 
    :> Get '[JSON] Res
  }
  deriving (Generic)

record :: Routes AsServer
record = Routes
  { _add = \a b -> return $ createResult "add" [a, b] $ a + b
  , _sub = \a b -> return $ createResult "sub" [a, b] $ a - b
  , _mul = \a b -> return $ createResult "mul" [a, b] $ a * b
  , _div = \a b -> return $ createResult "div" [a, b] $ a / b
  , _sqrt = \a -> return $ createResult "sqrt" [a] $ sqrt a
  , _pow = \a b -> return $ createResult "pow" [a, b] $ a ** b
  }

app :: Application
app = serve (Proxy :: Proxy (ToServant Routes AsApi :<|> SwaggerSchemaUI "swagger-ui" "swagger.json")) 
  (toServant record :<|> swaggerSchemaUIServer (toSwagger (Proxy :: Proxy (ToServant Routes AsApi))))
    
webAppEntry :: IO ()
webAppEntry = do
      run 8001 app

