{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Network.Wai.Middleware.Validation where

import           Data.Aeson                                 (ToJSON, encode, object, toJSON, (.=))
import           Data.ByteString.Builder                    (toLazyByteString)
import qualified Data.ByteString.Char8                      as S8
import qualified Data.ByteString.Lazy                       as L
import           Data.IORef                                 (atomicModifyIORef, newIORef, readIORef)
import           Network.HTTP.Types                         (ResponseHeaders, StdMethod,
                                                             badRequest400, hContentType,
                                                             internalServerError500, parseMethod,
                                                             statusCode, statusIsSuccessful)
import           Network.Wai                                (Middleware, Request, Response,
                                                             rawPathInfo, requestBody,
                                                             requestMethod, responseLBS,
                                                             responseStatus, responseToStream,
                                                             strictRequestBody)

import           Network.Wai.Middleware.Validation.Internal (ApiDefinition, getRequestBodySchema,
                                                             getResponseBodySchema, toApiDefinition,
                                                             validateJsonDocument)


data DefaultErrorJson = DefaultErrorJson
    { title  :: String
    , detail :: String
    } deriving (Show)

instance ToJSON DefaultErrorJson where
    toJSON (DefaultErrorJson t d) = object ["title" .= t, "detail" .= d]

-- | Make error string with JSON.
mkDefaultErrorJson :: String -> DefaultErrorJson
mkDefaultErrorJson = DefaultErrorJson "Validation failed"


-- | Make a middleware for Request/Response validation.
mkValidator' :: L.ByteString -> Maybe Middleware
mkValidator' = mkValidator mkDefaultErrorJson

mkValidator :: ToJSON a => (String -> a) -> L.ByteString -> Maybe Middleware
mkValidator mkErrorJson apiJson = (.) <$> mResValidator <*> mReqValidator
  where
    mApiDef = toApiDefinition apiJson
    mReqValidator = requestValidator mkErrorJson <$> mApiDef
    mResValidator = responseValidator mkErrorJson <$> mApiDef

-- | Make a middleware for Requestion validation.
mkRequestValidator' :: L.ByteString -> Maybe Middleware
mkRequestValidator' = mkRequestValidator mkDefaultErrorJson

mkRequestValidator :: ToJSON a => (String -> a) -> L.ByteString -> Maybe Middleware
mkRequestValidator mkErrorJson apiJson = requestValidator mkErrorJson <$> toApiDefinition apiJson

-- | Make a middleware for Response validation.
mkResponseValidator' :: L.ByteString -> Maybe Middleware
mkResponseValidator' = mkResponseValidator mkDefaultErrorJson

mkResponseValidator :: ToJSON a => (String -> a) -> L.ByteString -> Maybe Middleware
mkResponseValidator mkErrorJson apiJson = responseValidator mkErrorJson <$> toApiDefinition apiJson

requestValidator :: ToJSON a => (String -> a) -> ApiDefinition -> Middleware
requestValidator mkErrorJson apiDef app req sendResponse = do
    let
        eMethod = parseMethod $ requestMethod req
        path = S8.unpack $ rawPathInfo req
        mBodySchema = case eMethod of
            Right method -> getRequestBodySchema apiDef method path
            _            -> Nothing

    putStrLn ">>> [Request]"
    putStrLn $ ">>> Method: " ++ show eMethod
    putStrLn $ ">>> Path: " ++ path

    case mBodySchema of
        Nothing         -> app req sendResponse
        Just bodySchema -> do
            (body, newReq) <- getRequestBody req
            putStrLn $ ">>> Body: " ++ show body

            case validateJsonDocument apiDef bodySchema body of
                Right []   -> app newReq sendResponse
                Right errs -> respondError $ unlines errs
                Left err   -> respondError err
  where
    respondError msg = sendResponse $
        responseLBS badRequest400 [(hContentType, "application/json")] $ encode $ mkErrorJson msg

responseValidator :: ToJSON a => (String -> a) -> ApiDefinition -> Middleware
responseValidator mkErrorJson apiDef app req sendResponse = app req $ \res -> do
    let status = responseStatus res
    -- Validate only the success response.
    if statusIsSuccessful status
        then do
            let
                eMethod = parseMethod $ requestMethod req
                path = S8.unpack $ rawPathInfo req
                statusCode' = statusCode status
                mBodySchema = case eMethod of
                    Right method -> getResponseBodySchema apiDef method path statusCode'
                    _            -> Nothing

            putStrLn ">>> [Response]"
            putStrLn $ ">>> Method: " ++ show eMethod
            putStrLn $ ">>> Path: " ++ path
            putStrLn $ ">>> Status: " ++ show statusCode'

            case mBodySchema of
                Nothing         -> sendResponse res
                Just bodySchema -> do
                    body <- getResponseBody res
                    putStrLn $ ">>> Body': " ++ show body

                    case validateJsonDocument apiDef bodySchema body of
                        Right []   -> sendResponse res
                        -- REVIEW: It may be better not to include the error details in the response.
                        -- _ -> respondError "Invalid response body"
                        Right errs -> respondError $ unlines errs
                        Left err   -> respondError err

        else sendResponse res
  where
    respondError msg = sendResponse $
        responseLBS internalServerError500 [(hContentType, "application/json")] $ encode $ mkErrorJson msg

getRequestBody :: Request -> IO (L.ByteString, Request)
getRequestBody req = do
    body <- strictRequestBody req
    -- The body has been consumed and needs to be refilled.
    ref <- newIORef body
    let newRequestBody = atomicModifyIORef ref (L.empty,)
    let newReq = req { requestBody = L.toStrict <$> newRequestBody }
    return (body, newReq)

getResponseBody :: Response -> IO L.ByteString
getResponseBody res = do
    let (_, _, withBody) = responseToStream res
    withBody $ \streamingBody -> do
        ref <- newIORef mempty
        streamingBody
            (\b -> atomicModifyIORef ref $ \acc -> (acc <> b, ()))
            (pure ())
        toLazyByteString <$> readIORef ref

responseHeaders :: ResponseHeaders
responseHeaders = [(hContentType, "application/json")]

--
-- for non-middleware use
--

validateRequestBody :: StdMethod -> FilePath -> L.ByteString -> L.ByteString -> Either String [String]
validateRequestBody method path apiJson body = case toApiDefinition apiJson of
    Nothing     -> Left "Invalid OpenAPI document"
    Just apiDef -> case getRequestBodySchema apiDef method path of
        Nothing         -> Left "Schema not found"
        Just bodySchema -> validateJsonDocument apiDef bodySchema body

validateResponseBody :: StdMethod -> FilePath -> Int -> L.ByteString -> L.ByteString -> Either String [String]
validateResponseBody method path statusCode' apiJson body = case toApiDefinition apiJson of
    Nothing     -> Left "Invalid OpenAPI document"
    Just apiDef -> case getResponseBodySchema apiDef method path statusCode' of
        Nothing         -> Left "Schema not found"
        Just bodySchema -> validateJsonDocument apiDef bodySchema body
