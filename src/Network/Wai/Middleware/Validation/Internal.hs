{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Validation.Internal where

import           Control.Lens                  (at, (^.), (^?), _Just)
import           Data.Aeson                    (Value, decode)
import qualified Data.ByteString.Lazy          as L
import           Data.HashMap.Strict.InsOrd    (InsOrdHashMap, keys)
import qualified Data.Map.Strict               as M
import           Data.OpenApi                  (HttpStatusCode, OpenApi, Operation, PathItem,
                                                Referenced, Schema, components, content, default_,
                                                paths, requestBodies, requestBody, responses,
                                                schema, schemas, validateJSON, _pathItemDelete,
                                                _pathItemGet, _pathItemPatch, _pathItemPost,
                                                _pathItemPut)
import           Data.OpenApi.Schema.Generator (dereference)
import qualified Data.Text                     as T
import           Network.HTTP.Types            (StdMethod (DELETE, GET, PATCH, POST, PUT))
import           System.FilePath               (splitDirectories)

--
-- For reverse look up of path
-- https://swagger.io/specification/#path-templating-matching
--

data TemplatedPathComponent = Exact FilePath | ParameterValue deriving (Show)

instance Eq TemplatedPathComponent where
    Exact l == Exact r = l == r
    _ == _ = True

instance Ord TemplatedPathComponent where
    compare (Exact l) (Exact r) = compare l r
    compare _ _                 = EQ

-- | Convert FilePath to TemplatedPathComponent.
--
-- >>> toTemplatedPathComponent "foo"
-- Exact "foo"
-- >>> toTemplatedPathComponent "{foo}"
-- ParameterValue
toTemplatedPathComponent :: FilePath -> TemplatedPathComponent
toTemplatedPathComponent s
    | not (null s) && head s == '{' && last s == '}' = ParameterValue
    | otherwise = Exact s

type TemplatedPath = [TemplatedPathComponent]

-- | Convert FilePath to TemplatedPath.
--
-- >>> toTemplatedPath "/foo/{fooId}"
-- [Exact "/",Exact "foo",ParameterValue]
-- >>> toTemplatedPath "/bar/{barId}/baz/{bazId}"
-- [Exact "/",Exact "bar",ParameterValue,Exact "baz",ParameterValue]
toTemplatedPath :: FilePath -> TemplatedPath
toTemplatedPath p = map toTemplatedPathComponent $ splitDirectories p

type PathMap = M.Map TemplatedPath FilePath

-- | Convert list of FilePath to PathMap.
--
-- >>> makePathMap ["/foo", "/foo/{fooId}"]
-- fromList [([Exact "/",Exact "foo"],"/foo"),([Exact "/",Exact "foo",ParameterValue],"/foo/{fooId}")]
makePathMap :: [FilePath] -> PathMap
makePathMap ps = M.fromList $ zip (map toTemplatedPath ps) ps

-- | Look up a path (including a templated path) from PathMap.
--
-- >>> lookupDefinedPath "/foo" (makePathMap ["/foo", "/foo/{fooId}"])
-- Just "/foo"
-- >>> lookupDefinedPath "/foo/1" (makePathMap ["/foo", "/foo/{fooId}"])
-- Just "/foo/{fooId}"
-- >>> lookupDefinedPath "/bar" (makePathMap ["/foo", "/foo/{fooId}"])
-- Nothing
lookupDefinedPath :: FilePath -> PathMap -> Maybe FilePath
lookupDefinedPath realPath = M.lookup (toTemplatedPath realPath)


data ApiDefinition = ApiDefinition
    { getOpenApi :: OpenApi
    , getPathMap :: PathMap
    } deriving (Eq, Show)

-- | Create ApiDefinition instance from API document.
--
toApiDefinition :: L.ByteString -> Maybe ApiDefinition
toApiDefinition openApiJson = ApiDefinition <$> mOpenApi <*> mPathMap
  where
    mOpenApi = decode openApiJson :: Maybe OpenApi
    mKeys = keys <$> (mOpenApi ^? _Just . paths)
    mPathMap = case mKeys of
        -- OpenAPI Object must have `paths`
        -- https://swagger.io/specification/#openapi-object
        Just [] -> Nothing
        _       -> makePathMap <$> mKeys


newtype BodySchema = BodySchema { toReferencedSchema :: Referenced Schema } deriving (Eq, Show)

-- | Get request body schema.
--
getRequestBodySchema :: ApiDefinition -> StdMethod -> FilePath -> Maybe BodySchema
getRequestBodySchema a DELETE p = BodySchema <$> getRequestBodyReferencedSchema a _pathItemDelete p
getRequestBodySchema a GET    p = BodySchema <$> getRequestBodyReferencedSchema a _pathItemGet p
getRequestBodySchema a PATCH  p = BodySchema <$> getRequestBodyReferencedSchema a _pathItemPatch p
getRequestBodySchema a POST   p = BodySchema <$> getRequestBodyReferencedSchema a _pathItemPost p
getRequestBodySchema a PUT    p = BodySchema <$> getRequestBodyReferencedSchema a _pathItemPut p
getRequestBodySchema _ _      _ = Nothing

getRequestBodyReferencedSchema :: ApiDefinition -> (PathItem -> Maybe Operation) -> FilePath -> Maybe (Referenced Schema)
getRequestBodyReferencedSchema apiDef pathItemMethod realPath =
  let
    openApi = getOpenApi apiDef
    mDefinitionsRequestBody = openApi ^? components . requestBodies
    mOperation = getPathItem apiDef realPath >>= pathItemMethod
    mReferencedRequestBody = mOperation ^? _Just . requestBody . _Just
    mRequestBody = dereference <$> mDefinitionsRequestBody <*> mReferencedRequestBody
    mReferencedSchema = mRequestBody ^? _Just . content . at "application/json" . _Just . schema . _Just
  in
    mReferencedSchema

-- | Get response body schema.
--
getResponseBodySchema :: ApiDefinition -> StdMethod -> FilePath -> Int -> Maybe BodySchema
getResponseBodySchema a DELETE p s = BodySchema <$> getResponseBodyReferencedSchema a _pathItemDelete p s
getResponseBodySchema a GET    p s = BodySchema <$> getResponseBodyReferencedSchema a _pathItemGet p s
getResponseBodySchema a PATCH  p s = BodySchema <$> getResponseBodyReferencedSchema a _pathItemPatch p s
getResponseBodySchema a POST   p s = BodySchema <$> getResponseBodyReferencedSchema a _pathItemPost p s
getResponseBodySchema a PUT    p s = BodySchema <$> getResponseBodyReferencedSchema a _pathItemPut p s
getResponseBodySchema _ _      _ _ = Nothing

getResponseBodyReferencedSchema :: ApiDefinition -> (PathItem -> Maybe Operation) -> FilePath -> Int -> Maybe (Referenced Schema)
getResponseBodyReferencedSchema apiDef pathItemMethod realPath statusCode =
  let
    openApi = getOpenApi apiDef
    mDefinitionsResponse = openApi ^? components . responses
    mOperation = getPathItem apiDef realPath >>= pathItemMethod
    mResponses = mOperation ^? _Just . responses
    mReferencedResponse = case mResponses ^? _Just . at statusCode . _Just of
        Just rr -> Just rr
        Nothing -> mResponses ^? _Just . default_ . _Just
    mResponse = dereference <$> mDefinitionsResponse <*> mReferencedResponse
    mReferencedSchema = mResponse  ^? _Just . content . at "application/json" . _Just . schema . _Just
  in
    mReferencedSchema

getPathItem :: ApiDefinition -> FilePath -> Maybe PathItem
getPathItem apiDef realPath = mPath >>= \definedPath -> openApi ^? paths . at definedPath . _Just
  where
    openApi = getOpenApi apiDef
    mPath = lookupDefinedPath realPath $ getPathMap apiDef


-- | Validate JSON document.
--
validateJsonDocument :: ApiDefinition -> BodySchema -> L.ByteString -> Either String [String]
validateJsonDocument apiDef bodySchema dataJson = case decode dataJson :: Maybe Value of
    Nothing  -> Left "The document is not JSON."
    Just val -> case definitionsSchema' of
        Nothing -> Left "Schema objects are not defined in the OpenAPI document."
        Just ds -> case schema' of
            Nothing -> Left "The schema for the data is not defined in the OpenAPI document."
            Just s  -> Right $ map fixValidationError $ validateJSON ds s val
  where
    openApi = getOpenApi apiDef
    definitionsSchema' = openApi ^? components . schemas
    referencedSchema' = toReferencedSchema bodySchema
    schema' = (`dereference` referencedSchema') <$> definitionsSchema'

-- | Fix validation error message.
--
-- >>> fixValidationError "Hello, World!"
-- "Hello, World!"
-- >>> fixValidationError "expected JSON value of type OpenApiString"
-- "expected JSON value of type string"
-- >>> fixValidationError "expected JSON value of type OpenApiNumber"
-- "expected JSON value of type number"
-- >>> fixValidationError "expected JSON value of type OpenApiInteger"
-- "expected JSON value of type integer"
-- >>> fixValidationError "expected JSON value of type OpenApiBoolean"
-- "expected JSON value of type boolean"
fixValidationError :: String -> String
fixValidationError msg = T.unpack $ foldr replace' (T.pack msg) replacements
  where
    replace' (needle, replacement) = T.replace needle replacement
    replacements =
        -- replace internal type name with OpenAPI standard type name
        [ ("OpenApiString",  "string")
        , ("OpenApiNumber",  "number")
        , ("OpenApiInteger", "integer")
        , ("OpenApiBoolean", "boolean")
        ]
