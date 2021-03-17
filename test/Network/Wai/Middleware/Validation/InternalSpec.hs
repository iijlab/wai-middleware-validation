{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Network.Wai.Middleware.Validation.InternalSpec (spec) where

import           Data.ByteString.Lazy                       as L
import           Data.Either                                (isLeft)
import           Data.Maybe                                 (fromJust, isJust)
import           Data.OpenApi
import           Data.String.Here                           (here)
import           Network.HTTP.Types                         (StdMethod (GET, POST, PUT))
import           Test.Hspec

import           Network.Wai.Middleware.Validation.Internal


openApiJson :: L.ByteString
openApiJson = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "validator test", "version": "1.0.0" },
    "paths": {
        "/articles": {
            "get": {
                "responses": {
                    "200": {
                        "description": "OK",
                        "content": {
                            "application/json": {
                                "schema": {
                                    "type": "array",
                                    "items": {
                                        "$ref": "#/components/schemas/Article"
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "post": {
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "$ref": "#/components/schemas/Article"
                            }
                        }
                    }
                },
                "responses": { "default": { "description": "response example" } }
            }
        },
        "/articles/{articleId}": {
            "parameters": [
                {
                    "name": "articleId",
                    "in": "path",
                    "required": true,
                    "schema": { "type": "integer" }
                }
            ],
            "put": {
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "$ref": "#/components/schemas/Article"
                            }
                        }
                    }
                },
                "responses": { "default": { "description": "response example" } }
            }
        }
    },
    "components": {
        "schemas": {
            "Article": {
                "type": "object",
                "required": [ "cint", "ctxt" ],
                "properties": {
                    "cint": { "type": "integer" },
                    "ctxt": { "type": "string" }
                }
            }
        }
    }
}
|]

apiDef :: ApiDefinition
apiDef = fromJust $ toApiDefinition openApiJson

postRequestBodySchema :: BodySchema
postRequestBodySchema = fromJust $ getRequestBodySchema apiDef POST "/articles"

putRequestBodySchema :: BodySchema
putRequestBodySchema = fromJust $ getRequestBodySchema apiDef PUT "/articles/1"

-- postResponseBodySchema :: BodySchema
-- postResponseBodySchema = fromJust $ getPostResponseBodySchema apiDef "/articles" 200

-- putResponseBodySchema :: BodySchema
-- putResponseBodySchema = fromJust $ getPutResponseBodySchema apiDef "/articles/1" 200

spec :: Spec
spec = do
    describe "toApiDefinition" $ do
        it "returns Just ApiDefinition if the OpenAPI document is valid" $
            toApiDefinition openApiJson `shouldSatisfy` isJust

        it "returns Nothing if the OpenAPI document is invalid" $
            toApiDefinition "" `shouldBe` Nothing

        it "returns Nothing if the OpenAPI document has no paths object" $ do
            let json = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "components": {
    }
}
|]
            toApiDefinition json `shouldBe` Nothing

    describe "getRequestBodySchema" $ do
        context "when getting POST request body schema" $ do
            it "returns Just BodySchema if the specified path is defined" $
                getRequestBodySchema apiDef POST "/articles" `shouldSatisfy` isJust

            it "returns Nothing if the specified path is defined but POST method is not available" $
                getRequestBodySchema apiDef POST "/articles/1" `shouldBe` Nothing

            it "returns Nothing if the specified path is not defined" $
                getRequestBodySchema apiDef POST "/null" `shouldBe` Nothing

        context "when getting PUT request body schema" $ do
            it "returns Just BodySchema if the specified path is defined" $
                getRequestBodySchema apiDef PUT "/articles/1" `shouldSatisfy` isJust

            it "returns Nothing if the specified path is defined but PUT method is not available" $
                getRequestBodySchema apiDef PUT "/articles" `shouldBe` Nothing

            it "returns Nothing if the specified path is not defined" $
                getRequestBodySchema apiDef PUT "/null" `shouldBe` Nothing

        context "when getting GET request body schema" $
            it "returns Nothing if the specified path is defined but request body schema is not defined" $
                getRequestBodySchema apiDef GET "/articles" `shouldBe` Nothing

    describe "getRequestBodyReferencedSchema" $ do
        context "when getting POST request body schema" $ do
            it "returns Just (Referenced Schema) if the operation for the specified path is defined" $
                getRequestBodyReferencedSchema apiDef _pathItemPost "/articles" `shouldSatisfy` isJust

            it "returns Nothing if the operation for the specified path is not defined" $
                getRequestBodyReferencedSchema apiDef _pathItemPost "/articles/1" `shouldBe` Nothing

            it "returns Nothing if the specified path is defined" $
                getRequestBodyReferencedSchema apiDef _pathItemPost "/null" `shouldBe` Nothing

        context "when getting PUT request body schema" $ do
            it "returns Just (Referenced Schema) if the operation for the specified path is not defined" $
                getRequestBodyReferencedSchema apiDef _pathItemPut "/articles/1" `shouldSatisfy` isJust

            it "returns Nothing if the operation for the specified path is defined" $
                getRequestBodyReferencedSchema apiDef _pathItemPut "/articles" `shouldBe` Nothing

            it "returns Nothing if the specified path is defined" $
                getRequestBodyReferencedSchema apiDef _pathItemPut "/null" `shouldBe` Nothing

        context "when getting GET request body schema" $
            it "returns Nothing if the specified path is defined but request body schema is not defined" $
                getRequestBodyReferencedSchema apiDef _pathItemGet "/articles" `shouldBe` Nothing

    describe "getResponseBodySchema" $ do
        context "when getting GET response body schema" $ do
            it "returns Just BodySchema if the specified path and the status is defined" $
                getResponseBodySchema apiDef GET "/articles" 200 `shouldSatisfy` isJust

            it "returns Nothing if the specified path is defined but the status is not defined" $
                getResponseBodySchema apiDef GET "/articles" 300 `shouldBe` Nothing

            it "returns Nothing if the specified path is not defined" $
                getResponseBodySchema apiDef GET "/null" 200 `shouldBe` Nothing

        context "when getting POST request body schema" $
            it "returns Nothing if the specified path is defined but the response schema is not defined" $
                getResponseBodySchema apiDef POST "/articles" 200 `shouldBe` Nothing

    describe "getResponseBodyReferencedSchema" $ do
        context "when getting GET response body schema" $ do
            it "returns Just (Referenced Schema) if the operation for the specified path is defined" $
                getResponseBodyReferencedSchema apiDef _pathItemGet "/articles" 200 `shouldSatisfy` isJust

            it "returns Nothing if the operation for the specified path is not defined" $
                getResponseBodyReferencedSchema apiDef _pathItemGet "/articles" 300 `shouldBe` Nothing

            it "returns Nothing if the specified path is defined" $
                getResponseBodyReferencedSchema apiDef _pathItemGet "/null" 200 `shouldBe` Nothing

        context "when getting POST response body schema" $
            it "returns Nothing if the specified path is defined but the response schema is not defined" $
                getResponseBodyReferencedSchema apiDef _pathItemPost "/articles" 200 `shouldBe` Nothing

    describe "getPathItem" $ do
        it "returns Just PathItem if the specified path is defined" $
            getPathItem apiDef "/articles" `shouldSatisfy` isJust

        it "returns Just PathItem if the specified templated path is defined" $
            getPathItem apiDef "/articles/1" `shouldSatisfy` isJust

        it "returns Nothing if the specified path is not defined" $
            getPathItem apiDef "/null" `shouldBe` Nothing

    describe "validateJsonDocument" $ do
        context "when validating POST request body" $ do
            it "returns Right [] if the request body satisfy all constraints" $ do
                let requestBodyJson = [here|
{
    "cint": 1,
    "ctxt": "foo"
}
|]
                validateJsonDocument apiDef postRequestBodySchema requestBodyJson `shouldBe` Right []

            it "returns validation errors if the request body does not satisfy any constraints" $ do
                let requestBodyJson = [here|
{
    "cint": "foo",
    "ctxt": "foo"
}
|]
                validateJsonDocument apiDef postRequestBodySchema requestBodyJson `shouldSatisfy` hasValidationError

        context "when validating PUT request body" $ do
            it "returns Right [] if the request body satisfy all constraints" $ do
                let requestBodyJson = [here|
{
    "cint": 1,
    "ctxt": "foo"
}
|]
                validateJsonDocument apiDef putRequestBodySchema requestBodyJson `shouldBe` Right []

            it "returns validation errors if the request body does not satisfy any constraints" $ do
                let requestBodyJson = [here|
{
    "cint": "foo",
    "ctxt": "foo"
}
|]
                validateJsonDocument apiDef putRequestBodySchema requestBodyJson `shouldSatisfy` hasValidationError

            it "returns Left if the request body is not valid JSON" $ do
                let requestBodyJson = [here|
cint: 1
ctxt: "foo"
|]
                validateJsonDocument apiDef putRequestBodySchema requestBodyJson `shouldSatisfy` isLeft

hasValidationError :: Either String [String] -> Bool
hasValidationError (Right []) = False
hasValidationError (Right _)  = True
hasValidationError (Left _)   = False
