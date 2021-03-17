{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Network.Wai.Middleware.ValidationSpec (spec) where

import           Control.Monad                     (forM_)
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.ByteString.Lazy              as L
import           Data.Maybe                        (fromMaybe, isJust)
import           Data.String.Here                  (here)
import           Network.HTTP.Types                (StdMethod (GET, POST), methodPost, status201,
                                                    status400, status500)
import           Network.Wai                       (requestMethod, responseLBS)
import           Network.Wai.Test
import           Test.Hspec

import           Network.Wai.Middleware.Validation


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
                "responses": {
                    "201": {
                        "description": "OK",
                        "content": {
                            "application/json": {
                                "schema": {
                                    "$ref": "#/components/schemas/Article"
                                }
                            }
                        }
                    }
                }
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


spec :: Spec
spec = do
    describe "Middleware" $ do
        let
            mkApp responseBody _ respond = respond $ responseLBS status201 responseHeaders responseBody
            validResponseBody = [here| {"cint": 1, "ctxt": "RESPONSE"} |]
            invalidResponseBody = [here| {"cint": 1} |]
            mkSession requestBody = srequest $ SRequest (setPath (defaultRequest {requestMethod = methodPost}) "/articles") requestBody
            validRequestBody = [here| {"cint": 1, "ctxt": "REQUEST"} |]
            invalidRequestBody = [here| {"cint": 1, "ctxt": 0} |]

        context "request and response validation" $ do
            let validator = fromMaybe (error "Invalid OpenAPI document") (mkValidator' openApiJson)

            it "do nothing if the request and response body is valid" $ do
                sResponse <- liftIO $ runSession (mkSession validRequestBody) $ validator (mkApp validResponseBody)
                (simpleStatus sResponse, simpleBody sResponse) `shouldBe` (status201, validResponseBody)

            it "returns 400 if the request body is invalid" $ do
                sResponse <- liftIO $ runSession (mkSession invalidRequestBody) $ validator (mkApp validResponseBody)
                (simpleStatus sResponse, simpleBody sResponse) `shouldBe`
                    (status400, [here| {"title":"Validation failed","detail":"expected JSON value of type string\n"} |])

            it "returns 500 if the response body is invalid" $ do
                sResponse <- liftIO $ runSession (mkSession validRequestBody) $ validator (mkApp invalidResponseBody)
                (simpleStatus sResponse, simpleBody sResponse) `shouldBe`
                    (status500, [here| {"title":"Validation failed","detail":"property \"ctxt\" is required, but not found in \"{\\\"cint\\\":1}\"\n"} |])

        context "request validation" $ do
            let reqValidator = fromMaybe (error "Invalid OpenAPI document") (mkRequestValidator' openApiJson)

            it "do nothing if the request and response body is valid" $ do
                sResponse <- liftIO $ runSession (mkSession validRequestBody) $ reqValidator (mkApp validResponseBody)
                (simpleStatus sResponse, simpleBody sResponse) `shouldBe` (status201, validResponseBody)

            it "returns 400 if the request body is invalid" $ do
                sResponse <- liftIO $ runSession (mkSession invalidRequestBody) $ reqValidator (mkApp validResponseBody)
                (simpleStatus sResponse, simpleBody sResponse) `shouldBe`
                    (status400, [here| {"title":"Validation failed","detail":"expected JSON value of type string\n"} |])

            it "do nothing even if the response body is invalid" $ do
                sResponse <- liftIO $ runSession (mkSession validRequestBody) $ reqValidator (mkApp invalidResponseBody)
                (simpleStatus sResponse, simpleBody sResponse) `shouldBe` (status201, invalidResponseBody)

        context "response validation" $ do
            let resValidator = fromMaybe (error "Invalid OpenAPI document") (mkResponseValidator' openApiJson)

            it "do nothing if the request and response body is valid" $ do
                sResponse <- liftIO $ runSession (mkSession validRequestBody) $ resValidator (mkApp validResponseBody)
                (simpleStatus sResponse, simpleBody sResponse) `shouldBe` (status201, validResponseBody)

            it "do nothing even if the request body is invalid" $ do
                sResponse <- liftIO $ runSession (mkSession invalidRequestBody) $ resValidator (mkApp validResponseBody)
                (simpleStatus sResponse, simpleBody sResponse) `shouldBe` (status201, validResponseBody)

            it "returns 500 if the response body is invalid" $ do
                sResponse <- liftIO $ runSession (mkSession validRequestBody) $ resValidator (mkApp invalidResponseBody)
                (simpleStatus sResponse, simpleBody sResponse) `shouldBe`
                    (status500, [here| {"title":"Validation failed","detail":"property \"ctxt\" is required, but not found in \"{\\\"cint\\\":1}\"\n"} |])

    describe "mkValidator'" $ do
        it "returns Just Middleware if the OpenAPI document is valid" $
            isJust (mkValidator' openApiJson) `shouldBe` True

        it "returns Nothing if the OpenAPI document is invalid" $
            isJust (mkValidator' "") `shouldBe` False

        it "returns Nothing if the OpenAPI document has no paths object" $ do
            let json = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "components": {
    }
}
|]
            isJust (mkValidator' json) `shouldBe` False

    describe "mkRequestValidator'" $ do
        it "returns Just Middleware if the OpenAPI document is valid" $
            isJust (mkRequestValidator' openApiJson) `shouldBe` True

        it "returns Nothing if the OpenAPI document is invalid" $
            isJust (mkRequestValidator' "") `shouldBe` False

        it "returns Nothing if the OpenAPI document has no paths object" $ do
            let json = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "components": {
    }
}
|]
            isJust (mkRequestValidator' json) `shouldBe` False

    describe "mkResponseValidator'" $ do
        it "returns Just Middleware if the OpenAPI document is valid" $
            isJust (mkResponseValidator' openApiJson) `shouldBe` True

        it "returns Nothing if the OpenAPI document is invalid" $
            isJust (mkResponseValidator' "") `shouldBe` False

        it "returns Nothing if the OpenAPI document has no paths object" $ do
            let json = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "components": {
    }
}
|]
            isJust (mkResponseValidator' json) `shouldBe` False

    describe "validateRequestBody" $ do
        let makeOpenApiJson schemaJson = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "paths": {
        "/examples": {
            "post": {
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "$ref": "#/components/schemas/Example"
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
            "Example": {
                "type": "object",
                "properties": |] <> schemaJson <> [here|
            }
        }
    }
}
|]
        context "type: integer" $ do
            let
                tests =
                    [ ( "returns Right [] if the value is integer"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| {"mint": 1} |]
                      , Right []
                      )
                    , ( "returns Right [] if the value is integer"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| {"mint": 1} |]
                      , Right []
                      )
                    , ( "returns Right [] if the value is integer"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| {"mint": 1} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the value is a number contains a decimal part"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| {"mint": 1.1} |]
                      , Right ["not an integer"]
                      )
                    , ( "returns Right [error] if the value is a string type"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| {"mint": "text"} |]
                      , Right ["expected JSON value of type integer"]
                      )
                    , ( "returns Right [error] if the value is a boolean type"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| {"mint": true} |]
                      , Right ["expected JSON value of type integer"]
                      )
                    , ( "returns Right [error] if the value is an array type"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| {"mint": [0, 1, 2]} |]
                      , Right ["expected JSON value of type integer"]
                      )
                    , ( "returns Right [error] if the value is an object type"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| {"mint": {"k": "v"}} |]
                      , Right ["expected JSON value of type integer"]
                      )
                    , ( "returns Right [] if the value is the same as the minimum"
                      , [here| {"mint": {"type": "integer", "minimum": 10}} |]
                      , [here| {"mint": 10} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the value is less than the minimum"
                      , [here| {"mint": {"type": "integer", "minimum": 10}} |]
                      , [here| {"mint": 9} |]
                      , Right ["value 9.0 falls below minimum (should be >=10.0)"]
                      )
                    , ( "returns Right [] if the value is the same as the minimum and exclusiveMinimum is true"
                      , [here| {"mint": {"type": "integer", "minimum": 10, "exclusiveMinimum": true}} |]
                      , [here| {"mint": 10} |]
                      , Right ["value 10.0 falls below minimum (should be >10.0)"]
                      )
                    , ( "returns Right [] if the value is the same as the minimum and exclusiveMinimum is false"
                      , [here| {"mint": {"type": "integer", "minimum": 10, "exclusiveMinimum": false}} |]
                      , [here| {"mint": 10} |]
                      , Right []
                      )
                    , ( "returns Right [] if the value is the same as the maximum"
                      , [here| {"mint": {"type": "integer", "maximum": 10}} |]
                      , [here| {"mint": 10} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the value is greater than the maximum"
                      , [here| {"mint": {"type": "integer", "maximum": 10}} |]
                      , [here| {"mint": 11} |]
                      , Right ["value 11.0 exceeds maximum (should be <=10.0)"]
                      )
                    , ( "returns Right [] if the value is the same as the maximum and exclusiveMaximum is true"
                      , [here| {"mint": {"type": "integer", "maximum": 10, "exclusiveMaximum": true}} |]
                      , [here| {"mint": 11} |]
                      , Right ["value 11.0 exceeds maximum (should be <10.0)"]
                      )
                    , ( "returns Right [] if the value is the same as the maximum and exclusiveMaximum is false"
                      , [here| {"mint": {"type": "integer", "maximum": 10, "exclusiveMaximum": false}} |]
                      , [here| {"mint": 10} |]
                      , Right []
                      )
                    , ( "returns Right [] if the value is the multiple of the value of multipleOf"
                      , [here| {"mint": {"type": "integer", "multipleOf": 5}} |]
                      , [here| {"mint": 10} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the value is not the multiple of the value of multipleOf"
                      , [here| {"mint": {"type": "integer", "multipleOf": 4}} |]
                      , [here| {"mint": 10} |]
                      , Right ["expected a multiple of 4.0 but got 10.0"]
                      )
                    , ( "returns Right [] if the value is the maximum value of int32"
                      , [here| {"mint": {"type": "integer", "format": "int32"}} |]
                      , [here| {"mint": 2147483647} |]
                      , Right []
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateRequestBody POST "/examples" apiJson bodyJson `shouldBe` result

        context "type: number" $ do
            let
                tests =
                    [ ( "returns Right [] if the value is a number contains a decimal part"
                      , [here| {"mnum": {"type": "number"}} |]
                      , [here| {"mnum": 1.1} |]
                      , Right []
                      )
                    , ( "returns Right [] if the value is an integer"
                      , [here| {"mnum": {"type": "number"}} |]
                      , [here| {"mnum": 1} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the value is a string type"
                      , [here| {"mnum": {"type": "number"}} |]
                      , [here| {"mnum": "text"} |]
                      , Right ["expected JSON value of type number"]
                      )
                    , ( "returns Right [error] if the value is a boolean type"
                      , [here| {"mnum": {"type": "number"}} |]
                      , [here| {"mnum": true} |]
                      , Right ["expected JSON value of type number"]
                      )
                    , ( "returns Right [error] if the value is an array type"
                      , [here| {"mnum": {"type": "number"}} |]
                      , [here| {"mnum": [0, 1, 2 ]} |]
                      , Right ["expected JSON value of type number"]
                      )
                    , ( "returns Right [error] if the value is an object type"
                      , [here| {"mnum": {"type": "number"}} |]
                      , [here| {"mnum": {"k": "v"}} |]
                      , Right ["expected JSON value of type number"]
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateRequestBody POST "/examples" apiJson bodyJson `shouldBe` result

        context "type: string" $ do
            let
                tests =
                    [ ( "returns Right [] if the value is a string type"
                      , [here| {"mstr": {"type": "string"}} |]
                      , [here| {"mstr": "text"} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the value is an integer type"
                      , [here| {"mstr": {"type": "string"}} |]
                      , [here| {"mstr": 1} |]
                      , Right ["expected JSON value of type string"]
                      )
                    , ( "returns Right [error] if the value is a number type"
                      , [here| {"mstr": {"type": "string"}} |]
                      , [here| {"mstr": 1.1} |]
                      , Right ["expected JSON value of type string"]
                      )
                    , ( "returns Right [error] if the value is a boolean type"
                      , [here| {"mstr": {"type": "string"}} |]
                      , [here| {"mstr": true} |]
                      , Right ["expected JSON value of type string"]
                      )
                    , ( "returns Right [error] if the value is an array type"
                      , [here| {"mstr": {"type": "string"}} |]
                      , [here| {"mstr": [0, 1, 2]} |]
                      , Right ["expected JSON value of type string"]
                      )
                    , ( "returns Right [error] if the value is an object type"
                      , [here| {"mstr": {"type": "string"}} |]
                      , [here| {"mstr": {"k": "v"}} |]
                      , Right ["expected JSON value of type string"]
                      )
                    , ( "returns Right [] if the length of the value is the same as minLength"
                      , [here| {"mstr": {"type": "string", "minLength": 3}} |]
                      , [here| { "mstr": "abc" } |]
                      , Right []
                      )
                    , ( "returns Right [error] if the length of the value is less than minLength"
                      , [here| {"mstr": {"type": "string", "minLength": 4}} |]
                      , [here| { "mstr": "abc" } |]
                      , Right ["string is too short (length should be >=4)"]
                      )
                    , ( "returns Right [] if the length of the value is the same as maxLength"
                      , [here| {"mstr": {"type": "string", "maxLength": 3}} |]
                      , [here| { "mstr": "abc" } |]
                      , Right []
                      )
                    , ( "returns Right [error] if the length of the value is greater than maxLength"
                      , [here| {"mstr": {"type": "string", "maxLength": 2}} |]
                      , [here| { "mstr": "abc" } |]
                      , Right ["string is too long (length should be <=2)"]
                      )
                    , ( "returns Right [] if the value follows format:date"
                      , [here| {"mstr": {"type": "string", "format": "date"}} |]
                      , [here| { "mstr": "2017-07-21" } |]
                      , Right []
                      )
                    , ( "returns Right [] if the value follows format:date-time"
                      , [here| {"mstr": {"type": "string", "format": "date-time"}} |]
                      , [here| { "mstr": "2017-07-21T17:32:28Z" } |]
                      , Right []
                      )
                    , ( "returns Right [] if the value follows format:byte"
                      , [here| {"mstr": {"type": "string", "format": "byte"}} |]
                      , [here| {"mstr": "U3dhZ2dlciByb2Nrcw=="} |]
                      , Right []
                      )
                    , ( "returns Right [] if the value matches the pattern"
                      , [here| {"mstr": {"type": "string", "pattern": "^abc$"}} |]
                      , [here| {"mstr": "abc"} |]
                      , Right []
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateRequestBody POST "/examples" apiJson bodyJson `shouldBe` result

        context "type: string (pending)" $ do
            let
                tests =
                    [ ( "returns Right [] if the value does not follows format:date"
                      , [here| {"mstr": {"type": "string", "format": "date"}} |]
                      , [here| { "mstr": "not date!" } |]
                    --   , Right ["expected JSON value of type string and format of date"]
                      , "Keyword `format` is not supported"
                      )
                    , ( "returns Right [] if the value does not follows format:date-time"
                      ,  [here| {"mstr": {"type": "string", "format": "date-time"}} |]
                      ,  [here| {"mstr": "not datetime!"} |]
                    --   , Right ["expected JSON value of type string and format of date-time"]
                      , "Keyword `format` is not supported"
                      )
                    , ( "returns Right [] if the value does not follows format:byte"
                      , [here| {"mstr": {"type": "string", "format": "byte"}} |]
                      , [here| {"mstr": "not byte!"} |]
                    --   , Right ["expected JSON value of type string and format of byte"]
                      , "Keyword `format` is not supported"
                      )
                    , ( "returns Right [error] if the value does not matches the pattern"
                      , [here| {"mstr": {"type": "string", "pattern": "^abc$"}} |]
                      , [here| {"mstr": "def"} |]
                    --   , Right ["expected JSON value of type string and of pattern"]
                      , "Keyword `pattern` is not supported"
                      )
                    ] :: [(String, L.ByteString, L.ByteString, String)]
            -- forM_ tests $ \(description, schemaJson, bodyJson, result) ->
            --     it description $ do
            --         let apiJson = makeOpenApiJson schemaJson
            --         validateRequestBody POST "/examples" apiJson bodyJson `shouldNotBe` result
            forM_ tests $ \(description, _, _, reason) ->
                it description $
                    pendingWith reason

        context "type: boolean" $ do
            let
                tests =
                    [ ( "returns Right [] if the value is an boolean value"
                      , [here| {"mbol": {"type": "boolean"}} |]
                      , [here| {"mbol": true} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the value is an integer type"
                      , [here| {"mbol": {"type": "boolean"}} |]
                      , [here| {"mbol": 1} |]
                      , Right ["expected JSON value of type boolean"]
                      )
                    , ( "returns Right [error] if the value is a number type"
                      , [here| {"mbol": {"type": "boolean"}} |]
                      , [here| {"mbol": 1.1} |]
                      , Right ["expected JSON value of type boolean"]
                      )
                    , ( "returns Right [error] if the value is a string type"
                      , [here| {"mbol": {"type": "boolean"}} |]
                      , [here| {"mbol": "text"} |]
                      , Right ["expected JSON value of type boolean"]
                      )
                    , ( "returns Right [error] if the value is an array type"
                      , [here| {"mbol": {"type": "boolean"}} |]
                      , [here| {"mbol": [0, 1, 2]} |]
                      , Right ["expected JSON value of type boolean"]
                      )
                    , ( "returns Right [error] if the value is an object type"
                      , [here| {"mbol": {"type": "boolean"}} |]
                      , [here| {"mbol": {"k": "v"}} |]
                      , Right ["expected JSON value of type boolean"]
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateRequestBody POST "/examples" apiJson bodyJson `shouldBe` result

        context "type: array" $ do
            let
                tests =
                    [ ( "returns Right [] if all values in the list are the correct type"
                      , [here| {"mlst": {"type": "array", "items": {"type": "integer"}}} |]
                      , [here| {"mlst": [1, 2]} |]
                      , Right []
                      )
                    , ( "returns Right [error] if there are different types of values in the list"
                      , [here| {"mlst": {"type": "array", "items": {"type": "integer"}}} |]
                      , [here| {"mlst": [1, "2"]} |]
                      , Right ["expected JSON value of type integer"]
                      )
                    , ( "returns Right [] if all values in the tuple are the correct type"
                      , [here| {"mtpl": {"type": "array", "items": [{"type": "integer"}, {"type": "string"}]}} |]
                      , [here| {"mtpl": [1, "text"]} |]
                      , Right []
                      )
                    , ( "returns Right [error] if there are different types of values in the tuple"
                      , [here| {"mtpl": {"type": "array", "items": [{"type": "integer"}, {"type": "string"}]}} |]
                      , [here| {"mtpl": [1, 2]} |]
                      , Right ["expected JSON value of type string"]
                      )
                    , ( "returns Right [error] if the number of elements in the array is less than minItems"
                      , [here| {"mlst": {"type": "array", "items": {"type": "integer"}, "minItems": 2}} |]
                      , [here| {"mlst": [1]} |]
                      , Right ["array is too short (size should be >=2)"]
                      )
                    , ( "returns Right [error] if the number of elements in the array is greater than maxItems"
                      , [here| {"mlst": {"type": "array", "items": {"type": "integer"}, "maxItems": 2}} |]
                      , [here| {"mlst": [1, 2, 3]} |]
                      , Right ["array exceeds maximum size (should be <=2)"]
                      )
                    , ( "returns Right [] if uniqueItems is not specified and the array has some duplicated values"
                      , [here| {"mlst": {"type": "array", "items": {"type": "integer"}}} |]
                      , [here| {"mlst": [1, 2, 1]} |]
                      , Right []
                      )
                    , ( "returns Right [error] if uniqueItems is true and the array has some duplicated values"
                      , [here| {"mlst": {"type": "array", "items": {"type": "integer"}, "uniqueItems": true}} |]
                      , [here| {"mlst": [1, 2, 1]} |]
                      , Right ["array is expected to contain unique items, but it does not"]
                      )
                    , ( "returns Right [error] if uniqueItems is false and the array has some duplicated values"
                      , [here| {"mlst": {"type": "array", "items": {"type": "integer"}, "uniqueItems": false}} |]
                      , [here| {"mlst": [1, 2, 1]} |]
                      , Right []
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateRequestBody POST "/examples" apiJson bodyJson `shouldBe` result

        context "type: object" $ do
            let
                tests =
                    [ ( "returns Right [error] if the value contains undefined property"
                      , [here|
{
    "mobj": {
        "type": "object",
        "properties": {
            "mint": {"type": "integer"}
        }
    }
}
|]
                      , [here| {"mobj": {"mstr": "hello"}} |]
                      , Right ["property \"mstr\" is found in JSON value, but it is not mentioned in Swagger schema"]
                      )
                    , ( "returns Right [] if the value contains required property"
                      , [here|
{
    "mobj": {
        "type": "object",
        "required": ["cint", "cstr"],
        "properties": {
            "cint": {"type": "integer"},
            "cstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                      , [here| {"mobj": {"cint": 1, "cstr": "hello"}} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the value does not contains required property"
                      , [here|
{
    "mobj": {
        "type": "object",
        "required": ["cint", "cstr"],
        "properties": {
            "cint": {"type": "integer"},
            "cstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                      , [here| {"mobj": {"cint": 1, "mbol": true}} |]
                      , Right ["property \"cstr\" is required, but not found in \"{\\\"mbol\\\":true,\\\"cint\\\":1}\""]
                      )
                    , ( "returns Right [] if the number of properties of the value is greater than or equal to than minProperties"
                      , [here|
{
    "mobj": {
        "type": "object",
        "minProperties": 2,
        "properties": {
            "mint": {"type": "integer"},
            "mstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                      , [here| {"mobj": {"mint": 1, "mstr": "text"}} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the number of properties of the value is less than minProperties"
                      , [here|
{
    "mobj": {
        "type": "object",
        "minProperties": 2,
        "properties": {
            "mint": {"type": "integer"},
            "mstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                      , [here| {"mobj": {"mint": 1}} |]
                      , Right ["object size is too small (total number of properties should be >=2)"]
                      )
                    , ( "returns Right [] if the number of properties of the value is less than or equal to maxProperties"
                      , [here|
{
    "mobj": {
        "type": "object",
        "maxProperties": 2,
        "properties": {
            "mint": {"type": "integer"},
            "mstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                      , [here| {"mobj": {"mint": 1, "mstr": "text"}} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the number of properties of the value is greater than maxProperties"
                      , [here|
{
    "mobj": {
        "type": "object",
        "maxProperties": 2,
        "properties": {
            "mint": {"type": "integer"},
            "mstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                      , [here| {"mobj": {"mint": 1, "mstr": "text", "mbol": true}} |]
                      , Right ["object size exceeds maximum (total number of properties should be <=2)"]
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateRequestBody POST "/examples" apiJson bodyJson `shouldBe` result

        context "enum" $ do
            let
                tests =
                    [ ( "returns Right [] if the value is one of the enumerated values"
                      , [here| {"mint": {"type": "integer", "enum": [10, 20, 30]}} |]
                      , [here| {"mint": 10} |]
                      , Right []
                      )
                    , ( "returns Right [error] if the value is not one of the enumerated values"
                      , [here| {"mint": {"type": "integer", "enum": [20, 30]}} |]
                      , [here| {"mint": 10} |]
                      , Right ["expected one of \"[20,30]\" but got Number 10.0"]
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateRequestBody POST "/examples" apiJson bodyJson `shouldBe` result

        context "description" $ do
            let
                tests =
                    [ ( "returns Right [] if the description is defined"
                      , [here| {"mint": {"type": "integer", "description": "Can be any integer value"}} |]
                      , [here| {"mint": 1} |]
                      , Right []
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateRequestBody POST "/examples" apiJson bodyJson `shouldBe` result

        context "nullable" $ do
            let
                tests =
                    [ ( "returns Right [] if nullable is true and the value is null"
                      , [here| {"mint": {"type": "integer", "nullable": true}} |]
                      , [here| {"mint": null} |]
                      , Right []
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateRequestBody POST "/examples" apiJson bodyJson `shouldBe` result

        context "nullable (pending)" $ do
            let
                tests =
                    [ ( "returns Right [error] if nullable is false and the value is null"
                      , [here| {"mint": {"type": "integer", "nullable": false}} |]
                      , [here| {"mint": null} |]
                    --   , Right ["expected JSON value of type not null"]
                      , "Keyword `nullable` is not supported"
                      )
                    , ( "returns Right [error] if nullable is not specified and the value is null"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| {"mint": null} |]
                    --   , Right ["expected JSON value of type not null"]
                      , "Keyword `nullable` is not supported"
                      )
                    ] :: [(String, L.ByteString, L.ByteString, String)]
            -- forM_ tests $ \(description, schemaJson, bodyJson, result) ->
            --     it description $ do
            --         let apiJson = makeOpenApiJson schemaJson
            --         validateRequestBody POST "/examples" apiJson bodyJson `shouldNotBe` result
            forM_ tests $ \(description, _, _, reason) ->
                it description $
                    pendingWith reason

    describe "validateResponseBody" $ do
        let makeOpenApiJson schemaJson = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "paths": {
        "/examples": {
            "get": {
                "responses": {
                    "200": {
                        "description": "OK",
                        "content": {
                            "application/json": {
                                "schema": {
                                    "type": "array",
                                    "items": {
                                        "$ref": "#/components/schemas/Example"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    },
    "components": {
        "schemas": {
            "Example": {
                "type": "object",
                "properties": |] <> schemaJson <> [here|
            }
        }
    }
}
|]
        context "type: integer" $ do
            let
                tests =
                    [ ( "returns Right [] if the type of the value is valid"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| [{"mint": 1}, {"mint": 2}] |]
                      , Right []
                      )
                    , ( "returns Right [] if the type of the value is invalid"
                      , [here| {"mint": {"type": "integer"}} |]
                      , [here| [{"mint": 1}, {"mint": "two"}] |]
                      , Right ["expected JSON value of type integer"]
                      )
                    ]
            forM_ tests $ \(description, schemaJson, bodyJson, result) ->
                it description $ do
                    let apiJson = makeOpenApiJson schemaJson
                    validateResponseBody GET "/examples" 200 apiJson bodyJson `shouldBe` result
