---
title: Example for Yesod
---

The following is a step-by-step example of applying wai-middleware-validation when using Yesod.

## Step 1: Create a project

Create a Yesod project using `stack`.

```shell
$ stack new petstore yesodweb/sqlite
```

And you will see the following files generated under a new directory `petstore/`:

```
petstore
├── README.md
├── app
│   ├── DevelMain.hs
│   ├── devel.hs
│   └── main.hs
├── config
│   ├── favicon.ico
│   ├── keter.yml
│   ├── models.persistentmodels
│   ├── robots.txt
│   ├── routes.yesodroutes
│   ├── settings.yml
│   └── test-settings.yml
├── package.yaml
├── petstore.cabal
├── src
│   ├── Application.hs
│   ├── Foundation.hs
│   ├── Handler
│   │   ├── Comment.hs
│   │   ├── Common.hs
│   │   ├── Home.hs
│   │   └── Profile.hs
│   ├── Import
│   │   └── NoFoundation.hs
│   ├── Import.hs
│   ├── Model.hs
│   ├── Settings
│   │   └── StaticFiles.hs
│   └── Settings.hs
├── stack.yaml
...
```

## Step 2: Add an example code without validation

First, apply the following changes to the source files to implement a example API.

### Add the model

Add the model definition to `config/models.persistentmodels` as follows:

```diff
@@ -16,3 +16,9 @@ Comment json -- Adding "json" causes ToJSON and FromJSON instances to be derived
     userId UserId Maybe
     deriving Eq
     deriving Show
+
+Pet json
+    kind Text
+    age Int
+    deriving Eq
+    deriving Show
```

### Add the route

Add the route definition to `config/routes.yesodroutes` as follows:

```diff
@@ -12,3 +12,5 @@
 /comments CommentR POST
 
 /profile ProfileR GET
+
+/pets PetsR GET POST
```

### Add the handlers

Create `src/Handler/Pet.hs` with the following:

```haskell
module Handler.Pet where

import           Import


getPetsR :: Handler Value
getPetsR = do
    pets <- runDB $ selectList [] [Asc PetId]
    returnJson pets

postPetsR :: Handler Value
postPetsR = do
    pet <- requireCheckJsonBody :: Handler Pet
    pet' <- runDB $ insertEntity pet
    returnJson pet'
```

Add the following line to `src/Application.hs`.

```diff
@@ -44,6 +44,7 @@ import Handler.Common
 import Handler.Home
 import Handler.Comment
 import Handler.Profile
+import Handler.Pet
 
 -- This line actually creates our YesodDispatch instance. It is the second half
 -- of the call to mkYesodData which occurs in Foundation.hs. Please see the

```

Add the following line to `src/Foundation.hs`.

```diff
@@ -166,6 +166,7 @@ instance Yesod App where
     isAuthorized FaviconR _ = return Authorized
     isAuthorized RobotsR _ = return Authorized
     isAuthorized (StaticR _) _ = return Authorized
+    isAuthorized PetsR _ = return Authorized
 
     -- the profile route requires that the user is authenticated, so we
     -- delegate to that function
```

### Make sure it works

Run the devel server.

```shell
$ stack exec -- yesod devel
```

From another terminal, post the test data with the following:

```shell {2}
$ curl http://localhost:3000/pets -X POST -H "Content-Type: application/json" -d '{"kind":"dog","age":3}'
{"kind":"dog","age":3,"id":1}
```

Then, confirm that the data has been stored with the following:

```shell {2}
$ curl -s http://localhost:3000/pets
[{"kind":"dog","age":3,"id":1}]
```

## Step 3: Write the OpenAPI document

Next, write the specification of this endpoint as an OpenAPI document. Save the following as `config/openapi.json`.

We only deal with dogs and cats in our store.

```json {58-61}
{
    "openapi": "3.0.0",
    "info": {
        "title": "Pet Store API",
        "version": "1.0.0"
    },
    "paths": {
        "/pets": {
            "get": {
                "responses": {
                    "200": {
                        "description": "pet list",
                        "content": {
                            "application/json": {
                                "schema": {
                                    "type": "array",
                                    "items": {
                                        "$ref": "#/components/schemas/Pet"
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
                                "$ref": "#/components/schemas/Pet"
                            }
                        }
                    }
                },
                "responses": {
                    "default": {
                        "description": "new pet"
                    }
                }
            }
        }
    },
    "components": {
        "schemas": {
            "Pet": {
                "type": "object",
                "required": [
                    "kind",
                    "age"
                ],
                "properties": {
                    "id": {
                        "type": "integer"
                    },
                    "kind": {
                        "type": "string",
                        "enum": [
                            "dog",
                            "cat"
                        ]
                    },
                    "age": {
                        "type": "integer"
                    }
                }
            }
        }
    }
}
```

## Step 4: Apply the middleware

### Add the package dependency

Add `wai-middleware-validation` to `dependencies` in `package.yaml`.

```diff
@@ -42,6 +42,7 @@ dependencies:
 - case-insensitive
 - wai
 - foreign-store
+- wai-middleware-validation
 
 # The library contains all of our application code. The executable
 # defined below is just a thin wrapper.
```

You will probably need to add lines like the following to `extra-deps` in `stack.yaml`. Change the package version and hash value to appropriate values according to the build output.

```diff
@@ -41,6 +41,9 @@ packages:
 #   commit: e7b331f14bcffb8367cd58fbfc8b40ec7642100a
 #
 # extra-deps: []
+extra-deps:
+- wai-middleware-validation-0.1.0.0@sha256:39d458e8d15f017d0653e4557864d1cf672c2fb46f1d4cae09fffbb8ebc1ddc2,2299
+- openapi3-3.1.0@sha256:dd666c03bda1aeb1c50724385d0c79dd2c50b827413552fd522f7314e7081bc4,4652
 
 # Override default flag values for local packages and extra-deps
 # flags: {}

```

### Apply the middleware

And finally, add the code to apply the middleware. Change `src/Application.hs` as follows:

```diff
@@ -37,6 +37,8 @@ import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                              mkRequestLogger, outputFormat)
 import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                              toLogStr)
+import Network.Wai.Middleware.Validation    (mkValidator')
+import qualified Data.ByteString.Lazy as L
 
 -- Import all relevant handler modules here.
 -- Don't forget to add new modules to your cabal file!
@@ -95,7 +97,10 @@ makeApplication foundation = do
     logWare <- makeLogWare foundation
     -- Create the WAI application and apply middlewares
     appPlain <- toWaiAppPlain foundation
-    return $ logWare $ defaultMiddlewaresNoLogging appPlain
+    apiJson <- L.readFile "config/openapi.json"
+    let validator = fromMaybe (error "Invalid OpenAPI document") (mkValidator' apiJson)
+        app = validator appPlain
+    return $ logWare $ defaultMiddlewaresNoLogging app
 
 makeLogWare :: App -> IO Middleware
 makeLogWare foundation =
```

### Try!

Run the devel server again.

```shell
$ stack exec -- yesod devel
```

From another terminal, try the following command. Make sure you can add a cat.

```shell {2,5}
$ curl http://localhost:3000/pets -X POST -H "Content-Type: application/json" -d '{"kind":"cat","age":2}'
{"kind":"cat","age":2,"id":2}

$ curl -s http://localhost:3000/pets
[{"kind":"dog","age":3,"id":1},{"kind":"cat","age":2,"id":2}]
```

Then, try adding a tiger. We can't deal tigers in our store!

```shell {2}
$ curl http://localhost:3000/pets -X POST -H "Content-Type: application/json" -d '{"kind":"tiger","age":10}'
{"title":"Validation failed","detail":"expected one of \"[\\\"dog\\\",\\\"cat\\\"]\" but got String \"tiger\"\n"}
```

As you can see, using **wai-middleware-validation**, you can implement validation without writing any code in the handler.
