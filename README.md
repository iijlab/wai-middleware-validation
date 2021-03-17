# wai-middleware-validation [![CircleCI](https://circleci.com/gh/iij-ii/wai-middleware-validation.svg?style=svg)](https://app.circleci.com/pipelines/github/iij-ii/wai-middleware-validation)

wai-middleware-validation is a WAI Middleware that automates the validation of request and response bodies. It validates JSON format bodies according to the schema defined in the OpenAPI document.

## Usage

The following is an example of applying it to a Yesod application.

1. Define the request and response specifications as an OpenAPI document file in JSON format and place it in an arbitrary path. (In this case, we will use "config/openapi.json".)
2. Make the following modifications to `Application.hs`.
```diff
--- a/src/Application.hs
+++ b/src/Application.hs
@@ -37,6 +37,8 @@ import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                              mkRequestLogger, outputFormat)
 import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                              toLogStr)
+import Network.Wai.Middleware.Validation    (mkValidator')
+import qualified Data.ByteString.Lazy as L

 -- Import all relevant handler modules here.
 -- Don't forget to add new modules to your cabal file!
@@ -94,7 +97,10 @@ makeApplication foundation = do
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

## LICENCE

Copyright (c) IIJ Innovation Institute Inc.

Licensed under The 3-Clause BSD License.
