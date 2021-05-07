# wai-middleware-validation [![CircleCI](https://circleci.com/gh/iij-ii/wai-middleware-validation.svg?style=svg)](https://app.circleci.com/pipelines/github/iij-ii/wai-middleware-validation)

wai-middleware-validation is a WAI Middleware that automates the validation of request and response bodies. It validates JSON format bodies according to the schema defined in the OpenAPI document.

## Overview of Usage

1. Describe the schema of the request and response bodies in OpenAPI document.
2. Instansiate this middleware with that document as an argument.
3. Apply the middleware to the WAI application.
4. That's it! The middleware will automatically validate the request and response and return an error response if there is a validation error.

See [Examples](https://iij-ii.github.io/wai-middleware-validation/examples/yesod) for more details.

## License

Copyright (c) IIJ Innovation Institute Inc.

Licensed under The 3-Clause BSD License.
