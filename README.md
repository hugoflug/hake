# Hake
Easily run Haskell functions from your command line. 

Replace your Makefiles with Hakefiles and reap the benefits of a full, statically typed programming language.

All you need is a `Hakefile`:

```haskell
module Hakefile where

import GHC.Generics (Generic)
import System.Process (callCommand)
import Data.Aeson

newtype Service = 
  Service String
  deriving (Generic, FromJSON, Show)

newtype Version =
  Version String
  deriving (Generic, FromJSON, Show)

data Environment =
  EuStaging |
  UsStaging |
  EuProduction |
  UsProduction 
  deriving (Generic, FromJSON, Show)

data Deploy =
  Deploy {
    service :: Service,
    environment :: Environment,
    version :: Version
  } deriving (Generic, FromJSON, Show)

deploy :: Deploy -> IO ()
deploy cmd = do
  callCommand $ "echo deploying: " <> show cmd

```
This Hakefile defines a single command, `deploy`.

To define a command, just define a top-level function with a single argument, which must be an instance of `FromJSON`.

`DeriveGeneric` and `DeriveAnyClass` are available in Hakefiles, so a default `FromJSON` can be derived by just adding `deriving (Generic, FromJSON)` to your data type. If custom JSON deserialization is desired, just define a manual instance of `FromJSON`.

Commands in the Hakefile can now be called using JSON syntax:

```
> hake deploy '{ "service": "product-service", "environment": "EuProduction", "version": "2af667b" }'
deploying: Deploy {service = Service foo-service, environment = EuProduction, version = Version 2af667b}
```

They can also be called with a more CLI-friendly sugared syntax:

```
> hake deploy service=foo-service environment=EuProduction version=2af667b
deploying: Deploy {service = Service foo-service, environment = EuProduction, version = Version 2af667b}
```

Missing or invalid keys will result in validation errors:

```
> hake deploy service=foo-service environment=EuProduction
Error: key "version" not present
```

```
> hake deploy service=foo-service environment=EuProd
Error: The key "EuProd" was not found
```

Hakefiles are just Haskell, so other Haskell modules can be imported and used.

## Caveats

GHC must be installed to run `hake`, and Aeson must be available on `GHC_PACKAGE_PATH`.

Any other external libraries used in your Hakefile must also be available on `GHC_PACKAGE_PATH`.

Hakefiles currently must use the module name "Hakefile", so Hakefiles can't import other Hakefiles.

## TODO

- No-argument commands
- Specifying custom Haskell module names (e.g. `hake -m MyModule somecommand`)
- Extend sugared syntax (Supporting Int/Bools and non-records)