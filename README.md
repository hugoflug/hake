Configuration:
```haskell
#! /usr/bin/hake

newtype Service = 
  Service String
  deriving Generic

data Environment =
  EuStaging |
  UsStaging |
  EuProduction |
  UsProduction 
  deriving Generic

data Deploy =
  Deploy {
    service :: Service,
    environment :: Environment,
    version :: Version
  } deriving Generic

deploy :: Deploy -> IO ()
deploy cmd = do
  callCommand "echo fire the missiles!"

```

Usage examples:

```
> hake deploy service=foo-service environment=eu-production version=2af667b
fire the missiles!
```


```
> hake deploy service=foo-service environment=eu-production
Missing argument: 'version'
```

```
> hake deploy service=foo-service environment=eur-production
Invalid argument: 'environment=eur-production'
Expected one of: 'eu-staging', 'us-staging', 'eu-production', 'us-production'
```

Argument is either JSON, or CLI-sugared JSON, i.e.
a=b c=5 === { "a": "b", "c":"5" }

1. Lookup function name based on command name
2. Desugar argument
3. Parse argument as JSON
4. Call function with argument

Use hint for interpreting Haskell?