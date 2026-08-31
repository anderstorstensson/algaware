# Does a provider support parallel requests?

OpenAI accounts have per-minute rate limits comfortably above the
handful of concurrent station descriptions a report needs. The Gemini
free tier is limited to a few requests per minute (see `call_gemini`),
so it must stay sequential.

## Usage

``` r
llm_supports_parallel(provider = NULL)
```

## Arguments

- provider:

  Provider name, or NULL to auto-detect.

## Value

TRUE when independent requests may be performed concurrently.
