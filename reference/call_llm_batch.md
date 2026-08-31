# Call an LLM provider for a batch of independent prompts

For providers that support it (OpenAI), all requests are performed
concurrently with
[`httr2::req_perform_parallel()`](https://httr2.r-lib.org/reference/req_perform_parallel.html),
collapsing n sequential round trips into roughly the latency of the
slowest one. Other providers fall back to a sequential loop (Gemini
keeps its rate-limit delay). Each prompt gets an independent result: one
failed request never aborts the batch.

## Usage

``` r
call_llm_batch(prompts, provider = NULL, temperature = 0.3, on_progress = NULL)
```

## Arguments

- prompts:

  List of prompts, each a list with `system` and `user` strings.

- provider:

  Character string: `"openai"` or `"gemini"`. NULL (default)
  auto-detects.

- temperature:

  Sampling temperature (default: 0.3).

- on_progress:

  Optional callback `function(i, n)` invoked before each request in the
  sequential fallback (not used in the parallel path, where requests run
  at once).

## Value

A list the same length as `prompts`; each element is a list with `text`
(character or NULL) and `error` (character or NULL).
