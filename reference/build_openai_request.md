# Build an OpenAI chat-completions request

Shared by the single-call path (`call_openai`) and the parallel batch
path (`call_llm_batch`).

## Usage

``` r
build_openai_request(
  system_prompt,
  user_prompt,
  model = llm_model_name("openai"),
  temperature = 0.3
)
```

## Arguments

- system_prompt:

  System prompt string.

- user_prompt:

  User prompt string.

- model:

  OpenAI model name.

- temperature:

  Sampling temperature.

## Value

An httr2 request object.
