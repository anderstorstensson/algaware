# Call the OpenAI API

Call the OpenAI API

## Usage

``` r
call_openai(
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

  OpenAI model name (default: `llm_model_name("openai")`).

- temperature:

  Sampling temperature (default: 0.3). Lower values produce more
  deterministic, factual output; higher values are more creative. 0.3 is
  a good balance for scientific report text.

## Value

Character string with the generated text.
