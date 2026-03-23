# Call the Gemini API (OpenAI-compatible endpoint)

Uses Google's OpenAI-compatible chat completions endpoint. Includes a
rate-limit delay to stay within the free tier (5 RPM for Pro, 15 RPM for
Flash). On 429 responses, waits and retries up to 3 times with
increasing backoff.

## Usage

``` r
call_gemini(
  system_prompt,
  user_prompt,
  model = llm_model_name("gemini"),
  temperature = 0.3
)
```

## Arguments

- system_prompt:

  System prompt string.

- user_prompt:

  User prompt string.

- model:

  Gemini model name (default: "gemini-2.5-flash").

- temperature:

  Sampling temperature (default: 0.3).

## Value

Character string with the generated text.
