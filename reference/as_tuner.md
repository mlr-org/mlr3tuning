# Convert to a Tuner

Convert object to a
[Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md) or a list of
[Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md).

## Usage

``` r
as_tuner(x, ...)

# S3 method for class 'Tuner'
as_tuner(x, clone = FALSE, ...)

as_tuners(x, ...)

# Default S3 method
as_tuners(x, ...)

# S3 method for class 'list'
as_tuners(x, ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Additional arguments.

- clone:

  (`logical(1)`)  
  Whether to clone the object.
