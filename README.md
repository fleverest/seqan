
# seqan

Tools for online sequential analysis in R.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("fleverest/seqan")
```

## Demos and examples

### Using a local vector as a data source

``` r
x <- c(4, 2, 1, 5, 3)
src <- VectorSource(x)

get_n(src, 1, 3)
```

    ## [1] 4 2 1

``` r
get_rest(src, 3)
```

    ## [1] 1 5 3

``` r
get_length(src)
```

    ## [1] 5

``` r
# Peek inside.
str(src)
```

    ## <seqan::VectorSource>
    ##  @ description: chr "Vector 'x'"
    ##  @ x          : chr "x"
    ##  @ env        :<environment: 0x55ad91287620>

### Using a stream

``` r
stream <- Stream(source = src, transform_record = function(x) x + 1)

stream
```

    ## <seqan::Stream>
    ##  @ source           : <seqan::VectorSource>
    ##  .. @ description: chr "Vector 'x'"
    ##  .. @ x          : chr "x"
    ##  .. @ env        :<environment: 0x55ad91287620> 
    ##  @ transform_records: function (x)  
    ##  @ state            :<environment: 0x55ad90b559d8>

``` r
as.list(stream@state)
```

    ## $current_idx
    ## [1] 1

``` r
fetch(stream)
```

    ## [1] 5 3 2 6 4

``` r
fetch(stream)
```

    ## numeric(0)

``` r
x <- c(x, c(8, 5))

fetch(stream)
```

    ## [1] 9 6

``` r
stream
```

    ## <seqan::Stream>
    ##  @ source           : <seqan::VectorSource>
    ##  .. @ description: chr "Vector 'x'"
    ##  .. @ x          : chr "x"
    ##  .. @ env        :<environment: 0x55ad91287620> 
    ##  @ transform_records: function (x)  
    ##  @ state            :<environment: 0x55ad90b559d8>

``` r
as.list(stream@state)
```

    ## $current_idx
    ## [1] 8

### Using a simple statistic, with no data source

``` r
counter <- Counter()

counter
```

    ## <seqan::Counter>
    ##  @ stream     : NULL
    ##  @ description: chr "Data point counter"
    ##  @ state      :<environment: 0x55ad90f4e3b8>

``` r
value(counter)
```

    ## NULL

``` r
update(counter, new_x = c(2, 1, 7))

counter
```

    ## <seqan::Counter>
    ##  @ stream     : NULL
    ##  @ description: chr "Data point counter"
    ##  @ state      :<environment: 0x55ad90f4e3b8>

``` r
value(counter)  # current value
```

    ## [1] 3

``` r
value(counter, Inf)  # history of all values
```

    ## [1] 1 2 3

``` r
update(counter, new_x = c(-1, 5, 3))
value(counter)
```

    ## [1] 6

``` r
value(counter, Inf)
```

    ## [1] 1 2 3 4 5 6

``` r
is_stopped(counter)  # always FALSE, since no stopping rule
```

    ## [1] FALSE

``` r
reset(counter)
value(counter)
```

    ## integer(0)

``` r
value(counter, Inf)
```

    ## integer(0)

``` r
update(counter, new_x = c(1, 3, 2))
value(counter)
```

    ## [1] 3

``` r
value(counter, Inf)
```

    ## [1] 1 2 3

``` r
# Peek inside...
str(counter)
```

    ## <seqan::Counter>
    ##  @ stream     : NULL
    ##  @ description: chr "Data point counter"
    ##  @ state      :<environment: 0x55ad90f4e3b8>

``` r
as.list(counter@state)
```

    ## $ns
    ## integer(0)
    ## 
    ## $values
    ## [1] 1 2 3
    ## 
    ## $n
    ## [1] 3

### Using a simple statistic, with a local data source

``` r
reset(stream)

counter2 <- Counter(stream)

counter2
```

    ## <seqan::Counter>
    ##  @ stream     : <seqan::Stream>
    ##  .. @ source           : <seqan::VectorSource>
    ##  .. .. @ description: chr "Vector 'x'"
    ##  .. .. @ x          : chr "x"
    ##  .. .. @ env        :<environment: 0x55ad91287620> 
    ##  .. @ transform_records: function (x)  
    ##  .. @ state            :<environment: 0x55ad90b559d8> 
    ##  @ description: chr "Data point counter"
    ##  @ state      :<environment: 0x55ad908c1e38>

``` r
update(counter2)

counter2
```

    ## <seqan::Counter>
    ##  @ stream     : <seqan::Stream>
    ##  .. @ source           : <seqan::VectorSource>
    ##  .. .. @ description: chr "Vector 'x'"
    ##  .. .. @ x          : chr "x"
    ##  .. .. @ env        :<environment: 0x55ad91287620> 
    ##  .. @ transform_records: function (x)  
    ##  .. @ state            :<environment: 0x55ad90b559d8> 
    ##  @ description: chr "Data point counter"
    ##  @ state      :<environment: 0x55ad908c1e38>

``` r
value(counter2)
```

    ## [1] 7

``` r
x <- c(x, c(8, 9))
update(counter2)

counter2
```

    ## <seqan::Counter>
    ##  @ stream     : <seqan::Stream>
    ##  .. @ source           : <seqan::VectorSource>
    ##  .. .. @ description: chr "Vector 'x'"
    ##  .. .. @ x          : chr "x"
    ##  .. .. @ env        :<environment: 0x55ad91287620> 
    ##  .. @ transform_records: function (x)  
    ##  .. @ state            :<environment: 0x55ad90b559d8> 
    ##  @ description: chr "Data point counter"
    ##  @ state      :<environment: 0x55ad908c1e38>

``` r
value(counter2)
```

    ## [1] 9

## Acknowledgements

This package was developed during the 2025 NUMBAT Hackathon. We thank
[Monash NUMBATs](https://numbat.space/) and [Monash
EBS](https://www.monash.edu/business/ebs) for organising the event and
providing resources that enabled collaborative development.
