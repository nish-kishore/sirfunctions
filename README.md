# SIR Functions

## Description
Key functions used by the SIR team

## Installation and Updates
First make sure you have `devtools` installed (if not run `install.pacakges("devtools")`). Then run the following command:

```
devtools::install_github("nish-kishore/sirfunctions")
```

Remember to update from `sirfunctions` regularly. If no update is needed the line of code above will 
automatically skip. 

## Usage

After you can use any function developed in this package using

```
sirfunctions::{function}
```

for example:

```
sirfunctions::edav_io(io = "list")
```

You can find the documentation for any function by running 

```
?sirfunctions::{function}
```

for example: 

```
?sirfunctions::edav_io
```

## Functions

Here is a current list of functions included in `sirfunctions` with brief descriptions. 

*NOTE: Any "Get Data" functions or those that interact with EDAV require authenticated connection to CDC EDAV resources* 

### Get Data

1) `get_all_polio_data`: This is the primary function which loads all recent polio data.
Parameters can be used to specify date ranges and user verification. 
2) `edav_io`: Perform key writing/reading/editing functions for EDAV from your R system.
3) `load_clean_{resolution}_sp`: 'resolution` can be *dist*, *prov* or *ctry*. 
Parameters can be used to specify date or spatial ranges as well as the output. 

### Key calculations

1) `extract_country_data`:
2) `f.ev.rate.01`:
3) `f.npafp.rate.01`:
4) `f.stool.ad.01`:
5) `f.timelinedetection.01`:

### Helper functions


1) `test_EDAV_connection`:
2) `f.color.schemes`:
3) `f.expand.bbox`:
4) `f.metadata.tag`:
5) `f.plot.looks`:

## Release Schedule
- V2 to be released 4/1/2024, you can track progress [here](https://github.com/nish-kishore/sirfunctions/milestone/3).

Key features to be released are in development and will be shared shortly. 
