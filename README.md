# SIR Functions

## Description 
Key functions used by the SIR team 

## Usage
First make sure you have `devtools` installed. Then run the following command: 

```
devtools::install_github("nish-kishore/sirfunctions")
```

After you can use any function developed in this package using 

```
sirfunctions::{function}
```

for example: 

```
sirfunctions::hello_sir()
```

## Development 
In the `R` folder create a new Rscript with the same name as your function. 
Below you will find the required `roxygen` headers.
Ensure that the function has the follow `roxygen` headers:

```
#'Hello SIR Function
#'
#' @name hello_sir
#' @description An example function
#' @keywords sir
#' @returns Hello world
#' @export
#' @examples
#' hello_sir()

hello_sir <- function(){
  print("Hello SIR!")
}


```

 
