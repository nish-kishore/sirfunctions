#' Calculates the year a week number falls into.
#'
#' @param wk `numeric` Week number.
#'
#' @returns `str` The year the week falls into.
#'
#' @examples
#' f.year.roll(32)
#' @keywords internal
f.year.roll <- function(wk) {
  x <- NA
  x[wk <= 52.00] <- 1
  x[wk > 52.00 & wk <= 104.15] <- 2
  x[wk > 104.15 & wk <= 156] <- 3
  x[wk > 156 & wk <= 208] <- 4
  x[wk > 208 & wk <= 260] <- 5
  x[wk > 260 & wk <= 312] <- 6
  x[wk > 312 & wk <= 364] <- 7
  x[wk > 364 & wk <= 416] <- 8
  x[wk > 416 & wk <= 468] <- 9
  x[wk > 468 & wk <= 520] <- 10

  x <- factor(x,
              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
              labels = c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5", "Year 6", "Year 7", "Year 8", "Year 9", "Year 10")
  )

  return(x)
}
