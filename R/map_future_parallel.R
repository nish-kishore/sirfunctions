map_future_parallel <- function(vector, .f, .packages) {
  doFuture::registerDoFuture() ## tell foreach to use future

  if (stringr::str_starts(Sys.getenv("SF_PARTNER"), "posit_workbench")) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession) ## parallelize over a local PSOCK cluster
  }

  options(doFuture.rng.onMisuse = "ignore")
  xs <- seq_along(vector)

  progressr::handlers("cli")

  progressr::with_progress({
    p <- progressr::progressor(along = xs)
    y <-
      foreach::`%dopar%`(foreach::foreach(
        x = xs,
        .packages = .packages
      ), {
        # signal a progression update
        p()
        .f(vector[[x]])
      })
  })

  return(y)

  }
