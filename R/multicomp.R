multicomp <- function(model, x, conf_int) {
  validate_multicomp(new_multicomp(model, x, conf_int))
}

new_multicomp <- function(model, x, conf_int) {
  stopifnot(is.array(x))
  result <- list(all = x,
                 shortest = shortest_intervals(x),
                 model = model,
                 conf_int = conf_int)
  structure(
    result,
    class = "multicomp"
  )
}

shortest_intervals <- function(x) {
  idx <- apply(x[, "length", ], 1, which.min)
  result <- matrix(nrow = nrow(x), ncol = ncol(x))
  for (r in seq_len(nrow(x))) {
    result[r, ] <- x[r, , idx[r]]
  }

  result_df <- data.frame(result)
  colnames(result_df) <- colnames(x)
  result_df$source <- names(x[1, 1, ])[idx]
  result_df
}

validate_multicomp <- function(x) {
  if (!all(dim(x)[2:3] == 3:2)) {
    stop("Incorrect dimensions in input array.")
  }
  if (!all(colnames(x) == c("lower", "upper", "length"))) {
    stop("Incorrect column names in input array.")
  }

  x
}

#' @export
print.multicomp <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$model$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat(paste0("Shortest ", x$conf_int * 100, "% intervals:"), "\n")
  print(x$shortest)
}

#' @export
summary.multicomp <- function(object, ...) {
  print.multicomp(object, ...)
}
