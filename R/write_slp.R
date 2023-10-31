#' Write a WEPP slope file
#'
#' Writes a Water Erosion Prediction Project (WEPP) slope (*.slp) file.
#'
#' @param slp A \code{slp} object.
#' @param path Path to write to.
#' @export
#'
write_slp <- function(slp, path) {
  write(attr(slp, "version"), file = path)
  write("#\n# Written by WEPPR::write_slp():", file = path, append = TRUE)
  write(paste("# ", Sys.time(), "\n#"),        file = path, append = TRUE)

  n_OFEs <- length(unique(slp$n))
  write(n_OFEs,                                                   file = path, append = TRUE)
  write(paste(format(attr(slp, "azm"),    nsmall = 3),
              format(attr(slp, "fwidth"), nsmall = 3),
              sep = "  "),
        file = path, append = TRUE)

  for (n in 1:n_OFEs) {
    s = slp[slp$n == n,]

    nsplts = nrow(s)
    slplen = s$distance[nsplts]
    write(paste(nsplts, format(slplen, nsmall = 6), sep = " "), file = path, append = TRUE)

    line = character()
    for (i in 1:nsplts) {
      line = paste(line,
                   paste(format(s$p[i], nsmall = 6),
                         format(s$slope[i], nsmall = 6),
                         sep = ", "),
                   collapse = " ")
    }
    write(line, file = path, append = TRUE)
  }
}
