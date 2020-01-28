#' Write a WEPP soil file
#'
#' Writes a Water Erosion Prediction Project (WEPP) soil (*.sol) file.
#' This file contains soil information.
#'
#' @param sol A \code{sol} object.
#' @param path Path to write to.
#' @export
#'
write_sol <- function(sol, path) {
  write(attr(sol, "datver"), file = path)
  write(attr(sol, "solcom"), file = path, append = TRUE)

  ntemp <- length(unique(sol$layer))
  write(paste(ntemp, attr(sol, "ksflag"), collapse = " "),
        file = path, append = TRUE)

  for (n in 1:ntemp) {
    s = sol[sol$layer == n,]

    nsl = nrow(s)
    line1 = unique(s[,c("slid","texid","salb","sat","ki","kr","shcrit","avke")])
    stopifnot(nrow(line1)==1)

    write(paste(line1$slid,
                line1$texid,
                nsl,
                format(line1$salb,   nsmall = 6),
                format(line1$sat,    nsmall = 6),
                format(line1$ki,     nsmall = 6),
                format(line1$kr,     nsmall = 6),
                format(line1$shcrit, nsmall = 6),
                format(line1$avke,   nsmall = 6),
                sep = " "),
          file = path, append = TRUE)

    for (r in 1:nsl) {
      write(paste("  ", # Seems like this should be a tab
                  format(s$solthk[r], nsmall = 0),
                  format(s$sand[r],   nsmall = 1),
                  format(s$clay[r],   nsmall = 1),
                  format(s$orgmat[r], nsmall = 1),
                  format(s$cec[r],    nsmall = 1),
                  format(s$rfg[r],    nsmall = 1)),
            file = path, append = TRUE)
    }

    line1 = unique(s[,c("Ke","flag_Kb_Ke","unknown")])
    stopifnot(nrow(line1)==1)

    write(paste(format(line1$Ke,         nsmall = 0),
                format(line1$flag_Kb_Ke, nsmall = 6),
                format(line1$unknown,    nsmall = 0),
                sep = " "),
          file = path, append = TRUE)
  }
}
