#' Write a WEPP environment file
#'
#' Writes a Water Erosion Prediction Project (WEPP) environment (*.env) file.
#'
#' The env object that is written must have a 'units' attribute. This is read
#' from the env file when using the \code{\link{read_env}} function, but users
#' need to be careful if they are modifying this env object that they maintain
#' the units or modify the 'units' attribute appropriately. At this point, we
#' have no idea how WEPP deals with units, i.e. whether it reads the units from
#' the 3rd line in an env file or not.
#'
#' @param env A \code{env} object with 'units' attribute.
#' @param path Path to write to.
#' @export
#'
write_env <- function(env, path) {
  # decompose Dates column
  day = as.numeric(format(as.Date(env$Date,format="%Y-%m-%d"), format = "%d"))
  mo = as.numeric(format(as.Date(env$Date,format="%Y-%m-%d"), format = "%m"))
  year = as.numeric(format(as.Date(env$Date,format="%Y-%m-%d"), format = "%Y"))
  year = year - min(year) + 1

  env = cbind(day, mo, year, env[,!(names(env) == 'Date')])

  write(" EVENT OUTPUT - Written by WEPPR",
        file = path)

  write(paste(colnames(env), collapse = " "),
        file = path,
        append = TRUE)

  write(paste(attr(env, 'units'), collapse = " "),
        file = path,
        append = TRUE)

  write.table(format(env, digits = 3),
              file = path,
              append = TRUE,
              quote = FALSE,
              sep = "\t",
              col.names = FALSE,
              row.names = FALSE)
}
