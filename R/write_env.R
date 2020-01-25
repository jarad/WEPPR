#' Write a WEPP environment file
#'
#' Writes a Water Erosion Prediction Project (WEPP) environment (*.env) file.
#'
#' @param env A \code{env} object.
#' @param path Path to write to.
#' @export
#'
write_env <- function(env, path) {
  write(" EVENT OUTPUT - Written by WEPPR (units are harded-coded and therefore may not be correct)",
        file = path)

  write(paste(colnames(env), collapse = " "),
        file = path,
        append = TRUE)

  warning("Units (3rd line) are hard-coded and therefore might not be correct.")
  write("--- --  ----  (mm)    (mm)  kg/m^2 kg/m^2 kg/m^2    (m)  kg/m^2  kg/m^2    (m)  (kg/m)  ----",
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
