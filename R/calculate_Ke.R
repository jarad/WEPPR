#' Calculate time-invariant effective hydraulic conductivity (Ke-constant)
#'
#' @param hydrologic_group hydrologic soil group (A,B,C,D)
#' @param management management practice
#' @param sand
#'
#' @source page 26 of <https://www.ars.usda.gov/ARSUserFiles/50201000/WEPP/usersum.pdf>
#'
calculate_Ke <- function(hydrologic_group, management, sand = NULL) {

  stopifnot(hydrologic_group %in% c("A","B","C","D"))
  stopifnot(management %in% c("Fallow",
                              "Conv. Tillage - Row Crop",
                              "Conserv. Till. - Row Crop",
                              "Small Grain",
                              "Alfalfa",
                              "Pasture (Grazed)",
                              "Meadow (Grass)"))


  Kef <- calculate_Kef(hydrologic_group, sand)

  switch(management,
         "Fallow" = return(Kef),

         "Conv. Tillage - Row Crop" = {
           switch(hydrologic_group,
                  "A" = return(1.37 * Kef),
                  "B" = return(1.64 * Kef),
                  "C" = return(1.64 * Kef),
                  "D" = return(1.87 * Kef))
         },

         "Conserv. Till. - Row Crop" = {
           switch(hydrologic_group,
                  "A" = return(1.49 * Kef),
                  "B" = return(1.85 * Kef),
                  "B" = return(1.85 * Kef),
                  "B" = return(2.35 * Kef))
         },

         "Small Grain" = {
           switch(hydrologic_group,
                  "A" = return(1.84 * Kef),
                  "B" = return(2.14 * Kef),
                  "B" = return(2.14 * Kef),
                  "B" = return(2.48 * Kef))
         },

         "Alfalfa" = {
           switch(hydrologic_group,
                  "A" = return(2.86 * Kef),
                  "B" = return(3.75 * Kef),
                  "B" = return(3.75 * Kef),
                  "B" = return(6.23 * Kef))
         },

         "Pasture (Grazed)" = {
           switch(hydrologic_group,
                  "A" = return(3.66 * Kef),
                  "B" = return(4.34 * Kef),
                  "B" = return(4.34 * Kef),
                  "B" = return(5.96 * Kef))
         },

         "Meadow (Grass)" = {
           switch(hydrologic_group,
                  "A" = return(6.33 * Kef),
                  "B" = return(9.03 * Kef),
                  "B" = return(9.03 * Kef),
                  "B" = return(15.5 * Kef))
         })

  stop("Something is wrong if you got here.")
}


#' @describeIn calculate_Ke fallow soil Ke
#'
calculate_Kef <- function(hydrologic_group, sand) {
  if (is.null(sand) & hydrologic_group %in% c("B","D")) {
    stop(paste0("For hydrologic group ", hydrologic_group, "`sand` must be provided."))
  }

  return(
    switch(hydrologic_group,
           "A" = 14.2,
           "B" = 1.17 + 0.072 * sand,
           "C" = 0.50 + 0.032 * sand,
           "D" = 0.34,
           stop(paste0("Hydrologic group must be A, B, C, or D.")))
  )
}
