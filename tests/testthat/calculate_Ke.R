library("dplyr")

d <- expand.grid(hydrologic_group = c("A","B","C","D"),
                 management = c("Fallow",
                                "Conv. Tillage - Row Crop",
                                "Conserv. Till. - Row Crop",
                                "Small Grain",
                                "Alfalfa",
                                "Pasture (Grazed)",
                                "Meadow (Grass)"),
                 sand = c(0,1)) %>%

  mutate(Kef = 14.2 * (hydrologic_group == "A") +
           (1.17 + 0.072 * sand) * (hydrologic_group == "B") +
           (0.50 + 0.032 * sand) * (hydrologic_group == "C") +
           0.34 * (hydrologic_group == "D"),

         Ke = Kef # management == Fallow
         )





test_that("Kef calculation is correct", {

})
