##Single Value
Test_function <- KohnModel_Function(model_bodymass = 600, model_WaterEconomyIndex = 0.4,
                           model_Digestibility_of_food = 0.3, model_Carbohydrate_Content = 0.7, 
                           model_Protein_Content = 0.2, model_Fat_Content = 0.1,
                           model_air_temperature = 6, model_Free_Water_Content_Food = 0.3, 
                           model_Relative_Humidity = 0.6, model_d18O_surfacewater = -9, changeConstant = FALSE, SweatingSpecies = FALSE, PlotRange = FALSE)
Test_function

#Asking for changing constant values
Test_functionASK <- KohnModel_Function(model_bodymass = 600, model_WaterEconomyIndex = 0.4,
                                    model_Digestibility_of_food = 0.3, model_Carbohydrate_Content = 0.7, 
                                    model_Protein_Content = 0.2, model_Fat_Content = 0.1,
                                    model_air_temperature = 6, model_Free_Water_Content_Food = 0.3, 
                                    model_Relative_Humidity = 0.6, model_d18O_surfacewater = -9, changeConstant = TRUE, SweatingSpecies = FALSE, PlotRange = FALSE)
Test_functionASK

## Trying range of values
## Small range
Test_function2 <- KohnModel_Function(model_bodymass = c(600,800), model_WaterEconomyIndex = c(0.4, 0.5),
                                    model_Digestibility_of_food = 0.3, model_Carbohydrate_Content = 0.7, 
                                    model_Protein_Content = 0.2, model_Fat_Content = 0.1,
                                    model_air_temperature = c(6, 10, 20), model_Free_Water_Content_Food = 0.3, 
                                    model_Relative_Humidity = 0.6, model_d18O_surfacewater = c(-2, -9), changeConstant = FALSE, SweatingSpecies = FALSE, PlotRange = FALSE)
Test_function2

## Larger range
Test_function3 <- KohnModel_Function(model_bodymass = c(600,800,1500), model_WaterEconomyIndex = c(0.4, 0.5),
                                     model_Digestibility_of_food = 0.3, model_Carbohydrate_Content = c(0.7), 
                                     model_Protein_Content = 0.2, model_Fat_Content = c(0.1),
                                     model_air_temperature = c(6, 10, 20), model_Free_Water_Content_Food = 0.3, 
                                     model_Relative_Humidity = c(0.6,0.7,0.8), model_d18O_surfacewater = c(-2, -9), changeConstant = FALSE, SweatingSpecies = FALSE, PlotRange = FALSE)
Test_function3

## Producing plots
Test_functionPLOT <- KohnModel_Function(model_bodymass = c(600,800,1500), model_WaterEconomyIndex = c(0.4, 0.5),
                                     model_Digestibility_of_food = 0.3, model_Carbohydrate_Content = c(0.7), 
                                     model_Protein_Content = 0.2, model_Fat_Content = c(0.1),
                                     model_air_temperature = c(6, 10, 20), model_Free_Water_Content_Food = 0.3, 
                                     model_Relative_Humidity = c(0.6,0.7,0.8), model_d18O_surfacewater = c(-2, -9), changeConstant = FALSE, SweatingSpecies = FALSE, PlotRange = TRUE)
Test_functionPLOT
