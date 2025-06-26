KohnModel_Function <- function(model_bodymass = 0, model_WaterEconomyIndex = 0, model_Digestibility_of_food = 0,
                      model_Carbohydrate_Content = 0, model_Protein_Content= 0, 
                      model_Fat_Content = 0, model_Free_Water_Content_Food = 0, model_air_temperature= 0,
                      model_Relative_Humidity = 0, model_d18O_surfacewater= 0, changeConstant = FALSE, SweatingSpecies = FALSE, PlotRange = TRUE)
{

#Load Functions, this will be removed when the function will be gathered in the package
  source("Species_Function_V2.R")
  source("Food_Function_V2.R")
  source("Environment_Function_V2.R")
  source("Input_Function_V2.R")
  source("Output_Function_V2.R")
  source("d18OBW_Function_V2.R")
  
##Species
OS <- Species_Function(body_mass= model_bodymass, WaterEconomyIndex= model_WaterEconomyIndex, changeConstant = changeConstant)

## Food
OF <- Food_Function(Digestibility_of_food = model_Digestibility_of_food, Carbohydrate_Content = model_Carbohydrate_Content, Protein_Content = model_Protein_Content, 
                    Fat_Content = model_Fat_Content, Free_Water_Content_Food = model_Free_Water_Content_Food, changeConstant = changeConstant)

## Environment
OE <- Environment_Function(air_temperature = model_air_temperature,Relative_Humidity= model_Relative_Humidity, d18O_surfacewater= model_d18O_surfacewater)

## Oxygen Inputs
OI <- Input_Function(Species = OS, Food = OF ,Environment = OE)

## Oxygen Outputs
OO <- Outputs_Function(Inputs = OI, SweatingSpecies = SweatingSpecies)

##d18O values
d18O <- d18OBW_Function(Outputs = OO)

### Priting default plots if arguments are ranges of values (i.e. more than 1 value in any argument) ==========
if(PlotRange == TRUE & nrow(d18O) > 1)
{
  if(length(model_bodymass) > 1)
  {
    plot(d18O$d18Obw~d18O$Bodymass, xlab = "Body mass (in Kg)", ylab = "d18O body water", pch = 16)
  }
  if(length(model_WaterEconomyIndex) > 1)
  {
    plot(d18O$d18Obw~d18O$WEI, xlab = "Water Economy Index", ylab = "d18O body water", pch = 16)
  }
  if(length(model_Digestibility_of_food) > 1)
  {
    plot(d18O$d18Obw~d18O$Digestibility, xlab = "Digestibility", ylab = "d18O body water", pch = 16)
  }
  if(length(model_Carbohydrate_Content) > 1)
  {
    plot(d18O$d18Obw~d18O$foodcarbcontent, xlab = "Carbohydrate content in food", ylab = "d18O body water", pch = 16)
  }
  if(length(model_Protein_Content) > 1)
  {
    plot(d18O$d18Obw~d18O$foodproteincontent, xlab = "Protein content in food", ylab = "d18O body water", pch = 16)
  }
  if(length(model_Fat_Content) > 1)
  {
    plot(d18O$d18Obw~d18O$foodfatcontent, xlab = "Fat content in food", ylab = "d18O body water", pch = 16)
  }
  if(length(model_Free_Water_Content_Food) > 1)
  {
    plot(d18O$d18Obw~d18O$freeH20food, xlab = "Free water content in food", ylab = "d18O body water", pch = 16)
  }
  if(length(model_air_temperature) > 1)
  {
    plot(d18O$d18Obw~d18O$airtemp, xlab = "Air temperature", ylab = "d18O body water", pch = 16)
  }
  if(length(model_Relative_Humidity) > 1)
  {
    plot(d18O$d18Obw~d18O$Humidity, xlab = "Humidity", ylab = "d18O body water", pch = 16)
  }
  if(length(model_d18O_surfacewater) > 1)
  {
    plot(d18O$d18Obw~d18O$d18Osw, xlab = "d18 O surface water", ylab = "d18O body water", pch = 16)
  }
}

#Output of Kohn Model Function ========
return(d18O)
}


