## SET FUNCTION FOR FOOD
Food_Function <- function(Digestibility_of_food = 0, Carbohydrate_Content = 0,
                          Protein_Content = 0, Fat_Content = 0, Free_Water_Content_Food = 0,
                          printinfo = FALSE) {
  # Digestibility
  if (sum(Digestibility_of_food) == 0) {
    stop("Enter Digestibility value")
  }
  Digestibility <- Digestibility_of_food
  # energy extraction efficiency
  EEE <- 0.9
  message("Energy Extraction Efficiency standardized to EEE=0.9")
  # food carbohydrate
  foodcarbenergy <- 17300
  Ocarb <- 15.4
  Hcarb <- 30.9
  if (sum(Carbohydrate_Content) == 0) {
    stop("Enter Carbohydrate Content of Food as Proportion Value (ex:0.8)")
  }
  foodcarbcontent <- Carbohydrate_Content

  # food protein
  foodproteinenergy <- 20100
  Oprotein <- 3
  Hprotein <- 11
  if (sum(Protein_Content) == 0) {
    stop("Enter Protein Content of Food as Proportion Value (ex:0.8)")
  }
  foodproteincontent <- Protein_Content

  # e. food fat
  foodfatenergy <- 39700
  Ofat <- 2
  Hfat <- 6
  if (sum(Fat_Content) == 0) {
    stop("Enter Fat Content of Food as Proportion Value (ex:0.8)")
  }
  foodfatcontent <- Fat_Content

  # free H2O content of food
  if (sum(Free_Water_Content_Food) == 0) {
    stop("Enter Free Water Content of Food as Proportion Value (ex:0.4)")
  }
  freeH20food <- Free_Water_Content_Food


  ## Outputs of Food_Function
  Output_Food_Function <- list(
    Digestibility = Digestibility, EEE = EEE, foodcarbenergy = foodcarbenergy,
    foodcarbcontent = foodcarbcontent, Ocarb = Ocarb, Hcarb = Hcarb,
    foodproteinenergy = foodproteinenergy, Oprotein = Oprotein,
    Hprotein = Hprotein,
    foodfatenergy = foodfatenergy, Ofat = Ofat, Hfat = Hfat,
    foodproteincontent = foodproteincontent, foodfatcontent = foodfatcontent,
    freeH20food = freeH20food
  )
  return(Output_Food_Function)
}

# OF <- Food_Function(Digestibility_of_food = 0.6, Carbohydrate_Content = 0.8, Protein_Content = 0.1, Fat_Content = 0.1, Free_Water_Content_Food = 0.4)
# Food <- OF
