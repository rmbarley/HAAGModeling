#' Calculate Food Parameters for d18 Analysis
#'
#' This function calculates food-related parameters needed for oxygen isotope
#' analysis. It processes the digestibility and nutrient
#' content of food to determine energy content and nutrient ratios required by
#' other functions in the isotope calculations.
#'
#' @param Digestibility_of_food The digestibility of food as a proportion
#'   (0-1). This represents the fraction of food that can be absorbed.
#' @param Carbohydrate_Content The carbohydrate content of food as a
#'   proportion (0-1). This value should sum with protein and fat content to equal 1.0.
#' @param Protein_Content The protein content of food as a proportion
#'   (0-1). This value should sum with carbohydrate and fat content to equal 1.0.
#' @param Fat_Content The fat content of food as a proportion (0-1).
#'   This should sum with carbohydrate and protein content to equal 1.0.
#' @param Free_Water_Content_Food The free water content of food as a
#'   proportion (0-1).
#' @param printinfo If TRUE, intermediate calculations will be printed. The default is FALSE.
#'
#' @return The function will return the following list:
#' \describe{
#'   \item{Digestibility}{The provided digestibility value}
#'   \item{EEE}{Energy extraction efficiency (standardized to 0.9)}
#'   \item{foodcarbenergy}{Energy content of carbohydrates (17300 J/g)}
#'   \item{foodcarbcontent}{The provided carbohydrate content proportion}
#'   \item{Ocarb}{Oxygen atoms per carbohydrate unit (15.4)}
#'   \item{Hcarb}{Hydrogen atoms per carbohydrate unit (30.9)}
#'   \item{foodproteinenergy}{Energy content of proteins (20100 J/g)}
#'   \item{Oprotein}{Oxygen atoms per protein unit (3)}
#'   \item{Hprotein}{Hydrogen atoms per protein unit (11)}
#'   \item{foodfatenergy}{Energy content of fats (39700 J/g)}
#'   \item{Ofat}{Oxygen atoms per fat unit (2)}
#'   \item{Hfat}{Hydrogen atoms per fat unit (6)}
#'   \item{foodproteincontent}{The provided protein content proportion}
#'   \item{foodfatcontent}{The provided fat content proportion}
#'   \item{freeH20food}{The provided free water content proportion}
#' }
#'
#' @examples
#' # Example parameters for a herbivore diet
#' herbivore_food <- Food_Function(
#'   Digestibility_of_food = 0.6,
#'   Carbohydrate_Content = 0.8,
#'   Protein_Content = 0.1,
#'   Fat_Content = 0.1,
#'   Free_Water_Content_Food = 0.4
#' )
#'
#' # Example parameters for a carnivore diet
#' carnivore_food <- Food_Function(
#'   Digestibility_of_food = 0.85,
#'   Carbohydrate_Content = 0.1,
#'   Protein_Content = 0.7,
#'   Fat_Content = 0.2,
#'   Free_Water_Content_Food = 0.7,
#'   printinfo = TRUE
#' )

#' @export
Food_Function <- function(Digestibility_of_food = 0, Carbohydrate_Content = 0,
                          Protein_Content = 0, Fat_Content = 0, Free_Water_Content_Food = 0,
                          printinfo = FALSE) {
  # implementation here
}

##SET FUNCTION FOR FOOD
Food_Function <- function(Digestibility_of_food=0, Carbohydrate_Content=0,
                          Protein_Content=0, Fat_Content=0, Free_Water_Content_Food=0,
                          changeConstant = FALSE) {

  ## 0. PREPPING DATAFRAME FOR OUTPUTS ===========================================
  # Width = number of variables
  # Length = number of combination of results
  Food_outputs <- matrix(data = 0,   nrow = length(Digestibility_of_food)*length(Carbohydrate_Content)*length(Protein_Content)*length(Fat_Content)*length(Free_Water_Content_Food),
                         ncol = 15)
  colnames(Food_outputs) <- c("Digestibility", "EEE", "foodcarbenergy", "foodcarbcontent",
                                          "Ocarb", "Hcarb", "foodproteinenergy",
                                          "Oprotein", "Hprotein", "foodfatenergy",
                              "Ofat", "Hfat", "foodproteincontent", "foodfatcontent", "freeH20food")
  Food_outputs <- as.data.frame(Food_outputs)

  #FILLING DATAFRAME WITH ARGUMENTS VALUES AND CONSTANTS

  message("Energy Extraction Efficiency, fractions (O, H, energy) of carbohydrate, protein, and fat are standardized constants extracted from Kohn models and literature,
         but it can be modified by user by modifying the argument changeConstant to TRUE")

  # free H2O content of food ===================================================
  if(sum(Free_Water_Content_Food)==0)
  {
    stop("Enter Free Water Content of Food as Proportion Value (ex:0.4)")
  }
  freeH20food <- Free_Water_Content_Food

  Food_outputs$freeH20food <- freeH20food

  #Digestibility ===============================================================
  if(sum(Digestibility_of_food)==0)
  {
    stop("Enter Digestibility value")
  }
  Digestibility <- Digestibility_of_food
  #energy extraction efficiency
  Food_outputs$EEE <- 0.9
  if(changeConstant == FALSE){
  message("Energy Extraction Efficiency standardized to EEE = 0.9")}

  if(changeConstant == TRUE){
    EEE_temp <- readline("Please enter a SINGLE value for Energy Extraction Efficiency, 0.9 is the default value : ")
    Food_outputs$EEE <- as.numeric(EEE_temp)
  }

  Col_Digestibility_temp <- c()
  for(i in 1:length(Digestibility)){
    Col_Digestibility_temp <- c(Col_Digestibility_temp, rep(Digestibility[i], nrow(Food_outputs)/length(Digestibility)))} ## the dataframe is split in X part for X values of Digestibility
  Food_outputs$Digestibility <- Col_Digestibility_temp


  # food carbohydrate ==========================================================
  Food_outputs$foodcarbenergy <- 17300
  Food_outputs$Ocarb <- 15.4
  Food_outputs$Hcarb <- 30.9

  if(changeConstant == TRUE){
    foodcarbenergy_temp <- readline("Please enter a SINGLE value for the energy coming from carbohydrate, 17300 J is the default value : ")
    Food_outputs$foodcarbenergy <- as.numeric(foodcarbenergy_temp)

    Ocarb_temp <- readline("Please enter a SINGLE value for the oxygen coming from carbohydrate, 15.4 is the default value : ")
    Food_outputs$Ocarb <- as.numeric(Ocarb_temp)

    Hcarb_temp <- readline("Please enter a SINGLE value for the hydrogen coming from carbohydrate, 30.9 is the default value : ")
    Food_outputs$Hcarb <- as.numeric(Hcarb_temp)
  }

  if(sum(Carbohydrate_Content)==0)
  {
    stop("Enter Carbohydrate Content of Food as Proportion Value (ex:0.8)")
  }
  foodcarbcontent <- Carbohydrate_Content

  Col_foodcarbcontent_temp <- c()
  for(i in 1:length(foodcarbcontent)){
    Col_foodcarbcontent_temp <- c(Col_foodcarbcontent_temp, rep(foodcarbcontent[i], nrow(Food_outputs)/length(Digestibility)/length(foodcarbcontent)))}
  Col_foodcarbcontent_temp <- rep(Col_foodcarbcontent_temp, length(Digestibility))
  Food_outputs$foodcarbcontent <- Col_foodcarbcontent_temp


  # food protein ===============================================================
  Food_outputs$foodproteinenergy <- 20100
  Food_outputs$Oprotein <- 3
  Food_outputs$Hprotein <- 11

  if(changeConstant == TRUE){
    foodproteinenergy_temp <- readline("Please enter a SINGLE value for the energy coming from protein, 20100 J is the default value : ")
    Food_outputs$foodproteinenergy <- as.numeric(foodproteinenergy_temp)

    Opro_temp <- readline("Please enter a SINGLE value for the oxygen coming from protein, 3 is the default value : ")
    Food_outputs$Oprotein <- as.numeric(Opro_temp)

    Hpro_temp <- readline("Please enter a SINGLE value for the hydrogen coming from protein, 11 is the default value : ")
    Food_outputs$Hprotein <- as.numeric(Hpro_temp)
  }

  if(sum(Protein_Content)==0)
  {
    stop("Enter Protein Content of Food as Proportion Value (ex:0.8)")
  }
  foodproteincontent <- Protein_Content

  Col_foodproteincontent_temp <- c()
  for(i in 1:length(foodproteincontent)){
    Col_foodproteincontent_temp <- c(Col_foodproteincontent_temp,
                                     rep(foodproteincontent[i], nrow(Food_outputs)/length(Digestibility)/length(foodproteincontent)/length(foodcarbcontent)))}
  Col_foodproteincontent_temp <- rep(Col_foodproteincontent_temp, (length(Digestibility) * length(foodcarbcontent)))
  Food_outputs$foodproteincontent <- Col_foodproteincontent_temp



  # e. food fat ================================================================
  Food_outputs$foodfatenergy <- 39700
  Food_outputs$Ofat <- 2
  Food_outputs$Hfat <- 6

  if(changeConstant == TRUE){
    foodfatenergy_temp <- readline("Please enter a SINGLE value for the energy coming from fat, 39700 J is the default value : ")
    Food_outputs$foodfatenergy <- as.numeric(foodfatenergy_temp)

    Ofat_temp <- readline("Please enter a SINGLE value for the oxygen coming from fat, 2 is the default value : ")
    Food_outputs$Ofat <- as.numeric(Ofat_temp)

    Hfat_temp <- readline("Please enter a SINGLE value for the hydrogen coming from fat, 6 is the default value : ")
    Food_outputs$Hfat <- as.numeric(Hfat_temp)
  }

  if(sum(Fat_Content)==0)
  {
    stop("Enter Fat Content of Food as Proportion Value (ex:0.8)")
  }
  foodfatcontent <- Fat_Content

  Col_foodfatcontent_temp <- c()
  for(i in 1:length(foodfatcontent)){
    Col_foodfatcontent_temp <- c(Col_foodfatcontent_temp,
                                 rep(foodfatcontent[i], nrow(Food_outputs)/length(Digestibility)/length(foodproteincontent)/length(foodcarbcontent)/length(foodfatcontent)))}
  Col_foodfatcontent_temp <- rep(Col_foodfatcontent_temp, (length(Digestibility) * length(foodcarbcontent) * length(foodproteincontent)))
  Food_outputs$foodfatcontent <- Col_foodfatcontent_temp


  return(Food_outputs)
}
