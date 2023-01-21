#' DII_ASA24
#'
#' Calculate the Dietary Inflammatory Index for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The DII and its component scores
#' @examples
#' data("ASA24_exp")
#' DII_ASA24(ASA24_exp)
#' @export

DII_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  if ("FoodCode" %in% colnames(COHORT)){
    stop("Please use the population-level data. The file name should be like: Totals.csv")
  }
  
  #Serving size calculation for DII
  COHORT1 = COHORT %>%
    dplyr::mutate(
      ALCOHOL = ALC,
      VITB12 = VB12,
      VITB6 = VB6,
      BCAROTENE = BCAR,
      CAFFEINE = CAFF/1000,
      CARB = CARB,
      CHOLES = CHOLE,
      KCAL = KCAL,
      TOTALFAT = TFAT,
      FIBER = FIBE,
      FOLICACID = FA,
      IRON = IRON,
      MG = MAGN,
      MUFA = MFAT,
      NIACIN = NIAC,
      N3FAT = P205 + P225 + P226,
      N6FAT = P182 + P183 + P184 + P204,
      PROTEIN = PROT,
      PUFA = PFAT,
      RIBOFLAVIN = VB2,
      SATFAT = SFAT,
      SE = SELE,
      THIAMIN = VB1,
      VITA = VARA,
      VITC = VC,
      VITD = VITD,
      VITE = ATOC,
      ZN = ZINC
    ) %>%
    dplyr::select(UserName, RecallNo, ALCOHOL, VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
                  IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
                  VITC, VITD, VITE, ZN)
  
  COHORT2 = COHORT1 %>%
    tidyr::pivot_longer(-c(UserName, RecallNo), names_to="Variable", values_to="Value")
  
  Variable = c("ALCOHOL", "VITB12", "VITB6", "BCAROTENE", "CAFFEINE", "CARB", "CHOLES", "KCAL", "EUGENOL",
               "TOTALFAT", "FIBER", "FOLICACID","GARLIC", "GINGER","IRON", "MG", "MUFA", "NIACIN", "N3FAT", "N6FAT","ONION", "PROTEIN", "PUFA", 
               "RIBOFLAVIN","SAFFRON", "SATFAT", "SE", "THIAMIN","TRANSFAT","TURMERIC", "VITA","VITC", "VITD", "VITE", "ZN", "TEA",
               "FLA3OL","FLAVONES","FLAVONOLS","FLAVONONES","ANTHOC","ISOFLAVONES","PEPPER","THYME","ROSEMARY")
  
  Overall_inflammatory_score = c(-0.278, 0.106, -0.365, -0.584, -0.11, 0.097, 0.11, 0.18, -0.14, 0.298, -0.663, -0.19, -0.412, -0.453, 0.032, -0.484, -0.009,
                                 -0.246, -0.436, -0.159, -0.301, 0.021, -0.337, -0.068, -0.14, 0.373, -0.191, -0.098,0.229,-0.785, -0.401, -0.424, -0.446, -0.419, -0.313,
                                 -0.536,-0.415,-0.616,-0.467,-0.25,-0.131,-0.593,-0.131,-0.102,-0.013)
  
  Global_mean = c(13.98,5.15,1.47,3718,8.05,272.2,279.4,2056,0.01,71.4,18.8,273,4.35,59,13.35,310.1,27,25.9,1.06,10.8,35.9,
                  79.4,13.88,1.7,0.37,28.6,67,1.7,3.15,533.6,983.9,118.2,6.26,8.73,9.84,
                  1.69,95.8,1.55,17.7,11.7,18.05,1.2,10,0.33,1)
  
  SD = c(3.72,2.7,0.74,1720,6.67,40,51.2,338,0.08,19.4,4.9,70.7,2.9,63.2,3.71,139.4,6.1,11.77,1.06,7.5,18.4,13.9,3.76,0.79,1.78,
         8,25.1,0.66,3.75,754.3,518.6,43.46,2.21,1.49,2.19,
         1.53,85.9,0.07,6.79,3.82,21.14,0.2,7.07,0.99,15)
  
  DII_STD = base::data.frame(Variable, Overall_inflammatory_score, Global_mean, SD)
  
  #Score calculation for DII  
  
  COHORT2 %>%
    dplyr::inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
    dplyr::group_by(UserName, RecallNo) %>% 
    dplyr::summarize(
      DII_ALL = base::sum(ALCOHOL, VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
                          IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
                          VITC, VITD, VITE, ZN, na.rm = TRUE),
      
      DII_NOETOH =  base::sum(VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
                              IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
                              VITC, VITD, VITE, ZN, na.rm = TRUE),
      
      ALCOHOL = base::sum(ALCOHOL, na.rm = TRUE), 
      VITB12 = base::sum(VITB12, na.rm = TRUE), 
      VITB6 = base::sum(VITB6, na.rm = TRUE), 
      BCAROTENE = base::sum(BCAROTENE, na.rm = TRUE), 
      CAFFEINE = base::sum(CAFFEINE, na.rm = TRUE), 
      CARB = base::sum(CARB, na.rm = TRUE), 
      CHOLES = base::sum(CHOLES, na.rm = TRUE), 
      KCAL= base::sum(KCAL, na.rm = TRUE), 
      TOTALFAT= base::sum(TOTALFAT, na.rm = TRUE), 
      FIBER  = base::sum(FIBER, na.rm = TRUE), 
      FOLICACID  = base::sum(FOLICACID, na.rm = TRUE),
      IRON  = base::sum(IRON, na.rm = TRUE), 
      MG  = base::sum(MG, na.rm = TRUE), 
      MUFA  = base::sum(MUFA, na.rm = TRUE), 
      NIACIN  = base::sum(NIACIN, na.rm = TRUE), 
      N3FAT  = base::sum(N3FAT, na.rm = TRUE), 
      N6FAT  = base::sum(N6FAT, na.rm = TRUE), 
      PROTEIN  = base::sum(PROTEIN, na.rm = TRUE), 
      PUFA  = base::sum(PUFA, na.rm = TRUE), 
      RIBOFLAVIN  = base::sum(RIBOFLAVIN, na.rm = TRUE), 
      SATFAT  = base::sum(SATFAT, na.rm = TRUE), 
      SE  = base::sum(SE, na.rm = TRUE), 
      THIAMIN  = base::sum(THIAMIN, na.rm = TRUE), 
      VITA  = base::sum(VITA, na.rm = TRUE),
      VITC  = base::sum(VITC, na.rm = TRUE), 
      VITD  = base::sum(VITD, na.rm = TRUE), 
      VITE = base::sum(VITE, na.rm = TRUE), 
      ZN = base::sum(ZN, na.rm = TRUE)
    )
  
}
