#' DII_BLOCK Calculation
#'
#' Calculate the DII dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The DII index/score and its components
#' @examples
#' DII_BLOCK(RAW_DATA)
#' @export

DII_BLOCK = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  STD_FOOD_FREQ = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  STD_FREQ_SERV = c(0, 1/90, 1/30, 2.5/30, 1/7, 2/7, 3.5/7, 5.5/7, 1)
  STD_FOOD_PORT = c(1, 2, 3, 4)
  STD_PORT_SERV = c(0.25, 0.5, 1, 2)
  STD_FOOD_FREQ_DF = data.frame(STD_FOOD_FREQ, STD_FREQ_SERV, stringsAsFactors=FALSE)
  STD_FOOD_PORT_DF= data.frame(STD_FOOD_PORT, STD_PORT_SERV, stringsAsFactors=FALSE)
  
  #Functions to match actual food frequency and portion to the standards
  foodfreq = function(actual, reference=STD_FOOD_FREQ_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  foodport = function(actual, reference=STD_FOOD_PORT_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  #Serving size calculation for DII
  COHORT = RAW_DATA %>%
    dplyr::mutate(
      ALCOHOL = DT_ALCO,
      VITB12 = DT_VB12,
      VITB6 = DT_VITB6,
      BCAROTENE = DT_BCARO,
      CAFFEINE = DT_CAFFN/1000,
      CARB = DT_CARB,
      CHOLES = DT_CHOL,
      KCAL = DT_KCAL,
      TOTALFAT = DT_TFAT,
      FIBER = DT_FIBE,
      FOLICACID = DT_FOLAC,
      IRON = DT_IRON,
      MG = DT_MAGN,
      MUFA = DT_MFAT,
      NIACIN = DT_NIAC,
      N3FAT = DT_TOTN3,
      N6FAT = DT_TOTN6,
      PROTEIN = DT_PROT,
      PUFA = DT_PFAT,
      RIBOFLAVIN = DT_RIBO,
      SATFAT = DT_SFAT,
      SE = DT_SEL,
      THIAMIN = DT_THIA,
      TRANSFAT = DT_TRFAT,
      VITA = DT_VARAE,
      VITC = DT_VITC,
      VITD = DT_VITD*0.025,
      VITE = DT_ATOC,
      ZN = DT_ZINC,
      TEA = foodfreq(ICEDTEAFREQ)*ICEDTEAQUAN*240 + foodfreq(HOTTEAFREQ)*HOTTEAQUAN*240,
      ISOFLAVONES = DT_ISOFLV
    ) 
  
  COHORT1 = COHORT %>%
    dplyr::select(RESPONDENTID, ALCOHOL, VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                  IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,TRANSFAT,
                  VITA,VITC,VITD,VITE,ZN,TEA,ISOFLAVONES)
  
  COHORT1 = COHORT1 %>%
    tidyr::pivot_longer(-RESPONDENTID, names_to="Variable", values_to="Value")
  
  
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
  
  DII_STD = data.frame(Variable, Overall_inflammatory_score, Global_mean, SD)
  
  #Score calculation for DII  
  
  COHORT1 %>%
    inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
    dplyr::group_by(RESPONDENTID) %>%
    dplyr::summarize(
      DII_ALL = sum(ALCOHOL, VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                    IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,TRANSFAT,
                    VITA,VITC,VITD,VITE,ZN,TEA,ISOFLAVONES, na.rm = TRUE),
      
      DII_NOETOH =  sum(VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                        IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,TRANSFAT,
                        VITA,VITC,VITD,VITE,ZN,TEA,ISOFLAVONES, na.rm = TRUE),
      
      ALCOHOL = sum(ALCOHOL, na.rm = TRUE), 
      VITB12 = sum(VITB12, na.rm = TRUE), 
      VITB6 = sum(VITB6, na.rm = TRUE), 
      BCAROTENE = sum(BCAROTENE, na.rm = TRUE), 
      CAFFEINE = sum(CAFFEINE, na.rm = TRUE), 
      CARB = sum(CARB, na.rm = TRUE), 
      CHOLES = sum(CHOLES, na.rm = TRUE), 
      KCAL= sum(KCAL, na.rm = TRUE), 
      TOTALFAT= sum(TOTALFAT, na.rm = TRUE), 
      FIBER  = sum(FIBER, na.rm = TRUE), 
      FOLICACID  = sum(FOLICACID, na.rm = TRUE),
      IRON  = sum(IRON, na.rm = TRUE), 
      MG  = sum(MG, na.rm = TRUE), 
      MUFA  = sum(MUFA, na.rm = TRUE), 
      NIACIN  = sum(NIACIN, na.rm = TRUE), 
      N3FAT  = sum(N3FAT, na.rm = TRUE), 
      N6FAT  = sum(N6FAT, na.rm = TRUE), 
      PROTEIN  = sum(PROTEIN, na.rm = TRUE), 
      PUFA  = sum(PUFA, na.rm = TRUE), 
      RIBOFLAVIN  = sum(RIBOFLAVIN, na.rm = TRUE), 
      SATFAT  = sum(SATFAT, na.rm = TRUE), 
      SE  = sum(SE, na.rm = TRUE), 
      THIAMIN  = sum(THIAMIN, na.rm = TRUE), 
      TRANSFAT = sum(TRANSFAT, na.rm = TRUE),
      VITA  = sum(VITA, na.rm = TRUE),
      VITC  = sum(VITC), na.rm = TRUE, 
      VITD  = sum(VITD, na.rm = TRUE), 
      VITE = sum(VITE, na.rm = TRUE), 
      ZN = sum(ZN, na.rm = TRUE),
      TEA = sum(TEA, na.rm = TRUE),
      ISOFLAVONES = sum(ISOFLAVONES, na.rm = TRUE)
    )
  
}