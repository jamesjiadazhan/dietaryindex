#' DII_NHANES_FPED
#'
#' Calculate the DII for the NHANES_FPED data (after 2005) within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fpre_dr1tot_1718.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @return The DII and its component scores
#' @examples
#' FPED_PATH = "/Users/james/Desktop/data/fpre_dr1tot_1718.sas7bdat"
#' NUTRIENT_PATH = "/Users/james/Desktop/data/DR1TOT_J.XPT"
#' DEMO_PATH = "/Users/james/Desktop/data/DEMO_J.XPT"
#' DII_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)
#' @export

DII_NHANES_FPED = function(FPED_PATH, NUTRIENT_PATH, DEMO_PATH){
  
  if (is.character(FPED_PATH) == TRUE){
    FPED = read_sas(FPED_PATH)
  } else {
    FPED = FPED_PATH
  }
  
  if (is.character(NUTRIENT_PATH) == TRUE){
    NUTRIENT = read_xpt(NUTRIENT_PATH)
  } else {
    NUTRIENT = NUTRIENT_PATH
  }
  
  if (is.character(DEMO_PATH) == TRUE){
    DEMO = read_xpt(DEMO_PATH)
  } else {
    DEMO = DEMO_PATH
  }
  
  if ("DR1ILINE" %in% colnames(FPED) | "DR1ILINE" %in% colnames(NUTRIENT)){
    stop("Please use the population-level data. The file name should be like: Totals.csv")
  }
  
  NUTRIENT = NUTRIENT %>%
    filter(DR1DRSTZ == 1) %>%
    arrange(SEQN)
  
  DEMO = DEMO %>%
    filter(RIDAGEYR >= 2) %>%
    dplyr::select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
    arrange(SEQN)
  
  FPED = FPED %>%
    arrange(SEQN)
  
  COHORT = NUTRIENT %>%
    inner_join(DEMO, by = c("SEQN" = "SEQN")) %>%
    left_join(FPED, by = c("SEQN" = "SEQN"))
  
  #Serving size calculation for DII
  COHORT = COHORT %>%
    filter(DR1TKCAL > 0) %>%
    dplyr::mutate(
      ALCOHOL = DR1TALCO,
      VITB12 = DR1TVB12,
      VITB6 = DR1TVB6,
      BCAROTENE = DR1TBCAR,
      CAFFEINE = DR1TCAFF/1000,
      CARB = DR1TCARB,
      CHOLES = DR1TCHOL,
      KCAL = DR1TKCAL,
      TOTALFAT = DR1TTFAT,
      FIBER = DR1TFIBE,
      FOLICACID = DR1TFA,
      IRON = DR1TIRON,
      MG = DR1TMAGN,
      MUFA = DR1TMFAT,
      NIACIN = DR1TNIAC,
      N3FAT = DR1TP183 + DR1TP184 + DR1TP205 + DR1TP225 + DR1TP226,
      N6FAT = DR1TP183 + DR1TP204,
      PROTEIN = DR1TPROT,
      PUFA = DR1TPFAT,
      RIBOFLAVIN = DR1TVB2,
      SATFAT = DR1TSFAT,
      SE = DR1TSELE,
      THIAMIN = DR1TVB1,
      VITA = DR1TVARA,
      VITC = DR1TVC,
      VITD = tryCatch(DR1TVD*0.025, error = function(e) return(NULL)),
      VITE = DR1TATOC,
      ZN = DR1TZINC) %>%
    dplyr::select(SEQN, ALCOHOL, VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                  IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,
                  VITA,VITC,VITD,VITE,ZN)
  
  COHORT = COHORT %>%
    tidyr::pivot_longer(-SEQN, names_to="Variable", values_to="Value")
  
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
  
  COHORT %>%
    dplyr::inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
    dplyr::group_by(SEQN) %>%
    dplyr::summarize(
      DII_ALL = base::sum(ALCOHOL, VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                          IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,
                          VITA,VITC,VITD,VITE,ZN, na.rm = TRUE),
      
      DII_NOETOH =  base::sum(VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                              IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,
                              VITA,VITC,VITD,VITE,ZN, na.rm = TRUE),
      
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
