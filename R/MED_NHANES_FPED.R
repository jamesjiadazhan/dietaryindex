#' MED_NHANES_FPED
#'
#' Calculate the MED for the NHANES_FPED data (after 2005) within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fpre_dr1tot_1718.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @return The MED and its component scores
#' @examples
#' FPED_PATH = "/Users/james/Desktop/data/fpre_dr1tot_1718.sas7bdat"
#' NUTRIENT_PATH = "/Users/james/Desktop/data/DR1TOT_J.XPT"
#' DEMO_PATH = "/Users/james/Desktop/data/DEMO_J.XPT"
#' MED_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)
#' @export

MED_NHANES_FPED = function(FPED_PATH, NUTRIENT_PATH, DEMO_PATH){
  
  
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
  
  
  #Match participant response food frequency to the standard food frequency response code
  
  COHORT = COHORT %>%
    filter(DR1TKCAL > 0) %>%
    dplyr::mutate(
      FRT_FRTJ_SERV = DR1T_F_TOTAL,
      VEG_SERV = DR1T_V_REDOR_TOTAL + 0.5*DR1T_V_DRKGR + DR1T_V_OTHER + DR1T_V_STARCHY_OTHER,
      WGRAIN_SERV = DR1T_G_WHOLE,
      LEGUMES_SERV = (DR1T_V_LEGUMES*4) + DR1T_PF_SOY,
      NUTS_SERV = DR1T_PF_NUTSDS,
      FISH_SERV = DR1T_PF_SEAFD_HI + DR1T_PF_SEAFD_LOW,
      REDPROC_MEAT_SERV = (DR1T_PF_CUREDMEAT /1.5) + ((DR1T_PF_MEAT+DR1T_PF_ORGAN+DR1T_PF_POULT)/4),
      MONSATFAT_SERV = case_when(
        DR1TSFAT == 0 ~ 0, 
        TRUE ~ DR1TMFAT/DR1TSFAT
      ),
      ALCOHOL_SERV = DR1TALCO
    ) 
  
  ##Create variables and functions needed for MED
  median_healthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 0,
      actual >= median_score ~ 1
    )
  }
  
  median_unhealthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 1,
      actual >= median_score ~ 0
    )
  }
  
  print("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  
  COHORT %>%
    dplyr::mutate(
      MED_FRT = median_healthy(FRT_FRTJ_SERV),
      MED_VEG = median_healthy(VEG_SERV),
      MED_WGRAIN = median_healthy(WGRAIN_SERV),
      MED_LEGUMES = median_healthy(LEGUMES_SERV),
      MED_NUTS = median_healthy(NUTS_SERV),
      MED_FISH = median_healthy(FISH_SERV),
      MED_REDPROC_MEAT = median_unhealthy(REDPROC_MEAT_SERV),
      MED_MONSATFAT = median_healthy(MONSATFAT_SERV),
      MED_ALCOHOL = case_when(
        ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10 ~ 1, 
        TRUE ~ 0),
      
      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(SEQN, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
                  MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL,
                  
                  FRT_FRTJ_SERV, VEG_SERV, WGRAIN_SERV, LEGUMES_SERV, NUTS_SERV, FISH_SERV, REDPROC_MEAT_SERV, MONSATFAT_SERV, ALCOHOL_SERV)
}