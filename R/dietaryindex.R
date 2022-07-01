library(dplyr)

#' AHEI Calculation
#'
#' Calculate the AHEI dietary index, Alternative Healthy Eating Index, using given the serving sizes of foods and nutrients consumed per 1 day
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param VEG_SERV The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param FRT_SERV The serving size of All whole fruits and no fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g)
#' @param WGRAIN_SERV The serving size of whole grains, unit=grams/day
#' @param NUTSLEG_SERV The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts and legume or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param N3FAT_SERV The serving size of omega 3 fatty acid, unit=mg/day ( oz. = 28.35 g)
#' @param PUFA_SERV The serving size of PUFA, unit=\% of energy
#' @param SSB_FRTJ_SERV The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param TRANS_SERV The serving size of trans fat, unit=\% of energy
#' @param SODIUM_SERV The serving size of sodium, unit=mg/day
#' @param ALCOHOL_SERV The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit=drink/day (12 oz. beer; 5 oz. wine; 1.5 oz. spirits) 1 oz = 28.35 g
#' @return The AHEI index/score, AHEI
#' @examples
#' AHEI = function(SERV_DATA, VEG_SERV, FRT_SERV, WGRAIN_SERV, NUTSLEG_SERV, N3FAT_SERV, PUFA_SERV, SSB_FRTJ_SERV, REDPROC_MEAT_SERV, TRANS_SERV,SODIUM_SERV, ALCOHOL_SERV)
#' @export

#Score calculation for AHEI
AHEI = function(SERV_DATA, VEG_SERV, FRT_SERV, WGRAIN_SERV, NUTSLEG_SERV, N3FAT_SERV, PUFA_SERV,
                SSB_FRTJ_SERV, REDPROC_MEAT_SERV, TRANS_SERV, SODIUM_SERV, ALCOHOL_SERV){
  ##Create variables and functions needed for AHEI calculation
  AHEI_MIN = 0
  AHEI_MAX = 10
  AHEI_MIN_VEG_SERV = 0
  AHEI_MAX_VEG_SERV = 5
  AHEI_MIN_FRT_SERV = 0
  AHEI_MAX_FRT_SERV = 4
  AHEI_MIN_WGRAIN_F_SERV = 0
  AHEI_MAX_WGRAIN_F_SERV = 75
  AHEI_MIN_NUTSLEG_SERV = 0
  AHEI_MAX_NUTSLEG_SERV = 1
  AHEI_MIN_N3FAT_SERV = 0
  AHEI_MAX_N3FAT_SERV = 250
  AHEI_MIN_PUFA_SERV = 2
  AHEI_MAX_PUFA_SERV = 10
  AHEI_MIN_SSB_FRTJ_SERV = 1
  AHEI_MAX_SSB_FRTJ_SERV = 0
  AHEI_MIN_REDPROC_MEAT_SERV = 1.5
  AHEI_MAX_REDPROC_MEAT_SERV = 0
  AHEI_MIN_TRANS_SERV = 4
  AHEI_MAX_TRANS_SERV = 0.5

  SCORE_HEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= max_serv ~ max_score,
      actual_serv <= min_serv ~ min_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }

  SCORE_UNHEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= min_serv ~ min_score ,
      actual_serv <= max_serv ~ max_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }

  SODIUM_DECILE = quantile(SERV_DATA$SODIUM_SERV, probs=seq(0, 1, by=0.1))

  ##AHEI calculation
  SERV_DATA %>%
    mutate(
         AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
         AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
         AHEI_WGRAIN_F = SCORE_HEALTHY(WGRAIN_SERV, AHEI_MIN_WGRAIN_F_SERV, AHEI_MAX_WGRAIN_F_SERV, AHEI_MIN, AHEI_MAX),
         AHEI_NUTSLEG = SCORE_HEALTHY(NUTSLEG_SERV, AHEI_MIN_NUTSLEG_SERV, AHEI_MAX_NUTSLEG_SERV, AHEI_MIN, AHEI_MAX),
         AHEI_N3FAT = SCORE_HEALTHY(N3FAT_SERV, AHEI_MIN_N3FAT_SERV, AHEI_MAX_N3FAT_SERV, AHEI_MIN, AHEI_MAX),
         AHEI_PUFA = SCORE_HEALTHY(PUFA_SERV, AHEI_MIN_PUFA_SERV, AHEI_MAX_PUFA_SERV, AHEI_MIN, AHEI_MAX),

         AHEI_SSB_FRTJ = SCORE_UNHEALTHY(SSB_FRTJ_SERV, AHEI_MIN_SSB_FRTJ_SERV, AHEI_MAX_SSB_FRTJ_SERV, AHEI_MIN, AHEI_MAX),
         AHEI_REDPROC_MEAT = SCORE_UNHEALTHY(REDPROC_MEAT_SERV, AHEI_MIN_REDPROC_MEAT_SERV, AHEI_MAX_REDPROC_MEAT_SERV, AHEI_MIN, AHEI_MAX),
         AHEI_TRANS = SCORE_UNHEALTHY(TRANS_SERV, AHEI_MIN_TRANS_SERV, AHEI_MAX_TRANS_SERV, AHEI_MIN, AHEI_MAX),

         AHEI_SODIUM = case_when(
           SODIUM_SERV <= SODIUM_DECILE[11] & SODIUM_SERV >= SODIUM_DECILE[10] ~ 0,
           SODIUM_SERV < SODIUM_DECILE[10] & SODIUM_SERV >= SODIUM_DECILE[9] ~ 10/9,
           SODIUM_SERV < SODIUM_DECILE[9] & SODIUM_SERV >= SODIUM_DECILE[8] ~ 20/9,
           SODIUM_SERV < SODIUM_DECILE[8] & SODIUM_SERV >= SODIUM_DECILE[7] ~ 30/9,
           SODIUM_SERV < SODIUM_DECILE[7] & SODIUM_SERV >= SODIUM_DECILE[6] ~ 40/9,
           SODIUM_SERV < SODIUM_DECILE[6] & SODIUM_SERV >= SODIUM_DECILE[5] ~ 50/9,
           SODIUM_SERV < SODIUM_DECILE[5] & SODIUM_SERV >= SODIUM_DECILE[4] ~ 60/9,
           SODIUM_SERV < SODIUM_DECILE[4] & SODIUM_SERV >= SODIUM_DECILE[3] ~ 70/9,
           SODIUM_SERV < SODIUM_DECILE[3] & SODIUM_SERV >= SODIUM_DECILE[2] ~ 80/9,
           SODIUM_SERV < SODIUM_DECILE[2] & SODIUM_SERV >= SODIUM_DECILE[1] ~ 10
         ),
         AHEI_ALCOHOL_F = case_when(
           ALCOHOL_SERV >= 2.5 ~ 0,
           ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
           ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
           ALCOHOL_SERV <= 0.125 ~ 2.5,
           TRUE ~  0 + (ALCOHOL_SERV-0)*10/(0.5-0)
         ),
         AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN_F + AHEI_NUTSLEG + AHEI_N3FAT +
           AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL_F,

         AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN_F + AHEI_NUTSLEG + AHEI_N3FAT +
           AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
         ) %>%
    select(AHEI_ALL, AHEI_NOETOH)

}

#' DASH Calculation
#'
#' Calculate the DASH dietary index, Dietary Approaches to Stop Hypertension, using given the serving sizes of foods and nutrients consumed per 1 day
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param FRT_FRTJ_SERV The serving size of fruits and 100\% fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param NUTSLEG_SERV The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param WGRAIN_SERV The serving size of whole grains, unit=1oz
#' @param LOWF_DAIRY_SERV The serving size of low fat dairy, including 2\% or less fat milk + yogurt + low-fat ice cream and frozen yogurt + low-fat cheese, unit=servings/day = 1 glass milk + 1 cup yogurt + 1/2 cup ice cream/frozen yogurt + 1 slice cheese
#' @param SODIUM_SERV The serving size of sodium, unit=mg/day
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param SSB_FRTJ_SERV The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @return The DASH index/score
#' @examples
#' DASH = function(SERV_DATA, FRT_FRTJ_SERV, VEG_SERV, NUTSLEG_SERV, WGRAIN_SERV, LOWF_DAIRY_SERV, SODIUM_SERV, REDPROC_MEAT_SERV, SSB_FRTJ_SERV)
#' @export

#Score calculation for DASH
DASH = function(SERV_DATA, FRT_FRTJ_SERV, VEG_SERV, NUTSLEG_SERV, WGRAIN_SERV, LOWF_DAIRY_SERV,
                SODIUM_SERV, REDPROC_MEAT_SERV, SSB_FRTJ_SERV){
  ##Create variables and functions needed for AHEI calculation
  quintile_healthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 5,
      actual < quintile[5] & actual >= quintile[4] ~ 4,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 2,
      actual < quintile[2] & actual >= quintile[1] ~ 1
    )
  }

  quintile_unhealthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 1,
      actual < quintile[5] & actual >= quintile[4] ~ 2,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 4,
      actual < quintile[2] & actual >= quintile[1] ~ 5
    )
  }

  ##DASH calculation
  SERV_DATA %>%
    mutate(
      DASH_FRT = quintile_healthy(FRT_FRTJ_SERV),
      DASH_VEG = quintile_healthy(VEG_SERV),
      DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV),
      DASH_WGRAIN = quintile_healthy(WGRAIN_SERV),
      DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV),
      DASH_SODIUM = quintile_unhealthy(SODIUM_SERV),
      DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV),
      DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV),
      DASH_ALL = DASH_FRT+DASH_VEG+DASH_NUTSLEG+DASH_WGRAIN+DASH_LOWF_DAIRY+
        DASH_SODIUM+DASH_REDPROC_MEAT+DASH_SSB_FRTJ
    )%>%
    select(DASH_ALL)
}

#' DASHI Calculation
#'
#' Calculate the DASHI dietary index (serving size-based), Dietary Approaches to Stop Hypertension, using given the serving sizes of foods and nutrients consumed per 1 day
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param VEG_SERV The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param FRT_SERV The serving size of All whole fruits + 100\% juice,  unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param NUTSLEG_SERV The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param LOWFATDAIRY_SERV The serving size of low fat dairy, including 2\% or less fat milk + yogurt + low-fat ice cream and frozen yogurt + low-fat cheese, unit=servings/day = 1 glass milk + 1 cup yogurt + 1/2 cup ice cream/frozen yogurt + 1 slice cheese
#' @param WGRAIN_SERV The serving size of whole grains, unit=1oz
#' @param ALLMEAT_SERV The serving size of all meat consumption, including meat, fish, and poultry, unit=servings/day = 1oz
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param FATOIL_SERV The serving size of discretionary fats and oils, including added Plant oil + Animal fat, unit=servings/day = 1tbsp = 14 g
#' @param SODIUM_SERV The serving size of added sugar, unit=\% of total energy, 1 tsp = 4g, 1g = 4kcal
#' @param SODIUM_SERV The serving size of sodium, unit=mg/day
#' @return The DASHI index/score
#' @examples
#' DASHI = function(SERV_DATA, VEG_SERV, FRT_SERV, NUTSLEG_SERV, LOWFATDAIRY_SERV, WGRAIN_SERV, ALLMEAT_SERV, REDPROC_MEAT_SERV, FATOIL_SERV, ADDEDSUGAR_SERV, SODIUM_SERV)
#' @export

#Score calculation for DASHI
DASHI = function(SERV_DATA, VEG_SERV, FRT_SERV, NUTSLEG_SERV, LOWFATDAIRY_SERV, WGRAIN_SERV,
                 ALLMEAT_SERV, REDPROC_MEAT_SERV, FATOIL_SERV, ADDEDSUGAR_SERV, SODIUM_SERV){
  ##Create variables and functions needed for DASHI calculation
  DASHI_MIN = 0
  DASHI_MAX = 5

  DASHI_MIN_VEG_SERV = 0
  DASHI_MAX_VEG_SERV = 4
  DASHI_MIN_FRT_SERV = 0
  DASHI_MAX_FRT_SERV = 4
  DASHI_MIN_NUTSLEG_SERV = 0
  DASHI_MAX_NUTSLEG_SERV = 4/7
  DASHI_MIN_LOWFATDAIRY_SERV = 0
  DASHI_MAX_LOWFATDAIRY_SERV = 2
  DASHI_MIN_WGRAIN_SERV = 0
  DASHI_MAX_WGRAIN_SERV = 3

  DASHI_MIN_ALLMEAT_SERV = 6
  DASHI_MAX_ALLMEAT_SERV= 2
  DASHI_MIN_REDPROC_MEAT_SERV = 1.5
  DASHI_MAX_REDPROC_MEAT_SERV = 0.5
  DASHI_MIN_FATOIL_SERV = 3
  DASHI_MAX_FATOIL_SERV = 2
  DASHI_MIN_ADDEDSUGAR_SERV = 10
  DASHI_MAX_ADDEDSUGAR_SERV = 0
  DASHI_MIN_SODIUM_SERV = 2300
  DASHI_MAX_SODIUM_SERV = 0

  DASHI_HEALTHY = function(actual, min, max){
    case_when(
      actual >= max ~ DASHI_MAX,
      actual <= min ~ DASHI_MIN,
      TRUE ~ DASHI_MIN+(actual-min)*DASHI_MAX/(max-min)
    )
  }

  DASHI_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ DASHI_MIN ,
      actual <= max ~ DASHI_MAX,
      TRUE ~ DASHI_MIN+(actual-min)*DASHI_MAX/(max-min)
    )
  }

  ##DASHI calculation
  SERV_DATA %>%
    mutate(
      DASHI_VEG = DASHI_HEALTHY(VEG_SERV, DASHI_MIN_VEG_SERV, DASHI_MAX_VEG_SERV),
      DASHI_FRT = DASHI_HEALTHY(FRT_SERV, DASHI_MIN_FRT_SERV, DASHI_MAX_FRT_SERV),
      DASHI_NUTSLEG = DASHI_HEALTHY(NUTSLEG_SERV, DASHI_MIN_NUTSLEG_SERV, DASHI_MAX_NUTSLEG_SERV),
      DASHI_LOWFATDAIRY = DASHI_HEALTHY(LOWFATDAIRY_SERV, DASHI_MIN_LOWFATDAIRY_SERV, DASHI_MAX_LOWFATDAIRY_SERV),
      DASHI_WGRAIN = DASHI_HEALTHY(WGRAIN_SERV, DASHI_MIN_WGRAIN_SERV, DASHI_MAX_WGRAIN_SERV),

      DASHI_ALLMEAT = DASHI_UNHEALTHY(ALLMEAT_SERV, DASHI_MIN_ALLMEAT_SERV, DASHI_MAX_ALLMEAT_SERV),
      DASHI_REDPROC_MEAT = DASHI_UNHEALTHY(REDPROC_MEAT_SERV, DASHI_MIN_REDPROC_MEAT_SERV, DASHI_MAX_REDPROC_MEAT_SERV),
      DASHI_FATOIL = DASHI_UNHEALTHY(FATOIL_SERV, DASHI_MIN_FATOIL_SERV, DASHI_MAX_FATOIL_SERV),
      DASHI_ADDEDSUGAR = DASHI_UNHEALTHY(ADDEDSUGAR_SERV, DASHI_MIN_ADDEDSUGAR_SERV, DASHI_MAX_ADDEDSUGAR_SERV),
      DASHI_SODIUM = DASHI_UNHEALTHY(SODIUM_SERV, DASHI_MIN_SODIUM_SERV, DASHI_MAX_SODIUM_SERV),
      DASHI_ALL= DASHI_VEG + DASHI_FRT + DASHI_NUTSLEG + DASHI_LOWFATDAIRY +
        DASHI_WGRAIN + DASHI_ALLMEAT + DASHI_REDPROC_MEAT + DASHI_FATOIL + DASHI_ADDEDSUGAR + DASHI_SODIUM
    )%>%
    select(DASHI_ALL)
}

#' MED Calculation
#'
#' Calculate the MED dietary index, Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param FRT_FRTJ_SERV The serving size of All fruits and 100\% fruit juices, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV The serving size of All vegetables except potatoes and legumes, unit=0.5 c of vege; 1 cup of green leafy
#' @param WGRAIN_SERV The serving size of whole grains, including Whole-grain ready-to-eat cereals, cooked cereals, crackers, dark breads, brown rice, other grains, wheat germ, bran, popcorn, unit=1oz
#' @param LEGUMES_SERV The serving size of legumes, including Tofu, string beans, peas, beans, unit=*oz, 1 cup legume = 4 oz
#' @param NUTS_SERV The serving size of nuts, including Nuts, peanut butter, unit=1oz
#' @param FISH_SERV The serving size of all fish, including Fish and shrimp, breaded fish, unit=4oz
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=serving (4 oz. unprocessed meat; 1.5 oz. processed meat)
#' @param MONSATFAT_SERV The serving size of the ratio of monounsaturated fat to saturated fat, unit=ratio
#' @param ALCOHOL_SERV The serving size of alcohol, unit=13g
#' @return The MED index/score
#' @examples
#' MED = function(SERV_DATA, FRT_FRTJ_SERV, VEG_SERV, WGRAIN_SERV, LEGUMES_SERV, NUTS_SERV,FISH_SERV, REDPROC_MEAT_SERV, MONSATFAT_SERV, ALCOHOL_SERV)
#' @export

#Score calculation for MED
MED = function(SERV_DATA, FRT_FRTJ_SERV, VEG_SERV, WGRAIN_SERV, LEGUMES_SERV, NUTS_SERV,
               FISH_SERV, REDPROC_MEAT_SERV, MONSATFAT_SERV, ALCOHOL_SERV){
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

  SERV_DATA %>%
    mutate(
      MED_FRT = median_healthy(FRT_FRTJ_SERV),
      MED_VEG = median_healthy(VEG_SERV),
      MED_WGRAIN = median_healthy(WGRAIN_SERV),
      MED_LEGUMES = median_healthy(LEGUMES_SERV),
      MED_NUTS = median_healthy(NUTS_SERV),
      MED_FISH = median_healthy(FISH_SERV),
      MED_REDPROC_MEAT = median_unhealthy(REDPROC_MEAT_SERV),
      MED_MONSATFAT = median_healthy(MONSATFAT_SERV),
      MED_ALCOHOL = ifelse(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10, 1, 0),

      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    select(MED_ALL, MED_NOETOH)
}

#' MEDI Calculation
#'
#' Calculate the MEDI dietary index (serving size-based), Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param FRT_FRTJ_SERV The serving size of All fruits and 100\% fruit juices, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV The serving size of All raw and cooked vegetables, unit=0.5 c of vege; 1 cup of green leafy
#' @param LEGUMES_SERV The serving size of legumes, including Dried beans, lentils, peas, soups (split pea), tofu, soymilk, unit=1oz (28.35 g) of nuts and legume or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param WGRAIN_SERV The serving size of whole grains, including Whole wheat bread and flour, whole wheat pasta, brown rice, rusks,  whole grain breakfast cereals, couscous, semolina, unit=1 oz
#' @param FISH_SERV The serving size of all fish, including Fresh-water and sea-water fish; preserved fish such as salted fish, canned fish; shellfish (squid, prawns, mollusks), unit=serving (4 oz)
#' @param DAIRY_SERV The serving size of all dairy, including Milk, yogurt, cheese, custard, milk puddings, other milk products, unit=1 serving = 1 cup milk, 1 cup yogurt, 1 ½ ounces hard cheese (cheddar, mozzarella, Swiss, Parmesan), 1/3 cup shredded cheese, 2 ounces processed cheese (American), ½ cup ricotta cheese, 2 cups cottage cheese, 1 cup pudding made with milk, 1 cup frozen yogurt, 1 ½ cups ice cream
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=serving (4 oz. unprocessed meat; 1.5 oz. processed meat)
#' @param NUTS_SERV The serving size of nuts, including Peanuts, almonds, sunflower seeds, cashews, walnuts, unit=1oz
#' @param MONSATFAT_SERV The serving size of the ratio of monounsaturated fat to saturated fat, unit=ratio
#' @param ALCOHOL_SERV The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit=13g
#' @return The MEDI index/score
#' @examples
#' MED = function(SERV_DATA, FRT_FRTJ_SERV, VEG_SERV, WGRAIN_SERV, LEGUMES_SERV, NUTS_SERV,FISH_SERV, REDPROC_MEAT_SERV, MONSATFAT_SERV, ALCOHOL_SERV)
#' @export

#Score calculation for MEDI
MEDI = function(SERV_DATA, FRT_FRTJ_SERV, VEG_SERV, LEGUMES_SERV, WGRAIN_SERV, FISH_SERV, DAIRY_SERV, REDPROC_MEAT_SERV,
                NUTS_SERV, MONSATFAT_SERV, ALCOHOL_SERV){
  SERV_DATA %>%
    mutate(
      MEDI_FRT = ifelse(FRT_FRTJ_SERV >=3, 1, 0),
      MEDI_VEG = ifelse(VEG_SERV >= 3, 1, 0),
      MEDI_LEGUMES = ifelse(LEGUMES_SERV*7 >= 1.5, 1, 0),
      MEDI_WGRAIN = ifelse(WGRAIN_SERV >= 3, 1, 0),
      MEDI_FISH = ifelse(FISH_SERV*7 >= 2, 1, 0),
      MEDI_DAIRY = ifelse(DAIRY_SERV >= 2, 1, 0),
      MEDI_REDPROC_MEAT = ifelse(REDPROC_MEAT_SERV*7 < 4.5, 1, 0),
      MEDI_NUTS = ifelse(NUTS_SERV*7 >= 2, 1, 0),
      MEDI_MONSATFAT = ifelse(MONSATFAT_SERV >= 1.6, 1, 0),
      MEDI_ALCOHOL = ifelse(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10, 1, 0),

      MEDI_ALL = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT+
        MEDI_NUTS+MEDI_MONSATFAT+MEDI_ALCOHOL,
      MEDI_NOETOH = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT
      +MEDI_NUTS+MEDI_MONSATFAT
    )%>%
    select(MEDI_ALL, MEDI_NOETOH)
}

#' AHEIP Calculation
#'
#' Calculate the AHEIP dietary index (serving size-based), Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param VEG_SERV The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param FRT_SERV The serving size of All whole fruits and no fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g)
#' @param WHITERED_RT_SERV The serving size of the ratio of white and red meats, White meat = poultry + all fish, red meat = pork + beef + lamb + all organ meats + processed meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param FIBER_SERV The serving size of fibers, unit=grams/day
#' @param TRANS_SERV The serving size of trans fat, unit='\% of energy'
#' @param POLYSAT_RT The serving size of polyunsaturated/saturated fats, unit=ratio
#' @param CALCIUM_SERV The serving size of calcium, unit=mg/day
#' @param FOLATE_SERV The serving size of folate, unit=mcg/day
#' @param IRON_SERV The serving size of iron, unit=mg/day
#' @return The AHEIP index/score
#' @examples
#' AHEIP = function(SERV_DATA, VEG_SERV, FRT_SERV, WHITERED_RT_SERV, FIBER_SERV, TRANS_SERV, POLYSAT_RT, CALCIUM_SERV, FOLATE_SERV, IRON_SERV)
#' @export


#Score calculation for AHEIP
AHEIP = function(SERV_DATA, VEG_SERV, FRT_SERV, WHITERED_RT_SERV, FIBER_SERV, TRANS_SERV, POLYSAT_RT,
                 CALCIUM_SERV, FOLATE_SERV, IRON_SERV) {

  ##Create variables and functions needed for AHEIP calculation
  AHEIP_MIN = 0
  AHEIP_MAX = 10

  AHEIP_MIN_VEG_SERV = 0
  AHEIP_MAX_VEG_SERV = 5
  AHEIP_MIN_FRT_SERV = 0
  AHEIP_MAX_FRT_SERV = 4
  AHEIP_MIN_WHITERED_SERV = 0
  AHEIP_MAX_WHITERED_SERV = 4
  AHEIP_MIN_FIBER_SERV = 0
  AHEIP_MAX_FIBER_SERV = 25

  AHEIP_MIN_TRANS_SERV = 4
  AHEIP_MAX_TRANS_SERV = 0.5
  AHEIP_MIN_POLYSAT_SERV = 0.1
  AHEIP_MAX_POLYSAT_SERV = 1
  AHEIP_MIN_CALCIUM_SERV = 0
  AHEIP_MAX_CALCIUM_SERV = 1200
  AHEIP_MIN_FOLATE_SERV = 0
  AHEIP_MAX_FOLATE_SERV = 600
  AHEIP_MIN_IRON_SERV = 0
  AHEIP_MAX_IRON_SERV = 27

  AHEIP_HEALTHY = function(actual, min, max){
    case_when(
      actual >= max ~ AHEIP_MAX,
      actual <= min ~ AHEIP_MIN,
      TRUE ~ AHEIP_MIN+(actual-min)*AHEIP_MAX/(max-min)
    )
  }

  AHEIP_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ AHEIP_MIN ,
      actual <= max ~ AHEIP_MAX,
      TRUE ~ AHEIP_MIN+(actual-min)*AHEIP_MAX/(max-min)
    )
  }

  SERV_DATA %>%
    mutate(
      AHEIP_VEG = AHEIP_HEALTHY(VEG_SERV, AHEIP_MIN_VEG_SERV, AHEIP_MAX_VEG_SERV),
      AHEIP_FRT = AHEIP_HEALTHY(FRT_SERV, AHEIP_MIN_FRT_SERV, AHEIP_MAX_FRT_SERV),
      AHEIP_WHITEREAD = AHEIP_HEALTHY(WHITERED_RT_SERV, AHEIP_MIN_WHITERED_SERV, AHEIP_MAX_WHITERED_SERV),
      AHEIP_FIBER = AHEIP_HEALTHY(FIBER_SERV, AHEIP_MIN_FIBER_SERV, AHEIP_MAX_FIBER_SERV),
      AHEIP_TRANS = AHEIP_UNHEALTHY(TRANS_SERV, AHEIP_MIN_TRANS_SERV, AHEIP_MAX_TRANS_SERV),
      AHEIP_POLYSAT = AHEIP_HEALTHY(POLYSAT_RT, AHEIP_MIN_POLYSAT_SERV, AHEIP_MAX_POLYSAT_SERV),
      AHEIP_CALCIUM = AHEIP_HEALTHY(CALCIUM_SERV, AHEIP_MIN_CALCIUM_SERV, AHEIP_MAX_CALCIUM_SERV),
      AHEIP_FOLATE= AHEIP_HEALTHY(FOLATE_SERV, AHEIP_MIN_FOLATE_SERV, AHEIP_MAX_FOLATE_SERV),
      AHEIP_IRON = AHEIP_HEALTHY(IRON_SERV, AHEIP_MIN_IRON_SERV, AHEIP_MAX_IRON_SERV),
      AHEIP_ALL = AHEIP_VEG + AHEIP_FRT + AHEIP_WHITEREAD + AHEIP_FIBER + AHEIP_TRANS +
        AHEIP_POLYSAT + AHEIP_CALCIUM + AHEIP_FOLATE + AHEIP_IRON
    )%>%
    select(AHEIP_ALL)
}

#' HEI2015 Calculation
#'
#' Calculate the HEI2015 dietary index, Healthy eating index 2015, using given the serving sizes of foods and nutrients consumed per 1 day
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param TOTALFRT_SERV The serving size of total fruits including fruit juice, unit= cup eq.
#' @param FRT_SERV The serving size of Citrus, Melons, Berries + Other Intact Fruits, unit= cup eq.
#' @param VEG_SERV The serving size of vegetables Total Vegetables + Legumes (Beans and Peas) in cup equivalents, unit= cup eq.
#' @param GREENNBEAN_SERV The serving size of Dark Green Vegetables + Legumes (Beans and Peas) in cup equivalents, unit= cup eq.
#' @param TOTALPRO_SERV The serving size of Total Meat, Poultry, and Seafood (including organ meats and cured meats) + Eggs + Nuts and Seeds + Soy + Legumes (Beans and Peas) in oz equivalents, unit=oz. eq., 1 cup legume = 4 oz
#' @param SEAPLANTPRO_SERV The serving size of Seafood (high in n-3) + Seafood (low in n-3) + Soy + Nuts and Seeds + Legumes (Beans and Peas) in oz equivalents, unit=oz. eq., 1 cup legume = 4 oz
#' @param WHOLEGRAIN_SERV The serving size of whole grains, unit=oz. eq.
#' @param DAIRY_SERV The serving size of all dairy, unit=cup eq.
#' @param FATTYACID_SERV The serving size of (Total Monounsaturated Fatty Acids + Total Polyunsaturated Fatty Acids)/Total Saturated Fatty Acids, unit=g
#' @param REFINEDGRAIN_SERV The serving size of refined grains, unit=oz. eq.
#' @param SODIUM_SERV The serving size of sodium, unit=g
#' @param ADDEDSUGAR_SERV The serving size of added sugar, unit=\% of total energy, 1 tsp = 4g, 1g = 4kcal
#' @param SATFAT_SERV The serving size of Total Saturated Fatty Acids, unit=\% of energy, 1g = 9 kcal
#' @return The HEI2015 index/score
#' @examples
#' HEI2015 = function(SERV_DATA, TOTALFRT_SERV, FRT_SERV, VEG_SERV, GREEN_N_BEAN_SERV, TOTALPRO_SERV,  SEAPLANTPRO_SERV, WHOLEGRAIN_SERV, DAIRY_SERV, FATTYACID_SERV, REFINEDGRAIN_SERV,  SODIUM_SERV, ADDEDSUGAR_SERV, SATFAT_SERV)
#' @export

#Score calculation for HEI2015
HEI2015 = function(SERV_DATA, TOTALFRT_SERV, FRT_SERV, VEG_SERV, GREENNBEAN_SERV, TOTALPRO_SERV,
                   SEAPLANTPRO_SERV, WHOLEGRAIN_SERV, DAIRY_SERV, FATTYACID_SERV, REFINEDGRAIN_SERV,
                   SODIUM_SERV, ADDEDSUGAR_SERV, SATFAT_SERV){

  ##Create variables needed for HEI2015 calculation
  HEI2015_MIN = 0
  HEI2015_MAX1 = 5
  HEI2015_MAX2 = 10

  HEI2015_MIN_TOTALFRT_SERV = 0
  HEI2015_MAX_TOTALFRT_SERV = 0.8
  HEI2015_MIN_FRT_SERV = 0
  HEI2015_MAX_FRT_SERV = 0.4
  HEI2015_MIN_VEG_SERV = 0
  HEI2015_MAX_VEG_SERV = 1.1
  HEI2015_MIN_GREEN_N_BEAN_SERV = 0
  HEI2015_MAX_GREEN_N_BEAN_SERV = 0.2
  HEI2015_MIN_TOTALPRO_SERV = 0
  HEI2015_MAX_TOTALPRO_SERV = 2.5
  HEI2015_MIN_SEAPLANTPRO_SERV = 0
  HEI2015_MAX_SEAPLANTPRO_SERV = 0.8
  HEI2015_MIN_WHOLEGRAIN_SERV = 0
  HEI2015_MAX_WHOLEGRAIN_SERV = 1.5
  HEI2015_MIN_DAIRY_SERV = 0
  HEI2015_MAX_DAIRY_SERV = 1.3
  HEI2015_MIN_FATTYACID_SERV = 1.2
  HEI2015_MAX_FATTYACID_SERV = 2.5

  HEI2015_MIN_REFINEDGRAIN_SERV = 4.3
  HEI2015_MAX_REFINEDGRAIN_SERV = 1.8
  HEI2015_MIN_SODIUM_SERV = 2.0
  HEI2015_MAX_SODIUM_SERV = 1.1
  HEI2015_MIN_ADDEDSUGAR_SERV = 26
  HEI2015_MAX_ADDEDSUGAR_SERV = 6.5
  HEI2015_MIN_SATFAT_SERV = 16
  HEI2015_MAX_SATFAT_SERV = 8

  HEI2015_HEALTHY1 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX1,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX1/(max-min)
    )
  }

  HEI2015_HEALTHY2 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX2,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }

  HEI2015_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ HEI2015_MIN,
      actual <= max ~ HEI2015_MAX2,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }

  SERV_DATA %>%
    mutate(
      HEI2015_TOTALFRT = HEI2015_HEALTHY1(TOTALFRT_SERV, HEI2015_MIN_TOTALFRT_SERV, HEI2015_MAX_TOTALFRT_SERV),
      HEI2015_FRT = HEI2015_HEALTHY1(FRT_SERV, HEI2015_MIN_FRT_SERV, HEI2015_MAX_FRT_SERV),
      HEI2015_VEG = HEI2015_HEALTHY1(VEG_SERV, HEI2015_MIN_VEG_SERV, HEI2015_MAX_VEG_SERV),
      HEI2015_GREEN_N_BEAN = HEI2015_HEALTHY1(GREEN_N_BEAN_SERV, HEI2015_MIN_GREEN_N_BEAN_SERV, HEI2015_MAX_GREEN_N_BEAN_SERV),
      HEI2015_TOTALPRO = HEI2015_HEALTHY1(TOTALPRO_SERV, HEI2015_MIN_TOTALPRO_SERV, HEI2015_MAX_TOTALPRO_SERV),
      HEI2015_SEAPLANTPRO = HEI2015_HEALTHY1(SEAPLANTPRO_SERV, HEI2015_MIN_SEAPLANTPRO_SERV, HEI2015_MAX_SEAPLANTPRO_SERV),
      HEI2015_WHOLEGRAIN = HEI2015_HEALTHY2(WHOLEGRAIN_SERV, HEI2015_MIN_WHOLEGRAIN_SERV, HEI2015_MAX_WHOLEGRAIN_SERV),
      HEI2015_DAIRY = HEI2015_HEALTHY2(DAIRY_SERV, HEI2015_MIN_DAIRY_SERV, HEI2015_MAX_DAIRY_SERV),
      HEI2015_FATTYACID = HEI2015_HEALTHY2(FATTYACID_SERV, HEI2015_MIN_FATTYACID_SERV, HEI2015_MAX_FATTYACID_SERV),

      HEI2015_REFINEDGRAIN = HEI2015_UNHEALTHY(REFINEDGRAIN_SERV, HEI2015_MIN_REFINEDGRAIN_SERV, HEI2015_MAX_REFINEDGRAIN_SERV),
      HEI2015_SODIUM = HEI2015_UNHEALTHY(SODIUM_SERV, HEI2015_MIN_SODIUM_SERV, HEI2015_MAX_SODIUM_SERV),
      HEI2015_ADDEDSUGAR = HEI2015_UNHEALTHY(ADDEDSUGAR_SERV, HEI2015_MIN_ADDEDSUGAR_SERV, HEI2015_MAX_ADDEDSUGAR_SERV),
      HEI2015_SATFAT = HEI2015_UNHEALTHY(SATFAT_SERV, HEI2015_MIN_SATFAT_SERV, HEI2015_MAX_SATFAT_SERV),

      HEI2015_ALL= HEI2015_TOTALFRT + HEI2015_FRT + HEI2015_VEG + HEI2015_GREEN_N_BEAN +
        HEI2015_TOTALPRO + HEI2015_SEAPLANTPRO + HEI2015_WHOLEGRAIN + HEI2015_DAIRY +
        HEI2015_FATTYACID + HEI2015_REFINEDGRAIN + HEI2015_SODIUM + HEI2015_ADDEDSUGAR +
        HEI2015_SATFAT
    )%>%
    select(HEI2015_ALL)
}
