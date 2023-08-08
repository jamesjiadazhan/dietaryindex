#' HEI2020_V2 Calculation
#'
#' Calculate the HEI2020_V2 dietary index, Healthy eating index 2020 and HEI-Toddlers-2020, using given the serving sizes of foods and nutrients consumed per 1 day. This version has the Added sugar serving size as tsp (teaspoon) and the saturated fat serving size as g (gram), instead of the % of total energy for added sugar and saturated fat in the original HEI2020.
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param AGE The age of the participant in years
#' @param TOTALKCAL_HEI2020 The total calorie from all foods and drinks
#' @param TOTALFRT_SERV_HEI2020 The serving size of total fruits including fruit juice, unit= cup eq.
#' @param FRT_SERV_HEI2020 The serving size of Citrus, Melons, Berries + Other Intact Fruits, unit= cup eq.
#' @param VEG_SERV_HEI2020 The serving size of vegetables Total Vegetables + Legumes (Beans and Peas) in cup equivalents, unit= cup eq.
#' @param GREENNBEAN_SERV_HEI2020 The serving size of Dark Green Vegetables + Legumes (Beans and Peas) in cup equivalents, unit= cup eq.
#' @param TOTALPRO_SERV_HEI2020 The serving size of Total Meat, Poultry, and Seafood (including organ meats and cured meats) + Eggs + Nuts and Seeds + Soy + Legumes (Beans and Peas) in oz equivalents, unit=oz. eq., 1 cup legume = 4 oz
#' @param SEAPLANTPRO_SERV_HEI2020 The serving size of Seafood (high in n-3) + Seafood (low in n-3) + Soy + Nuts and Seeds + Legumes (Beans and Peas) in oz equivalents, unit=oz. eq., 1 cup legume = 4 oz
#' @param WHOLEGRAIN_SERV_HEI2020 The serving size of whole grains, unit=oz. eq.
#' @param DAIRY_SERV_HEI2020 The serving size of all dairy, unit=cup eq.
#' @param FATTYACID_SERV_HEI2020 The serving size of (Total Monounsaturated Fatty Acids + Total Polyunsaturated Fatty Acids)/Total Saturated Fatty Acids, unit=g
#' @param REFINEDGRAIN_SERV_HEI2020 The serving size of refined grains, unit=oz. eq.
#' @param SODIUM_SERV_HEI2020 The serving size of sodium, unit=g
#' @param ADDEDSUGAR_SERV_HEI2020 The serving size of added sugar, unit=tsp
#' @param SATFAT_SERV_HEI2020 The serving size of Total Saturated Fatty Acids, unit=g
#' @return The HEI2020_V2 index/score
#' @examples
#' data("HEI2020_VALIDATION")
#' HEI2020_V2(SERV_DATA = HEI2020_VALIDATION, RESPONDENTID = HEI2020_VALIDATION$id, AGE = HEI2020_VALIDATION$age, TOTALKCAL_HEI2020 = HEI2020_VALIDATION$kcal, TOTALFRT_SERV_HEI2020 = HEI2020_VALIDATION$total_fruit, FRT_SERV_HEI2020 = HEI2020_VALIDATION$whole_fruit, VEG_SERV_HEI2020 = HEI2020_VALIDATION$total_vegetable, GREENNBEAN_SERV_HEI2020 = HEI2020_VALIDATION$green_and_bean, TOTALPRO_SERV_HEI2020 = HEI2020_VALIDATION$total_protein, SEAPLANTPRO_SERV_HEI2020 = HEI2020_VALIDATION$seafood_plant_protein, WHOLEGRAIN_SERV_HEI2020 = HEI2020_VALIDATION$whole_grain, DAIRY_SERV_HEI2020 = HEI2020_VALIDATION$dairy, FATTYACID_SERV_HEI2020 = HEI2020_VALIDATION$fatty_acid, REFINEDGRAIN_SERV_HEI2020 = HEI2020_VALIDATION$refined_grain, SODIUM_SERV_HEI2020 = HEI2020_VALIDATION$sodium, ADDEDSUGAR_SERV_HEI2020 = HEI2020_VALIDATION$added_sugar, SATFAT_SERV_HEI2020 = HEI2020_VALIDATION$saturated_fat)
#' @export

# Score calculation for HEI2020_V2
HEI2020_V2 = function(SERV_DATA, RESPONDENTID, AGE, TOTALKCAL_HEI2020, TOTALFRT_SERV_HEI2020, FRT_SERV_HEI2020, VEG_SERV_HEI2020, GREENNBEAN_SERV_HEI2020, TOTALPRO_SERV_HEI2020,
                      SEAPLANTPRO_SERV_HEI2020, WHOLEGRAIN_SERV_HEI2020, DAIRY_SERV_HEI2020, FATTYACID_SERV_HEI2020, REFINEDGRAIN_SERV_HEI2020,
                      SODIUM_SERV_HEI2020, ADDEDSUGAR_SERV_HEI2020, SATFAT_SERV_HEI2020) {
    ## Create variables needed for HEI2020_V2 calculation
    HEI2020_MIN = 0
    HEI2020_MAX1 = 5
    HEI2020_MAX2 = 10

    HEI2020_MIN_TOTALFRT_SERV = 0
    HEI2020_MAX_TOTALFRT_SERV = 0.8
    HEI2020_MIN_FRT_SERV = 0
    HEI2020_MAX_FRT_SERV = 0.4
    HEI2020_MIN_VEG_SERV = 0
    HEI2020_MAX_VEG_SERV = 1.1
    HEI2020_MIN_GREENNBEAN_SERV = 0
    HEI2020_MAX_GREENNBEAN_SERV = 0.2
    HEI2020_MIN_TOTALPRO_SERV = 0
    HEI2020_MAX_TOTALPRO_SERV = 2.5
    HEI2020_MIN_SEAPLANTPRO_SERV = 0
    HEI2020_MAX_SEAPLANTPRO_SERV = 0.8
    HEI2020_MIN_WHOLEGRAIN_SERV = 0
    HEI2020_MAX_WHOLEGRAIN_SERV = 1.5
    HEI2020_MIN_DAIRY_SERV = 0
    HEI2020_MAX_DAIRY_SERV = 1.3
    HEI2020_MIN_FATTYACID_SERV = 1.2
    HEI2020_MAX_FATTYACID_SERV = 2.5

    HEI2020_MIN_REFINEDGRAIN_SERV = 4.3
    HEI2020_MAX_REFINEDGRAIN_SERV = 1.8
    HEI2020_MIN_SODIUM_SERV = 2.0
    HEI2020_MAX_SODIUM_SERV = 1.1
    HEI2020_MIN_ADDEDSUGAR_SERV = 26
    HEI2020_MAX_ADDEDSUGAR_SERV = 6.5
    HEI2020_MIN_SATFAT_SERV = 16
    HEI2020_MAX_SATFAT_SERV = 8

    ## Create variables needed for HEI-Toddlers-2020 calculation
    HEI2020_TODDLERS_MIN_TOTALFRT_SERV = 0
    HEI2020_TODDLERS_MAX_TOTALFRT_SERV = 0.7
    HEI2020_TODDLERS_MIN_FRT_SERV = 0
    HEI2020_TODDLERS_MAX_FRT_SERV = 0.3
    HEI2020_TODDLERS_MIN_VEG_SERV = 0
    HEI2020_TODDLERS_MAX_VEG_SERV = 0.9
    HEI2020_TODDLERS_MIN_GREENNBEAN_SERV = 0
    HEI2020_TODDLERS_MAX_GREENNBEAN_SERV = 0.1
    HEI2020_TODDLERS_MIN_TOTALPRO_SERV = 0
    HEI2020_TODDLERS_MAX_TOTALPRO_SERV = 2.0
    HEI2020_TODDLERS_MIN_SEAPLANTPRO_SERV = 0
    HEI2020_TODDLERS_MAX_SEAPLANTPRO_SERV = 0.5
    HEI2020_TODDLERS_MIN_WHOLEGRAIN_SERV = 0
    HEI2020_TODDLERS_MAX_WHOLEGRAIN_SERV = 1.5
    HEI2020_TODDLERS_MIN_DAIRY_SERV = 0
    HEI2020_TODDLERS_MAX_DAIRY_SERV = 2.0
    HEI2020_TODDLERS_MIN_FATTYACID_SERV = 0.9
    HEI2020_TODDLERS_MAX_FATTYACID_SERV = 1.5

    HEI2020_TODDLERS_MIN_REFINEDGRAIN_SERV = 3.4
    HEI2020_TODDLERS_MAX_REFINEDGRAIN_SERV = 1.5
    HEI2020_TODDLERS_MIN_SODIUM_SERV = 1.7
    HEI2020_TODDLERS_MAX_SODIUM_SERV = 1.1
    HEI2020_TODDLERS_MIN_ADDEDSUGAR_SERV = 13.8
    HEI2020_TODDLERS_MAX_ADDEDSUGAR_SERV = 0
    HEI2020_TODDLERS_MIN_SATFAT_SERV = 18.2
    HEI2020_TODDLERS_MAX_SATFAT_SERV = 12.2

    HEI2020_HEALTHY1 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2020_MAX1,
            actual <= min ~ HEI2020_MIN,
            TRUE ~ HEI2020_MIN + (actual - min) * HEI2020_MAX1 / (max - min)
        )
    }

    HEI2020_HEALTHY2 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2020_MAX2,
            actual <= min ~ HEI2020_MIN,
            TRUE ~ HEI2020_MIN + (actual - min) * HEI2020_MAX2 / (max - min)
        )
    }

    HEI2020_UNHEALTHY = function(actual, min, max) {
        case_when(
            actual >= min ~ HEI2020_MIN,
            actual <= max ~ HEI2020_MAX2,
            TRUE ~ HEI2020_MIN + (actual - min) * HEI2020_MAX2 / (max - min)
        )
    }

    # calculate the serving size for each food group
    SERV_DATA = SERV_DATA %>%
        dplyr::mutate(
            RESPONDENTID = RESPONDENTID,
            AGE = AGE,
            TOTALKCAL_HEI2020 = TOTALKCAL_HEI2020,
            # create food group serving size variables
            ## healthy food groups
            TOTALFRT_SERV_HEI2020 = TOTALFRT_SERV_HEI2020 / (TOTALKCAL_HEI2020 / 1000),
            FRT_SERV_HEI2020 = (FRT_SERV_HEI2020) / (TOTALKCAL_HEI2020 / 1000),
            VEG_SERV_HEI2020 = (VEG_SERV_HEI2020) / (TOTALKCAL_HEI2020 / 1000),
            GREENNBEAN_SERV_HEI2020 = (GREENNBEAN_SERV_HEI2020) / (TOTALKCAL_HEI2020 / 1000),
            TOTALPRO_SERV_HEI2020 = (TOTALPRO_SERV_HEI2020) / (TOTALKCAL_HEI2020 / 1000),
            SEAPLANTPRO_SERV_HEI2020 = (SEAPLANTPRO_SERV_HEI2020) / (TOTALKCAL_HEI2020 / 1000),
            WHOLEGRAIN_SERV_HEI2020 = WHOLEGRAIN_SERV_HEI2020 / (TOTALKCAL_HEI2020 / 1000),
            DAIRY_SERV_HEI2020 = DAIRY_SERV_HEI2020 / (TOTALKCAL_HEI2020 / 1000),
            FATTYACID_SERV_HEI2020 = FATTYACID_SERV_HEI2020,

            ## unhealthy food groups
            REFINEDGRAIN_SERV_HEI2020 = REFINEDGRAIN_SERV_HEI2020 / (TOTALKCAL_HEI2020 / 1000),
            SODIUM_SERV_HEI2020 = (SODIUM_SERV_HEI2020) / (TOTALKCAL_HEI2020 / 1000),
            ### calculate the % of total energy for added sugar using tsp as the input serving size, given that 1 tsp = 4g, 1g = 4kcal
            ADDEDSUGAR_SERV_HEI2020 = ((ADDEDSUGAR_SERV_HEI2020 * 4 * 4) / TOTALKCAL_HEI2020) * 100,
            ### calculate the % of total energy for saturated fat using g as the input serving size, given that 1g = 9 kcal
            SATFAT_SERV_HEI2020 = ((SATFAT_SERV_HEI2020 * 9) / TOTALKCAL_HEI2020) * 100
        ) %>%
        mutate(
            # calculate the HEI2020_V2 score for each food group given the serving size

            ## healthy food groups
            HEI2020_TOTALFRT = case_when(
                # only calculate HEI2020_V2 for children 2 years and older and adults
                AGE >= 2 ~ HEI2020_HEALTHY1(TOTALFRT_SERV_HEI2020, HEI2020_MIN_TOTALFRT_SERV, HEI2020_MAX_TOTALFRT_SERV),
                # for children under 2 years old, use the toddler's standard
                AGE < 2 ~ HEI2020_HEALTHY1(TOTALFRT_SERV_HEI2020, HEI2020_TODDLERS_MIN_TOTALFRT_SERV, HEI2020_TODDLERS_MAX_TOTALFRT_SERV),
            ),
            HEI2020_FRT = case_when(
                AGE >= 2 ~ HEI2020_HEALTHY1(FRT_SERV_HEI2020, HEI2020_MIN_FRT_SERV, HEI2020_MAX_FRT_SERV),
                AGE < 2 ~ HEI2020_HEALTHY1(FRT_SERV_HEI2020, HEI2020_TODDLERS_MIN_FRT_SERV, HEI2020_TODDLERS_MAX_FRT_SERV),
            ),
            HEI2020_VEG = case_when(
                AGE >= 2 ~ HEI2020_HEALTHY1(VEG_SERV_HEI2020, HEI2020_MIN_VEG_SERV, HEI2020_MAX_VEG_SERV),
                AGE < 2 ~ HEI2020_HEALTHY1(VEG_SERV_HEI2020, HEI2020_TODDLERS_MIN_VEG_SERV, HEI2020_TODDLERS_MAX_VEG_SERV),
            ),
            HEI2020_GREENNBEAN = case_when(
                AGE >= 2 ~ HEI2020_HEALTHY1(GREENNBEAN_SERV_HEI2020, HEI2020_MIN_GREENNBEAN_SERV, HEI2020_MAX_GREENNBEAN_SERV),
                AGE < 2 ~ HEI2020_HEALTHY1(GREENNBEAN_SERV_HEI2020, HEI2020_TODDLERS_MIN_GREENNBEAN_SERV, HEI2020_TODDLERS_MAX_GREENNBEAN_SERV),
            ),
            HEI2020_TOTALPRO = case_when(
                AGE >= 2 ~ HEI2020_HEALTHY1(TOTALPRO_SERV_HEI2020, HEI2020_MIN_TOTALPRO_SERV, HEI2020_MAX_TOTALPRO_SERV),
                AGE < 2 ~ HEI2020_HEALTHY1(TOTALPRO_SERV_HEI2020, HEI2020_TODDLERS_MIN_TOTALPRO_SERV, HEI2020_TODDLERS_MAX_TOTALPRO_SERV),
            ),
            HEI2020_SEAPLANTPRO = case_when(
                AGE >= 2 ~ HEI2020_HEALTHY1(SEAPLANTPRO_SERV_HEI2020, HEI2020_MIN_SEAPLANTPRO_SERV, HEI2020_MAX_SEAPLANTPRO_SERV),
                AGE < 2 ~ HEI2020_HEALTHY1(SEAPLANTPRO_SERV_HEI2020, HEI2020_TODDLERS_MIN_SEAPLANTPRO_SERV, HEI2020_TODDLERS_MAX_SEAPLANTPRO_SERV),
            ),
            HEI2020_WHOLEGRAIN = case_when(
                AGE >= 2 ~ HEI2020_HEALTHY2(WHOLEGRAIN_SERV_HEI2020, HEI2020_MIN_WHOLEGRAIN_SERV, HEI2020_MAX_WHOLEGRAIN_SERV),
                AGE < 2 ~ HEI2020_HEALTHY2(WHOLEGRAIN_SERV_HEI2020, HEI2020_TODDLERS_MIN_WHOLEGRAIN_SERV, HEI2020_TODDLERS_MAX_WHOLEGRAIN_SERV),
            ),
            HEI2020_DAIRY = case_when(
                AGE >= 2 ~ HEI2020_HEALTHY2(DAIRY_SERV_HEI2020, HEI2020_MIN_DAIRY_SERV, HEI2020_MAX_DAIRY_SERV),
                AGE < 2 ~ HEI2020_HEALTHY2(DAIRY_SERV_HEI2020, HEI2020_TODDLERS_MIN_DAIRY_SERV, HEI2020_TODDLERS_MAX_DAIRY_SERV),
            ),
            HEI2020_FATTYACID = case_when(
                AGE >= 2 ~ HEI2020_HEALTHY2(FATTYACID_SERV_HEI2020, HEI2020_MIN_FATTYACID_SERV, HEI2020_MAX_FATTYACID_SERV),
                AGE < 2 ~ HEI2020_HEALTHY2(FATTYACID_SERV_HEI2020, HEI2020_TODDLERS_MIN_FATTYACID_SERV, HEI2020_TODDLERS_MAX_FATTYACID_SERV),
            ),

            ## unhealthy food groups
            HEI2020_REFINEDGRAIN = case_when(
                AGE >= 2 ~ HEI2020_UNHEALTHY(REFINEDGRAIN_SERV_HEI2020, HEI2020_MIN_REFINEDGRAIN_SERV, HEI2020_MAX_REFINEDGRAIN_SERV),
                AGE < 2 ~ HEI2020_UNHEALTHY(REFINEDGRAIN_SERV_HEI2020, HEI2020_TODDLERS_MIN_REFINEDGRAIN_SERV, HEI2020_TODDLERS_MAX_REFINEDGRAIN_SERV),
            ),
            HEI2020_SODIUM = case_when(
                AGE >= 2 ~ HEI2020_UNHEALTHY(SODIUM_SERV_HEI2020, HEI2020_MIN_SODIUM_SERV, HEI2020_MAX_SODIUM_SERV),
                AGE < 2 ~ HEI2020_UNHEALTHY(SODIUM_SERV_HEI2020, HEI2020_TODDLERS_MIN_SODIUM_SERV, HEI2020_TODDLERS_MAX_SODIUM_SERV),
            ),
            HEI2020_ADDEDSUGAR = case_when(
                AGE >= 2 ~ HEI2020_UNHEALTHY(ADDEDSUGAR_SERV_HEI2020, HEI2020_MIN_ADDEDSUGAR_SERV, HEI2020_MAX_ADDEDSUGAR_SERV),
                AGE < 2 ~ HEI2020_UNHEALTHY(ADDEDSUGAR_SERV_HEI2020, HEI2020_TODDLERS_MIN_ADDEDSUGAR_SERV, HEI2020_TODDLERS_MAX_ADDEDSUGAR_SERV),
            ),
            HEI2020_SATFAT = case_when(
                AGE >= 2 ~ HEI2020_UNHEALTHY(SATFAT_SERV_HEI2020, HEI2020_MIN_SATFAT_SERV, HEI2020_MAX_SATFAT_SERV),
                AGE < 2 ~ HEI2020_UNHEALTHY(SATFAT_SERV_HEI2020, HEI2020_TODDLERS_MIN_SATFAT_SERV, HEI2020_TODDLERS_MAX_SATFAT_SERV),
            ),

            ## total HEI2020 score
            HEI2020_ALL = HEI2020_TOTALFRT + HEI2020_FRT + HEI2020_VEG + HEI2020_GREENNBEAN +
                HEI2020_TOTALPRO + HEI2020_SEAPLANTPRO + HEI2020_WHOLEGRAIN + HEI2020_DAIRY +
                HEI2020_FATTYACID + HEI2020_REFINEDGRAIN + HEI2020_SODIUM + HEI2020_ADDEDSUGAR +
                HEI2020_SATFAT
        )

    ## Set HEI2020_ALL to 0 if TOTALKCAL_HEI2020 is 0
    for (i in 1:length(SERV_DATA$TOTALKCAL_HEI2020)) {
        if (SERV_DATA$TOTALKCAL_HEI2020[i] == 0) {
            SERV_DATA$HEI2020_TOTALFRT[i] = 0
            SERV_DATA$HEI2020_FRT[i] = 0
            SERV_DATA$HEI2020_VEG[i] = 0
            SERV_DATA$HEI2020_GREENNBEAN[i] = 0
            SERV_DATA$HEI2020_TOTALPRO[i] = 0
            SERV_DATA$HEI2020_SEAPLANTPRO[i] = 0
            SERV_DATA$HEI2020_WHOLEGRAIN[i] = 0
            SERV_DATA$HEI2020_DAIRY[i] = 0
            SERV_DATA$HEI2020_FATTYACID[i] = 0
            SERV_DATA$HEI2020_REFINEDGRAIN[i] = 0
            SERV_DATA$HEI2020_ADDEDSUGAR[i] = 0
            SERV_DATA$HEI2020_ALL[i] = 0
        }
    }

    SERV_DATA %>%
        dplyr::select(
            RESPONDENTID, AGE, TOTALKCAL_HEI2020, HEI2020_ALL, HEI2020_TOTALFRT, HEI2020_FRT, HEI2020_VEG, HEI2020_GREENNBEAN,
            HEI2020_TOTALPRO, HEI2020_SEAPLANTPRO, HEI2020_WHOLEGRAIN, HEI2020_DAIRY,
            HEI2020_FATTYACID, HEI2020_REFINEDGRAIN, HEI2020_SODIUM, HEI2020_ADDEDSUGAR,
            HEI2020_SATFAT
        )
}
