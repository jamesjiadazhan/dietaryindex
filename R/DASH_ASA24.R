#' DASH_ASA24
#'
#' Calculate the DASH for the ASA24 data within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Items.csv
#' @param SKIM_MILK_code The food code for skim milk, default is the skim milk code from 17-18 FNDDS file
#' @param LOWF_CHEESE_code The food code for low-fat cheese, default is the low-fat cheese code from 17-18 FNDDS file
#' @param SSB_code The food code for sugar sweetened beverage, default is the SSB code from 17-18 FNDDS file
#' @param RECALL_SUMMARIZE Whether to summarize the recalls to the person level, default is TRUE
#' @return The DASH and its component scores
#' @examples
#' data("ASA24_exp_detailed")
#' DASH_ASA24(ASA24_exp_detailed, SKIM_MILK_code = NULL, LOWF_CHEESE_code = NULL, SSB_code = NULL, RECALL_SUMMARIZE = TRUE)
#' @export


DASH_ASA24 = function(DATA_PATH, SKIM_MILK_code = NULL, LOWF_CHEESE_code = NULL, SSB_code = NULL, RECALL_SUMMARIZE = TRUE) {
    if (is.character(DATA_PATH) == TRUE) {
        COHORT = read_csv(DATA_PATH)
    } else {
        COHORT = DATA_PATH
    }

    if (is.null(SSB_code)) {
        # load the SSB codes from 17-18 FNDDS file
        COFFEE = c(12200100, 12210200, 12210210, 12210260, 12210270, 12210280, 12210310, 12210400, 12210420, 12210430, 12210440, 12210505, 12210520, 91703600, 92100000, 92100500, 92101000, 92101500, 92101600, 92101610, 92101630, 92101700, 92101800, 92101810, 92101820, 92101850, 92101851, 92101900, 92101901, 92101903, 92101904, 92101905, 92101906, 92101910, 92101911, 92101913, 92101917, 92101918, 92101919, 92101920, 92101921, 92101923, 92101925, 92101926, 92101928, 92101930, 92101931, 92101933, 92101935, 92101936, 92101938, 92101950, 92101955, 92101960, 92101965, 92101970, 92101975, 92102000, 92102010, 92102020, 92102030, 92102040, 92102050, 92102060, 92102070, 92102080, 92102090, 92102100, 92102110, 92102400, 92102401, 92102450, 92102500, 92102501, 92102502, 92102503, 92102504, 92102505, 92102510, 92102511, 92102512, 92102513, 92102514, 92102515, 92102600, 92102601, 92102602, 92102610, 92102611, 92102612, 92103000, 92104000, 92111000, 92111010, 92114000, 92121000, 92121001, 92121010, 92121020, 92121030, 92121040, 92121041, 92121050, 92130000, 92130001, 92130005, 92130006, 92130010, 92130011, 92130020, 92130021, 92130030, 92130031, 92152000, 92152010, 92161000, 92161001, 92161002, 92162000, 92162001, 92162002, 92171000, 92171010, 92191100, 92191105, 92191200, 92191400, 92192000, 92192030, 92192040, 92193000, 92193005, 92193020, 92193025, 92201010, 92291300, 93202000, 93301400)
        TEA = c(53246000, 92302000, 92302500, 92303010, 92303100, 92304100, 92305010, 92305040, 92305050, 92305090, 92305110, 92305180, 92305900, 92305910, 92305920, 92306000, 92306090, 92306700, 92306800, 92307000, 92307400, 92308000, 92308010, 92308020, 92308030, 92308040, 92308050, 92308500, 92308510, 92308520, 92308530, 92308540, 92308550, 92309000, 92309010, 92309020, 92309030, 92309040, 92309050, 92309500, 92309510, 92309520)
        COFFEE_TEA = c(COFFEE, TEA)
        DRINK = c(11511100, 11511200, 11511300, 11511400, 11511550, 11511600, 11511610, 11511700, 11512010, 11512020, 11512030, 11512100, 11512110, 11512120, 11553130, 11560000, 64134030, 67260000, 75200700, 91301130, 92101920, 92101921, 92101923, 92101925, 92101926, 92101928, 92101930, 92101931, 92101933, 92101935, 92101936, 92101938, 92102000, 92102010, 92102020, 92102030, 92102040, 92102050, 92102060, 92102070, 92102080, 92102090, 92102100, 92102110, 92307500, 92307510, 92307520, 92400000, 92400100, 92410310, 92410315, 92410320, 92410340, 92410350, 92410360, 92410370, 92410390, 92410400, 92410410, 92410420, 92410510, 92410520, 92410550, 92410560, 92410610, 92410620, 92410710, 92410720, 92410810, 92410820, 92411510, 92411520, 92411610, 92411620, 92432000, 92433000, 92510610, 92510650, 92510955, 92510960, 92511015, 92513000, 92513010, 92530410, 92530510, 92530610, 92530950, 92531030, 92541010, 92542000, 92550030, 92550035, 92550040, 92550110, 92550200, 92550370, 92550400, 92550405, 92550610, 92550620, 92552000, 92552010, 92552020, 92552030, 92582100, 92582110, 92900100, 92900110, 92900200, 92900300, 93301216, 95101000, 95101010, 95102000, 95103000, 95103010, 95104000, 95105000, 95106000, 95106010, 95110000, 95110010, 95110020, 95120000, 95120010, 95120020, 95120050, 95310200, 95310400, 95310500, 95310550, 95310555, 95310560, 95310600, 95310700, 95310750, 95310800, 95311000, 95312400, 95312410, 95312500, 95312550, 95312555, 95312560, 95312600, 95312700, 95312800, 95312900, 95312905, 95313200, 95320200, 95320500, 95321000, 95322200, 95322500, 95323000)
        SSB = c(COFFEE_TEA, DRINK)
        print("Since no SSB code is provided, the default SSB code from 17-18 FNDDS file is used.")
    } else {
        SSB = SSB_code
    }

    if (is.null(SKIM_MILK_code)) {
        SKIM_MILK = c(11111170, 11113000, 11114320, 11115000, 11120000, 11121300, 11212050, 11511000, 11511300, 11511610, 11512020, 11512110, 11513300, 11513370, 11513384, 11513394, 11513700, 11513804, 11513854, 11514140, 11514350, 11519205)
        print("Since no skim milk code is provided, the default skim milk code from 17-18 FNDDS file is used.")
    } else {
        SKIM_MILK = SKIM_MILK_code
    }

    if (is.null(LOWF_CHEESE_code)) {
        LOWF_CHEESE = c(14204010, 14204020, 14206010, 14207010)
        print("Since no low-fat cheese code is provided, the default low-fat cheese code from 17-18 FNDDS file is used.")
    } else {
        LOWF_CHEESE = LOWF_CHEESE_code
    }


    if (!("FoodCode" %in% colnames(COHORT))) {
        stop("Please use the individual-level data since this function needs to calculate sugar sweetened beverage serving using data from individual food items. The file name should be like: Items.csv")
    }

    # Calculates total food group and nutrient intake over all possible days reported per individual per day.
    COHORT = COHORT %>%
        dplyr::mutate(
            SKIM_MILK_SERV = case_when(
                FoodCode %in% SKIM_MILK ~ D_MILK,
                TRUE ~ 0
            ),
            LOWF_CHEESECREAM_SERV = case_when(
                FoodCode %in% LOWF_CHEESE ~ D_CHEESE * 4,
                TRUE ~ 0
            ),
            ADDED_SUGAR_SSB_SERV = case_when(
                FoodCode %in% SSB ~ ADD_SUGARS,
                TRUE ~ 0
            )
        ) %>%
        # summarize each individual food pattern variables into one row per person for all days reported
        dplyr::group_by(UserName, UserID, RecallNo) %>%
        dplyr::summarize(
            ENERGY = sum(KCAL),
            FRT_FRTJ_SERV = sum(F_TOTAL),
            VEG_SERV = sum(V_DRKGR + (V_REDOR_TOTAL + V_STARCHY_OTHER + V_OTHER) / 0.5),
            NUTSLEG_SERV = sum(PF_NUTSDS + PF_SOY + PF_LEGUMES),
            WGRAIN_SERV = sum(G_WHOLE),
            LOWF_DAIRY_SERV = sum(SKIM_MILK_SERV + LOWF_CHEESECREAM_SERV + D_YOGURT),
            SODIUM_SERV = sum(SODI / (ENERGY / 2000)),
            REDPROC_MEAT_SERV = sum((PF_CUREDMEAT / 1.5) + ((PF_MEAT + PF_ORGAN) / 4)),
            SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV * 4 / 26)),
            ## save the group level variables for later use and silent the warning message
            .groups = "keep"
        )

    # if RECALL_SUMMARIZE = TRUE, summarize the food group and nutrient intake over all days reported per individual per day
    if (RECALL_SUMMARIZE == TRUE) {
        print("RECALL_SUMMARIZE = TRUE, summarizing HEI2015 for ASA24 data by averaging over all possible recalls per person per day...")

        COHORT = COHORT %>%
            # average across all days reported to results per person per day
            dplyr::group_by(UserName, UserID) %>%
            dplyr::summarize(
                ENERGY = mean(ENERGY),
                FRT_FRTJ_SERV = mean(FRT_FRTJ_SERV),
                VEG_SERV = mean(VEG_SERV),
                NUTSLEG_SERV = mean(NUTSLEG_SERV),
                WGRAIN_SERV = mean(WGRAIN_SERV),
                LOWF_DAIRY_SERV = mean(LOWF_DAIRY_SERV),
                SODIUM_SERV = mean(SODIUM_SERV),
                REDPROC_MEAT_SERV = mean(REDPROC_MEAT_SERV),
                SSB_FRTJ_SERV = mean(SSB_FRTJ_SERV),
                ## save the group level variables for later use and silent the warning message
                .groups = "keep"
            )
    }
    # if RECALL_SUMMARIZE = FALSE, keep the food group and nutrient intake over all days reported per individual per day
    else {
        print("RECALL_SUMMARIZE is FALSE, skipping summarization step...")
    }

    ## Create variables and functions needed for DASH calculation
    quintile_healthy = function(actual) {
        quintile = quantile(actual, probs = seq(0, 1, by = 0.2))
        case_when(
            actual <= quintile[6] & actual >= quintile[5] ~ 5,
            actual < quintile[5] & actual >= quintile[4] ~ 4,
            actual < quintile[4] & actual >= quintile[3] ~ 3,
            actual < quintile[3] & actual >= quintile[2] ~ 2,
            actual < quintile[2] & actual >= quintile[1] ~ 1
        )
    }

    quintile_unhealthy = function(actual) {
        quintile = quantile(actual, probs = seq(0, 1, by = 0.2))
        case_when(
            actual <= quintile[6] & actual >= quintile[5] ~ 1,
            actual < quintile[5] & actual >= quintile[4] ~ 2,
            actual < quintile[4] & actual >= quintile[3] ~ 3,
            actual < quintile[3] & actual >= quintile[2] ~ 4,
            actual < quintile[2] & actual >= quintile[1] ~ 5
        )
    }

    print("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")

    ## DASH calculation
    COHORT %>%
        dplyr::mutate(
            DASH_FRT = quintile_healthy(FRT_FRTJ_SERV),
            DASH_VEG = quintile_healthy(VEG_SERV),
            DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV),
            DASH_WGRAIN = quintile_healthy(WGRAIN_SERV),
            DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV),
            DASH_SODIUM = quintile_unhealthy(SODIUM_SERV),
            DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV),
            DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV),
            DASH_ALL = DASH_FRT + DASH_VEG + DASH_NUTSLEG + DASH_WGRAIN + DASH_LOWF_DAIRY +
                DASH_SODIUM + DASH_REDPROC_MEAT + DASH_SSB_FRTJ
        ) %>%
        dplyr::select(
            UserName, UserID, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
            DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ
        )
}
