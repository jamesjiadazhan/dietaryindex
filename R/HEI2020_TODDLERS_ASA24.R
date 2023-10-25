#' HEI2020_TODDLERS_ASA24
#'
#' Calculate the HEI2020_TODDLERS for the ASA24 data within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The average HEI2020_TODDLERS and its component scores for each individual per day.
#' @examples
#' data("ASA24_exp")
#' HEI2020_TODDLERS_ASA24(ASA24_exp, RECALL_SUMMARIZE = TRUE)
#' @export

HEI2020_TODDLERS_ASA24 = function(DATA_PATH, RECALL_SUMMARIZE = TRUE) {
    if (is.character(DATA_PATH) == TRUE) {
        COHORT = read_csv(DATA_PATH)
    } else {
        COHORT = DATA_PATH
    }

    if ("FoodCode" %in% colnames(COHORT)) {
        stop("Please use the population-level data. The file name should be like: Totals.csv")
    }


    # If RECALL_SUMMARIZE is TRUE, the function calculates total food group and nutrient intake over all possible days and then average them by reporting dietary index per individual per day.
    if (RECALL_SUMMARIZE == TRUE) {
        print("RECALL_SUMMARIZE = TRUE, summarizing HEI2020_TODDLERS for ASA24 data by averaging over all possible recalls per person per day...")

        ## get sum per person of variables of interest if multiple recalls from the same person exist
        COHORT = COHORT %>%
            # arrange by UserName, UserID
            dplyr::arrange(UserName, UserID) %>%
            dplyr::group_by(UserName, UserID) %>%
            dplyr::summarise(
                KCAL = mean(KCAL),
                PROT = mean(PROT),
                TFAT = mean(TFAT),
                CARB = mean(CARB),
                MOIS = mean(MOIS),
                ALC = mean(ALC),
                CAFF = mean(CAFF),
                THEO = mean(THEO),
                SUGR = mean(SUGR),
                FIBE = mean(FIBE),
                CALC = mean(CALC),
                IRON = mean(IRON),
                MAGN = mean(MAGN),
                PHOS = mean(PHOS),
                POTA = mean(POTA),
                SODI = mean(SODI),
                ZINC = mean(ZINC),
                COPP = mean(COPP),
                SELE = mean(SELE),
                VC = mean(VC),
                VB1 = mean(VB1),
                VB2 = mean(VB2),
                NIAC = mean(NIAC),
                VB6 = mean(VB6),
                FOLA = mean(FOLA),
                FA = mean(FA),
                FF = mean(FF),
                FDFE = mean(FDFE),
                VB12 = mean(VB12),
                VARA = mean(VARA),
                RET = mean(RET),
                BCAR = mean(BCAR),
                ACAR = mean(ACAR),
                CRYP = mean(CRYP),
                LYCO = mean(LYCO),
                LZ = mean(LZ),
                ATOC = mean(ATOC),
                VK = mean(VK),
                CHOLE = mean(CHOLE),
                SFAT = mean(SFAT),
                S040 = mean(S040),
                S060 = mean(S060),
                S080 = mean(S080),
                S100 = mean(S100),
                S120 = mean(S120),
                S140 = mean(S140),
                S160 = mean(S160),
                S180 = mean(S180),
                MFAT = mean(MFAT),
                M161 = mean(M161),
                M181 = mean(M181),
                M201 = mean(M201),
                M221 = mean(M221),
                PFAT = mean(PFAT),
                P182 = mean(P182),
                P183 = mean(P183),
                P184 = mean(P184),
                P204 = mean(P204),
                P205 = mean(P205),
                P225 = mean(P225),
                P226 = mean(P226),
                VITD = mean(VITD),
                CHOLN = mean(CHOLN),
                VITE_ADD = mean(VITE_ADD),
                B12_ADD = mean(B12_ADD),
                F_TOTAL = mean(F_TOTAL),
                F_CITMLB = mean(F_CITMLB),
                F_OTHER = mean(F_OTHER),
                F_JUICE = mean(F_JUICE),
                V_TOTAL = mean(V_TOTAL),
                V_DRKGR = mean(V_DRKGR),
                V_REDOR_TOTAL = mean(V_REDOR_TOTAL),
                V_REDOR_TOMATO = mean(V_REDOR_TOMATO),
                V_REDOR_OTHER = mean(V_REDOR_OTHER),
                V_STARCHY_TOTAL = mean(V_STARCHY_TOTAL),
                V_STARCHY_POTATO = mean(V_STARCHY_POTATO),
                V_STARCHY_OTHER = mean(V_STARCHY_OTHER),
                V_OTHER = mean(V_OTHER),
                V_LEGUMES = mean(V_LEGUMES),
                G_TOTAL = mean(G_TOTAL),
                G_WHOLE = mean(G_WHOLE),
                G_REFINED = mean(G_REFINED),
                PF_TOTAL = mean(PF_TOTAL),
                PF_MPS_TOTAL = mean(PF_MPS_TOTAL),
                PF_MEAT = mean(PF_MEAT),
                PF_CUREDMEAT = mean(PF_CUREDMEAT),
                PF_ORGAN = mean(PF_ORGAN),
                PF_POULT = mean(PF_POULT),
                PF_SEAFD_HI = mean(PF_SEAFD_HI),
                PF_SEAFD_LOW = mean(PF_SEAFD_LOW),
                PF_EGGS = mean(PF_EGGS),
                PF_SOY = mean(PF_SOY),
                PF_NUTSDS = mean(PF_NUTSDS),
                PF_LEGUMES = mean(PF_LEGUMES),
                D_TOTAL = mean(D_TOTAL),
                D_MILK = mean(D_MILK),
                D_YOGURT = mean(D_YOGURT),
                D_CHEESE = mean(D_CHEESE),
                OILS = mean(OILS),
                SOLID_FATS = mean(SOLID_FATS),
                ADD_SUGARS = mean(ADD_SUGARS),
                A_DRINKS = mean(A_DRINKS),
                ## save the group level variables for later use and silent the warning message
                .groups = "keep"
            )
    }
    # If RECALL_SUMMARIZE is FALSE, the function calculates total food group and nutrient intake over all possible days reported per individual per day.
    else {
        # do nothing
        print("RECALL_SUMMARIZE is FALSE, skipping summarization step...")
    }

    # Calculate serving size needed for HEI2020_TODDLERS calculation
    COHORT = COHORT %>%
        dplyr::mutate(
            TOTALFRT_SERV = F_TOTAL / (KCAL / 1000),
            FRT_SERV = (F_CITMLB + F_OTHER) / (KCAL / 1000),
            VEG_SERV = (V_TOTAL + V_LEGUMES) / (KCAL / 1000),
            GREENNBEAN_SERV = (V_DRKGR + V_LEGUMES) / (KCAL / 1000),
            TOTALPRO_SERV = (PF_MPS_TOTAL + PF_EGGS + PF_NUTSDS + PF_SOY + PF_LEGUMES) / (KCAL / 1000),
            SEAPLANTPRO_SERV = (PF_SEAFD_HI + PF_SEAFD_LOW + PF_NUTSDS + PF_SOY + PF_LEGUMES) / (KCAL / 1000),
            WHOLEGRAIN_SERV = G_WHOLE / (KCAL / 1000),
            DAIRY_SERV = D_TOTAL / (KCAL / 1000),
            FATTYACID_SERV = case_when(
                SFAT == 0 ~ 0,
                SFAT != 0 ~ (MFAT + PFAT) / SFAT
            ),
            REFINEDGRAIN_SERV = G_REFINED / (KCAL / 1000),
            SODIUM_SERV = (SODI / 1000) / (KCAL / 1000),
            ADDEDSUGAR_SERV = ((ADD_SUGARS * 4 * 4) / KCAL) * 100,
            SATFAT_SERV = ((SFAT * 9) / KCAL) * 100,
            TOTALKCAL = KCAL
        )


    ## Create variables needed for HEI2020_TODDLERS calculation
    HEI2020_TODDLERS_MIN = 0
    HEI2020_TODDLERS_MAX1 = 5
    HEI2020_TODDLERS_MAX2 = 10

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

    HEI2020_TODDLERS_HEALTHY1 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2020_TODDLERS_MAX1,
            actual <= min ~ HEI2020_TODDLERS_MIN,
            TRUE ~ HEI2020_TODDLERS_MIN + (actual - min) * HEI2020_TODDLERS_MAX1 / (max - min)
        )
    }

    HEI2020_TODDLERS_HEALTHY2 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2020_TODDLERS_MAX2,
            actual <= min ~ HEI2020_TODDLERS_MIN,
            TRUE ~ HEI2020_TODDLERS_MIN + (actual - min) * HEI2020_TODDLERS_MAX2 / (max - min)
        )
    }

    HEI2020_TODDLERS_UNHEALTHY = function(actual, min, max) {
        case_when(
            actual >= min ~ HEI2020_TODDLERS_MIN,
            actual <= max ~ HEI2020_TODDLERS_MAX2,
            TRUE ~ HEI2020_TODDLERS_MIN + (actual - min) * HEI2020_TODDLERS_MAX2 / (max - min)
        )
    }

    # calculate HEI2020_TODDLERS total and component scores
    COHORT = COHORT %>%
        dplyr::mutate(
            HEI2020_TODDLERS_TOTALFRT = HEI2020_TODDLERS_HEALTHY1(TOTALFRT_SERV, HEI2020_TODDLERS_MIN_TOTALFRT_SERV, HEI2020_TODDLERS_MAX_TOTALFRT_SERV),
            HEI2020_TODDLERS_FRT = HEI2020_TODDLERS_HEALTHY1(FRT_SERV, HEI2020_TODDLERS_MIN_FRT_SERV, HEI2020_TODDLERS_MAX_FRT_SERV),
            HEI2020_TODDLERS_VEG = HEI2020_TODDLERS_HEALTHY1(VEG_SERV, HEI2020_TODDLERS_MIN_VEG_SERV, HEI2020_TODDLERS_MAX_VEG_SERV),
            HEI2020_TODDLERS_GREENNBEAN = HEI2020_TODDLERS_HEALTHY1(GREENNBEAN_SERV, HEI2020_TODDLERS_MIN_GREENNBEAN_SERV, HEI2020_TODDLERS_MAX_GREENNBEAN_SERV),
            HEI2020_TODDLERS_TOTALPRO = HEI2020_TODDLERS_HEALTHY1(TOTALPRO_SERV, HEI2020_TODDLERS_MIN_TOTALPRO_SERV, HEI2020_TODDLERS_MAX_TOTALPRO_SERV),
            HEI2020_TODDLERS_SEAPLANTPRO = HEI2020_TODDLERS_HEALTHY1(SEAPLANTPRO_SERV, HEI2020_TODDLERS_MIN_SEAPLANTPRO_SERV, HEI2020_TODDLERS_MAX_SEAPLANTPRO_SERV),
            HEI2020_TODDLERS_WHOLEGRAIN = HEI2020_TODDLERS_HEALTHY2(WHOLEGRAIN_SERV, HEI2020_TODDLERS_MIN_WHOLEGRAIN_SERV, HEI2020_TODDLERS_MAX_WHOLEGRAIN_SERV),
            HEI2020_TODDLERS_DAIRY = HEI2020_TODDLERS_HEALTHY2(DAIRY_SERV, HEI2020_TODDLERS_MIN_DAIRY_SERV, HEI2020_TODDLERS_MAX_DAIRY_SERV),
            HEI2020_TODDLERS_FATTYACID = HEI2020_TODDLERS_HEALTHY2(FATTYACID_SERV, HEI2020_TODDLERS_MIN_FATTYACID_SERV, HEI2020_TODDLERS_MAX_FATTYACID_SERV),
            HEI2020_TODDLERS_REFINEDGRAIN = HEI2020_TODDLERS_UNHEALTHY(REFINEDGRAIN_SERV, HEI2020_TODDLERS_MIN_REFINEDGRAIN_SERV, HEI2020_TODDLERS_MAX_REFINEDGRAIN_SERV),
            HEI2020_TODDLERS_SODIUM = HEI2020_TODDLERS_UNHEALTHY(SODIUM_SERV, HEI2020_TODDLERS_MIN_SODIUM_SERV, HEI2020_TODDLERS_MAX_SODIUM_SERV),
            HEI2020_TODDLERS_ADDEDSUGAR = HEI2020_TODDLERS_UNHEALTHY(ADDEDSUGAR_SERV, HEI2020_TODDLERS_MIN_ADDEDSUGAR_SERV, HEI2020_TODDLERS_MAX_ADDEDSUGAR_SERV),
            HEI2020_TODDLERS_SATFAT = HEI2020_TODDLERS_UNHEALTHY(SATFAT_SERV, HEI2020_TODDLERS_MIN_SATFAT_SERV, HEI2020_TODDLERS_MAX_SATFAT_SERV),
            HEI2020_TODDLERS_ALL = HEI2020_TODDLERS_TOTALFRT + HEI2020_TODDLERS_FRT + HEI2020_TODDLERS_VEG + HEI2020_TODDLERS_GREENNBEAN +
                HEI2020_TODDLERS_TOTALPRO + HEI2020_TODDLERS_SEAPLANTPRO + HEI2020_TODDLERS_WHOLEGRAIN + HEI2020_TODDLERS_DAIRY +
                HEI2020_TODDLERS_FATTYACID + HEI2020_TODDLERS_REFINEDGRAIN + HEI2020_TODDLERS_SODIUM + HEI2020_TODDLERS_ADDEDSUGAR +
                HEI2020_TODDLERS_SATFAT
        )

    for (i in 1:length(COHORT$TOTALKCAL)) {
        if (COHORT$TOTALKCAL[i] == 0) {
            COHORT$HEI2020_TODDLERS_TOTALFRT[i] = 0
            COHORT$HEI2020_TODDLERS_FRT[i] = 0
            COHORT$HEI2020_TODDLERS_VEG[i] = 0
            COHORT$HEI2020_TODDLERS_GREENNBEAN[i] = 0
            COHORT$HEI2020_TODDLERS_TOTALPRO[i] = 0
            COHORT$HEI2020_TODDLERS_SEAPLANTPRO[i] = 0
            COHORT$HEI2020_TODDLERS_WHOLEGRAIN[i] = 0
            COHORT$HEI2020_TODDLERS_DAIRY[i] = 0
            COHORT$HEI2020_TODDLERS_FATTYACID[i] = 0
            COHORT$HEI2020_TODDLERS_REFINEDGRAIN[i] = 0
            COHORT$HEI2020_TODDLERS_ADDEDSUGAR[i] = 0
            COHORT$HEI2020_TODDLERS_ALL[i] = 0
        }
    }

    print("The results should be only used for HEI-Toddlers 2020 (age 1-2 years), not for HEI-2020 (non-toddlers, age > 2 years).")

    COHORT %>%
        dplyr::select(
            UserName, UserID, TOTALKCAL, HEI2020_TODDLERS_ALL, HEI2020_TODDLERS_TOTALFRT, HEI2020_TODDLERS_FRT, HEI2020_TODDLERS_VEG, HEI2020_TODDLERS_GREENNBEAN,
            HEI2020_TODDLERS_TOTALPRO, HEI2020_TODDLERS_SEAPLANTPRO, HEI2020_TODDLERS_WHOLEGRAIN, HEI2020_TODDLERS_DAIRY,
            HEI2020_TODDLERS_FATTYACID, HEI2020_TODDLERS_REFINEDGRAIN, HEI2020_TODDLERS_SODIUM, HEI2020_TODDLERS_ADDEDSUGAR,
            HEI2020_TODDLERS_SATFAT
        )
}
