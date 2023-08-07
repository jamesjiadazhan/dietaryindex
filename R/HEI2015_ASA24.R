#' HEI2015_ASA24
#'
#' Calculate the HEI2015 for the ASA24 data within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The average HEI2015 and its component scores for each individual per day.
#' @examples
#' data("ASA24_exp")
#' HEI2015_ASA24(ASA24_exp)
#' @export


HEI2015_ASA24 = function(DATA_PATH) {
    if (is.character(DATA_PATH) == TRUE) {
        COHORT = read_csv(DATA_PATH)
    } else {
        COHORT = DATA_PATH
    }

    if ("FoodCode" %in% colnames(COHORT)) {
        stop("Please use the population-level data. The file name should be like: Totals.csv")
    }

    # Calculates total food group and nutrient intake over all possible days reported per individual per day.
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

    # Calculate serving size needed for HEI2015 calculation
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


    ## Create variables needed for HEI2015 calculation
    HEI2015_MIN = 0
    HEI2015_MAX1 = 5
    HEI2015_MAX2 = 10

    HEI2015_MIN_TOTALFRT_SERV = 0
    HEI2015_MAX_TOTALFRT_SERV = 0.8
    HEI2015_MIN_FRT_SERV = 0
    HEI2015_MAX_FRT_SERV = 0.4
    HEI2015_MIN_VEG_SERV = 0
    HEI2015_MAX_VEG_SERV = 1.1
    HEI2015_MIN_GREENNBEAN_SERV = 0
    HEI2015_MAX_GREENNBEAN_SERV = 0.2
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

    HEI2015_HEALTHY1 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2015_MAX1,
            actual <= min ~ HEI2015_MIN,
            TRUE ~ HEI2015_MIN + (actual - min) * HEI2015_MAX1 / (max - min)
        )
    }

    HEI2015_HEALTHY2 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2015_MAX2,
            actual <= min ~ HEI2015_MIN,
            TRUE ~ HEI2015_MIN + (actual - min) * HEI2015_MAX2 / (max - min)
        )
    }

    HEI2015_UNHEALTHY = function(actual, min, max) {
        case_when(
            actual >= min ~ HEI2015_MIN,
            actual <= max ~ HEI2015_MAX2,
            TRUE ~ HEI2015_MIN + (actual - min) * HEI2015_MAX2 / (max - min)
        )
    }

    # calculate HEI2015 total and component scores
    COHORT = COHORT %>%
        dplyr::mutate(
            HEI2015_TOTALFRT = HEI2015_HEALTHY1(TOTALFRT_SERV, HEI2015_MIN_TOTALFRT_SERV, HEI2015_MAX_TOTALFRT_SERV),
            HEI2015_FRT = HEI2015_HEALTHY1(FRT_SERV, HEI2015_MIN_FRT_SERV, HEI2015_MAX_FRT_SERV),
            HEI2015_VEG = HEI2015_HEALTHY1(VEG_SERV, HEI2015_MIN_VEG_SERV, HEI2015_MAX_VEG_SERV),
            HEI2015_GREENNBEAN = HEI2015_HEALTHY1(GREENNBEAN_SERV, HEI2015_MIN_GREENNBEAN_SERV, HEI2015_MAX_GREENNBEAN_SERV),
            HEI2015_TOTALPRO = HEI2015_HEALTHY1(TOTALPRO_SERV, HEI2015_MIN_TOTALPRO_SERV, HEI2015_MAX_TOTALPRO_SERV),
            HEI2015_SEAPLANTPRO = HEI2015_HEALTHY1(SEAPLANTPRO_SERV, HEI2015_MIN_SEAPLANTPRO_SERV, HEI2015_MAX_SEAPLANTPRO_SERV),
            HEI2015_WHOLEGRAIN = HEI2015_HEALTHY2(WHOLEGRAIN_SERV, HEI2015_MIN_WHOLEGRAIN_SERV, HEI2015_MAX_WHOLEGRAIN_SERV),
            HEI2015_DAIRY = HEI2015_HEALTHY2(DAIRY_SERV, HEI2015_MIN_DAIRY_SERV, HEI2015_MAX_DAIRY_SERV),
            HEI2015_FATTYACID = HEI2015_HEALTHY2(FATTYACID_SERV, HEI2015_MIN_FATTYACID_SERV, HEI2015_MAX_FATTYACID_SERV),
            HEI2015_REFINEDGRAIN = HEI2015_UNHEALTHY(REFINEDGRAIN_SERV, HEI2015_MIN_REFINEDGRAIN_SERV, HEI2015_MAX_REFINEDGRAIN_SERV),
            HEI2015_SODIUM = HEI2015_UNHEALTHY(SODIUM_SERV, HEI2015_MIN_SODIUM_SERV, HEI2015_MAX_SODIUM_SERV),
            HEI2015_ADDEDSUGAR = HEI2015_UNHEALTHY(ADDEDSUGAR_SERV, HEI2015_MIN_ADDEDSUGAR_SERV, HEI2015_MAX_ADDEDSUGAR_SERV),
            HEI2015_SATFAT = HEI2015_UNHEALTHY(SATFAT_SERV, HEI2015_MIN_SATFAT_SERV, HEI2015_MAX_SATFAT_SERV),
            HEI2015_ALL = HEI2015_TOTALFRT + HEI2015_FRT + HEI2015_VEG + HEI2015_GREENNBEAN +
                HEI2015_TOTALPRO + HEI2015_SEAPLANTPRO + HEI2015_WHOLEGRAIN + HEI2015_DAIRY +
                HEI2015_FATTYACID + HEI2015_REFINEDGRAIN + HEI2015_SODIUM + HEI2015_ADDEDSUGAR +
                HEI2015_SATFAT
        )

    for (i in 1:length(COHORT$TOTALKCAL)) {
        if (COHORT$TOTALKCAL[i] == 0) {
            COHORT$HEI2015_TOTALFRT[i] = 0
            COHORT$HEI2015_FRT[i] = 0
            COHORT$HEI2015_VEG[i] = 0
            COHORT$HEI2015_GREENNBEAN[i] = 0
            COHORT$HEI2015_TOTALPRO[i] = 0
            COHORT$HEI2015_SEAPLANTPRO[i] = 0
            COHORT$HEI2015_WHOLEGRAIN[i] = 0
            COHORT$HEI2015_DAIRY[i] = 0
            COHORT$HEI2015_FATTYACID[i] = 0
            COHORT$HEI2015_REFINEDGRAIN[i] = 0
            COHORT$HEI2015_ADDEDSUGAR[i] = 0
            COHORT$HEI2015_ALL[i] = 0
        }
    }

    print("The output is the average dietary index and its component scores for each individual per day, handling the multiple recalls or single recall for each individual accordingly.")

    COHORT %>%
        dplyr::select(
            UserName, UserID, TOTALKCAL, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
            HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
            HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
            HEI2015_SATFAT
        )
}
