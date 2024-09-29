#' MED_ASA24
#'
#' Calculate the MED for the ASA24 data within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @param RECALL_SUMMARIZE Whether to summarize the food group and nutrient intake over all days reported per individual per day. Default is TRUE.
#' @return The MED and its component scores
#' @examples
#' data("ASA24_exp")
#' MED_ASA24(ASA24_exp, RECALL_SUMMARIZE = TRUE)
#' @export

MED_ASA24 = function(DATA_PATH, RECALL_SUMMARIZE = TRUE) {
    if (is.character(DATA_PATH) == TRUE) {
        COHORT = read_csv(DATA_PATH)
    } else {
        COHORT = DATA_PATH
    }

    if ("FoodCode" %in% colnames(COHORT)) {
        stop("Please use the population-level data. The file name should be like: Totals.csv")
    }

    # if RECALL_SUMMARIZE = TRUE, summarize the food group and nutrient intake over all days reported per individual per day
    if (RECALL_SUMMARIZE == TRUE) {
        message("RECALL_SUMMARIZE = TRUE, summarizing HEI2015 for ASA24 data by averaging over all possible recalls per person per day...")


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
    # if RECALL_SUMMARIZE = FALSE, keep the food group and nutrient intake over all days reported per individual per day
    else {
        message("RECALL_SUMMARIZE is FALSE, skipping summarization step...")
    }

    # Serving size calculation for MED
    COHORT = COHORT %>%
        dplyr::mutate(
            FRT_FRTJ_SERV = F_TOTAL,
            VEG_SERV = V_DRKGR + (V_REDOR_TOTAL + V_STARCHY_OTHER + V_OTHER) / 0.5,
            WGRAIN_SERV = G_WHOLE,
            LEGUMES_SERV = PF_SOY + PF_LEGUMES,
            NUTS_SERV = PF_NUTSDS,
            FISH_SERV = PF_SEAFD_HI + PF_SEAFD_LOW,
            REDPROC_MEAT_SERV = (PF_CUREDMEAT / 1.5) + ((PF_MEAT + PF_ORGAN) / 4),
            MONSATFAT_SERV = case_when(
                SFAT == 0 ~ 0,
                TRUE ~ MFAT / SFAT
            ),
            ALCOHOL_SERV = ALC,
        )

    ## Create variables and functions needed for MED
    median_healthy = function(actual) {
        median_score = median(actual)
        case_when(
            actual < median_score ~ 0,
            actual >= median_score ~ 1
        )
    }

    median_unhealthy = function(actual) {
        median_score = median(actual)
        case_when(
            actual < median_score ~ 1,
            actual >= median_score ~ 0
        )
    }

    message("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")


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
                ALCOHOL_SERV <= 25 & ALCOHOL_SERV >= 10 ~ 1,
                TRUE ~ 0
            ),
            MED_ALL = MED_FRT + MED_VEG + MED_WGRAIN + MED_LEGUMES + MED_NUTS + MED_FISH + MED_REDPROC_MEAT + MED_MONSATFAT + MED_ALCOHOL,
            MED_NOETOH = MED_FRT + MED_VEG + MED_WGRAIN + MED_LEGUMES + MED_NUTS + MED_FISH + MED_REDPROC_MEAT + MED_MONSATFAT
        ) %>%
        dplyr::select(
            UserName, UserID, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
            MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL
        )
}
