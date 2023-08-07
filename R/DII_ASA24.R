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

DII_ASA24 = function(DATA_PATH) {
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


    # Serving size calculation for DII
    COHORT1 = COHORT %>%
        dplyr::mutate(
            ALCOHOL = ALC,
            VITB12 = VB12,
            VITB6 = VB6,
            BCAROTENE = BCAR,
            CAFFEINE = CAFF / 1000,
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
        dplyr::select(
            UserName, UserID, ALCOHOL, VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
            IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
            VITC, VITD, VITE, ZN
        )

    COHORT2 = COHORT1 %>%
        tidyr::pivot_longer(-c(UserName, UserID), names_to = "Variable", values_to = "Value")

    Variable = c(
        "ALCOHOL", "VITB12", "VITB6", "BCAROTENE", "CAFFEINE", "CARB", "CHOLES", "KCAL", "EUGENOL",
        "TOTALFAT", "FIBER", "FOLICACID", "GARLIC", "GINGER", "IRON", "MG", "MUFA", "NIACIN", "N3FAT", "N6FAT", "ONION", "PROTEIN", "PUFA",
        "RIBOFLAVIN", "SAFFRON", "SATFAT", "SE", "THIAMIN", "TRANSFAT", "TURMERIC", "VITA", "VITC", "VITD", "VITE", "ZN", "TEA",
        "FLA3OL", "FLAVONES", "FLAVONOLS", "FLAVONONES", "ANTHOC", "ISOFLAVONES", "PEPPER", "THYME", "ROSEMARY"
    )

    Overall_inflammatory_score = c(
        -0.278, 0.106, -0.365, -0.584, -0.11, 0.097, 0.11, 0.18, -0.14, 0.298, -0.663, -0.19, -0.412, -0.453, 0.032, -0.484, -0.009,
        -0.246, -0.436, -0.159, -0.301, 0.021, -0.337, -0.068, -0.14, 0.373, -0.191, -0.098, 0.229, -0.785, -0.401, -0.424, -0.446, -0.419, -0.313,
        -0.536, -0.415, -0.616, -0.467, -0.25, -0.131, -0.593, -0.131, -0.102, -0.013
    )

    Global_mean = c(
        13.98, 5.15, 1.47, 3718, 8.05, 272.2, 279.4, 2056, 0.01, 71.4, 18.8, 273, 4.35, 59, 13.35, 310.1, 27, 25.9, 1.06, 10.8, 35.9,
        79.4, 13.88, 1.7, 0.37, 28.6, 67, 1.7, 3.15, 533.6, 983.9, 118.2, 6.26, 8.73, 9.84,
        1.69, 95.8, 1.55, 17.7, 11.7, 18.05, 1.2, 10, 0.33, 1
    )

    SD = c(
        3.72, 2.7, 0.74, 1720, 6.67, 40, 51.2, 338, 0.08, 19.4, 4.9, 70.7, 2.9, 63.2, 3.71, 139.4, 6.1, 11.77, 1.06, 7.5, 18.4, 13.9, 3.76, 0.79, 1.78,
        8, 25.1, 0.66, 3.75, 754.3, 518.6, 43.46, 2.21, 1.49, 2.19,
        1.53, 85.9, 0.07, 6.79, 3.82, 21.14, 0.2, 7.07, 0.99, 15
    )

    DII_STD = data.frame(Variable, Overall_inflammatory_score, Global_mean, SD)

    print("The output is the average dietary index and its component scores for each individual per day, handling the multiple recalls or single recall for each individual accordingly.")


    # Score calculation for DII
    COHORT2 %>%
        dplyr::inner_join(DII_STD, by = c("Variable")) %>%
        dplyr::mutate(
            Z_SCORE = (Value - Global_mean) / SD,
            PERCENTILE = pnorm(Z_SCORE) * 2 - 1,
            IND_DII_SCORE = PERCENTILE * Overall_inflammatory_score
        ) %>%
        tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
        dplyr::group_by(UserName, UserID) %>%
        dplyr::summarize(
            DII_ALL = sum(ALCOHOL, VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
                IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
                VITC, VITD, VITE, ZN,
                na.rm = TRUE
            ),
            DII_NOETOH = sum(VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
                IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
                VITC, VITD, VITE, ZN,
                na.rm = TRUE
            ),
            ALCOHOL = sum(ALCOHOL, na.rm = TRUE),
            VITB12 = sum(VITB12, na.rm = TRUE),
            VITB6 = sum(VITB6, na.rm = TRUE),
            BCAROTENE = sum(BCAROTENE, na.rm = TRUE),
            CAFFEINE = sum(CAFFEINE, na.rm = TRUE),
            CARB = sum(CARB, na.rm = TRUE),
            CHOLES = sum(CHOLES, na.rm = TRUE),
            KCAL = sum(KCAL, na.rm = TRUE),
            TOTALFAT = sum(TOTALFAT, na.rm = TRUE),
            FIBER = sum(FIBER, na.rm = TRUE),
            FOLICACID = sum(FOLICACID, na.rm = TRUE),
            IRON = sum(IRON, na.rm = TRUE),
            MG = sum(MG, na.rm = TRUE),
            MUFA = sum(MUFA, na.rm = TRUE),
            NIACIN = sum(NIACIN, na.rm = TRUE),
            N3FAT = sum(N3FAT, na.rm = TRUE),
            N6FAT = sum(N6FAT, na.rm = TRUE),
            PROTEIN = sum(PROTEIN, na.rm = TRUE),
            PUFA = sum(PUFA, na.rm = TRUE),
            RIBOFLAVIN = sum(RIBOFLAVIN, na.rm = TRUE),
            SATFAT = sum(SATFAT, na.rm = TRUE),
            SE = sum(SE, na.rm = TRUE),
            THIAMIN = sum(THIAMIN, na.rm = TRUE),
            VITA = sum(VITA, na.rm = TRUE),
            VITC = sum(VITC, na.rm = TRUE),
            VITD = sum(VITD, na.rm = TRUE),
            VITE = sum(VITE, na.rm = TRUE),
            ZN = sum(ZN, na.rm = TRUE)
        )
}
