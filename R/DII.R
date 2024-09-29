#' DII
#'
#' Calculate the DII dietary index, Dietary Inflammation Index, using given the serving sizes of foods and nutrients consumed per 1 day. Not all parameters are needed. You can give as many parameters as you have, and the results will only include scores for the parameters you included.
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param REPEATNUM The number of repeated record with each participant, 1st collection=1, 2nd collection =2, etc. If no repeat number is given, the default is 1
#' @param ALCOHOL_DII Unit=g
#' @param VITB12_DII Unit=μg
#' @param VITB6_DII Unit=mg
#' @param BCAROTENE_DII Unit=μg
#' @param CAFFEINE_DII Unit=g
#' @param CARB_DII Unit=g
#' @param CHOLES_DII Unit=mg
#' @param KCAL_DII Unit=KCAL_DII
#' @param EUGENOL_DII Unit=mg
#' @param TOTALFAT_DII Unit=g
#' @param FIBER_DII Unit=g
#' @param FOLICACID_DII Unit=μg
#' @param GARLIC_DII Unit=g
#' @param GINGER_DII Unit=g
#' @param IRON_DII Unit=mg
#' @param MG_DII Unit=mg
#' @param MUFA_DII Unit=g
#' @param NIACIN_DII Unit=mg
#' @param N3FAT_DII Unit=g
#' @param N6FAT_DII Unit=g
#' @param ONION_DII Unit=g
#' @param PROTEIN_DII Unit=g
#' @param PUFA_DII Unit=g
#' @param RIBOFLAVIN_DII Unit=mg
#' @param SAFFRON_DII Unit=g
#' @param SATFAT_DII Unit=g
#' @param SE_DII Unit=μg
#' @param THIAMIN_DII Unit=mg
#' @param TRANSFAT_DII Unit=g
#' @param TURMERIC_DII Unit=mg
#' @param VITA_DII Unit=RE
#' @param VITC_DII Unit=mg
#' @param VITD_DII Unit=μg
#' @param VITE_DII Unit=mg
#' @param ZN_DII Unit=mg
#' @param TEA_DII Unit=g
#' @param FLA3OL_DII Unit=mg
#' @param FLAVONES_DII Unit=mg
#' @param FLAVONOLS_DII Unit=mg
#' @param FLAVONONES_DII Unit=mg
#' @param ANTHOC_DII Unit=mg
#' @param ISOFLAVONES_DII Unit=mg
#' @param PEPPER_DII Unit=g
#' @param THYME_DII Unit=mg
#' @param ROSEMARY_DII Unit=mg
#' @return The DII index/score
#' @examples
#' data("DHQ3_exp")
#' DII(DHQ3_exp, DHQ3_exp$`Respondent ID`, 1, DHQ3_exp$`Alcohol (g)`, DHQ3_exp$`Vitamin B12 (mcg)`, DHQ3_exp$`Vitamin B6 (mg)`)
#' @export

# Score calculation for DII

DII = function(SERV_DATA, RESPONDENTID, REPEATNUM = 1, ALCOHOL_DII = NULL, VITB12_DII = NULL, VITB6_DII = NULL, BCAROTENE_DII = NULL,
               CAFFEINE_DII = NULL, CARB_DII = NULL, CHOLES_DII = NULL, KCAL_DII = NULL, EUGENOL_DII = NULL,
               TOTALFAT_DII = NULL, FIBER_DII = NULL, FOLICACID_DII = NULL, GARLIC_DII = NULL, GINGER_DII = NULL, IRON_DII = NULL, MG_DII = NULL,
               MUFA_DII = NULL, NIACIN_DII = NULL, N3FAT_DII = NULL, N6FAT_DII = NULL, ONION_DII = NULL, PROTEIN_DII = NULL, PUFA_DII = NULL,
               RIBOFLAVIN_DII = NULL, SAFFRON_DII = NULL, SATFAT_DII = NULL, SE_DII = NULL, THIAMIN_DII = NULL, TRANSFAT_DII = NULL, TURMERIC_DII = NULL,
               VITA_DII = NULL, VITC_DII = NULL, VITD_DII = NULL, VITE_DII = NULL, ZN_DII = NULL, TEA_DII = NULL,
               FLA3OL_DII = NULL, FLAVONES_DII = NULL, FLAVONOLS_DII = NULL, FLAVONONES_DII = NULL, ANTHOC_DII = NULL, ISOFLAVONES_DII = NULL,
               PEPPER_DII = NULL, THYME_DII = NULL, ROSEMARY_DII = NULL) {
    SERV_DATA = SERV_DATA %>%
        mutate(
            RESPONDENTID = RESPONDENTID,
            REPEATNUM = REPEATNUM,
            ALCOHOL_DII = ALCOHOL_DII,
            VITB12_DII = VITB12_DII,
            VITB6_DII = VITB6_DII,
            BCAROTENE_DII = BCAROTENE_DII,
            CAFFEINE_DII = CAFFEINE_DII,
            CARB_DII = CARB_DII,
            CHOLES_DII = CHOLES_DII,
            KCAL_DII = KCAL_DII,
            EUGENOL_DII = EUGENOL_DII,
            TOTALFAT_DII = TOTALFAT_DII,
            FIBER_DII = FIBER_DII,
            FOLICACID_DII = FOLICACID_DII,
            GARLIC_DII = GARLIC_DII,
            GINGER_DII = GINGER_DII,
            IRON_DII = IRON_DII,
            MG_DII = MG_DII,
            MUFA_DII = MUFA_DII,
            NIACIN_DII = NIACIN_DII,
            N3FAT_DII = N3FAT_DII,
            N6FAT_DII = N6FAT_DII,
            ONION_DII = ONION_DII,
            PROTEIN_DII = PROTEIN_DII,
            PUFA_DII = PUFA_DII,
            RIBOFLAVIN_DII = RIBOFLAVIN_DII,
            SAFFRON_DII = SAFFRON_DII,
            SATFAT_DII = SATFAT_DII,
            SE_DII = SE_DII,
            THIAMIN_DII = THIAMIN_DII,
            TRANSFAT_DII = TRANSFAT_DII,
            TURMERIC_DII = TURMERIC_DII,
            VITA_DII = VITA_DII,
            VITC_DII = VITC_DII,
            VITD_DII = VITD_DII,
            VITE_DII = VITE_DII,
            ZN_DII = ZN_DII,
            TEA_DII = TEA_DII,
            FLA3OL_DII = FLA3OL_DII,
            FLAVONES_DII = FLAVONES_DII,
            FLAVONOLS_DII = FLAVONOLS_DII,
            FLAVONONES_DII = FLAVONONES_DII,
            ANTHOC_DII = ANTHOC_DII,
            ISOFLAVONES_DII = ISOFLAVONES_DII,
            PEPPER_DII = PEPPER_DII,
            THYME_DII = THYME_DII,
            ROSEMARY_DII = ROSEMARY_DII
        )

    message("It is normal to see warnings if you do not provide all arguments using DII. The algorithm will only count the arguments you enter to calculate the DII. All warnings are about the first column you don't use. For example, if you only entered alcohol, vitamin b12, and vitamin b6, all warnings would remind you that bcarotene is not found.")

    COHORT = SERV_DATA %>%
        dplyr::select(
            RESPONDENTID, REPEATNUM, ALCOHOL_DII, VITB12_DII, VITB6_DII, BCAROTENE_DII, CAFFEINE_DII, CARB_DII, CHOLES_DII, KCAL_DII, EUGENOL_DII,
            TOTALFAT_DII, FIBER_DII, FOLICACID_DII, GARLIC_DII, GINGER_DII, IRON_DII, MG_DII, MUFA_DII, NIACIN_DII, N3FAT_DII, N6FAT_DII, ONION_DII, PROTEIN_DII, PUFA_DII,
            RIBOFLAVIN_DII, SAFFRON_DII, SATFAT_DII, SE_DII, THIAMIN_DII, TRANSFAT_DII, TURMERIC_DII, VITA_DII, VITC_DII, VITD_DII, VITE_DII, ZN_DII, TEA_DII,
            FLA3OL_DII, FLAVONES_DII, FLAVONOLS_DII, FLAVONONES_DII, ANTHOC_DII, ISOFLAVONES_DII, PEPPER_DII, THYME_DII, ROSEMARY_DII
        ) %>%
        tidyr::pivot_longer(-c(RESPONDENTID, REPEATNUM), names_to = "Variable", values_to = "Value")

    Variable = c(
        "ALCOHOL_DII", "VITB12_DII", "VITB6_DII", "BCAROTENE_DII", "CAFFEINE_DII", "CARB_DII", "CHOLES_DII", "KCAL_DII", "EUGENOL_DII",
        "TOTALFAT_DII", "FIBER_DII", "FOLICACID_DII", "GARLIC_DII", "GINGER_DII", "IRON_DII", "MG_DII", "MUFA_DII", "NIACIN_DII", "N3FAT_DII", "N6FAT_DII", "ONION_DII", "PROTEIN_DII", "PUFA_DII",
        "RIBOFLAVIN_DII", "SAFFRON_DII", "SATFAT_DII", "SE_DII", "THIAMIN_DII", "TRANSFAT_DII", "TURMERIC_DII", "VITA_DII", "VITC_DII", "VITD_DII", "VITE_DII", "ZN_DII", "TEA_DII",
        "FLA3OL_DII", "FLAVONES_DII", "FLAVONOLS_DII", "FLAVONONES_DII", "ANTHOC_DII", "ISOFLAVONES_DII", "PEPPER_DII", "THYME_DII", "ROSEMARY_DII"
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

    DII_STD = base::data.frame(Variable, Overall_inflammatory_score, Global_mean, SD)

    # Score calculation for DII

    COHORT = COHORT %>%
        dplyr::inner_join(DII_STD, by = c("Variable")) %>%
        dplyr::mutate(
            Z_SCORE = (Value - Global_mean) / SD,
            PERCENTILE = pnorm(Z_SCORE) * 2 - 1,
            IND_DII_SCORE = PERCENTILE * Overall_inflammatory_score
        ) %>%
        tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
        dplyr::group_by(RESPONDENTID, REPEATNUM) %>%
        dplyr::summarize(
            ALCOHOL_DII = base::sum(ALCOHOL_DII, na.rm = TRUE),
            VITB12_DII = base::sum(VITB12_DII, na.rm = TRUE),
            VITB6_DII = base::sum(VITB6_DII, na.rm = TRUE),
            BCAROTENE_DII = base::sum(BCAROTENE_DII, na.rm = TRUE),
            CAFFEINE_DII = base::sum(CAFFEINE_DII, na.rm = TRUE),
            CARB_DII = base::sum(CARB_DII, na.rm = TRUE),
            CHOLES_DII = base::sum(CHOLES_DII, na.rm = TRUE),
            KCAL_DII = base::sum(KCAL_DII, na.rm = TRUE),
            EUGENOL_DII = base::sum(EUGENOL_DII, na.rm = TRUE),
            TOTALFAT_DII = base::sum(TOTALFAT_DII, na.rm = TRUE),
            FIBER_DII = base::sum(FIBER_DII, na.rm = TRUE),
            FOLICACID_DII = base::sum(FOLICACID_DII, na.rm = TRUE),
            GARLIC_DII = base::sum(GARLIC_DII, na.rm = TRUE),
            GINGER_DII = base::sum(GINGER_DII, na.rm = TRUE),
            IRON_DII = base::sum(IRON_DII, na.rm = TRUE),
            MG_DII = base::sum(MG_DII, na.rm = TRUE),
            MUFA_DII = base::sum(MUFA_DII, na.rm = TRUE),
            NIACIN_DII = base::sum(NIACIN_DII, na.rm = TRUE),
            N3FAT_DII = base::sum(N3FAT_DII, na.rm = TRUE),
            N6FAT_DII = base::sum(N6FAT_DII, na.rm = TRUE),
            ONION_DII = base::sum(ONION_DII, na.rm = TRUE),
            PROTEIN_DII = base::sum(PROTEIN_DII, na.rm = TRUE),
            PUFA_DII = base::sum(PUFA_DII, na.rm = TRUE),
            RIBOFLAVIN_DII = base::sum(RIBOFLAVIN_DII, na.rm = TRUE),
            SAFFRON_DII = base::sum(SAFFRON_DII, na.rm = TRUE),
            SATFAT_DII = base::sum(SATFAT_DII, na.rm = TRUE),
            SE_DII = base::sum(SE_DII, na.rm = TRUE),
            THIAMIN_DII = base::sum(THIAMIN_DII, na.rm = TRUE),
            TRANSFAT_DII = base::sum(TRANSFAT_DII, na.rm = TRUE),
            TURMERIC_DII = base::sum(TURMERIC_DII, na.rm = TRUE),
            VITA_DII = base::sum(VITA_DII, na.rm = TRUE),
            VITC_DII = base::sum(VITC_DII, na.rm = TRUE),
            VITD_DII = base::sum(VITD_DII, na.rm = TRUE),
            VITE_DII = base::sum(VITE_DII, na.rm = TRUE),
            ZN_DII = base::sum(ZN_DII, na.rm = TRUE),
            TEA_DII = base::sum(TEA_DII, na.rm = TRUE),
            FLA3OL_DII = base::sum(FLA3OL_DII, na.rm = TRUE),
            FLAVONES_DII = base::sum(FLAVONES_DII, na.rm = TRUE),
            FLAVONOLS_DII = base::sum(FLAVONOLS_DII, na.rm = TRUE),
            FLAVONONES_DII = base::sum(FLAVONONES_DII, na.rm = TRUE),
            ANTHOC_DII = base::sum(ANTHOC_DII, na.rm = TRUE),
            ISOFLAVONES_DII = base::sum(ISOFLAVONES_DII, na.rm = TRUE),
            PEPPER_DII = base::sum(PEPPER_DII, na.rm = TRUE),
            THYME_DII = base::sum(THYME_DII, na.rm = TRUE),
            ROSEMARY_DII = base::sum(ROSEMARY_DII, na.rm = TRUE),
        )

    for (i in 1:length(COHORT$RESPONDENTID)) {
        if (is.null(SERV_DATA$ALCOHOL_DII) == TRUE) {
            COHORT$ALCOHOL_DII[i] = 0
        } else if (is.null(SERV_DATA$VITB12_DII) == TRUE) {
            COHORT$VITB12_DII[i] = 0
        } else if (is.null(SERV_DATA$VITB6_DII) == TRUE) {
            COHORT$VITB6_DII[i] = 0
        } else if (is.null(SERV_DATA$BCAROTENE_DII) == TRUE) {
            COHORT$BCAROTENE_DII[i] = 0
        } else if (is.null(SERV_DATA$CAFFEINE_DII) == TRUE) {
            COHORT$CAFFEINE_DII[i] = 0
        } else if (is.null(SERV_DATA$CARB_DII) == TRUE) {
            COHORT$CARB_DII[i] = 0
        } else if (is.null(SERV_DATA$CHOLES_DII) == TRUE) {
            COHORT$CHOLES_DII[i] = 0
        } else if (is.null(SERV_DATA$KCAL_DII) == TRUE) {
            COHORT$KCAL_DII[i] = 0
        } else if (is.null(SERV_DATA$EUGENOL_DII) == TRUE) {
            COHORT$EUGENOL_DII[i] = 0
        } else if (is.null(SERV_DATA$TOTALFAT_DII) == TRUE) {
            COHORT$TOTALFAT_DII[i] = 0
        } else if (is.null(SERV_DATA$FIBER_DII) == TRUE) {
            COHORT$FIBER_DII[i] = 0
        } else if (is.null(SERV_DATA$FOLICACID_DII) == TRUE) {
            COHORT$FOLICACID_DII[i] = 0
        } else if (is.null(SERV_DATA$GARLIC_DII) == TRUE) {
            COHORT$GARLIC_DII[i] = 0
        } else if (is.null(SERV_DATA$GINGER_DII) == TRUE) {
            COHORT$GINGER_DII[i] = 0
        } else if (is.null(SERV_DATA$IRON_DII) == TRUE) {
            COHORT$IRON_DII[i] = 0
        } else if (is.null(SERV_DATA$MG_DII) == TRUE) {
            COHORT$MG_DII[i] = 0
        } else if (is.null(SERV_DATA$MUFA_DII) == TRUE) {
            COHORT$MUFA_DII[i] = 0
        } else if (is.null(SERV_DATA$NIACIN_DII) == TRUE) {
            COHORT$NIACIN_DII[i] = 0
        } else if (is.null(SERV_DATA$N3FAT_DII) == TRUE) {
            COHORT$N3FAT_DII[i] = 0
        } else if (is.null(SERV_DATA$N6FAT_DII) == TRUE) {
            COHORT$N6FAT_DII[i] = 0
        } else if (is.null(SERV_DATA$ONION_DII) == TRUE) {
            COHORT$ONION_DII[i] = 0
        } else if (is.null(SERV_DATA$PROTEIN_DII) == TRUE) {
            COHORT$PROTEIN_DII[i] = 0
        } else if (is.null(SERV_DATA$PUFA_DII) == TRUE) {
            COHORT$PUFA_DII[i] = 0
        } else if (is.null(SERV_DATA$RIBOFLAVIN_DII) == TRUE) {
            COHORT$RIBOFLAVIN_DII[i] = 0
        } else if (is.null(SERV_DATA$SAFFRON_DII) == TRUE) {
            COHORT$SAFFRON_DII[i] = 0
        } else if (is.null(SERV_DATA$SATFAT_DII) == TRUE) {
            COHORT$SATFAT_DII[i] = 0
        } else if (is.null(SERV_DATA$SE_DII) == TRUE) {
            COHORT$SE_DII[i] = 0
        } else if (is.null(SERV_DATA$THIAMIN_DII) == TRUE) {
            COHORT$THIAMIN_DII[i] = 0
        } else if (is.null(SERV_DATA$TRANSFAT_DII) == TRUE) {
            COHORT$TRANSFAT_DII[i] = 0
        } else if (is.null(SERV_DATA$TURMERIC_DII) == TRUE) {
            COHORT$TURMERIC_DII[i] = 0
        } else if (is.null(SERV_DATA$VITA_DII) == TRUE) {
            COHORT$VITA_DII[i] = 0
        } else if (is.null(SERV_DATA$VITC_DII) == TRUE) {
            COHORT$VITC_DII[i] = 0
        } else if (is.null(SERV_DATA$VITD_DII) == TRUE) {
            COHORT$VITD_DII[i] = 0
        } else if (is.null(SERV_DATA$VITE_DII) == TRUE) {
            COHORT$VITE_DII[i] = 0
        } else if (is.null(SERV_DATA$ZN_DII) == TRUE) {
            COHORT$ZN_DII[i] = 0
        } else if (is.null(SERV_DATA$TEA_DII) == TRUE) {
            COHORT$TEA_DII[i] = 0
        } else if (is.null(SERV_DATA$FLA3OL_DII) == TRUE) {
            COHORT$FLA3OL_DII[i] = 0
        } else if (is.null(SERV_DATA$FLAVONES_DII) == TRUE) {
            COHORT$FLAVONES_DII[i] = 0
        } else if (is.null(SERV_DATA$FLAVONOLS_DII) == TRUE) {
            COHORT$FLAVONOLS_DII[i] = 0
        } else if (is.null(SERV_DATA$FLAVONONES_DII) == TRUE) {
            COHORT$FLAVONONES_DII[i] = 0
        } else if (is.null(SERV_DATA$ANTHOC_DII) == TRUE) {
            COHORT$ANTHOC_DII[i] = 0
        } else if (is.null(SERV_DATA$ISOFLAVONES_DII) == TRUE) {
            COHORT$ISOFLAVONES_DII[i] = 0
        } else if (is.null(SERV_DATA$PEPPER_DII) == TRUE) {
            COHORT$PEPPER_DII[i] = 0
        } else if (is.null(SERV_DATA$THYME_DII) == TRUE) {
            COHORT$THYME_DII[i] = 0
        } else if (is.null(SERV_DATA$ROSEMARY_DII) == TRUE) {
            COHORT$ROSEMARY_DII[i] = 0
        }
    }


    COHORT %>%
        dplyr::mutate(
            DII_ALL = ALCOHOL_DII + VITB12_DII + VITB6_DII + BCAROTENE_DII + CAFFEINE_DII + CARB_DII + CHOLES_DII + KCAL_DII + EUGENOL_DII +
                TOTALFAT_DII + FIBER_DII + FOLICACID_DII + GARLIC_DII + GINGER_DII + IRON_DII + MG_DII + MUFA_DII + NIACIN_DII + N3FAT_DII + N6FAT_DII + ONION_DII + PROTEIN_DII + PUFA_DII +
                RIBOFLAVIN_DII + SAFFRON_DII + SATFAT_DII + SE_DII + THIAMIN_DII + TRANSFAT_DII + TURMERIC_DII + VITA_DII + VITC_DII + VITD_DII + VITE_DII + ZN_DII + TEA_DII +
                FLA3OL_DII + FLAVONES_DII + FLAVONOLS_DII + FLAVONONES_DII + ANTHOC_DII + ISOFLAVONES_DII + PEPPER_DII + THYME_DII + ROSEMARY_DII,
            DII_NOETOH = VITB12_DII + VITB6_DII + BCAROTENE_DII + CAFFEINE_DII + CARB_DII + CHOLES_DII + KCAL_DII + EUGENOL_DII +
                TOTALFAT_DII + FIBER_DII + FOLICACID_DII + GARLIC_DII + GINGER_DII + IRON_DII + MG_DII + MUFA_DII + NIACIN_DII + N3FAT_DII + N6FAT_DII + ONION_DII + PROTEIN_DII + PUFA_DII +
                RIBOFLAVIN_DII + SAFFRON_DII + SATFAT_DII + SE_DII + THIAMIN_DII + TRANSFAT_DII + TURMERIC_DII + VITA_DII + VITC_DII + VITD_DII + VITE_DII + ZN_DII + TEA_DII +
                FLA3OL_DII + FLAVONES_DII + FLAVONOLS_DII + FLAVONONES_DII + ANTHOC_DII + ISOFLAVONES_DII + PEPPER_DII + THYME_DII + ROSEMARY_DII,
        ) %>%
        dplyr::select(RESPONDENTID, DII_ALL, DII_NOETOH, everything())
}
