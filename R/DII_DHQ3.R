#' DII_DHQ3
#'
#' Calculate the Dietary Inflammation Index (DII) for the DHQ3 data within 1 step (authors: Morgan Wolff, James Zhan)
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Total Daily Results file, ending with results.csv
#' @return The DII and its component scores
#' @examples
#' data("DHQ3_exp")
#' DII_DHQ3(DHQ3_exp)
#' @export

DII_DHQ3 = function(DATA_PATH) {
    if (is.character(DATA_PATH) == TRUE) {
        COHORT = read_csv(DATA_PATH)
    } else {
        COHORT = DATA_PATH
    }

    if ("Food ID" %in% colnames(COHORT)) {
        stop("Please use population-level data for this function. Population-level data should be like results.csv")
    }

    # create variables for DII
    COHORT = COHORT %>%
        dplyr::mutate(
            ALCOHOL = `Alcohol (g)`,
            VITAMIN_B12 = `Vitamin B12 (mcg)`,
            VITAMIN_B6 = `Vitamin B6 (mg)`,
            BETA_CAROTENE = `Beta-carotene (mcg)`,
            CAFFEINE = `Caffeine (mg)` / 1000,
            CARBOHYDRATE = `Carbohydrate (g)`,
            CHOLESTEROL = `Cholesterol (mg)`,
            ENERGY = `Energy (kcal)`,
            TOTAL_FAT = `Total fat (g)`,
            FIBER = `Dietary fiber (g)`,
            FOLIC_ACID = `Folic acid (mcg)`,
            IRON = `Iron (mg)`,
            MAGNESIUM = `Magnesium (mg)`,
            MUFA = `Total monounsaturated fatty acids (g)`,
            NIACIN = `Niacin (mg)`,
            N3_FATTY_ACID = `*Omega-3 fatty acids (g)`,
            N6_FATTY_ACID = `PFA 18:2 (Octadecadienoic) (g)` + `PFA 18:3 (Octadecatrienoic) (g)` + `PFA 20:4 (Eicosatetraenoic) (g)`,
            PROTEIN = `Protein (g)`,
            PUFA = `Total polyunsaturated fatty acids (g)`,
            RIBOFLAVIN = `Riboflavin (Vitamin B2) (mg)`,
            SATURATED_FAT = `Total saturated fatty acids (g)`,
            SELENIUM = `Selenium (mcg)`,
            THIAMIN = `Thiamin (Vitamin B1) (mg)`,
            TRANS_FAT = `*Total trans fatty acitds (g)`,
            VITAMIN_A = `*Total vitamin A activity (RE) (mcg)`,
            VITAMIN_C = `Vitamin C (mg)`,
            VITAMIN_D = `Vitamin D (D2 + D3) (mcg)`,
            VITAMIN_E = `Vitamin E as alpha-tocopherol (mg)`,
            ZINC = `Zinc (mg)`
        )

    # calculate DII
    DII_DHQ3_result = DII(
        SERV_DATA = COHORT, 
        RESPONDENTID = COHORT$`Respondent ID`, 
        REPEATNUM = 1, 
        ALCOHOL_DII = COHORT$ALCOHOL,
        VITB12_DII = COHORT$VITAMIN_B12,
        VITB6_DII = COHORT$VITAMIN_B6,
        BCAROTENE_DII = COHORT$BETA_CAROTENE,
        CAFFEINE_DII = COHORT$CAFFEINE,
        CARB_DII = COHORT$CARBOHYDRATE,
        CHOLES_DII = COHORT$CHOLESTEROL,
        KCAL_DII = COHORT$ENERGY,
        TOTALFAT_DII = COHORT$TOTAL_FAT,
        FIBER_DII = COHORT$FIBER,
        FOLICACID_DII = COHORT$FOLIC_ACID,
        IRON_DII = COHORT$IRON,
        MG_DII = COHORT$MAGNESIUM,
        MUFA_DII = COHORT$MUFA,
        NIACIN_DII = COHORT$NIACIN,
        N3FAT_DII = COHORT$N3_FATTY_ACID,
        N6FAT_DII = COHORT$N6_FATTY_ACID,
        PROTEIN_DII = COHORT$PROTEIN,
        PUFA_DII = COHORT$PUFA,
        RIBOFLAVIN_DII = COHORT$RIBOFLAVIN,
        SATFAT_DII = COHORT$SATURATED_FAT,
        SE_DII = COHORT$SELENIUM,
        THIAMIN_DII = COHORT$THIAMIN,
        TRANSFAT_DII = COHORT$TRANS_FAT,
        VITA_DII = COHORT$VITAMIN_A,
        VITC_DII = COHORT$VITAMIN_C,
        VITD_DII = COHORT$VITAMIN_D,
        VITE_DII = COHORT$VITAMIN_E,
        ZN_DII = COHORT$ZINC
    )

    # update the first column name to "Respondent ID"
    colnames(DII_DHQ3_result)[1] = "Respondent ID"

    return(DII_DHQ3_result)
}
