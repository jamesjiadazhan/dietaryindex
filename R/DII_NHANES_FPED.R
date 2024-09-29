#' DII_NHANES_FPED
#'
#' Calculate the DII for the NHANES_FPED data (after 2005) within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fpre_dr1tot_1718.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @return The DII and its component scores: DII is the total DII score (the sum of all component scores); DII_NOETOH is the DII score without alcohol
#' @examples
#' data("NHANES_20172018")
#' DII_NHANES_FPED(FPED_PATH = NHANES_20172018$FPED, NUTRIENT_PATH = NHANES_20172018$NUTRIENT, DEMO_PATH = NHANES_20172018$DEMO, FPED_PATH2 = NHANES_20172018$FPED2, NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2)
#' @export

DII_NHANES_FPED = function(FPED_PATH = NULL, NUTRIENT_PATH = NULL, DEMO_PATH, FPED_PATH2 = NULL, NUTRIENT_PATH2 = NULL) {
    # stop if the input data is not provided for any day
    if (is.null(FPED_PATH) & is.null(NUTRIENT_PATH) & is.null(FPED_PATH2) & is.null(NUTRIENT_PATH2)) {
        stop("Please provide the file path for the FPED and NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

    # Load the DII internal database for the calculation
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

    # first day data calculation if the user provides the first day data for FPED_PATH and NUTRIENT_PATH
    if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH)) {
        if (is.character(FPED_PATH) == TRUE) {
            FPED = read_sas(FPED_PATH)
        } else {
            FPED = FPED_PATH
        }

        if (is.character(NUTRIENT_PATH) == TRUE) {
            NUTRIENT = read_xpt(NUTRIENT_PATH)
        } else {
            NUTRIENT = NUTRIENT_PATH
        }

        if (is.character(DEMO_PATH) == TRUE) {
            DEMO = read_xpt(DEMO_PATH)
        } else {
            DEMO = DEMO_PATH
        }

        if ("DR1ILINE" %in% colnames(FPED) | "DR1ILINE" %in% colnames(NUTRIENT)) {
            stop("Please use the population-level first day data. The file name should be like: Totals.csv")
        }

        # Select only the high quality data
        NUTRIENT = NUTRIENT %>%
            filter(DR1DRSTZ == 1) %>%
            arrange(SEQN)

        DEMO = DEMO %>%
            filter(RIDAGEYR >= 2) %>%
            dplyr::select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
            arrange(SEQN)

        FPED = FPED %>%
            arrange(SEQN)

        # Merge the demographic data with the nutrient and FPED data
        COHORT = NUTRIENT %>%
            inner_join(DEMO, by = c("SEQN" = "SEQN")) %>%
            left_join(FPED, by = c("SEQN" = "SEQN"))

        # Check if DR1TVD exists in the data frame
        has_DR1TVD <- "DR1TVD" %in% colnames(COHORT)

        # Serving size calculation for DII
        COHORT = COHORT %>%
            filter(DR1TKCAL > 0) %>%
            dplyr::mutate(
                ALCOHOL = DR1TALCO,
                VITB12 = DR1TVB12,
                VITB6 = DR1TVB6,
                BCAROTENE = DR1TBCAR,
                CAFFEINE = DR1TCAFF / 1000,
                CARB = DR1TCARB,
                CHOLES = DR1TCHOL,
                KCAL = DR1TKCAL,
                TOTALFAT = DR1TTFAT,
                FIBER = DR1TFIBE,
                FOLICACID = DR1TFA,
                IRON = DR1TIRON,
                MG = DR1TMAGN,
                MUFA = DR1TMFAT,
                NIACIN = DR1TNIAC,
                N3FAT = DR1TP183 + DR1TP184 + DR1TP205 + DR1TP225 + DR1TP226,
                N6FAT = DR1TP183 + DR1TP204,
                PROTEIN = DR1TPROT,
                PUFA = DR1TPFAT,
                RIBOFLAVIN = DR1TVB2,
                SATFAT = DR1TSFAT,
                SE = DR1TSELE,
                THIAMIN = DR1TVB1,
                VITA = DR1TVARA,
                VITC = DR1TVC,
                VITD = if (has_DR1TVD) DR1TVD * 0.025 else NULL,
                VITE = DR1TATOC,
                ZN = DR1TZINC
            )

        # Columns to select
        select_cols <- c(
            "SEQN", "ALCOHOL", "VITB12", "VITB6", "BCAROTENE", "CAFFEINE", "CARB", "CHOLES", "KCAL", "TOTALFAT", "FIBER", "FOLICACID",
            "IRON", "MG", "MUFA", "NIACIN", "N3FAT", "N6FAT", "PROTEIN", "PUFA", "RIBOFLAVIN", "SATFAT", "SE", "THIAMIN",
            "VITA", "VITC", "VITE", "ZN"
        )

        # Include VITD if it exists
        if (has_DR1TVD) {
            select_cols <- c(select_cols, "VITD")
            message("VITD is included in the calculation in the first day of NHANES data.")
        } else {
            message("VITD is not included in the calculation in the first day of NHANES data.")
        }


        COHORT = COHORT %>%
            # Select only the necessary columns
            dplyr::select(one_of(select_cols)) %>%
            # transform the data frame from wide to long
            tidyr::pivot_longer(-SEQN, names_to = "Variable", values_to = "Value")

        # Select all the columns except SEQN
        component_variable = setdiff(select_cols, "SEQN")

        # Score calculation for DII
        COHORT = COHORT %>%
            dplyr::inner_join(DII_STD, by = c("Variable")) %>%
            dplyr::mutate(
                Z_SCORE = (Value - Global_mean) / SD,
                PERCENTILE = pnorm(Z_SCORE) * 2 - 1,
                IND_DII_SCORE = PERCENTILE * Overall_inflammatory_score
            ) %>%
            tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
            dplyr::group_by(SEQN) %>%
            dplyr::summarize(
                DII_ALL = sum(across(all_of(component_variable), \(x) sum(x, na.rm = TRUE))),
                DII_NOETOH = sum(across(all_of(setdiff(component_variable, "ALCOHOL")), \(x) sum(x, na.rm = TRUE))),
                across(all_of(component_variable), \(x) sum(x, na.rm = TRUE))
            )
    }

    # the second day data calculation if the user provides the second day data for FPED_PATH2 and NUTRIENT_PATH2
    if (!is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2)) {
        if (is.character(FPED_PATH2) == TRUE) {
            FPED2 = read_sas(FPED_PATH2)
        } else {
            FPED2 = FPED_PATH2
        }

        if (is.character(NUTRIENT_PATH2) == TRUE) {
            NUTRIENT2 = read_xpt(NUTRIENT_PATH2)
        } else {
            NUTRIENT2 = NUTRIENT_PATH2
        }

        if (is.character(DEMO_PATH) == TRUE) {
            DEMO = read_xpt(DEMO_PATH)
        } else {
            DEMO = DEMO_PATH
        }

        if ("DR2ILINE" %in% colnames(FPED2) | "DR2ILINE" %in% colnames(NUTRIENT2)) {
            stop("Please use the population-level second day data. The file name should be like: Totals.csv")
        }

        NUTRIENT2 = NUTRIENT2 %>%
            filter(DR2DRSTZ == 1) %>%
            arrange(SEQN)

        DEMO = DEMO %>%
            filter(RIDAGEYR >= 2) %>%
            dplyr::select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
            arrange(SEQN)

        FPED2 = FPED2 %>%
            arrange(SEQN)

        COHORT2 = NUTRIENT2 %>%
            inner_join(DEMO, by = c("SEQN" = "SEQN")) %>%
            left_join(FPED2, by = c("SEQN" = "SEQN"))

        # Check if DR2TVD exists in the data frame
        has_DR2TVD <- "DR2TVD" %in% colnames(COHORT2)

        # Serving size calculation for DII
        COHORT2 = COHORT2 %>%
            filter(DR2TKCAL > 0) %>%
            dplyr::mutate(
                ALCOHOL = DR2TALCO,
                VITB12 = DR2TVB12,
                VITB6 = DR2TVB6,
                BCAROTENE = DR2TBCAR,
                CAFFEINE = DR2TCAFF / 1000,
                CARB = DR2TCARB,
                CHOLES = DR2TCHOL,
                KCAL = DR2TKCAL,
                TOTALFAT = DR2TTFAT,
                FIBER = DR2TFIBE,
                FOLICACID = DR2TFA,
                IRON = DR2TIRON,
                MG = DR2TMAGN,
                MUFA = DR2TMFAT,
                NIACIN = DR2TNIAC,
                N3FAT = DR2TP183 + DR2TP184 + DR2TP205 + DR2TP225 + DR2TP226,
                N6FAT = DR2TP183 + DR2TP204,
                PROTEIN = DR2TPROT,
                PUFA = DR2TPFAT,
                RIBOFLAVIN = DR2TVB2,
                SATFAT = DR2TSFAT,
                SE = DR2TSELE,
                THIAMIN = DR2TVB1,
                VITA = DR2TVARA,
                VITC = DR2TVC,
                VITD = if (has_DR2TVD) DR2TVD * 0.025 else NULL,
                VITE = DR2TATOC,
                ZN = DR2TZINC
            )

        # Columns to select
        select_cols <- c(
            "SEQN", "ALCOHOL", "VITB12", "VITB6", "BCAROTENE", "CAFFEINE", "CARB", "CHOLES", "KCAL", "TOTALFAT", "FIBER", "FOLICACID",
            "IRON", "MG", "MUFA", "NIACIN", "N3FAT", "N6FAT", "PROTEIN", "PUFA", "RIBOFLAVIN", "SATFAT", "SE", "THIAMIN",
            "VITA", "VITC", "VITE", "ZN"
        )

        # Include VITD if it exists
        if (has_DR2TVD) {
            select_cols <- c(select_cols, "VITD")
            message("VITD is included in the calculation in the second day of NHANES data.")
        } else {
            message("VITD is not included in the calculation in the second day of NHANES data.")
        }


        COHORT2 = COHORT2 %>%
            # Select only the necessary columns
            dplyr::select(one_of(select_cols)) %>%
            # transform the data frame from wide to long
            tidyr::pivot_longer(-SEQN, names_to = "Variable", values_to = "Value")

        # Select all the columns except SEQN
        component_variable = setdiff(select_cols, "SEQN")

        # Score calculation for DII
        COHORT2 = COHORT2 %>%
            dplyr::inner_join(DII_STD, by = c("Variable")) %>%
            dplyr::mutate(
                Z_SCORE = (Value - Global_mean) / SD,
                PERCENTILE = pnorm(Z_SCORE) * 2 - 1,
                IND_DII_SCORE = PERCENTILE * Overall_inflammatory_score
            ) %>%
            tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
            dplyr::group_by(SEQN) %>%
            dplyr::summarize(
                DII_ALL = sum(across(all_of(component_variable), \(x) sum(x, na.rm = TRUE))),
                DII_NOETOH = sum(across(all_of(setdiff(component_variable, "ALCOHOL")), \(x) sum(x, na.rm = TRUE))),
                across(all_of(component_variable), \(x) sum(x, na.rm = TRUE))
            )
    }

    if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH) & is.null(FPED_PATH2) & is.null(NUTRIENT_PATH2)) {
        # message a reminder that this function does not use all the original DII variables
        message("Reminder: This function does not use all the original DII variables. Eugenol, garlic, ginger, onion, trans fat, turmeric, Green/black tea, Flavan-3-ol, Flavones, Flavonols, Flavonones, Anthocyanidins, Isoflavones, Pepper, Thyme/oregano, Rosemary are not included because they are not available in NHANES.")
        return(COHORT)
    }

    if (is.null(FPED_PATH) & is.null(NUTRIENT_PATH) & !is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2)) {
        # message a reminder that this function does not use all the original DII variables
        message("Reminder: This function does not use all the original DII variables. Eugenol, garlic, ginger, onion, trans fat, turmeric, Green/black tea, Flavan-3-ol, Flavones, Flavonols, Flavonones, Anthocyanidins, Isoflavones, Pepper, Thyme/oregano, Rosemary are not included because they are not available in NHANES.")
        return(COHORT2)
    }

    # Check which columns exist in both COHORT and COHORT2
    common_cols <- intersect(colnames(COHORT), colnames(COHORT2))

    # Remove 'SEQN' as it is the joining key, not an average-able variable
    common_cols <- setdiff(common_cols, "SEQN")

    # Initialize an empty data frame for the joined and averaged data
    COHORT12 <- data.frame()

    # Perform the join and averaging only if both data sets are non-null and have common columns
    if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH) & !is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2) & length(common_cols) > 0) {
        # Perform inner join to merge the two data sets
        COHORT12 <- inner_join(COHORT, COHORT2, by = "SEQN")

        # Dynamically generate the mutate expressions for averaging the common columns between the two data sets
        avg_exprs <- setNames(lapply(common_cols, function(col) {
            rlang::parse_expr(paste0(col, " = (", col, ".x + ", col, ".y) / 2"))
        }), common_cols)

        # Perform the averaging calculations
        COHORT12 <- COHORT12 %>%
            mutate(!!!avg_exprs)

        # Explicitly select the columns of interest
        COHORT12 <- COHORT12 %>%
            dplyr::select(SEQN, !!!common_cols)

        # message a reminder that this function does not use all the original DII variables
        message("Reminder: This function does not use all the original DII variables. Eugenol, garlic, ginger, onion, trans fat, turmeric, Green/black tea, Flavan-3-ol, Flavones, Flavonols, Flavonones, Anthocyanidins, Isoflavones, Pepper, Thyme/oregano, Rosemary are not included because they are not available in NHANES.")
        return(COHORT12)
    }
}
