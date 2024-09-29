#' DII_NHANES_MPED
#'
#' Calculate the DII for the NHANES_MPED data (before 2005, 1999-2004) within 1 step for day 1, day 2, or day 1 and 2 combined (age >= 2 only)
#' @import dplyr
#' @import readr
#' @import haven
#' @param MPED_PER_100_GRAM_PATH The file path for the MPED per 100 gram data for the day 1 and day 2 data. The file name should be like: pyr_tot_d1.sas7bdat
#' @param WJFRT The file path for the WJFRT data for the day 1 and day2 data. The file name should be like: wjfrt.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data for the day 1 data. The file name should be like: DR1TOT_J.XPT or DRXTOT_B.XPT
#' @param NUTRIENT_IND_PATH The file path for the NUTRIENT_IND data for the day 1 data. The file name should be like: DR1IFF_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @param NUTRIENT_PATH2 The file path for the NUTRIENT2 data for the day 2 data. The file name should be like: DR2TOT_J.XPT
#' @param NUTRIENT_IND_PATH2 The file path for the NUTRIENT_IND2 data for the day 2 data The file name should be like: DR2IFF_J.XPT
#' @return The DII and its component scores and serving sizes
#' @examples
#' data("NHANES_20032004")
#' DII_NHANES_MPED(MPED_PER_100_GRAM_PATH = NHANES_20032004$MPED_PER_100_GRAM, WJFRT = NHANES_20032004$WJFRT, NUTRIENT_PATH = NHANES_20032004$NUTRIENT, NUTRIENT_IND_PATH = NHANES_20032004$NUTRIENT_IND, DEMO_PATH = NHANES_20032004$DEMO, NUTRIENT_PATH2 = NHANES_20032004$NUTRIENT2, NUTRIENT_IND_PATH2 = NHANES_20032004$NUTRIENT_IND2)
#' @export

DII_NHANES_MPED = function(MPED_PER_100_GRAM_PATH = NULL, WJFRT = NULL, NUTRIENT_PATH = NULL, NUTRIENT_IND_PATH = NULL, DEMO_PATH, NUTRIENT_PATH2 = NULL, NUTRIENT_IND_PATH2 = NULL) {
    ## Create variables needed for DII calculation
    DII_MIN = 0
    DII_MAX1 = 5
    DII_MAX2 = 10

    DII_HEALTHY1 = function(actual, min, max) {
        case_when(
            actual >= max ~ DII_MAX1,
            actual <= min ~ DII_MIN,
            TRUE ~ DII_MIN + (actual - min) * DII_MAX1 / (max - min)
        )
    }

    DII_HEALTHY2 = function(actual, min, max) {
        case_when(
            actual >= max ~ DII_MAX2,
            actual <= min ~ DII_MIN,
            TRUE ~ DII_MIN + (actual - min) * DII_MAX2 / (max - min)
        )
    }

    DII_UNHEALTHY = function(actual, min, max) {
        case_when(
            actual >= min ~ DII_MIN,
            actual <= max ~ DII_MAX2,
            TRUE ~ DII_MIN + (actual - min) * DII_MAX2 / (max - min)
        )
    }

    # stop if the input data is not provided for any day
    if (is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        stop("Please provide the file path for the MPED and NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

    # load the MPED per 100 gram data
    ## if MPED_PER_100_GRAM_PATH is a character
    if (is.character(MPED_PER_100_GRAM_PATH) == TRUE) {
        # contains "sas"
        if (grepl("sas", MPED_PER_100_GRAM_PATH) == TRUE) {
            MPED_PER_100_GRAM = read_sas(MPED_PER_100_GRAM_PATH)
        }
        # does not contain "sas"
        else {
            # Column widths - derived from the character positions
            widths <- c(8, 1, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8)

            # Column names
            col_names <- c(
                "DRDIFDCD", "EQUIVFLAG", "MODCODE", "G_TOTAL", "G_WHL", "G_NWHL",
                "V_TOTAL", "V_DRKGR", "V_DPYEL", "V_POTATO", "V_STARCY", "V_TOMATO",
                "V_OTHER", "F_TOTAL", "F_CITMLB", "F_OTHER", "D_TOTAL", "D_MILK",
                "D_YOGURT", "D_CHEESE", "M_MPF", "M_MEAT", "M_ORGAN", "M_FRANK",
                "M_POULT", "M_FISH_HI", "M_FISH_LO", "M_EGG", "M_SOY", "M_NUTSD",
                "LEGUMES", "DISCFAT_OIL", "DISCFAT_SOL", "ADD_SUG", "A_BEV"
            )

            # Read the file with fixed width format
            MPED_PER_100_GRAM <- read.fwf(MPED_PER_100_GRAM_PATH, widths = widths, header = FALSE)
            # Assign the column names
            names(MPED_PER_100_GRAM) <- col_names
        }
    } else {
        MPED_PER_100_GRAM = MPED_PER_100_GRAM_PATH
    }

    # load the WJFRT data (whole fruit and fruit juice per 100 gram data)
    if (is.character(WJFRT) == TRUE) {
        WJFRT = read_sas(WJFRT)
    } else {
        WJFRT = WJFRT
    }

    # load the DEMOGRAPHIC data
    if (is.character(DEMO_PATH) == TRUE) {
        DEMO = read_xpt(DEMO_PATH)
    } else {
        DEMO = DEMO_PATH
    }

    ################## processing the MPED and WJFRT data to make them ready for analysis ##################
    # Step 1: locate the required datasets and variables and make necessary edits to the datasets

    ## part a: get MPED per 100 grams of food and perform edits and corrections

    if (("DRDIMC" %in% colnames(MPED_PER_100_GRAM)) == TRUE) {
        # Rename variables
        MPED_PER_100_GRAM = MPED_PER_100_GRAM %>%
            mutate(
                FOODCODE = 1 * DRDIFDCD,
                MODCODE = 1 * DRDIMC
            )
    } else {
        # Rename variables
        MPED_PER_100_GRAM = MPED_PER_100_GRAM %>%
            mutate(
                FOODCODE = 1 * DRDIFDCD
            )
    }

    # Move soy beverages out of soybean products into dairy
    MPED_PER_100_GRAM_2 <- MPED_PER_100_GRAM %>%
        mutate(
            M_SOY = case_when(
                FOODCODE == 11310000 ~ 0,
                FOODCODE == 11320000 ~ 0,
                FOODCODE == 11321000 ~ 0,
                FOODCODE == 11330000 ~ 0,
                TRUE ~ M_SOY
            ),
            D_TOTAL = case_when(
                FOODCODE == 11310000 ~ round(100 * (1 / 244), 3),
                FOODCODE == 11320000 ~ round(100 * (1 / 245), 3),
                FOODCODE == 11321000 ~ round(100 * (1 / 240), 3),
                FOODCODE == 11330000 ~ round(100 * (1 / 245), 3),
                TRUE ~ D_TOTAL
            )
        )

    # Replace some food codes with values to correct for previously identified errors in the database
    ## pizza (n=3) values from FPED 11/12 data
    MPED_PER_100_GRAM_3 <- MPED_PER_100_GRAM_2 %>%
        mutate(
            G_TOTAL = case_when(
                FOODCODE == 58106210 ~ 1.88,
                FOODCODE == 58106220 ~ 1.75,
                FOODCODE == 58106230 ~ 1.88,
                TRUE ~ G_TOTAL
            ),
            G_WHL = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0,
                TRUE ~ G_WHL
            ),
            G_NWHL = case_when(
                FOODCODE == 58106210 ~ 1.88,
                FOODCODE == 58106220 ~ 1.75,
                FOODCODE == 58106230 ~ 1.88,
                TRUE ~ G_NWHL
            ),
            V_TOTAL = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0.12,
                TRUE ~ V_TOTAL
            ),
            V_TOMATO = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0.12,
                TRUE ~ V_TOMATO
            ),
            D_TOTAL = case_when(
                FOODCODE == 58106210 ~ 0.70,
                FOODCODE == 58106220 ~ 0.66,
                FOODCODE == 58106230 ~ 0.66,
                TRUE ~ D_TOTAL
            ),
            D_CHEESE = case_when(
                FOODCODE == 58106210 ~ 0.70,
                FOODCODE == 58106220 ~ 0.66,
                FOODCODE == 58106230 ~ 0.66,
                TRUE ~ D_CHEESE
            ),
            DISCFAT_OIL = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0.44,
                TRUE ~ DISCFAT_OIL
            ),
            DISCFAT_SOL = case_when(
                FOODCODE == 58106210 ~ 8.00,
                FOODCODE == 58106220 ~ 10.62,
                FOODCODE == 58106230 ~ 8.82,
                TRUE ~ DISCFAT_SOL
            ),
            ADD_SUG = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0.19,
                TRUE ~ ADD_SUG
            )
        )

    # part b: get juice data per 100 grams of food;
    ## rename variable
    ### if the column name contains "ModCode"
    if (("ModCode" %in% colnames(WJFRT)) == TRUE) {
        # rename the column names
        colnames(WJFRT)[1] <- c("FOODCODE")
        colnames(WJFRT)[2] <- c("MODCODE")

        # Merge the data frames using inner join
        MPED_PER_100_GRAM_4 <- inner_join(MPED_PER_100_GRAM_3, WJFRT, by = c("FOODCODE", "MODCODE"))
    }
    ### if the column name does not contain "ModCode"
    else {
        # rename the column names
        colnames(WJFRT)[1] <- c("FOODCODE")

        # update the column name from "frtjuice" to "FRTJUICE" and "wholefrt" to "WHOLEFRT"
        if ("frtjuice" %in% colnames(WJFRT) & "wholefrt" %in% colnames(WJFRT)) {
            colnames(WJFRT)[2] <- c("FRTJUICE")
            colnames(WJFRT)[3] <- c("WHOLEFRT")
        }

        # Merge the data frames using inner join
        MPED_PER_100_GRAM_4 <- inner_join(MPED_PER_100_GRAM_3, WJFRT, by = c("FOODCODE"))
    }

    # get demographic data for persons aged two and older
    DEMO_2 <- DEMO %>%
        filter(RIDAGEYR >= 2) %>% # persons aged 2 and older
        select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
        arrange(SEQN)

    # start with the first day data calculation
    if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_IND_PATH)) {
        # load the NUTRIENT data
        if (is.character(NUTRIENT_PATH) == TRUE) {
            NUTRIENT = read_xpt(NUTRIENT_PATH)
        } else {
            NUTRIENT = NUTRIENT_PATH
        }

        # load the NUTRIENT_IND data
        if (is.character(NUTRIENT_IND_PATH) == TRUE) {
            NUTRIENT_IND = read_xpt(NUTRIENT_IND_PATH)
        } else {
            NUTRIENT_IND = NUTRIENT_IND_PATH
        }

        # stop if the user provides the individual-level data
        if ("DR1ILINE" %in% colnames(NUTRIENT) | "DRXILINE" %in% colnames(NUTRIENT)) {
            stop("Please use the population-level data for the first day data. The file name should contain: TOT")
        }

        # if NHANES 2001-2002 data is used as evidenced by the presence of DRDDRSTZ
        if ("DRDDRSTZ" %in% colnames(NUTRIENT)) {
            # rename the variables in NUTRIENT
            NUTRIENT = NUTRIENT %>%
                mutate(
                    DR1DRSTZ = DRDDRSTZ,
                    DR1TKCAL = DRXTKCAL,
                    DR1TSFAT = DRXTSFAT,
                    DR1TALCO = DRXTALCO,
                    DR1TSODI = DRDTSODI,
                    DR1TMFAT = DRXTMFAT,
                    DR1TVB12 = DRXTVB12,
                    DR1TVB6 = DRXTVB6,
                    DR1TBCAR = DRXTBCAR,
                    DR1TCAFF = DRXTCAFF,
                    DR1TCARB = DRXTCARB,
                    DR1TCHOL = DRXTCHOL,
                    DR1TTFAT = DRXTTFAT,
                    DR1TFIBE = DRXTFIBE,
                    DR1TFA = DRXTFA,
                    DR1TIRON = DRXTIRON,
                    DR1TMAGN = DRXTMAGN,
                    DR1TNIAC = DRXTNIAC,
                    DR1TP182 = DRXTP182,
                    DR1TP183 = DRXTP183,
                    DR1TP184 = DRXTP184,
                    DR1TP204 = DRXTP204,
                    DR1TP205 = DRXTP205,
                    DR1TP225 = DRXTP225,
                    DR1TP226 = DRXTP226,
                    DR1TPROT = DRXTPROT,
                    DR1TPFAT = DRXTPFAT,
                    DR1TVB2 = DRXTVB2,
                    DR1TSFAT = DRXTSFAT,
                    DR1TSELE = DRXTSELE,
                    DR1TVB1 = DRXTVB1,
                    DR1TVARA = DRXTVARA,
                    DR1TVC = DRXTVC,
                    DR1TATOC = DRXTATOC,
                    DR1TZINC = DRXTZINC
                )

            # rename the variables in NUTRIENT_IND
            NUTRIENT_IND = NUTRIENT_IND %>%
                mutate(
                    DR1DRSTZ = DRDDRSTZ
                )
        }
        # if NHANES 1999-2000 data is used as evidenced by the presence of DRXDRSTZ
        else if ("DRDDRSTS" %in% colnames(NUTRIENT)) {
            # rename the variables in NUTRIENT
            NUTRIENT = NUTRIENT %>%
                mutate(
                    DR1DRSTZ = DRDDRSTS,
                    DR1TKCAL = DRXTKCAL,
                    DR1TSFAT = DRXTSFAT,
                    DR1TALCO = DRXTALCO,
                    DR1TSODI = DRDTSODI,
                    DR1TMFAT = DRXTMFAT,
                    DR1TVB12 = DRXTVB12,
                    DR1TVB6 = DRXTVB6,
                    DR1TBCAR = DRXTCARO,
                    DR1TCAFF = DRXTCAFF,
                    DR1TCARB = DRXTCARB,
                    DR1TCHOL = DRXTCHOL,
                    DR1TTFAT = DRXTTFAT,
                    DR1TFIBE = DRXTFIBE,
                    DR1TFA = DRXTFOLA,
                    DR1TIRON = DRXTIRON,
                    DR1TMAGN = DRXTMAGN,
                    DR1TNIAC = DRXTNIAC,
                    DR1TP182 = DRXTP182,
                    DR1TP183 = DRXTP183,
                    DR1TP184 = DRXTP184,
                    DR1TP204 = DRXTP204,
                    DR1TP205 = DRXTP205,
                    DR1TP225 = DRXTP225,
                    DR1TP226 = DRXTP226,
                    DR1TPROT = DRXTPROT,
                    DR1TPFAT = DRXTPFAT,
                    DR1TVB2 = DRXTVB2,
                    DR1TSFAT = DRXTSFAT,
                    DR1TSELE = DRXTSELE,
                    DR1TVB1 = DRXTVB1,
                    DR1TVARA = DRXTVARE,
                    DR1TVC = DRXTVC,
                    DR1TATOC = DRXTVE,
                    DR1TZINC = DRXTZINC
                )

            # rename the variables in NUTRIENT_IND
            NUTRIENT_IND = NUTRIENT_IND %>%
                mutate(
                    DR1DRSTZ = DRDDRSTS
                )
        }
        # check if DR1IFDCD is in the NUTRIENT_IND colnames
        if ("DR1IFDCD" %in% colnames(NUTRIENT_IND)) {
            # get individual food intake data for people with reliable dietary recall status
            NUTRIENT_IND <- NUTRIENT_IND %>%
                mutate(
                    FOODCODE = 1 * DR1IFDCD, # convert variable name and type
                    MODCODE = 1 * DR1MC
                ) %>%
                filter(DR1DRSTZ == 1) # reliable dietary recall status


            # get individual total food intake for people with reliable recall status
            NUTRIENT_2 <- NUTRIENT %>%
                filter(DR1DRSTZ == 1) %>% # reliable dietary recall status
                arrange(SEQN)

            ## merge the two datasets
            # combine food intake and MPED plus WHOLE FRUIT data on a food level
            MPED_PER_100_GRAM_5 <- inner_join(NUTRIENT_IND, MPED_PER_100_GRAM_4, by = c("FOODCODE", "MODCODE"))
        } else if ("DRDIFDCD" %in% colnames(NUTRIENT_IND)) {
            # get individual food intake data for people with reliable dietary recall status
            NUTRIENT_IND <- NUTRIENT_IND %>%
                mutate(
                    FOODCODE = 1 * DRDIFDCD # convert variable name and type
                ) %>%
                filter(DR1DRSTZ == 1) # reliable dietary recall status


            # get individual total food intake for people with reliable recall status
            NUTRIENT_2 <- NUTRIENT %>%
                filter(DR1DRSTZ == 1) %>% # reliable dietary recall status
                arrange(SEQN)

            ## merge the two datasets
            # combine food intake and MPED plus WHOLE FRUIT data on a food level
            MPED_PER_100_GRAM_5 <- inner_join(NUTRIENT_IND, MPED_PER_100_GRAM_4, by = c("FOODCODE"), relationship = "many-to-many")
        }


        # convert individuals' food intake amounts from grams to MyPyramid equivalents
        # Get the column names
        column_names <- colnames(MPED_PER_100_GRAM_5)

        # Identify the columns between G_TOTAL and A_BEV
        start_index <- which(column_names == "G_TOTAL")
        end_index <- which(column_names == "A_BEV")
        selected_columns <- column_names[start_index:end_index]

        # Add WHOLEFRT and FRTJUICE to the list
        selected_columns <- c(selected_columns, "WHOLEFRT", "FRTJUICE")

        # Apply the conversion
        ## check if DR1IGRMS is in the MPED_PER_100_GRAM_5 colnames
        if ("DR1IGRMS" %in% colnames(MPED_PER_100_GRAM_5)) {
            MPED_IND <- MPED_PER_100_GRAM_5 %>%
                mutate(across(all_of(selected_columns), ~ .x * (DR1IGRMS / 100)))
        }
        ## check if DRXIGRMS is in the MPED_PER_100_GRAM_5 colnames
        else if ("DRXIGRMS" %in% colnames(MPED_PER_100_GRAM_5)) {
            MPED_IND <- MPED_PER_100_GRAM_5 %>%
                mutate(across(all_of(selected_columns), ~ .x * (DRXIGRMS / 100)))
        }

        # arrange the data by SEQN
        MPED_IND_2 <- MPED_IND %>%
            arrange(SEQN)

        # calculate the sum of each food group for each individual
        MPED <- MPED_IND_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), sum, .names = "{.col}"), na.rm = TRUE)

        # combine nutrient and demographic data on a person level;
        COHORT = inner_join(NUTRIENT_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT_2 = left_join(COHORT, MPED, by = "SEQN")

        # Check if DR1TVD exists in the data frame
        has_DR1TVD <- "DR1TVD" %in% colnames(COHORT)

        # calculate the DII food group serving size / 1000 kcal
        COHORT_3 = COHORT_2 %>%
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
                N6FAT = DR1TP182 + DR1TP204,
                PROTEIN = DR1TPROT,
                PUFA = DR1TPFAT,
                RIBOFLAVIN = DR1TVB2,
                SATFAT = DR1TSFAT,
                SE = DR1TSELE,
                THIAMIN = DR1TVB1,
                VITA = DR1TVARA,
                VITC = DR1TVC,
                VITD = if (has_DR1TVD) DR1TVD else NULL,
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
            message("VITD is included in the calculation in the first day of NHANES data.")
            # use the DII generic function to calculate the DII total and component scores
        } else {
            message("VITD is not included in the calculation in the first day of NHANES data.")
        }

        COHORT_4 = DII(
            SERV_DATA = COHORT_3,
            RESPONDENTID = COHORT_3$SEQN,
            REPEATNUM = 1,
            ALCOHOL_DII = COHORT_3$ALCOHOL,
            VITB12_DII = COHORT_3$VITB12,
            VITB6_DII = COHORT_3$VITB6,
            BCAROTENE_DII = COHORT_3$BCAROTENE,
            CAFFEINE_DII = COHORT_3$CAFFEINE,
            CARB_DII = COHORT_3$CARB,
            CHOLES_DII = COHORT_3$CHOLES,
            KCAL_DII = COHORT_3$KCAL,
            TOTALFAT_DII = COHORT_3$TOTALFAT,
            FIBER_DII = COHORT_3$FIBER,
            FOLICACID_DII = COHORT_3$FOLICACID,
            IRON_DII = COHORT_3$IRON,
            MG_DII = COHORT_3$MG,
            MUFA_DII = COHORT_3$MUFA,
            NIACIN_DII = COHORT_3$NIACIN,
            N3FAT_DII = COHORT_3$N3FAT,
            N6FAT_DII = COHORT_3$N6FAT,
            PROTEIN_DII = COHORT_3$PROTEIN,
            PUFA_DII = COHORT_3$PUFA,
            RIBOFLAVIN_DII = COHORT_3$RIBOFLAVIN,
            SATFAT_DII = COHORT_3$SATFAT,
            SE_DII = COHORT_3$SE,
            THIAMIN_DII = COHORT_3$THIAMIN,
            VITA_DII = COHORT_3$VITA,
            VITC_DII = COHORT_3$VITC,
            VITD_DII = COHORT_3$VITD,
            VITE_DII = COHORT_3$VITE,
            ZN_DII = COHORT_3$ZN
        )

        COHORT_4 = COHORT_4 %>%
            mutate(
                SEQN = RESPONDENTID
            ) %>%
            ## ungroup RESPONDENTID
            ungroup() %>%
            ## remove RESPONDENTID
            select(-RESPONDENTID) %>%
            select(SEQN, DII_ALL:ROSEMARY_DII)
    }

    # start with the second day data calculation
    if (!is.null(NUTRIENT_PATH2) & !is.null(NUTRIENT_IND_PATH2)) {
        # load the NUTRIENT data
        if (is.character(NUTRIENT_PATH2) == TRUE) {
            NUTRIENT2 = read_xpt(NUTRIENT_PATH2)
        } else {
            NUTRIENT2 = NUTRIENT_PATH2
        }


        # load the NUTRIENT_IND data
        if (is.character(NUTRIENT_IND_PATH2) == TRUE) {
            NUTRIENT_IND2 = read_xpt(NUTRIENT_IND_PATH2)
        } else {
            NUTRIENT_IND2 = NUTRIENT_IND_PATH2
        }


        if ("DR2ILINE" %in% colnames(NUTRIENT2) | "DRXILINE" %in% colnames(NUTRIENT2)) {
            stop("Please use the population-level data for the second day data. The file name should contain: TOT")
        }


        # get individual food intake data for people with reliable dietary recall status
        NUTRIENT_IND2 <- NUTRIENT_IND2 %>%
            mutate(
                FOODCODE = 1 * DR2IFDCD, # convert variable name and type
                MODCODE = 1 * DR2MC
            ) %>%
            filter(DR2DRSTZ == 1) # reliable dietary recall status

        # get individual total food intake for people with reliable recall status
        NUTRIENT2_2 <- NUTRIENT2 %>%
            filter(DR2DRSTZ == 1) %>% # reliable dietary recall status
            arrange(SEQN)

        ## merge the two datasets
        # combine food intake and MPED plus WHOLE FRUIT data on a food level
        MPED_PER_100_GRAM2_5 <- inner_join(NUTRIENT_IND2, MPED_PER_100_GRAM_4, by = c("FOODCODE", "MODCODE"))

        # convert individuals' food intake amounts from grams to MyPyramid equivalents
        # Get the column names
        column_names <- colnames(MPED_PER_100_GRAM2_5)

        # Identify the columns between G_TOTAL and A_BEV
        start_index <- which(column_names == "G_TOTAL")
        end_index <- which(column_names == "A_BEV")
        selected_columns <- column_names[start_index:end_index]

        # Add WHOLEFRT and FRTJUICE to the list
        selected_columns <- c(selected_columns, "WHOLEFRT", "FRTJUICE")

        # Apply the conversion
        MPED_IND2 <- MPED_PER_100_GRAM2_5 %>%
            mutate(across(all_of(selected_columns), ~ .x * (DR2IGRMS / 100)))

        # arrange the data by SEQN
        MPED_IND2_2 <- MPED_IND2 %>%
            arrange(SEQN)

        # calculate the sum of each food group for each individual
        MPED2 <- MPED_IND2_2 %>%
            group_by(SEQN) %>%
            summarise_at(vars(selected_columns), sum, na.rm = TRUE)

        # combine NUTRIENT2 and demographic data on a person level;
        COHORT2 = inner_join(NUTRIENT2_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT2_2 = left_join(COHORT2, MPED2, by = "SEQN")

        # Check if DR2TVD exists in the data frame
        has_DR2TVD <- "DR2TVD" %in% colnames(COHORT2)

        # calculate the DII food group serving size / 1000 kcal
        COHORT2_3 = COHORT2_2 %>%
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
                N6FAT = DR2TP182 + DR2TP204,
                PROTEIN = DR2TPROT,
                PUFA = DR2TPFAT,
                RIBOFLAVIN = DR2TVB2,
                SATFAT = DR2TSFAT,
                SE = DR2TSELE,
                THIAMIN = DR2TVB1,
                VITA = DR2TVARA,
                VITC = DR2TVC,
                VITD = if (has_DR2TVD) DR2TVD else NULL,
                VITE = DR2TATOC,
                ZN = DR2TZINC
            )

        # Include VITD if it exists
        if (has_DR2TVD) {
            message("VITD is included in the calculation in the first day of NHANES data.")
        } else {
            message("VITD is not included in the calculation in the first day of NHANES data.")
        }

        COHORT2_4 = DII(
            SERV_DATA = COHORT2_3,
            RESPONDENTID = COHORT2_3$SEQN,
            REPEATNUM = 1,
            ALCOHOL_DII = COHORT2_3$ALCOHOL,
            VITB12_DII = COHORT2_3$VITB12,
            VITB6_DII = COHORT2_3$VITB6,
            BCAROTENE_DII = COHORT2_3$BCAROTENE,
            CAFFEINE_DII = COHORT2_3$CAFFEINE,
            CARB_DII = COHORT2_3$CARB,
            CHOLES_DII = COHORT2_3$CHOLES,
            KCAL_DII = COHORT2_3$KCAL,
            TOTALFAT_DII = COHORT2_3$TOTALFAT,
            FIBER_DII = COHORT2_3$FIBER,
            FOLICACID_DII = COHORT2_3$FOLICACID,
            IRON_DII = COHORT2_3$IRON,
            MG_DII = COHORT2_3$MG,
            MUFA_DII = COHORT2_3$MUFA,
            NIACIN_DII = COHORT2_3$NIACIN,
            N3FAT_DII = COHORT2_3$N3FAT,
            N6FAT_DII = COHORT2_3$N6FAT,
            PROTEIN_DII = COHORT2_3$PROTEIN,
            PUFA_DII = COHORT2_3$PUFA,
            RIBOFLAVIN_DII = COHORT2_3$RIBOFLAVIN,
            SATFAT_DII = COHORT2_3$SATFAT,
            SE_DII = COHORT2_3$SE,
            THIAMIN_DII = COHORT2_3$THIAMIN,
            VITA_DII = COHORT2_3$VITA,
            VITC_DII = COHORT2_3$VITC,
            VITD_DII = COHORT2_3$VITD,
            VITE_DII = COHORT2_3$VITE,
            ZN_DII = COHORT2_3$ZN
        )

        COHORT2_4 = COHORT2_4 %>%
            mutate(
                SEQN = RESPONDENTID
            ) %>%
            ## ungroup RESPONDENTID
            ungroup() %>%
            ## remove RESPONDENTID
            select(-RESPONDENTID) %>%
            select(SEQN, DII_ALL:ROSEMARY_DII)
    }


    if (!is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        # message a reminder that this function does not use all the original DII variables
        message("Reminder: This function does not use all the original DII variables. Eugenol, garlic, ginger, onion, trans fat, turmeric, Green/black tea, Flavan-3-ol, Flavones, Flavonols, Flavonones, Anthocyanidins, Isoflavones, Pepper, Thyme/oregano, Rosemary are not included because they are not available in NHANES.")
        return(COHORT_4)
    } else if (is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        # message a reminder that this function does not use all the original DII variables
        message("Reminder: This function does not use all the original DII variables. Eugenol, garlic, ginger, onion, trans fat, turmeric, Green/black tea, Flavan-3-ol, Flavones, Flavonols, Flavonones, Anthocyanidins, Isoflavones, Pepper, Thyme/oregano, Rosemary are not included because they are not available in NHANES.")
        return(COHORT2_4)
    }

    # Check which columns exist in both COHORT and COHORT2
    common_cols <- intersect(colnames(COHORT_4), colnames(COHORT2_4))

    # Remove 'SEQN' as it is the joining key, not an average-able variable
    common_cols <- setdiff(common_cols, "SEQN")

    # Initialize an empty data frame for the joined and averaged data
    COHORT12 <- data.frame()

    # Perform the join and averaging only if both data sets are non-null and have common columns
    if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2) & length(common_cols) > 0) {
        # Perform inner join to merge the two data sets
        COHORT12 <- inner_join(COHORT_4, COHORT2_4, by = "SEQN")

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
