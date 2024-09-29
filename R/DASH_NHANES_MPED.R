#' DASH_NHANES_MPED
#'
#' Calculate the DASH for the NHANES_MPED data (before 2005, 1999-2004) within 1 step for day 1, day 2, or day 1 and 2 combined (age >= 2 only)
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
#' @param SKIM_MILK_code The food code for skim milk. The default food codes are from 17-18 FNDDS file.
#' @param LOWF_CHEESE_code The food code for low-fat cheese. The default food codes are from 17-18 FNDDS file.
#' @param SSB_code The food code for sugar-sweetened beverages (SSB). The default food codes are from 17-18 FNDDS file.
#' @return The DASH and its component scores and serving sizes
#' @examples
#' data("NHANES_20032004")
#' DASH_NHANES_MPED(MPED_PER_100_GRAM_PATH = NHANES_20032004$MPED_PER_100_GRAM, WJFRT = NHANES_20032004$WJFRT, NUTRIENT_PATH = NHANES_20032004$NUTRIENT, NUTRIENT_IND_PATH = NHANES_20032004$NUTRIENT_IND, DEMO_PATH = NHANES_20032004$DEMO, NUTRIENT_PATH2 = NHANES_20032004$NUTRIENT2, NUTRIENT_IND_PATH2 = NHANES_20032004$NUTRIENT_IND2)
#' @export

DASH_NHANES_MPED = function(MPED_PER_100_GRAM_PATH = NULL, WJFRT = NULL, NUTRIENT_PATH = NULL, NUTRIENT_IND_PATH = NULL, DEMO_PATH, NUTRIENT_PATH2 = NULL, NUTRIENT_IND_PATH2 = NULL, SKIM_MILK_code = NULL, LOWF_CHEESE_code = NULL, SSB_code = NULL) {

    # stop if the input data is not provided for any day
    if (is.null(NUTRIENT_PATH) & is.null(NUTRIENT_IND_PATH) & is.null(NUTRIENT_PATH2) & is.null(NUTRIENT_IND_PATH2)) {
        stop("Please provide the file path for the NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }


    if (is.null(SSB_code)) {
        # load the SSB codes from 17-18 FNDDS file as default
        COFFEE = c(12200100, 12210200, 12210210, 12210260, 12210270, 12210280, 12210310, 12210400, 12210420, 12210430, 12210440, 12210505, 12210520, 91703600, 92100000, 92100500, 92101000, 92101500, 92101600, 92101610, 92101630, 92101700, 92101800, 92101810, 92101820, 92101850, 92101851, 92101900, 92101901, 92101903, 92101904, 92101905, 92101906, 92101910, 92101911, 92101913, 92101917, 92101918, 92101919, 92101920, 92101921, 92101923, 92101925, 92101926, 92101928, 92101930, 92101931, 92101933, 92101935, 92101936, 92101938, 92101950, 92101955, 92101960, 92101965, 92101970, 92101975, 92102000, 92102010, 92102020, 92102030, 92102040, 92102050, 92102060, 92102070, 92102080, 92102090, 92102100, 92102110, 92102400, 92102401, 92102450, 92102500, 92102501, 92102502, 92102503, 92102504, 92102505, 92102510, 92102511, 92102512, 92102513, 92102514, 92102515, 92102600, 92102601, 92102602, 92102610, 92102611, 92102612, 92103000, 92104000, 92111000, 92111010, 92114000, 92121000, 92121001, 92121010, 92121020, 92121030, 92121040, 92121041, 92121050, 92130000, 92130001, 92130005, 92130006, 92130010, 92130011, 92130020, 92130021, 92130030, 92130031, 92152000, 92152010, 92161000, 92161001, 92161002, 92162000, 92162001, 92162002, 92171000, 92171010, 92191100, 92191105, 92191200, 92191400, 92192000, 92192030, 92192040, 92193000, 92193005, 92193020, 92193025, 92201010, 92291300, 93202000, 93301400)
        TEA = c(53246000, 92302000, 92302500, 92303010, 92303100, 92304100, 92305010, 92305040, 92305050, 92305090, 92305110, 92305180, 92305900, 92305910, 92305920, 92306000, 92306090, 92306700, 92306800, 92307000, 92307400, 92308000, 92308010, 92308020, 92308030, 92308040, 92308050, 92308500, 92308510, 92308520, 92308530, 92308540, 92308550, 92309000, 92309010, 92309020, 92309030, 92309040, 92309050, 92309500, 92309510, 92309520)
        COFFEE_TEA = c(COFFEE, TEA)
        DRINK = c(
            11511100, 11511200, 11511300, 11511400, 11511550, 11511600, 11511610, 11511700, 11512010, 11512020, 11512030, 11512100, 11512110, 11512120, 11553130, 11560000, 64134030, 67260000, 75200700, 91301130, 92101920, 92101921, 92101923, 92101925, 92101926, 92101928, 92101930, 92101931, 92101933, 92101935, 92101936, 92101938, 92102000, 92102010, 92102020, 92102030, 92102040, 92102050, 92102060, 92102070, 92102080, 92102090, 92102100, 92102110, 92307500, 92307510, 92307520, 92400000, 92400100, 92410310, 92410315, 92410320, 92410340, 92410350, 92410360, 92410370, 92410390, 92410400, 92410410, 92410420, 92410510, 92410520, 92410550, 92410560, 92410610, 92410620, 92410710, 92410720, 92410810, 92410820, 92411510, 92411520, 92411610, 92411620, 92432000, 92433000, 92510610, 92510650, 92510955, 92510960, 92511015, 92513000, 92513010, 92530410, 92530510, 92530610, 92530950, 92531030, 92541010, 92542000, 92550030, 92550035, 92550040, 92550110, 92550200, 92550370, 92550400, 92550405, 92550610, 92550620, 92552000, 92552010, 92552020, 92552030, 92582100, 92582110, 92900100, 92900110, 92900200, 92900300, 93301216, 95101000, 95101010, 95102000, 95103000, 95103010, 95104000, 95105000, 95106000, 95106010, 95110000, 95110010, 95110020, 95120000, 95120010, 95120020, 95120050, 95310200, 95310400, 95310500, 95310550, 95310555, 95310560, 95310600, 95310700, 95310750, 95310800, 95311000, 95312400, 95312410, 95312500, 95312550, 95312555, 95312560, 95312600, 95312700, 95312800, 95312900, 95312905, 95313200, 95320200, 95320500, 95321000, 95322200, 95322500,
            95323000
        )
        SSB = c(COFFEE_TEA, DRINK)
        message("Since no SSB code is provided, the default SSB code from 17-18 FNDDS file is used.")
    } else {
        # use the provided SSB code
        SSB = SSB_code
    }

    if (is.null(SKIM_MILK_code)) {
        SKIM_MILK = c(11111170, 11113000, 11114320, 11115000, 11120000, 11121300, 11212050)
        message("Since no skim milk code is provided, the default skim milk code from 17-18 FNDDS file is used.")
    } else {
        SKIM_MILK = SKIM_MILK_code
    }

    if (is.null(LOWF_CHEESE_code)) {
        LOWF_CHEESE = c(14204010, 14204020, 14206010, 14207010)
        message("Since no low-fat cheese code is provided, the default low-fat cheese code from 17-18 FNDDS file is used.")
    } else {
        LOWF_CHEESE = LOWF_CHEESE_code
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
                    DR1TPFAT = DRXTPFAT
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
                    DR1TPFAT = DRXTPFAT
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

        # Create the variable for added sugars, skim milk, and low fat cheese
        MPED_IND_2 = MPED_IND_2 %>%
            dplyr::mutate(
                # create the variable for added sugars from SSB
                ADDED_SUGAR_SSB_SERV = case_when(
                    DR1IFDCD %in% SSB ~ ADD_SUG,
                    TRUE ~ 0
                ),
                # create the variable for skim milk from SKIM_MILK_code
                SKIM_MILK_SERV = case_when(
                    DR1IFDCD %in% SKIM_MILK ~ D_MILK,
                    TRUE ~ 0
                ),
                # create the variable for low fat cheese and cream from LOWF_CHEESE_code
                LOWF_CHEESECREAM_SERV = case_when(
                    DR1IFDCD %in% LOWF_CHEESE ~ D_CHEESE * 4,
                    TRUE ~ 0
                )
            )

        # Add ADDED_SUGAR_SSB_SERV, SKIM_MILK_SERV, and LOWF_CHEESECREAM_SERV to the list
        selected_columns <- c(selected_columns, "ADDED_SUGAR_SSB_SERV", "SKIM_MILK_SERV", "LOWF_CHEESECREAM_SERV")

        # calculate the sum of each food group for each individual
        MPED <- MPED_IND_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), ~ sum(.x, na.rm = TRUE)))

        # combine nutrient and demographic data on a person level;
        COHORT = inner_join(NUTRIENT_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT_2 = left_join(COHORT, MPED, by = "SEQN")

        # calculate the DASH food group serving size / 1000 kcal
        COHORT_3 = COHORT_2 %>%
            dplyr::mutate(
                FRT_FRTJ_SERV = F_TOTAL,
                VEG_SERV = V_DRKGR + ((V_ORANGE + V_TOMATO + V_OTHER + V_STARCY) / 0.5),
                NUTSLEG_SERV = M_NUTSD + M_SOY + LEGUMES*4,
                WGRAIN_SERV = G_WHL,
                LOWF_DAIRY_SERV = SKIM_MILK_SERV + LOWF_CHEESECREAM_SERV + D_YOGURT,
                SODIUM_SERV = DR1TSODI / (DR1TKCAL / 2000),
                REDPROC_MEAT_SERV = (M_FRANK / 1.5) + ((M_MEAT + M_ORGAN) / 4),
                SSB_FRTJ_SERV = (ADDED_SUGAR_SSB_SERV * 4 / 26)
            )

        # use the DASH generic function to calculate the DASH total and component scores
        COHORT_4 = suppressMessages(DASH(
            COHORT_3, 
            RESPONDENTID = COHORT_3$SEQN, 
            TOTALKCAL_DASH = COHORT_3$DR1TKCAL, 
            FRT_FRTJ_SERV_DASH = COHORT_3$FRT_FRTJ_SERV, 
            VEG_SERV_DASH = COHORT_3$VEG_SERV, 
            NUTSLEG_SERV_DASH = COHORT_3$NUTSLEG_SERV, 
            WGRAIN_SERV_DASH = COHORT_3$WGRAIN_SERV, 
            LOWF_DAIRY_SERV_DASH = COHORT_3$LOWF_DAIRY_SERV, 
            SODIUM_SERV_DASH = COHORT_3$SODIUM_SERV, 
            REDPROC_MEAT_SERV_DASH = COHORT_3$REDPROC_MEAT_SERV,
            SSB_FRTJ_SERV_DASH = COHORT_3$SSB_FRTJ_SERV
        ))

        COHORT_4 = COHORT_4 %>%
            dplyr::rename(
                SEQN = RESPONDENTID
            )
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

        # if NHANES 2001-2002 data is used as evidenced by the presence of DRDDRSTZ
        if ("DRDDRSTZ" %in% colnames(NUTRIENT2)) {
            NUTRIENT2 = NUTRIENT2 %>%
                mutate(
                    DR2DRSTZ = DRDDRSTZ,
                    DR2TKCAL = DRXTKCAL,
                    DR2TSFAT = DRXTSFAT,
                    DR2TALCO = DRXTALCO,
                    DR2TSODI = DRDTSODI,
                    DR2TMFAT = DRXTMFAT,
                    DR2TPFAT = DRXTPFAT
                )
        }
        # if NHANES 1999-2000 data is used as evidenced by the presence of DRXDRSTZ
        else if ("DRDDRSTS" %in% colnames(NUTRIENT2)) {
            NUTRIENT2 = NUTRIENT2 %>%
                mutate(
                    DR2DRSTZ = DRDDRSTS,
                    DR2TKCAL = DRXTKCAL,
                    DR2TSFAT = DRXTSFAT,
                    DR2TALCO = DRXTALCO,
                    DR2TSODI = DRDTSODI,
                    DR2TMFAT = DRXTMFAT,
                    DR2TPFAT = DRXTPFAT
                )
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

        # Create the variable for added sugars, skim milk, and low fat cheese
        MPED_IND2_2 = MPED_IND2_2 %>%
            dplyr::mutate(
                # create the variable for added sugars from SSB
                ADDED_SUGAR_SSB_SERV = case_when(
                    DR2IFDCD %in% SSB ~ ADD_SUG,
                    TRUE ~ 0
                ),
                # create the variable for skim milk from SKIM_MILK_code
                SKIM_MILK_SERV = case_when(
                    DR2IFDCD %in% SKIM_MILK ~ D_MILK,
                    TRUE ~ 0
                ),
                # create the variable for low fat cheese and cream from LOWF_CHEESE_code
                LOWF_CHEESECREAM_SERV = case_when(
                    DR2IFDCD %in% LOWF_CHEESE ~ D_CHEESE * 4,
                    TRUE ~ 0
                )
            )

        # Add ADDED_SUGAR_SSB_SERV, SKIM_MILK_SERV, and LOWF_CHEESECREAM_SERV to the list
        selected_columns <- c(selected_columns, "ADDED_SUGAR_SSB_SERV", "SKIM_MILK_SERV", "LOWF_CHEESECREAM_SERV")

        # calculate the sum of each food group for each individual
        MPED2 <- MPED_IND2_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), ~ sum(.x, na.rm = TRUE)))

        # combine NUTRIENT2 and demographic data on a person level;
        COHORT2 = inner_join(NUTRIENT2_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT2_2 = left_join(COHORT2, MPED2, by = "SEQN")

        # calculate the DASH food group serving size / 1000 kcal
        COHORT2_3 = COHORT2_2 %>%
            dplyr::mutate(
                FRT_FRTJ_SERV = F_TOTAL,
                VEG_SERV = V_DRKGR + ((V_ORANGE + V_TOMATO + V_OTHER + V_STARCY) / 0.5),
                NUTSLEG_SERV = M_NUTSD + M_SOY + LEGUMES*4,
                WGRAIN_SERV = G_WHL,
                LOWF_DAIRY_SERV = SKIM_MILK_SERV + LOWF_CHEESECREAM_SERV + D_YOGURT,
                SODIUM_SERV = DR2TSODI / (DR2TKCAL / 2000),
                REDPROC_MEAT_SERV = (M_FRANK / 1.5) + ((M_MEAT + M_ORGAN) / 4),
                SSB_FRTJ_SERV = (ADDED_SUGAR_SSB_SERV * 4 / 26)
            )

        # use the DASH generic function to calculate the DASH total and component scores
        COHORT2_4 = suppressMessages(DASH(
            COHORT2_3, 
            RESPONDENTID = COHORT2_3$SEQN, 
            TOTALKCAL_DASH = COHORT2_3$DR2TKCAL, 
            FRT_FRTJ_SERV_DASH = COHORT2_3$FRT_FRTJ_SERV, 
            VEG_SERV_DASH = COHORT2_3$VEG_SERV, 
            NUTSLEG_SERV_DASH = COHORT2_3$NUTSLEG_SERV, 
            WGRAIN_SERV_DASH = COHORT2_3$WGRAIN_SERV, 
            LOWF_DAIRY_SERV_DASH = COHORT2_3$LOWF_DAIRY_SERV, 
            SODIUM_SERV_DASH = COHORT2_3$SODIUM_SERV, 
            REDPROC_MEAT_SERV_DASH = COHORT2_3$REDPROC_MEAT_SERV,
            SSB_FRTJ_SERV_DASH = COHORT2_3$SSB_FRTJ_SERV
        ))

        COHORT2_4 = COHORT2_4 %>%
            dplyr::rename(
                SEQN = RESPONDENTID
            )
    }

    if (!is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        message("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")
        return(COHORT_4)
    } else if (is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        message("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")
        return(COHORT2_4)
    }
    # merge two days data if they both exist
    else if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        COHORT12 = inner_join(COHORT_4, COHORT2_4, by = "SEQN") %>%
            mutate(
                DASH_ALL = (DASH_ALL.x + DASH_ALL.y) / 2,
                DASH_FRT = (DASH_FRT.x + DASH_FRT.y) / 2,
                DASH_VEG = (DASH_VEG.x + DASH_VEG.y) / 2,
                DASH_NUTSLEG = (DASH_NUTSLEG.x + DASH_NUTSLEG.y) / 2,
                DASH_WGRAIN = (DASH_WGRAIN.x + DASH_WGRAIN.y) / 2,
                DASH_LOWF_DAIRY = (DASH_LOWF_DAIRY.x + DASH_LOWF_DAIRY.y) / 2,
                DASH_SODIUM = (DASH_SODIUM.x + DASH_SODIUM.y) / 2,
                DASH_REDPROC_MEAT = (DASH_REDPROC_MEAT.x + DASH_REDPROC_MEAT.y) / 2,
                DASH_SSB_FRTJ = (DASH_SSB_FRTJ.x + DASH_SSB_FRTJ.y) / 2
            ) %>%
            dplyr::select(
                SEQN, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
                DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ
            )
        message("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")
        return(COHORT12)
    }
}
