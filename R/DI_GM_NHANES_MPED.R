#' DI_GM_NHANES_MPED
#'
#' Calculate the Dietary Index for Gut Microbiota using a binary cutoff for the NHANES_MPED data (before 2005, 1999-2004) within 1 step for day 1, day 2, or day 1 and 2 combined (age >= 2 only)
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
#' @param AVOCADO_CODE The code for avocado in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param BROCCOLI_CODE The code for broccoli in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param CHICKPEA_CODE The code for chickpea in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param COFFEE_CODE The code for coffee in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param CRANBERRY_CODE The code for cranberry in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param FERMENT_DAIRY_CODE The code for fermented dairy in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param GREEN_TEA_CODE The code for green tea in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param SOYBEAN_CODE The code for soybean foods in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @return The DI_GM and its component scores and serving sizes
#' @examples
#' data("NHANES_20032004")
#' DI_GM_NHANES_MPED(MPED_PER_100_GRAM_PATH = NHANES_20032004$MPED_PER_100_GRAM, WJFRT = NHANES_20032004$WJFRT, NUTRIENT_PATH = NHANES_20032004$NUTRIENT, NUTRIENT_IND_PATH = NHANES_20032004$NUTRIENT_IND, DEMO_PATH = NHANES_20032004$DEMO, NUTRIENT_PATH2 = NHANES_20032004$NUTRIENT2, NUTRIENT_IND_PATH2 = NHANES_20032004$NUTRIENT_IND2)
#' @export

DI_GM_NHANES_MPED = function(MPED_PER_100_GRAM_PATH = NULL, WJFRT = NULL, DEMO_PATH, NUTRIENT_PATH = NULL, NUTRIENT_IND_PATH = NULL, NUTRIENT_PATH2 = NULL, NUTRIENT_IND_PATH2 = NULL, AVOCADO_CODE = NULL, BROCCOLI_CODE = NULL, CHICKPEA_CODE = NULL, COFFEE_CODE = NULL, CRANBERRY_CODE = NULL, FERMENTED_DAIRY_CODE = NULL, GREEN_TEA_CODE = NULL, SOYBEAN_CODE = NULL) {

    # stop if the input data is not provided for any day
    if (is.null(NUTRIENT_PATH) & is.null(NUTRIENT_IND_PATH) & is.null(NUTRIENT_PATH2) & is.null(NUTRIENT_IND_PATH2)) {
        stop("Please provide the file path for the NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

    if (is.null(AVOCADO_CODE)) {
        # load the default AVOCADO codes 
        AVOCADO = c(63105010)
        message("The default food codes for avocado from 17-18 FNDDS file is used.")
    } else {
        # use the provided AVOCADO code
        AVOCADO = AVOCADO_CODE
    }

    # if no BROCCOLI code is provided, use the default BROCCOLI codes
    if (is.null(BROCCOLI_CODE)) {
        # load the 
        BROCCOLI = c(72103000, 72103030, 72201100, 72201190, 72201211, 72201212, 72201220, 72201221, 72201222, 72201223, 72201224, 72201226, 72201227, 72203000, 72203070)  
        message("The default food codes for broccoli from 17-18 FNDDS file is used.")
    } else {
        # use the provided BROCCOLI code
        BROCCOLI = BROCCOLI_CODE
    }

    if (is.null(CHICKPEA_CODE)) {
        CHICKPEA = c(41301990, 41302010, 41302020, 41302040, 41302080, 41302110)
        message("The default food codes for chickpea from 17-18 FNDDS file is used.")
    } else {
        # use the provided CHICKPEA code
        CHICKPEA = CHICKPEA_CODE
    }

    if (is.null(COFFEE_CODE)) {
        COFFEE = c(92100000, 92100500, 92101000, 92101500, 92101600, 92101610, 92101630, 92101700, 92101800, 92101810, 
                92101820, 92101850, 92101851, 92101900, 92101901, 92101903, 92101904, 92101905, 92101906, 92101910, 
                92101911, 92101913, 92101917, 92101918, 92101919, 92101920, 92101921, 92101923, 92101925, 92101926, 
                92101928, 92101930, 92101931, 92101933, 92101935, 92101936, 92101938, 92101950, 92101955, 92101960, 
                92101965, 92101970, 92101975, 92102000, 92102010, 92102020, 92102030, 92102040, 92102050, 92102060, 
                92102070, 92102080, 92102090, 92102100, 92102110, 92102400, 92102401, 92102450, 92102500, 92102501, 
                92102502, 92102503, 92102504, 92102505, 92102510, 92102511, 92102512, 92102513, 92102514, 92102515, 
                92102600, 92102601, 92102602, 92102610, 92102611, 92102612, 92103000, 92104000, 92111000, 92111010, 
                92114000, 92121000, 92121001, 92121010, 92121020, 92121030, 92121040, 92121041, 92121050, 92130000, 
                92130001, 92130005, 92130006, 92130010, 92130011, 92130020, 92130021, 92130030, 92130031, 92152000, 
                92152010, 92161000, 92161001, 92161002, 92162000, 92162001, 92162002, 92171000, 92171010, 92201010)
        message("The default food codes for coffee from 17-18 FNDDS file is used.")
    } else {
        # use the provided COFFEE code
        COFFEE = COFFEE_CODE
    }

    if (is.null(CRANBERRY_CODE)) {
        CRANBERRY = c(62109100, 63207010, 63207110)
        message("The default food codes for cranberry from 17-18 FNDDS file is used.")
    } else {
        # use the provided CRANBERRY code
        CRANBERRY = CRANBERRY_CODE
    }

    if (is.null(FERMENTED_DAIRY_CODE)) {
        FERMENTED_DAIRY = c(11400000, 11400010, 11410000, 11411010, 11411100, 11411200, 11411300, 11411390, 11411400, 11411410,
                        11411420, 11430000, 11431000, 11432000, 11433000, 11433990, 11434000, 11434010, 11434020, 11434090,
                        11434100, 11434200, 11434300, 11435000, 11435010, 11435020, 11435030, 11435100, 11436000, 11446000,
                        11480010, 11480020, 11480030, 11480040, 42401100, 67250100, 67250150, 67404070, 67404300, 67404500,
                        67408500, 67413700, 67430500, 14010000, 14101010, 14102010, 14103010, 14103020, 14104100, 14104110,
                        14104115, 14104200, 14104250, 14104400, 14104600, 14104700, 14105010, 14105200, 14106010, 14106200,
                        14106500, 14107010, 14107030, 14107040, 14107060, 14107200, 14107250, 14108010, 14108015, 14108020,
                        14108060, 14108200, 14108400, 14108420, 14109010, 14109020, 14109030, 14109040, 14110010, 14120010,
                        14120020, 14131000, 14200100, 14201010, 14201200, 14201500, 14202010, 14202020, 14203010, 14203020,
                        14203510, 14204010, 14204020, 14206010, 14207010, 14410100, 14410110, 14410120, 14410130, 14410210,
                        14410330, 14410500, 14410600, 14410620, 14420100, 14420160, 14420300, 14502000, 14610200, 14610210,
                        14610250, 14610520, 11115400, 12310100, 12310350, 12310370, 12320100, 11115000, 11115100, 11115200,
                        11115300)
        message("The default food codes for fermented dairy from 17-18 FNDDS file is used.")
    } else {
        # use the provided FERMENTED_DAIRY code
        FERMENTED_DAIRY = FERMENTED_DAIRY_CODE
    }


    if (is.null(GREEN_TEA_CODE)) {
        GREEN_TEA = c(92303010, 92303100, 92305900, 92305910, 92305920, 92308500, 92308510, 92308520, 92308530, 92308540, 92308550, 92309500, 92309510, 92309520)
        message("The default food codes for green tea from 17-18 FNDDS file is used.")
    } else {
        # use the provided GREEN_TEA code
        GREEN_TEA = GREEN_TEA_CODE
    }

    if (is.null(SOYBEAN_CODE)) {
        SOYBEAN = c(11320000, 11320100, 11320200, 11321000, 11321100, 11321200, 41107010, 41410010)
        message("The default food codes for soybean from 17-18 FNDDS file is used.")
    } else {
        # use the provided SOYBEAN code
        SOYBEAN = SOYBEAN_CODE
    }


    # load the MPED per 100 gram data
    ## if MPED_PER_100_GRAM_PATH is a character
    if (is.character(MPED_PER_100_GRAM_PATH) == TRUE) {
        # contains "sas"
        if (grepl("sas", MPED_PER_100_GRAM_PATH) == TRUE) {
            MPED_PER_100_GRAM = read_sas(MPED_PER_100_GRAM_PATH)
        } else {
            # does not contain "sas"
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
    } else {
        ### if the column name does not contain "ModCode"
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
        message("Calculating the DI_GM total and component scores for the first day data...")

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
        } else if ("DRDDRSTS" %in% colnames(NUTRIENT)) {
            # if NHANES 1999-2000 data is used as evidenced by the presence of DRXDRSTZ
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

            # rename V_DPYEL to V_ORANGE
            colnames(MPED_PER_100_GRAM_5)[colnames(MPED_PER_100_GRAM_5) == "V_DPYEL"] <- "V_ORANGE"
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
        } else if ("DRXIGRMS" %in% colnames(MPED_PER_100_GRAM_5)) {
            ## check if DRXIGRMS is in the MPED_PER_100_GRAM_5 colnames
            # rename the column name from DRXIGRMS to DR1IGRMS
            colnames(MPED_PER_100_GRAM_5)[colnames(MPED_PER_100_GRAM_5) == "DRXIGRMS"] <- "DR1IGRMS"

            MPED_IND <- MPED_PER_100_GRAM_5 %>%
                mutate(across(all_of(selected_columns), ~ .x * (DR1IGRMS / 100)))
        }

        # arrange the data by SEQN
        MPED_IND_2 <- MPED_IND %>%
            arrange(SEQN)

        # Create the variable for olive oil, sweets, fat oil, and added sugars from SSB
        MPED_IND_2 = MPED_IND_2 %>%
            dplyr::mutate(
                # create the variable for each food group serving size
                AVOCADO_SERV = case_when(
                    FOODCODE %in% AVOCADO ~ DR1IGRMS,
                    TRUE ~ 0
                ),
                BROCCOLI_SERV = case_when(
                    FOODCODE %in% BROCCOLI ~ DR1IGRMS,
                    TRUE ~ 0
                ),
                CHICKPEA_SERV = case_when(
                    FOODCODE %in% CHICKPEA ~ DR1IGRMS,
                    TRUE ~ 0
                ),
                COFFEE_SERV = case_when(
                    FOODCODE %in% COFFEE ~ DR1IGRMS,
                    TRUE ~ 0
                ),
                CRANBERRY_SERV = case_when(
                    FOODCODE %in% CRANBERRY ~ DR1IGRMS,
                    TRUE ~ 0
                ),
                FERMENTED_DAIRY_SERV = case_when(
                    FOODCODE %in% FERMENTED_DAIRY ~ DR1IGRMS,
                    TRUE ~ 0
                ),
                GREEN_TEA_SERV = case_when(
                    FOODCODE %in% GREEN_TEA ~ DR1IGRMS,
                    TRUE ~ 0
                ),
                SOYBEAN_SERV = case_when(
                    FOODCODE %in% SOYBEAN ~ DR1IGRMS,
                    TRUE ~ 0
                )
            )

        # Add each food group serving size to the list
        selected_columns <- c(selected_columns, "AVOCADO_SERV", "BROCCOLI_SERV", "CHICKPEA_SERV", "COFFEE_SERV", "CRANBERRY_SERV", "FERMENTED_DAIRY_SERV", "GREEN_TEA_SERV", "SOYBEAN_SERV")

        # calculate the sum of each food group for each individual
        MPED <- MPED_IND_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), ~ sum(.x, na.rm = TRUE)))

        # combine nutrient and demographic data on a person level;
        COHORT = inner_join(NUTRIENT_2, DEMO_2, by = "SEQN")

        # select only participants with more than 0 kcal intake
        COHORT = COHORT %>%
            filter(DR1TKCAL > 0)

        # combine all data on a person level;
        COHORT_2 = left_join(COHORT, MPED, by = "SEQN")

        # calculate the DI_GM food group serving size / 1000 kcal
        COHORT_3 = COHORT_2 %>%
            dplyr::mutate(
                AVOCADO_SERV_DI_GM = AVOCADO_SERV,
                BROCCOLI_SERV_DI_GM = BROCCOLI_SERV,
                CHICKPEA_SERV_DI_GM = CHICKPEA_SERV,
                COFFEE_SERV_DI_GM = COFFEE_SERV,
                CRANBERRY_SERV_DI_GM = CRANBERRY_SERV,
                FERMENTED_DAIRY_SERV_DI_GM = FERMENTED_DAIRY_SERV,
                GREEN_TEA_SERV_DI_GM = GREEN_TEA_SERV,
                SOYBEAN_SERV_DI_GM = SOYBEAN_SERV,

                FIBER_SERV_DI_GM = DR1TFIBE,
                WHOLE_GRAIN_SERV_DI_GM = G_WHL,
                TOTAL_FAT_PERCENTAGE_SERV_DI_GM = (DR1TTFAT * 9/DR1TKCAL) * 100,
                PROCESSED_MEAT_SERV_DI_GM = M_FRANK,
                RED_MEAT_SERV_DI_GM = M_MEAT,
                REFINED_GRAIN_SERV_DI_GM = G_NWHL
            )

        # use the DI_GM generic function to calculate the DI_GM total and component scores
        COHORT_DI_GM_1 = DI_GM(
            SERV_DATA = COHORT_3, 
            RESPONDENTID = COHORT_3$SEQN, 
            GENDER = COHORT_3$RIAGENDR,

            AVOCADO = COHORT_3$AVOCADO_SERV_DI_GM, 
            BROCCOLI = COHORT_3$BROCCOLI_SERV_DI_GM, 
            CHICKPEA = COHORT_3$CHICKPEA_SERV_DI_GM, 
            COFFEE = COHORT_3$COFFEE_SERV_DI_GM, 
            CRANBERRY = COHORT_3$CRANBERRY_SERV_DI_GM, 
            FERMENTED_DAIRY = COHORT_3$FERMENTED_DAIRY_SERV_DI_GM, 
            FIBER = COHORT_3$FIBER_SERV_DI_GM, 
            GREEN_TEA = COHORT_3$GREEN_TEA_SERV_DI_GM, 
            SOYBEAN = COHORT_3$SOYBEAN_SERV_DI_GM, 
            WHOLE_GRAIN = COHORT_3$WHOLE_GRAIN_SERV_DI_GM, 
            TOTAL_FAT_PERCENTAGE = COHORT_3$TOTAL_FAT_PERCENTAGE_SERV_DI_GM, 
            PROCESSED_MEAT = COHORT_3$PROCESSED_MEAT_SERV_DI_GM, 
            RED_MEAT = COHORT_3$RED_MEAT_SERV_DI_GM, 
            REFINED_GRAIN = COHORT_3$REFINED_GRAIN_SERV_DI_GM
        )

        # rename the columns: RESPONDENTID to SEQN, GENDER to RIAGENDR
        COHORT_DI_GM_1 = COHORT_DI_GM_1 %>%
            dplyr::rename(
                SEQN = RESPONDENTID,
                RIAGENDR = GENDER
            )
    }

    # start with the second day data calculation
    if (!is.null(NUTRIENT_PATH2) & !is.null(NUTRIENT_IND_PATH2)) {
        message("Calculating the DI_GM total and component scores for the second day data...")

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
        } else if ("DRDDRSTS" %in% colnames(NUTRIENT2)) {
            # if NHANES 1999-2000 data is used as evidenced by the presence of DRXDRSTZ
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

        # Create the variable for each food group serving size
        MPED_IND2_2 = MPED_IND2_2 %>%
            dplyr::mutate(
                AVOCADO_SERV = case_when(
                    FOODCODE %in% AVOCADO ~ DR2IGRMS,
                    TRUE ~ 0
                ),
                BROCCOLI_SERV = case_when(
                    FOODCODE %in% BROCCOLI ~ DR2IGRMS,
                    TRUE ~ 0
                ),
                CHICKPEA_SERV = case_when(
                    FOODCODE %in% CHICKPEA ~ DR2IGRMS,
                    TRUE ~ 0
                ),
                COFFEE_SERV = case_when(
                    FOODCODE %in% COFFEE ~ DR2IGRMS,
                    TRUE ~ 0
                ),
                CRANBERRY_SERV = case_when(
                    FOODCODE %in% CRANBERRY ~ DR2IGRMS,
                    TRUE ~ 0
                ),
                FERMENTED_DAIRY_SERV = case_when(
                    FOODCODE %in% FERMENTED_DAIRY ~ DR2IGRMS,
                    TRUE ~ 0
                ),
                GREEN_TEA_SERV = case_when(
                    FOODCODE %in% GREEN_TEA ~ DR2IGRMS,
                    TRUE ~ 0
                ),
                SOYBEAN_SERV = case_when(
                    FOODCODE %in% SOYBEAN ~ DR2IGRMS,
                    TRUE ~ 0
                )
            )

        # Add each food group serving size to the list
        selected_columns <- c(selected_columns, "AVOCADO_SERV", "BROCCOLI_SERV", "CHICKPEA_SERV", "COFFEE_SERV", "CRANBERRY_SERV", "FERMENTED_DAIRY_SERV", "GREEN_TEA_SERV", "SOYBEAN_SERV")

        # calculate the sum of each food group for each individual
        MPED2 <- MPED_IND2_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), ~ sum(.x, na.rm = TRUE)))

        # combine NUTRIENT2 and demographic data on a person level;
        COHORT2 = inner_join(NUTRIENT2_2, DEMO_2, by = "SEQN")

        # select only participants with more than 0 kcal intake
        COHORT2 = COHORT2 %>%
            filter(DR2TKCAL > 0)

        # combine all data on a person level;
        COHORT2_2 = left_join(COHORT2, MPED2, by = "SEQN")

        # calculate the DI_GM food group serving size / 1000 kcal
        COHORT2_3 = COHORT2_2 %>%
            dplyr::mutate(
                AVOCADO_SERV_DI_GM = AVOCADO_SERV,
                BROCCOLI_SERV_DI_GM = BROCCOLI_SERV,
                CHICKPEA_SERV_DI_GM = CHICKPEA_SERV,
                COFFEE_SERV_DI_GM = COFFEE_SERV,
                CRANBERRY_SERV_DI_GM = CRANBERRY_SERV,
                FERMENTED_DAIRY_SERV_DI_GM = FERMENTED_DAIRY_SERV,
                GREEN_TEA_SERV_DI_GM = GREEN_TEA_SERV,
                SOYBEAN_SERV_DI_GM = SOYBEAN_SERV,
                FIBER_SERV_DI_GM = DR2TFIBE,
                WHOLE_GRAIN_SERV_DI_GM = G_WHL,
                TOTAL_FAT_PERCENTAGE_SERV_DI_GM = (DR2TTFAT * 9/DR2TKCAL) * 100,
                PROCESSED_MEAT_SERV_DI_GM = M_FRANK,
                RED_MEAT_SERV_DI_GM = M_MEAT,
                REFINED_GRAIN_SERV_DI_GM = G_NWHL
            )


        # use the DI_GM generic function to calculate the DI_GM total and component scores
        COHORT_DI_GM_2 = DI_GM(
            SERV_DATA = COHORT2_3, 
            RESPONDENTID = COHORT2_3$SEQN, 
            GENDER = COHORT2_3$RIAGENDR,

            AVOCADO = COHORT2_3$AVOCADO_SERV_DI_GM, 
            BROCCOLI = COHORT2_3$BROCCOLI_SERV_DI_GM, 
            CHICKPEA = COHORT2_3$CHICKPEA_SERV_DI_GM, 
            COFFEE = COHORT2_3$COFFEE_SERV_DI_GM, 
            CRANBERRY = COHORT2_3$CRANBERRY_SERV_DI_GM, 
            FERMENTED_DAIRY = COHORT2_3$FERMENTED_DAIRY_SERV_DI_GM, 
            FIBER = COHORT2_3$FIBER_SERV_DI_GM, 
            GREEN_TEA = COHORT2_3$GREEN_TEA_SERV_DI_GM, 
            SOYBEAN = COHORT2_3$SOYBEAN_SERV_DI_GM, 
            WHOLE_GRAIN = COHORT2_3$WHOLE_GRAIN_SERV_DI_GM, 
            TOTAL_FAT_PERCENTAGE = COHORT2_3$TOTAL_FAT_PERCENTAGE_SERV_DI_GM, 
            PROCESSED_MEAT = COHORT2_3$PROCESSED_MEAT_SERV_DI_GM, 
            RED_MEAT = COHORT2_3$RED_MEAT_SERV_DI_GM, 
            REFINED_GRAIN = COHORT2_3$REFINED_GRAIN_SERV_DI_GM
        )

        # rename the columns: RESPONDENTID to SEQN, GENDER to RIAGENDR
        COHORT_DI_GM_2 = COHORT_DI_GM_2 %>%
            dplyr::rename(
                SEQN = RESPONDENTID,
                RIAGENDR = GENDER
            )
    }

    if (!is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        return(COHORT_DI_GM_1)
    } else if (is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        return(COHORT_DI_GM_2)
    } else if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        # merge two days data if they both exist
        COHORT_1_2_DI_GM = inner_join(COHORT_DI_GM_1, COHORT_DI_GM_2, by = "SEQN") %>%
            mutate(
                RIAGENDR = RIAGENDR.x,
                DI_GM_TOTAL = (DI_GM_TOTAL.x + DI_GM_TOTAL.y) / 2,
                DI_GM_AVOCADO = (DI_GM_AVOCADO.x + DI_GM_AVOCADO.y) / 2, 
                DI_GM_BROCCOLI = (DI_GM_BROCCOLI.x + DI_GM_BROCCOLI.y) / 2,
                DI_GM_CHICKPEA = (DI_GM_CHICKPEA.x + DI_GM_CHICKPEA.y) / 2,
                DI_GM_COFFEE = (DI_GM_COFFEE.x + DI_GM_COFFEE.y) / 2,
                DI_GM_CRANBERRY = (DI_GM_CRANBERRY.x + DI_GM_CRANBERRY.y) / 2,
                DI_GM_FERMENTED_DAIRY = (DI_GM_FERMENTED_DAIRY.x + DI_GM_FERMENTED_DAIRY.y) / 2,
                DI_GM_FIBER = (DI_GM_FIBER.x + DI_GM_FIBER.y) / 2,
                DI_GM_GREEN_TEA = (DI_GM_GREEN_TEA.x + DI_GM_GREEN_TEA.y) / 2,
                DI_GM_SOYBEAN = (DI_GM_SOYBEAN.x + DI_GM_SOYBEAN.y) / 2,
                DI_GM_WHOLE_GRAIN = (DI_GM_WHOLE_GRAIN.x + DI_GM_WHOLE_GRAIN.y) / 2,
                DI_GM_TOTAL_FAT_PERCENTAGE = (DI_GM_TOTAL_FAT_PERCENTAGE.x + DI_GM_TOTAL_FAT_PERCENTAGE.y) / 2,
                DI_GM_PROCESSED_MEAT = (DI_GM_PROCESSED_MEAT.x + DI_GM_PROCESSED_MEAT.y) / 2,
                DI_GM_RED_MEAT = (DI_GM_RED_MEAT.x + DI_GM_RED_MEAT.y) / 2,
                DI_GM_REFINED_GRAIN = (DI_GM_REFINED_GRAIN.x + DI_GM_REFINED_GRAIN.y) / 2
            ) %>%
            dplyr::select(
                SEQN, 
                RIAGENDR, 
                DI_GM_TOTAL, 
                DI_GM_AVOCADO, 
                DI_GM_BROCCOLI, 
                DI_GM_CHICKPEA, 
                DI_GM_COFFEE, 
                DI_GM_CRANBERRY, 
                DI_GM_FERMENTED_DAIRY, 
                DI_GM_FIBER, 
                DI_GM_GREEN_TEA, 
                DI_GM_SOYBEAN, 
                DI_GM_WHOLE_GRAIN, 
                DI_GM_TOTAL_FAT_PERCENTAGE, 
                DI_GM_PROCESSED_MEAT, 
                DI_GM_RED_MEAT, 
                DI_GM_REFINED_GRAIN
            )

        return(COHORT_1_2_DI_GM)
    }
}
