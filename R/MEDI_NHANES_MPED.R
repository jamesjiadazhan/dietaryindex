#' MEDI_NHANES_MPED
#'
#' Calculate the MEDI for the NHANES_MPED data (before 2005, 1999-2004) within 1 step for day 1, day 2, or day 1 and 2 combined (age >= 2 only)
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
#' @param SWEETS_code The code for sweets in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param FAT_OIL_code The code for fat and oil in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param SSB_code The code for sugar-sweetened beverages in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param OLIVE_OIL_code The code for olive oil in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @return The MEDI and its component scores and serving sizes
#' @examples
#' data("NHANES_20032004")
#' MEDI_NHANES_MPED(MPED_PER_100_GRAM_PATH = NHANES_20032004$MPED_PER_100_GRAM, WJFRT = NHANES_20032004$WJFRT, NUTRIENT_PATH = NHANES_20032004$NUTRIENT, NUTRIENT_IND_PATH = NHANES_20032004$NUTRIENT_IND, DEMO_PATH = NHANES_20032004$DEMO, NUTRIENT_PATH2 = NHANES_20032004$NUTRIENT2, NUTRIENT_IND_PATH2 = NHANES_20032004$NUTRIENT_IND2)
#' @export

MEDI_NHANES_MPED = function(MPED_PER_100_GRAM_PATH = NULL, WJFRT = NULL, NUTRIENT_PATH = NULL, NUTRIENT_IND_PATH = NULL, DEMO_PATH, NUTRIENT_PATH2 = NULL, NUTRIENT_IND_PATH2 = NULL, SWEETS_code = NULL, FAT_OIL_code = NULL, SSB_code = NULL, OLIVE_OIL_code = NULL) {

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

    if (is.null(SWEETS_code)) {
        # load the SSB codes from 17-18 FNDDS file as default
        SWEETS = c(11459990, 11460000, 11460100, 11460150, 11460160, 11460170, 11460190, 11460200, 11460250, 11460300, 11460400, 11460410, 11460420, 11460430, 11460440, 11461000, 11461200, 11461250, 11461260, 11461270, 11461280, 13110000, 13110100, 13110110, 13110120, 13110130, 13110140, 13110200, 13110210, 13110220, 13110310, 13110320, 13110330, 13120050, 13120100, 13120110, 13120120, 13120121, 13120130, 13120140, 13120300, 13120310, 13120400, 13120500, 13120550, 13120700, 13120710, 13120720, 13120730, 13120740, 13120750, 13120760, 13120770, 13120780, 13120790, 13121000, 13121100, 13121200, 13121300, 13121400, 13121500, 13122100, 13122500, 13126000, 13127000, 13127010, 13130100, 13130300, 13130310, 13130320, 13130330, 13130340, 13130590, 13130600, 13130610, 13130620, 13130630, 13130640, 13130700, 13135000, 13135010, 13136000, 13140100, 13140110, 13140450, 13140500, 13140550, 13140570, 13140575, 13140580, 13140600, 13140630, 13140650, 13140660, 13140670, 13140680, 13140700, 13140710, 13140900, 13142000, 13150000, 13160150, 13160160, 13160400, 13160410, 13160420, 13161000, 13161500, 13161520, 13161600, 13161630, 13170000, 13200110, 13210110, 13210150, 13210160, 13210180, 13210190, 13210220, 13210250, 13210260, 13210270, 13210280, 13210290, 13210300, 13210350, 13210410, 13210450, 13210500, 13210520, 13210530, 13210610, 13210710, 13210750, 13210810, 13210820, 13220110, 13220120, 13220210, 13220220, 13220230, 13220235, 13220240, 13220245, 13230110, 13230120, 13230130, 13230140, 13230200, 13230500, 13230510, 13241000, 13250000, 13250100, 13250200, 13252100, 13252200, 13252500, 13252600, 32120100, 32120200, 32401000, 41480000, 41480010, 44201000, 51000200, 51000300, 51000400, 51150000, 51153000, 51154010, 51154100, 51154510, 51154550, 51154600, 51155000, 51156500, 51157000, 51158100, 51159000, 51160000, 51160100, 51160110, 51161000, 51161020, 51161030, 51161050, 51161250, 51161270, 51161280, 51165000, 51166000, 51166100, 51166200, 51166500, 51167000, 51168000, 51180010, 51180030, 51180080, 51183990, 51184200, 51184210, 51184220, 51184230, 51184240, 51184250, 51184260, 51186010, 51186100, 51186130, 51186160, 51187020, 51188100, 51300100, 51301700, 51301750, 51301800, 51301805, 51301820, 51301900, 51302500, 51302520, 51303010, 51303030, 51303050, 51303070, 51303100, 51320010, 51320060, 51320070, 51320500, 51320550, 51320560, 51320700, 51320710, 51320720, 51401200, 51404500, 51404550, 51420000, 51421000, 51501080, 51502010, 51503000, 51503040, 51620000, 51620020, 51620030, 51630000, 51630100, 51630200, 51808100, 52101000, 52101030, 52101040, 52101050, 52101100, 52101150, 52102040, 52103000, 52104010, 52104040, 52104100, 52104200, 52105100, 52105200, 52201000, 52202060, 52206010, 52206060, 52207010, 52208010, 52208020, 52209010, 52211010, 52213010, 52220110, 52301000, 52302010, 52302020, 52302500, 52302600, 52303010, 52303500, 52304000, 52304010, 52304040, 52304100, 52304150, 52306010, 52306300, 52306500, 52306550, 52306700, 52311010, 52401000, 52403000, 52404060, 52405010, 52407000, 52408000, 53100050, 53100070, 53100100, 53101100, 53101200, 53101250, 53102100, 53102200, 53102600, 53102700, 53102800, 53103000, 53104100, 53104260, 53104300, 53104400, 53104500, 53104550, 53104600, 53105270, 53105275, 53105300, 53105500, 53106500, 53108200, 53108220, 53109200, 53109220, 53109300, 53110000, 53111000, 53112000, 53112100, 53113000, 53114000, 53114100, 53115100, 53115200, 53115310, 53115320, 53115410, 53115450, 53116000, 53116020, 53116270, 53116350, 53116390, 53116500, 53116510, 53116550, 53116570, 53116600, 53116650, 53117100, 53117200, 53118100, 53118200, 53118300, 53118410, 53118500, 53118550, 53119000, 53120270, 53120275, 53121270, 53121275, 53122070, 53122080, 53123070, 53123080, 53123500, 53124110, 53200100, 53201000, 53202000, 53203000, 53203500, 53204000, 53204010, 53204100, 53204840, 53204860, 53205250, 53205260, 53206000, 53206020, 53206030, 53206100, 53206500, 53206550, 53207000, 53207020, 53207050, 53208000, 53208200, 53209005, 53209010, 53209015, 53209020, 53209100, 53209500, 53210000, 53210900, 53211000, 53215500, 53220000, 53220010, 53220030, 53220040, 53222010, 53222020, 53223000, 53223100, 53224000, 53224250, 53225000, 53226000, 53226500, 53226550, 53226600, 53228000, 53230000, 53231000, 53231400, 53233000, 53233010, 53233040, 53233050, 53233060, 53233080, 53233100, 53234000, 53234100, 53234250, 53235000, 53235500, 53235600, 53236000, 53236100, 53237000, 53237010, 53237500, 53238000, 53239000, 53239010, 53239050, 53239100, 53240000, 53240010, 53241500, 53241510, 53241600, 53242000, 53242500, 53243000, 53243010, 53243050, 53244010, 53244020, 53246000, 53247000, 53247050, 53247500, 53251100, 53260030, 53260200, 53260300, 53260400, 53260500, 53260600, 53261000, 53270100, 53300100, 53300170, 53300180, 53301000, 53301070, 53301080, 53301500, 53301750, 53302000, 53302070, 53302080, 53303000, 53303070, 53303500, 53303510, 53303570, 53304000, 53304050, 53304070, 53305000, 53305010, 53305070, 53305080, 53305700, 53305720, 53305750, 53306000, 53306070, 53307000, 53307050, 53307070, 53307080, 53307500, 53307570, 53308000, 53308070, 53308300, 53308500, 53309000, 53309070, 53310000, 53310050, 53311000, 53311050, 53311070, 53312000, 53313000, 53314000, 53340000, 53340500, 53341000, 53341070, 53341500, 53341750, 53342000, 53342070, 53343000, 53343070, 53344000, 53344070, 53344200, 53344300, 53345000, 53345070, 53346000, 53346500, 53347000, 53347070, 53347100, 53347500, 53347600, 53348000, 53348070, 53360000, 53365000, 53366000, 53370000, 53371000, 53373000, 53381000, 53381070, 53382000, 53385000, 53385070, 53385500, 53386000, 53386050, 53386250, 53386500, 53387000, 53390000, 53390100, 53391000, 53391100, 53391150, 53391200, 53400200, 53400300, 53410100, 53410200, 53410300, 53410500, 53410800, 53410850, 53410860, 53410880, 53410900, 53415100, 53415120, 53415200, 53415220, 53415300, 53415400, 53415500, 53415600, 53420000, 53420100, 53420200, 53420210, 53420250, 53420300, 53420310, 53420400, 53420410, 53430000, 53430100, 53430200, 53430700, 53430750, 53440000, 53440300, 53440500, 53440600, 53440700, 53440750, 53440800, 53441110, 53441210, 53450000, 53450300, 53450500, 53450800, 53451000, 53451500, 53451750, 53452100, 53452120, 53452130, 53452150, 53452170, 53452200, 53452400, 53452420, 53452450, 53452500, 53453150, 53453170, 53500100, 53510000, 53510100, 53511000, 53520000, 53520110, 53520120, 53520140, 53520150, 53520160, 53520200, 53520500, 53520600, 53520700, 53521100, 53521110, 53521120, 53521130, 53521140, 53521210, 53521220, 53521230, 53530000, 53530010, 53610100, 53610170, 53610200, 54102010, 54102015, 54102020, 54102050, 54102060, 54102100, 54102200, 55100005, 55100010, 55100015, 55100020, 55100025, 55100030, 55100035, 55100040, 55100050, 55100055, 55100060, 55100065, 55100070, 55100080, 55101000, 55101015, 55103000, 55103020, 55103100, 55105000, 55105100, 55105200, 55105205, 55106000, 55200010, 55200020, 55200030, 55200040, 55200050, 55200060, 55200070, 55200080, 55200090, 55200100, 55200110, 55200120, 55200130, 55200200, 55201000, 55203000, 55203600, 55203700, 55204000, 55205000, 55208000, 55211050, 55212000, 55300010, 55300020, 55300030, 55300040, 55300050, 55300055, 55300060, 55301000, 55301010, 55301015, 55301020, 55301025, 55301030, 55301031, 55301040, 55301048, 55301050, 55301055, 55310100, 55400010, 55401000, 55501000, 55610300, 55702100, 55801000, 55801010, 56201550, 56205200, 56205230, 56205240, 58117110, 58117210, 58118110, 58118210, 58123120, 58124210, 58149160, 58157110, 58157210, 61113500, 63113030, 63203700, 63403150, 63420100, 63420110, 63420200, 63430100, 63430110, 63430500, 71930200, 71945020, 91101000, 91101010, 91101020, 91102010, 91104100, 91300010, 91300100, 91301020, 91301030, 91301040, 91301050, 91301060, 91301080, 91301081, 91301082, 91301090, 91301100, 91301120, 91301130, 91301200, 91301250, 91301510, 91302010, 91302020, 91303000, 91303500, 91304010, 91304020, 91304030, 91304040, 91304050, 91304060, 91304070, 91304080, 91304090, 91304250, 91304300, 91305010, 91305020, 91306020, 91306025, 91306030, 91306040, 91351010, 91351020, 91361020, 91361040, 91401000, 91402000, 91403000, 91404000, 91405000, 91405500, 91406000, 91406500, 91406600, 91407100, 91407120, 91407150, 91501010, 91501015, 91501020, 91501030, 91501040, 91501050, 91501060, 91501070, 91501080, 91501090, 91501100, 91501110, 91501120, 91511010, 91511020, 91511030, 91511050, 91511060, 91511070, 91511080, 91511090, 91511100, 91511110, 91512010, 91520100, 91550100, 91550300, 91560100, 91580000, 91601000, 91611000, 91611050, 91611100, 91621000, 91700010, 91700500, 91701010, 91701020, 91701030, 91702010, 91703010, 91703020, 91703030, 91703040, 91703050, 91703060, 91703070, 91703080, 91703150, 91703200, 91703250, 91703300, 91703400, 91703500, 91703600, 91705010, 91705020, 91705030, 91705040, 91705050, 91705060, 91705070, 91705090, 91705200, 91705300, 91705310, 91705400, 91705410, 91705420, 91705430, 91705500, 91706000, 91706100, 91706400, 91707000, 91707010, 91708000, 91708010, 91708020, 91708030, 91708040, 91708070, 91708100, 91708150, 91708160, 91709000, 91713010, 91713020, 91713030, 91713040, 91713050, 91713060, 91713070, 91713080, 91713090, 91713100, 91715000, 91715100, 91715200, 91715300, 91716010, 91716110, 91718000, 91718050, 91718100, 91718110, 91718200, 91718300, 91721000, 91723000, 91723010, 91723020, 91723050, 91726000, 91726110, 91726130, 91726140, 91726150, 91726410, 91726420, 91726425, 91727010, 91728000, 91728500, 91731000, 91731010, 91731060, 91731100, 91731150, 91732000, 91732100, 91733000, 91733200, 91734000, 91734100, 91734200, 91734300, 91734400, 91734450, 91734500, 91735000, 91736000, 91739010, 91739600, 91742010, 91745010, 91745020, 91745040, 91745100, 91746010, 91746100, 91746120, 91746150, 91746200, 91750000, 91760000, 91760100, 91760200, 91760500, 91760700, 91770000, 91770010, 91770020, 91770030, 91770050, 91800100, 91801000, 91802000)
        message("Since no SWEETS code is provided, the default SSB code from 17-18 FNDDS file is used")
    } else {
        # use the provided SWEETS code
        SWEETS = SWEETS
    }

    if (is.null(FAT_OIL_code)) {
        FAT_OIL = c(22621100, 81100000, 81100500, 81101000, 81101010, 81101020, 81101100, 81101110, 81101120, 81101500, 81101520, 81201000, 81202000, 81203200, 81204000, 81322000, 82101000, 82101300, 82101500, 82102000, 82103000, 82103500, 82104000, 82105000, 82105500, 82105750, 82105800, 82106000, 82107000, 82108000, 82108250, 82108500, 82108700, 82109000, 83100100, 83101000, 83101600, 83102000, 83103000, 83104000, 83105500, 83106000, 83109000, 83112000, 83112500, 83112950, 83112990, 83114000, 83115000, 83200100, 83201000, 83201400, 83202020, 83203000, 83204500, 83205450, 83206000, 83206500, 83207000, 83208500, 83210100, 83300100, 83300200, 83300300, 83300400, 83300500, 83300600, 83300800, 83300900, 83301000)
        message("Since no FAT_OIL code is provided, the default FAT_OIL code from 17-18 FNDDS file is used")
    } else {
        FAT_OIL = FAT_OIL_code
    }

    if (is.null(OLIVE_OIL_code)) {
        OLIVE_OIL = c(82104000)
        message("Since no OLIVE_OIL code is provided, the default OLIVE_OIL code from 17-18 FNDDS file is used")
    } else {
        OLIVE_OIL = OLIVE_OIL_code
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
        }
        ## check if DRXIGRMS is in the MPED_PER_100_GRAM_5 colnames
        else if ("DRXIGRMS" %in% colnames(MPED_PER_100_GRAM_5)) {
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
                # create the variable for olive oil from OLIVE_OIL_code
                OLIVE_OIL_SERV = case_when(
                    FOODCODE %in% OLIVE_OIL ~ DR1IGRMS / 10,
                    TRUE ~ 0
                ),
                # create the variable for sweets from SWEETS_SERV_IND
                SWEETS_SERV_IND = case_when(
                    FOODCODE %in% SWEETS ~ DR1IGRMS / 50,
                    TRUE ~ 0
                ),
                # create the variable for fat oil from FAT_OIL_SERV
                FAT_OIL_SERV = case_when(
                    FOODCODE %in% FAT_OIL ~ DR1IGRMS / 10,
                    TRUE ~ 0
                ),
                # create the variable for added sugars from SSB
                ADDED_SUGAR_SSB_SERV = case_when(
                    FOODCODE %in% SSB ~ ADD_SUG,
                    TRUE ~ 0
                )
            )

        # Add OLIVE_OIL_SERV, SWEETS_SERV_IND, FAT_OIL_SERV, and ADDED_SUGAR_SSB_SERV to the list
        selected_columns <- c(selected_columns, "OLIVE_OIL_SERV", "SWEETS_SERV_IND", "FAT_OIL_SERV", "ADDED_SUGAR_SSB_SERV")

        # calculate the sum of each food group for each individual
        MPED <- MPED_IND_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), ~ sum(.x, na.rm = TRUE)))

        # combine nutrient and demographic data on a person level;
        COHORT = inner_join(NUTRIENT_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT_2 = left_join(COHORT, MPED, by = "SEQN")

        # calculate the MEDI food group serving size / 1000 kcal
        COHORT_3 = COHORT_2 %>%
            dplyr::mutate(
                OLIVE_OIL_SERV = OLIVE_OIL_SERV,
                FRT_SERV_MEDI = WHOLEFRT,
                VEG_SERV_MEDI = V_DRKGR/2 + (V_ORANGE + V_TOMATO + V_OTHER + V_STARCY),
                LEGUMES_SERV_MEDI = (M_SOY + LEGUMES*4)/1.5,
                NUTS_SERV_MEDI = M_NUTSD,
                FISH_SEAFOOD_SERV_MEDI = (M_FISH_HI + M_FISH_LO) / 4,
                ALCOHOL_SERV_MEDI = A_BEV,
                SSB_SERV_MEDI = ADDED_SUGAR_SSB_SERV * 4 / 26,
                SWEETS_SERV_MEDI = SWEETS_SERV_IND,
                DISCRET_FAT_SERV_MEDI = FAT_OIL_SERV,
                REDPROC_MEAT_SERV_MEDI = (M_FRANK + M_MEAT + M_ORGAN) / 5
            )

        # use the MEDI generic function to calculate the MEDI total and component scores
        COHORT_4 = MEDI(
            SERV_DATA = COHORT_3, 
            RESPONDENTID = COHORT_3$SEQN, 
            OLIVE_OIL_SERV_MEDI = COHORT_3$OLIVE_OIL_SERV, 
            FRT_SERV_MEDI = COHORT_3$FRT_SERV_MEDI, 
            VEG_SERV_MEDI = COHORT_3$VEG_SERV_MEDI, 
            LEGUMES_SERV_MEDI = COHORT_3$LEGUMES_SERV_MEDI, 
            NUTS_SERV_MEDI = COHORT_3$NUTS_SERV_MEDI, 
            FISH_SEAFOOD_SERV_MEDI = COHORT_3$FISH_SEAFOOD_SERV_MEDI, 
            ALCOHOL_SERV_MEDI = COHORT_3$ALCOHOL_SERV_MEDI, 
            SSB_SERV_MEDI = COHORT_3$SSB_SERV_MEDI, 
            SWEETS_SERV_MEDI = COHORT_3$SWEETS_SERV_MEDI, 
            DISCRET_FAT_SERV_MEDI = COHORT_3$DISCRET_FAT_SERV_MEDI, 
            REDPROC_MEAT_SERV_MEDI = COHORT_3$REDPROC_MEAT_SERV_MEDI
        )

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

        # Create the variable for olive oil, sweets, fat oil, and added sugars from SSB
        MPED_IND2_2 = MPED_IND2_2 %>%
            dplyr::mutate(
                # create the variable for olive oil from OLIVE_OIL_code
                OLIVE_OIL_SERV = case_when(
                    DR2IFDCD %in% OLIVE_OIL ~ DR2IGRMS / 10,
                    TRUE ~ 0
                ),
                # create the variable for sweets from SWEETS_SERV_IND
                SWEETS_SERV_IND = case_when(
                    DR2IFDCD %in% SWEETS ~ DR2IGRMS / 50,
                    TRUE ~ 0
                ),
                # create the variable for fat oil from FAT_OIL_SERV
                FAT_OIL_SERV = case_when(
                    DR2IFDCD %in% FAT_OIL ~ DR2IGRMS / 10,
                    TRUE ~ 0
                ),
                # create the variable for added sugars from SSB
                ADDED_SUGAR_SSB_SERV = case_when(
                    DR2IFDCD %in% SSB ~ ADD_SUG,
                    TRUE ~ 0
                )
            )

        # Add OLIVE_OIL_SERV, SWEETS_SERV_IND, FAT_OIL_SERV, and ADDED_SUGAR_SSB_SERV to the list
        selected_columns <- c(selected_columns, "OLIVE_OIL_SERV", "SWEETS_SERV_IND", "FAT_OIL_SERV", "ADDED_SUGAR_SSB_SERV")

        # calculate the sum of each food group for each individual
        MPED2 <- MPED_IND2_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), ~ sum(.x, na.rm = TRUE)))

        # combine NUTRIENT2 and demographic data on a person level;
        COHORT2 = inner_join(NUTRIENT2_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT2_2 = left_join(COHORT2, MPED2, by = "SEQN")

        # calculate the MEDI food group serving size / 1000 kcal
        COHORT2_3 = COHORT2_2 %>%
            dplyr::mutate(
                OLIVE_OIL_SERV = OLIVE_OIL_SERV,
                FRT_SERV_MEDI = WHOLEFRT,
                VEG_SERV_MEDI = V_DRKGR/2 + (V_ORANGE + V_TOMATO + V_OTHER + V_STARCY),
                LEGUMES_SERV_MEDI = (M_SOY + LEGUMES*4)/1.5,
                NUTS_SERV_MEDI = M_NUTSD,
                FISH_SEAFOOD_SERV_MEDI = (M_FISH_HI + M_FISH_LO) / 4,
                ALCOHOL_SERV_MEDI = A_BEV,
                SSB_SERV_MEDI = ADDED_SUGAR_SSB_SERV * 4 / 26,
                SWEETS_SERV_MEDI = SWEETS_SERV_IND,
                DISCRET_FAT_SERV_MEDI = FAT_OIL_SERV,
                REDPROC_MEAT_SERV_MEDI = (M_FRANK + M_MEAT + M_ORGAN) / 5
            )

        # use the MEDI generic function to calculate the MEDI total and component scores
        COHORT2_4 = MEDI(
            SERV_DATA = COHORT2_3, 
            RESPONDENTID = COHORT2_3$SEQN, 
            OLIVE_OIL_SERV_MEDI = COHORT2_3$OLIVE_OIL_SERV, 
            FRT_SERV_MEDI = COHORT2_3$FRT_SERV_MEDI, 
            VEG_SERV_MEDI = COHORT2_3$VEG_SERV_MEDI, 
            LEGUMES_SERV_MEDI = COHORT2_3$LEGUMES_SERV_MEDI, 
            NUTS_SERV_MEDI = COHORT2_3$NUTS_SERV_MEDI, 
            FISH_SEAFOOD_SERV_MEDI = COHORT2_3$FISH_SEAFOOD_SERV_MEDI, 
            ALCOHOL_SERV_MEDI = COHORT2_3$ALCOHOL_SERV_MEDI, 
            SSB_SERV_MEDI = COHORT2_3$SSB_SERV_MEDI, 
            SWEETS_SERV_MEDI = COHORT2_3$SWEETS_SERV_MEDI, 
            DISCRET_FAT_SERV_MEDI = COHORT2_3$DISCRET_FAT_SERV_MEDI, 
            REDPROC_MEAT_SERV_MEDI = COHORT2_3$REDPROC_MEAT_SERV_MEDI
        )

        COHORT2_4 = COHORT2_4 %>%
            dplyr::rename(
                SEQN = RESPONDENTID
            )
    }

    if (!is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        return(COHORT_4)
    } else if (is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        return(COHORT2_4)
    }
    # merge two days data if they both exist
    else if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        COHORT12 = inner_join(COHORT_4, COHORT2_4, by = "SEQN") %>%
            mutate(
                MEDI_ALL = (MEDI_ALL.x + MEDI_ALL.y) / 2,
                MEDI_NOETOH = (MEDI_NOETOH.x + MEDI_NOETOH.y) / 2,
                MEDI_OLIVE_OIL = (MEDI_OLIVE_OIL.x + MEDI_OLIVE_OIL.y) / 2,
                MEDI_FRT = (MEDI_FRT.x + MEDI_FRT.y) / 2,
                MEDI_VEG = (MEDI_VEG.x + MEDI_VEG.y) / 2,
                MEDI_LEGUMES = (MEDI_LEGUMES.x + MEDI_LEGUMES.y) / 2,
                MEDI_NUTS = (MEDI_NUTS.x + MEDI_NUTS.y) / 2,
                MEDI_FISH = (MEDI_FISH.x + MEDI_FISH.y) / 2,
                MEDI_ALCOHOL = (MEDI_ALCOHOL.x + MEDI_ALCOHOL.y) / 2,
                MEDI_SSB = (MEDI_SSB.x + MEDI_SSB.y) / 2,
                MEDI_SWEETS = (MEDI_SWEETS.x + MEDI_SWEETS.y) / 2,
                MEDI_DISCRET_FAT = (MEDI_DISCRET_FAT.x + MEDI_DISCRET_FAT.y) / 2,
                MEDI_REDPROC_MEAT = (MEDI_REDPROC_MEAT.x + MEDI_REDPROC_MEAT.y) / 2
            ) %>%
            dplyr::select(
                SEQN, MEDI_ALL, MEDI_NOETOH,
                MEDI_OLIVE_OIL, MEDI_FRT, MEDI_VEG, MEDI_LEGUMES, MEDI_NUTS, MEDI_FISH, MEDI_ALCOHOL, MEDI_SSB, MEDI_SWEETS, MEDI_DISCRET_FAT, MEDI_REDPROC_MEAT
            )
        return(COHORT12)
    }
}
