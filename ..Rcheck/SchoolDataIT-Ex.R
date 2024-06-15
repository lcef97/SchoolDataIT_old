pkgname <- "SchoolDataIT"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('SchoolDataIT')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Get_AdmUnNames")
### * Get_AdmUnNames

flush(stderr()); flush(stdout())

### Name: Get_AdmUnNames
### Title: Download the names and codes of Italian LAU and NUTS-3
###   administrative units
### Aliases: Get_AdmUnNames

### ** Examples







cleanEx()
nameEx("Get_BroadBand")
### * Get_BroadBand

flush(stderr()); flush(stdout())

### Name: Get_BroadBand
### Title: Download the data regarding the broad band connection activation
###   in Italian schools
### Aliases: Get_BroadBand

### ** Examples









cleanEx()
nameEx("Get_DB_MIUR")
### * Get_DB_MIUR

flush(stderr()); flush(stdout())

### Name: Get_DB_MIUR
### Title: Download the database of Italian public schools buildings
### Aliases: Get_DB_MIUR

### ** Examples







cleanEx()
nameEx("Get_InnerAreas")
### * Get_InnerAreas

flush(stderr()); flush(stdout())

### Name: Get_InnerAreas
### Title: Download the classification of peripheral municipalities
### Aliases: Get_InnerAreas

### ** Examples



InnerAreas <- Get_InnerAreas(autoAbort = TRUE)

InnerAreas[, c(1,9,13)]




cleanEx()
nameEx("Get_Invalsi_IS")
### * Get_Invalsi_IS

flush(stderr()); flush(stdout())

### Name: Get_Invalsi_IS
### Title: Download the Invalsi census survey data
### Aliases: Get_Invalsi_IS

### ** Examples






cleanEx()
nameEx("Get_Registry")
### * Get_Registry

flush(stderr()); flush(stdout())

### Name: Get_Registry
### Title: Download the registry of Italian public schools from the school
###   registry section
### Aliases: Get_Registry

### ** Examples









cleanEx()
nameEx("Get_RiskMap")
### * Get_RiskMap

flush(stderr()); flush(stdout())

### Name: Get_RiskMap
### Title: Download the Map of Risks of Italian Municipality
### Aliases: Get_RiskMap

### ** Examples








cleanEx()
nameEx("Get_School2mun")
### * Get_School2mun

flush(stderr()); flush(stdout())

### Name: Get_School2mun
### Title: Associate a Municipality (LAU) code to each school
### Aliases: Get_School2mun

### ** Examples







cleanEx()
nameEx("Get_Shapefile")
### * Get_Shapefile

flush(stderr()); flush(stdout())

### Name: Get_Shapefile
### Title: Download the boundaries of NUTS-3 (Provinces) and LAU
###   (Municipalities) Italian administrative units from the ISTAT website
### Aliases: Get_Shapefile

### ** Examples







cleanEx()
nameEx("Get_nstud")
### * Get_nstud

flush(stderr()); flush(stdout())

### Name: Get_nstud
### Title: Download students' number data
### Aliases: Get_nstud

### ** Examples








cleanEx()
nameEx("Get_nteachers_prov")
### * Get_nteachers_prov

flush(stderr()); flush(stdout())

### Name: Get_nteachers_prov
### Title: Download the number of teachers in Italian schools by province
### Aliases: Get_nteachers_prov

### ** Examples



nteachers23 <- Get_nteachers_prov(2023, filename = "DOCTIT", autoAbort = TRUE)
nteachers23[, c(3,4,5)]





cleanEx()
nameEx("Group_DB_MIUR")
### * Group_DB_MIUR

flush(stderr()); flush(stdout())

### Name: Group_DB_MIUR
### Title: Aggregate the database of Italian public schools buildings at
###   the municipality and province level
### Aliases: Group_DB_MIUR

### ** Examples



library(magrittr)
DB23_MIUR <- example_input_DB23_MIUR %>% Util_DB_MIUR_num(verbose = FALSE) %>%
    Group_DB_MIUR(InnerAreas = FALSE)



DB23_MIUR$Municipality_data[, -c(1,2,4)]
summary(DB23_MIUR$Municipality_data)

DB23_MIUR$Province_data[, -c(1,3)]
summary(DB23_MIUR$Province_data)








cleanEx()
nameEx("Group_nstud")
### * Group_nstud

flush(stderr()); flush(stdout())

### Name: Group_nstud
### Title: Aggregate the students number data by class at the municipality
###   and province level
### Aliases: Group_nstud

### ** Examples

Year <- 2023

nstud23_aggr <- Group_nstud(data = example_input_nstud23, Year = Year,
                           input_Registry = example_input_Registry23,
                           InnerAreas = FALSE,  input_School2mun = example_School2mun23)

summary(nstud23_aggr$Municipality_data[,c(46,47,48)])

summary(nstud23_aggr$Province_data[,c(44,45,46)])





cleanEx()
nameEx("Group_teachers4stud")
### * Group_teachers4stud

flush(stderr()); flush(stdout())

### Name: Group_teachers4stud
### Title: Arrange the number of teachers per students in public Italian
###   schools at the province level
### Aliases: Group_teachers4stud

### ** Examples







cleanEx()
nameEx("Map_DB")
### * Map_DB

flush(stderr()); flush(stdout())

### Name: Map_DB
### Title: Map school data
### Aliases: Map_DB

### ** Examples




DB23 <- Set_DB(Year = 2023, level = "NUTS-3",
       Invalsi_grade = c(10,13), NA_autoRM = TRUE,
       input_Invalsi_IS = example_Invalsi23_prov, input_nstud = example_input_nstud23,
       input_InnerAreas = example_InnerAreas,
       input_School2mun = example_School2mun23,
       input_AdmUnNames = example_AdmUnNames20220630,
       nteachers = FALSE, BroadBand = FALSE, SchoolBuildings = FALSE)




Map_DB(DB23, field = "Students_per_class_13", input_shp = example_Prov22_shp, level = "NUTS-3",
 col_rev = TRUE, plot = "ggplot")

Map_DB(DB23, field = "Inner_area", input_shp = example_Prov22_shp, order = "High",
 level = "NUTS-3",col_rev = TRUE, plot = "ggplot")

Map_DB(DB23, field = "M_Mathematics_10", input_shp = example_Prov22_shp, level = "NUTS-3",
 plot = "ggplot")





cleanEx()
nameEx("Map_Invalsi")
### * Map_Invalsi

flush(stderr()); flush(stdout())

### Name: Map_Invalsi
### Title: Display a map of Invalsi scores
### Aliases: Map_Invalsi

### ** Examples





 Map_Invalsi(subj = "Italian", grade = 13, level = "NUTS-3", Year = 2023, WLE = FALSE,
  data = example_Invalsi23_prov, input_shp = example_Prov22_shp, plot = "ggplot")

 Map_Invalsi(subj = "Italian", grade = 5, level = "NUTS-3", Year = 2023, WLE = TRUE,
  data = example_Invalsi23_prov, input_shp = example_Prov22_shp, plot = "ggplot")








cleanEx()
nameEx("Map_School_Buildings")
### * Map_School_Buildings

flush(stderr()); flush(stdout())

### Name: Map_School_Buildings
### Title: Display data fom the school buildings database
### Aliases: Map_School_Buildings

### ** Examples






  library(magrittr)

  DB23_MIUR <- example_input_DB23_MIUR %>%
    Util_DB_MIUR_num(track.deleted = FALSE) %>%
    Group_DB_MIUR(InnerAreas = FALSE, count_missing = FALSE)

  DB23_MIUR %>% Map_School_Buildings(field = "School_bus",
     order = "Primary",level = "NUTS-3",  plot = "ggplot",
     input_shp = example_Prov22_shp)

  DB23_MIUR %>% Map_School_Buildings(field = "Railway_transport",
     order = "High",level = "NUTS-3", plot = "ggplot",
     input_shp = example_Prov22_shp)

  DB23_MIUR %>% Map_School_Buildings(field = "Context_without_disturbances",
     order = "Middle",level = "NUTS-3", plot = "ggplot",
     input_shp = example_Prov22_shp, col_rev = TRUE)








cleanEx()
nameEx("Set_DB")
### * Set_DB

flush(stderr()); flush(stdout())

### Name: Set_DB
### Title: Build up a comprehensive database regarding the school system
### Aliases: Set_DB

### ** Examples




DB23_prov <- Set_DB(Year = 2023, level = "NUTS-3",Invalsi_grade = c(5, 8, 13),
      Invalsi_subj = "Italian",nteachers = FALSE, BroadBand = FALSE,
      SchoolBuildings_count_missing = FALSE,NA_autoRM= TRUE,
      input_SchoolBuildings = example_input_DB23_MIUR[, -c(11:18, 10:27)],
      input_Invalsi_IS = example_Invalsi23_prov,
      input_nstud = example_input_nstud23,
      input_InnerAreas = example_InnerAreas,
      input_School2mun = example_School2mun23,
      input_AdmUnNames = example_AdmUnNames20220630)


DB23_prov

summary(DB23_prov[, -c(22:62)])








cleanEx()
nameEx("Util_Check_nstud_availability")
### * Util_Check_nstud_availability

flush(stderr()); flush(stdout())

### Name: Util_Check_nstud_availability
### Title: Check how many schools in the school registries are included in
###   the students count dataframe
### Aliases: Util_Check_nstud_availability

### ** Examples


nstud23 <- Util_nstud_wide(example_input_nstud23, verbose = FALSE)

Util_Check_nstud_availability(nstud23, Year = 2023,
  input_Registry = example_input_Registry23, InnerAreas = FALSE,
  input_School2mun = example_School2mun23, input_Prov_shp = example_Prov22_shp)








cleanEx()
nameEx("Util_DB_MIUR_num")
### * Util_DB_MIUR_num

flush(stderr()); flush(stdout())

### Name: Util_DB_MIUR_num
### Title: Convert the raw school buildings data to numeric or Boolean
###   variables
### Aliases: Util_DB_MIUR_num

### ** Examples


library(magrittr)

DB23_MIUR_num <- example_input_DB23_MIUR %>% Util_DB_MIUR_num(track_deleted = FALSE)


DB23_MIUR_num[, -c(1,4,6,8,9,10)]
summary(DB23_MIUR_num)





cleanEx()
nameEx("Util_Invalsi_filter")
### * Util_Invalsi_filter

flush(stderr()); flush(stdout())

### Name: Util_Invalsi_filter
### Title: Filter the Invalsi data by subject, school grade and year.
### Aliases: Util_Invalsi_filter

### ** Examples




Util_Invalsi_filter(subj = c("Italian", "Mathematics"), grade = 5, level = "NUTS-3", Year = 2023,
                   WLE = FALSE, data = example_Invalsi23_prov)

Util_Invalsi_filter(subj = c("Italian", "Mathematics"), grade = 5, level = "NUTS-3", Year = 2023,
                    WLE = TRUE, data = example_Invalsi23_prov)

Invalsi23_high <- Util_Invalsi_filter(subj = "Italian", grade = c(10,13), level = "NUTS-3",
                                      Year = 2023, data = example_Invalsi23_prov)


 summary(Invalsi23_high)




cleanEx()
nameEx("Util_nstud_wide")
### * Util_nstud_wide

flush(stderr()); flush(stdout())

### Name: Util_nstud_wide
### Title: Clean the raw dataframe of the number of students and arrange it
###   in a wide format
### Aliases: Util_nstud_wide

### ** Examples



nstud.default <- Util_nstud_wide(example_input_nstud23)


nstud.narrow <- Util_nstud_wide(example_input_nstud23,
  UB_nstud_byclass = 35, LB_nstud_byclass = 5 )

nrow(nstud.default)
nrow(nstud.narrow)

nstud.default

summary(nstud.default)





### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
