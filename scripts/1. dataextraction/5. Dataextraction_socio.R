# Get socio-demographic data from regionalstatistik.de. #
#-------------------------------#
# (1) Jede Nutzung ist unter den Bedingungen dieser „Datenlizenz Deutschland – Namensnennung – Version 2.0" zulässig.
# Die bereitgestellten Daten und Metadaten dürfen für die kommerzielle und nicht kommerzielle Nutzung insbesondere
#   1. vervielfältigt, ausgedruckt, präsentiert, verändert, bearbeitet sowie an Dritte übermittelt werden;
#   2. mit eigenen Daten und Daten Anderer zusammengeführt und zu selbständigen neuen Datensätzen verbunden werden;
#   3. in interne und externe Geschäftsprozesse, Produkte und Anwendungen in öffentlichen und nicht öffentlichen elektronischen Netzwerken eingebunden werden.
# (2) Bei der Nutzung ist sicherzustellen, dass folgende Angaben als Quellenvermerk enthalten sind:
#   1. Bezeichnung des Bereitstellers nach dessen Maßgabe,
#   2. der Vermerk „Datenlizenz Deutschland – Namensnennung – Version 2.0" oder „dl-de/by-2-0" mit Verweis auf den Lizenztext unter www.govdata.de/dl-de/by-2-0 sowie
#   3. einen Verweis auf den Datensatz (URI).
# Dies gilt nur soweit die datenhaltende Stelle die Angaben 1. bis 3. zum Quellenvermerk bereitstellt.
# (3) Veränderungen, Bearbeitungen, neue Gestaltungen oder sonstige Abwandlungen sind im Quellenvermerk mit dem Hinweis zu versehen, dass die Daten geändert wurden.

#### setup ####
library("tidyverse")
library("wiesbaden")

# data directory
setwd('../../data/socio')

# set user, password, db
regio <- c(user = "RE004315", password = "zvzkaElBdEGLgAAQ9EOI", db = "regio")

#### population by gender and age ####
population <- retrieve_data(tablename = "12411KJ003", genesis = regio, startyear = 2018)

population <- population %>%
  # remove invalid data and keep only district level (no municipality)
  filter(BEVSTD_qual == "e" & str_length(KREISE) == "5") %>%
  # reduce to relevant set of vars, rename
  select(ags = KREISE, ges = GES, age_cat = ALTX20, population = BEVSTD_val)

# save data
write.csv2(population, "Data_district_regstat_population-age-gender_20181231.csv", row.names = FALSE)

#### patients by ICD diagnosis and age  ####
patients <- retrieve_data(tablename = "23131KJ001", genesis = regio, startyear = 2017)

patients <- patients %>%
  # remove invalid data and keep only district level (no municipality)
  filter(GESD01_qual == "e" & str_length(KREISE) == "5") %>%
  # reduce to relevant set of vars, rename
  select(ags = KREISE, icd = ICD10D, age_cat = ALTD10, patients = GESD01_val)

# save data
write.csv2(patients, "Data_district_regstat_patients-age-icd_20171231.csv", row.names = FALSE)

#### hospitals and beds  ####
hospitals <- retrieve_data(tablename = "23111KJ003", genesis = regio, startyear = 2017)

hospitals <- hospitals %>%
  # remove invalid data and keep only district level (no municipality)
  filter(GES054_qual == "e" & GES059_qual == "e" & str_length(KREISE) == "5") %>%
  # reduce to relevant set of vars, rename
  select(ags = KREISE, hospitals = GES054_val, beds = GES059_val)

# save data
write.csv2(hospitals, "Data_district_regstat_hospitals-beds_20171231.csv", row.names = FALSE)

#### unemployment rate: Arbeitslosenquote bezogen auf alle zivilen Erwerbspersonen  ####
unemployment <- retrieve_data(tablename = "13211KJ009", genesis = regio, startyear = 2018)

unemployment <- unemployment %>%
  # remove invalid data and keep only district level (no municipality)
  filter(ERWP10_qual == "e" & str_length(KREISE) == "5") %>%
  # reduce to relevant set of vars, rename
  select(ags = KREISE, unemp_rate = ERWP10_val)

# save data
write.csv2(unemployment, "Data_district_regstat_unemployment-rate_20181231.csv", row.names = FALSE)

#### people in employment: sozialversicherungspflichtige Beschäftigte am Arbeitsort  ####
employment <- retrieve_data(tablename = "13111KJ011", genesis = regio, startyear = 2019)

employment <- employment %>%
  # remove invalid data and keep only district level (no municipality)
  filter(ERW032_qual == "e" & str_length(KREISE) == "5") %>%
  # reduce to relevant set of vars, rename
  select(ags = KREISE, employment = ERW032_val)

# save data
write.csv2(employment, "Data_district_regstat_employment_20190630.csv", row.names = FALSE)

#### students in general education ####
students <- retrieve_data(tablename = "21111KJ001", genesis = regio, startyear = 2017)

 students <- students %>%
  # remove invalid data and keep only district level (no municipality)
  filter(BIL003_qual == "e" & str_length(KREISE) == "5") %>%
  # keep only totals
  filter(BILSA8 == "INSGESAMT1") %>%
  # reduce to relevant set of vars, rename
  select(ags = KREISE, students = BIL003_val)

# save data
write.csv2(students, "Data_district_regstat_students_20171231.csv", row.names = FALSE)

#### recreational area: Siedlungsfläche nach Art der tatsächlichen Nutzung ####
recreation_area <- retrieve_data(tablename = "33111KJ005", genesis = regio, startyear = 2018)

recreation_area <- recreation_area %>%
  # remove invalid data and keep only district level (no municipality)
  filter(FLCQM5_qual == "e" & str_length(KREISE) == "5") %>%
  # keep only total land use and recreational land use (Sport-, Freizeit- und Erholungsfläche)
  filter(ADV09A %in% c("ADVN09-1", "ADVN09-18")) %>%
  # calculate proportions by district 
  group_by(KREISE) %>%
  mutate(prop = FLCQM5_val / sum(FLCQM5_val)) %>%
  ungroup() %>%
  # drop rows with totals
  filter(ADV09A == "ADVN09-18") %>%
  # reduce to relevant set of vars, rename
  select(ags = KREISE, recreation_area_prop = prop)

# save data
write.csv2(recreation_area, "Data_district_regstat_recreation-area_20181231.csv", row.names = FALSE)
