# log files in STATA
#Close any potentially open log file
if (is.element("log", search())) {close(log)}

#Create a local variable "date" corresponding to the current date
date <- format(Sys.Date(), "%Y%m%d")

#Create a new log file with the name "class1_date.log" and overwrite if it already exists
log_file <- paste0("replication_", date, ".log")
sink(log_file, append = FALSE, type = "output")


############################################################################################################################# 
######################### DATASET PREPARATION ######################### 

# ***RoO source files elaborated manually by Authors
# ***further questions on the files may be addressed to Laura Puccio
# 
# // RoO_NAFTA.dta
# // RoO_CUSFTA.dta
# 
# ****** Trade flows and tariffs data: data generation
# 
# ***Raw files available at WITS: https://wits.worldbank.org/
#   
#   //DTA_Mexico19912003 WTO countries
# //HS92conversion // DTA conversion of raw data avilailable for download at the UN 
# 
# //DO_convertingPref_toHS1992 // generates intermediate data NOTE CHANGE FOLDER ADDRESS

#set as working directory
setwd("~/Downloads/113151-V1-2/data/Trade")

#Following codes are for creating the main trade datasets (tariffs and imports) used in the regressions


# STEP # (i)

#set working directory 
setwd("~/Downloads/113151-V1-2/data/Trade/Raw-data")


# Upload data on Mexico imports (from WTO countries)
# Import data using "DTA_Mexico19912003 WTO countries.dta"
library(foreign)
Mexico_data <-  read_dta("DTA_Mexico19912003-WTO-countries.dta")
#Mexico_data2 <- read_dta("DTA_Mexico19912003-WTO-countries.dta")
#View(Mexico_data2)
# Lower variables' names
names(Mexico_data)[names(Mexico_data) == "SelectedNomen"] <- "selectedNomen"
names(Mexico_data)[names(Mexico_data) == "NativeNomen"] <- "nativenomen"
names(Mexico_data)[names(Mexico_data) == "Reporter"] <- "reporters"
names(Mexico_data)[names(Mexico_data) == "ReporterName"] <- "reportername"
names(Mexico_data)[names(Mexico_data) == "Product"] <- "product"
names(Mexico_data)[names(Mexico_data) == "ProductName"] <- "productname"
names(Mexico_data)[names(Mexico_data) == "Partner"] <- "partner"
names(Mexico_data)[names(Mexico_data) == "PartnerName"] <- "partnername"
names(Mexico_data)[names(Mexico_data) == "TariffYear"] <- "tariffyear"
names(Mexico_data)[names(Mexico_data) == "TradeYear"] <- "tradeyear"
names(Mexico_data)[names(Mexico_data) == "TradeSource"] <- "tradesource"
names(Mexico_data)[names(Mexico_data) == "DutyType"] <- "dutytype"
names(Mexico_data)[names(Mexico_data) == "SimpleAverage"] <- "simpleaverage"
names(Mexico_data)[names(Mexico_data) == "WeightedAverage"] <- "weightedaverage"
names(Mexico_data)[names(Mexico_data) == "ImportsValuein1000USD"] <- "imports"
names(Mexico_data)[names(Mexico_data) == "tradeyear"] <- "year"


# STEP # (ii)

# FIRST, DROP NAFTA COUNTRIES AND COUNTRIES WITH PARTIAL FTA WITH MEXICO :
# (YOU WILL COME BACK TO NAFTA COUNTRIES LATER ON)

# United states and canada
Mexico_data <- Mexico_data[!(Mexico_data$partnername %in% c("Canada", "United States")), ]

# EFTA non EU countries
Mexico_data <- Mexico_data[!(Mexico_data$partnername %in% c("Switzerland", "Iceland", "Norway")), ]

# WTO group
Mexico_data <- Mexico_data[!(Mexico_data$partnername == "WTO All members (123) --- WTO-ALL  "), ]

#  STEP # (iii)		
# 
# IDENTIFY COUNTRIES WITH WHICH MEXICO HAD AN RTA in 2003
# countries with some kind of FTA wit Mexico in 2003
# (this should correspond to the list of countries for which
# we have info on pref tariffs, "DTA_pref_2004_Mexico_as_importerHS1992",
# excluding Uruguay, since the agreement was implemented in 2004)

View(Mexico_data)

#Others
Mexico_data$RTAmex <- ifelse(Mexico_data$partnername == "Chile" | Mexico_data$partnername == "Israel", 1, 0)
#EU countries by 2003
Mexico_data$RTAmex <- ifelse(Mexico_data$partnername %in% c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "Sweden", "United Kingdom", "Greece"), 1, Mexico_data$RTAmex)
#Latin American countries with Partial RTA
Mexico_data$RTAmex <- ifelse(Mexico_data$partnername %in% c("Brazil", "Argentina", "Uruguay", "Paraguay", "Cuba", "Bolivia", "Ecuador", "Colombia", "Peru", "Guatemala", "Panama", "Venezuela"), 1, Mexico_data$RTAmex)
Mexico_data$RTAmex <- ifelse(is.na(Mexico_data$RTAmex), 0, Mexico_data$RTAmex)
#label variable
names(Mexico_data$RTAmex) <- "==1 if Mexico had an RTA with a given country"
#DROP useless variables and ORDER
Mexico_data <- subset(Mexico_data, select = c("year", "partner", "product", "RTAmex","nativenomen","reporters","reportername", "partnername","imports"))
Mexico_data <- Mexico_data[order(Mexico_data$year, Mexico_data$partner, Mexico_data$product),]
#SAVE IT
Mexico_data2<-Mexico_data

###### 	STEP # (iv)

###### 	CONVERTING HS2002 TO HS1992

# keep only year 2003
Mexico_data <- filter(Mexico_data, year == 2003)
#Rename product variable
names(Mexico_data)[names(Mexico_data) == "HS2002"] <- "product"

#Merge with HS1992 concordance table
HS92conversion <- read_dta("HS92conversion.dta")
View(HS92conversion)

#Rename product variable
names(HS92conversion)[names(HS92conversion) == "HS2002"] <- "product"


Mexico_data <- merge(Mexico_data, HS92conversion, by = "product", all.x = TRUE)
View(Mexico_data)

#remove merge indicator variable
Mexico_data$matched <- NULL

#Rename product variable


# order the dataset by 'product'
Mexico_data <- Mexico_data %>% arrange(product)

#Compute sum of trade flows for goods with same product-partner-year (PPY)
Mexico_data <- Mexico_data %>%
  group_by(product, partner, year) %>%
  mutate(imports2 = sum(imports)) %>%
  filter(row_number() == 1)


#Save 2003 imports data
write_dta(Mexico_data, "imports2003.dta")



#Merge 1991 and 2003 data back together
data1991 <- Mexico_data2
data2003 <- Mexico_data      # we can also use : read.dta("imports2003.dta")
names(data2003)[names(data2003) == "imports2"] <- "imports"

View(data2003)
data1991 <- data1991 %>% filter(year != 2003)
data <- rbind(
  data.frame(c(data1991, sapply(setdiff(names(data2003), names(data1991)), function(x) NA))),
  data.frame(c(data2003, sapply(setdiff(names(data1991), names(data2003)), function(x) NA)))
)


data$nativenomen[data$year == 2003] <- "H0"

write.csv(data, file = "TEMP_regression3.csv", row.names = FALSE)

#now all goods are defined according to the H0 classification


# STEP # (v) MERGE IT WITH INFORMATION ON TARIFFS
# MFN: for all countries in 1991 and 2003 (countries for which RTA==0)

setwd("~/Downloads/113151-V1-2/data/Trade/Raw-data")

DTA_MFN <- read_dta("MFN_MEX.dta")
colnames(DTA_MFN)[1] <- "year" # assuming the column name is "product"
colnames(DTA_MFN)[2] <- "product"
colnames(DTA_MFN)[3] <- "MFN_tariff_MEX"
View(DTA_MFN)
temp_regression <- merge(data, DTA_MFN, by=c("product", "year"), all.x=TRUE)
temp_regression <- temp_regression[!is.na(temp_regression$MFN_tariff_MEX), ] # drop rows with missing values

temp_regression<- temp_regression[,-c(ncol(temp_regression))]

# PREFERENTIAL: for a list of countries, we want to use preferential tariffs for 2003
# NOTE: since we do not have info for 2003, we upload here data for 2004
# gen another variable which defines the country/European Union member
temp_regression$countrycode_merge <- ifelse(temp_regression$partnername %in% c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "Sweden", "United Kingdom", "Greece"), 918, temp_regression$partner)
temp_regression$countrycode_merge[is.na(temp_regression$countrycode_merge)] <- temp_regression$partner[is.na(temp_regression$countrycode_merge)]
colnames(temp_regression)[ncol(temp_regression)] <- "countrycode"

View(temp_regression)

#Do the merge
setwd("~/Downloads/113151-V1-2/data/Trade/Raw-data")
DTA_pref_2004_Mexico_as_importerHS1992 <- read_dta("DTA_pref_2004_Mexico_as_importerHS1992.dta")
colnames(DTA_pref_2004_Mexico_as_importerHS1992)[1] <- "countrycode"



temp_regression_merged <- merge(temp_regression, DTA_pref_2004_Mexico_as_importerHS1992, by=c("product", "countrycode"), all.x=TRUE)
temp_regression_merged <- temp_regression_merged[!is.na(temp_regression$pref2004), ] # drop rows with missing values

#STEP # (vi) COMPUTE INDUSTRY DUMMIES
#Create new variables for different levels of HS codes
temp_regression_merged$HS6digit <- as.character(temp_regression_merged$product)
temp_regression_merged$HS2digit <- ifelse(temp_regression_merged$product > 99999, substr(temp_regression_merged$HS6digit, 1, 2), substr(temp_regression_merged$HS6digit, 1, 1))
temp_regression_merged$HS2digit <- as.numeric(temp_regression_merged$HS2digit)
temp_regression_merged$HS3digit <- ifelse(temp_regression_merged$product > 99999, substr(temp_regression_merged$HS6digit, 1, 3), substr(temp_regression_merged$HS6digit, 1, 2))
temp_regression_merged$HS3digit <- as.numeric(temp_regression_merged$HS3digit)
temp_regression_merged$HS4digit <- ifelse(temp_regression_merged$product > 99999, substr(temp_regression_merged$HS6digit, 1, 4), substr(temp_regression_merged$HS6digit, 1, 3))
temp_regression_merged$HS4digit <- as.numeric(temp_regression_merged$HS4digit)
temp_regression_merged$HS5digit <- ifelse(temp_regression_merged$product > 99999, substr(temp_regression_merged$HS6digit, 1, 5), substr(temp_regression_merged$HS6digit, 1, 4))
temp_regression_merged$HS5digit <- as.numeric(temp_regression_merged$HS5digit)
#Classify industries based on HS codes
temp_regression_merged$Industry <- ifelse(temp_regression_merged$HS2digit >= 14, "Vegetable",
                      ifelse(temp_regression_merged$HS2digit > 14 & temp_regression_merged$HS2digit <= 24, "Derived Products from Agriculture",
                             ifelse(temp_regression_merged$HS2digit >= 25 & temp_regression_merged$HS2digit <= 40, "Mineral,Chemicals and Plastic",
                                    ifelse(temp_regression_merged$HS2digit >= 44 & temp_regression_merged$HS2digit <= 49, "Wood",
                                           ifelse((temp_regression_merged$HS2digit >= 41 & temp_regression_merged$HS2digit <= 43) | (temp_regression_merged$HS2digit >= 50 & temp_regression_merged$HS2digit <= 56), "Leather and textiles",
                                                  ifelse(temp_regression_merged$HS2digit >= 57 & temp_regression_merged$HS2digit <= 67, "Apparel and footwear",
                                                         ifelse(temp_regression_merged$HS2digit >= 68 & temp_regression_merged$HS2digit <= 83, "Derived from minerals and metals",
                                                                ifelse(temp_regression_merged$HS2digit >= 84 & temp_regression_merged$HS2digit <= 89, "Machinery and vehicles",
                                                                     ifelse(temp_regression_merged$HS2digit >= 90 & temp_regression_merged$HS2digit <= 97, "Other manufacturing", NA)))))))))

# order the columns
temp_regression_merged <- temp_regression_merged %>% 
  select(year, partner, product, reportername, partnername, imports, RTAmex, MFN_tariff_MEX, pref2004)

# sort the dataset
temp_regression_merged <- arrange(temp_regression_merged, product, year, partnername)



#step (vii) RESHAPE THE DATASET
library(tidyr)

# reshape the data set
temp_regression_merged <- pivot_wider(temp_regression_merged, names_from = year, values_from = c(imports, MFN_tariff_MEX))
colnames(temp_regression_merged)

# column names

names(temp_regression_merged)[names(temp_regression_merged) == "MFN_tariff_MEX_1991"] <- "MFN_tariff_MEX1991"
names(temp_regression_merged)[names(temp_regression_merged) == "MFN_tariff_MEX_2003"] <- "MFN_tariff_MEX2003"
names(temp_regression_merged)[names(temp_regression_merged) == "imports_1991"] <- "imports1991"
names(temp_regression_merged)[names(temp_regression_merged) == "imports_2003"] <- "imports2003"

temp_regression_merged <- temp_regression_merged %>% 
  mutate(
    imports1991_v2 = ifelse(is.na(imports1991), 0, imports1991),
    imports2003_v2 = ifelse(is.na(imports2003), 0, imports2003)
  )

attr(temp_regression_merged$imports1991_v2, "label") <- "1991 imports (including zero imports)"
attr(temp_regression_merged$imports2003_v2, "label") <- "2003 imports (including zero imports)"


#  STEP # (viii) UPLOAD MEXICO PREF TARIFFS TO CAN AND USA 2004

setwd("~/Downloads/113151-V1-2/data/Trade/Raw-data")
library(haven)
DTA_pref_2004_Mexico_as_importer_USA_CAN_HS1992 <- read_dta("DTA_pref_2004_Mexico_as_importer_USA_CAN_HS1992.dta")
View(DTA_pref_2004_Mexico_as_importer_USA_CAN_HS1992)


##### Merge these two datasets
new_merged <- merge(temp_regression_merged, DTA_pref_2004_Mexico_as_importer_USA_CAN_HS1992, by = "product", all.x = TRUE)

#drop observations without information on flows
new_merged <- new_merged %>%
  filter(!is.na(pref_CAN_2004) | !is.na(pref_USA_2004))

#compute average of the two tariffs
new_merged$pref_NAFTA_2004 <- rowMeans(new_merged[, c("pref_CAN_2004", "pref_USA_2004")], na.rm = TRUE)

#Generate variables for use in regressions
new_merged$delta_tariff_NAFTA <- new_merged$pref_NAFTA_2004 - new_merged$MFN_tariff_MEX1991
new_merged$delta_tariff_USA <- new_merged$pref_USA_2004 - new_merged$MFN_tariff_MEX1991
new_merged$delta_tariff_CAN <- new_merged$pref_CAN_2004 - new_merged$MFN_tariff_MEX1991
new_merged$delta_tariff <- new_merged$MFN_tariff_MEX2003 - new_merged$MFN_tariff_MEX1991
new_merged$delta_imports <- new_merged$imports2003 - new_merged$imports1991
new_merged$delta_imports_v2 <- new_merged$imports2003_v2 - new_merged$imports1991_v2

# STEP # (ix) CONSTRUCT FLOWS OF MEXICAN IMPORTS FROM USA AND CANADA

setwd("~/Downloads/113151-V1-2/data/Trade/Raw-data")
DTA_Mexico19912003_WTO_countries <- read_dta("DTA_Mexico19912003-WTO-countries.dta")


### Lower variables' names 
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="SelectedNomen"] <- "selectedNomen"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="NativeNomen"] <- "nativenomen"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="Reporter"] <- "reporters"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="ReporterName"] <- "reportername"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="Product"] <- "product"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="ProductName"] <- "productname"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="Partner"] <- "partner"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="PartnerName"] <- "partnername"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="TariffYear"] <- "tariffyear"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="TradeYear"] <- "tradeyear"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="TradeSource"] <- "tradesource"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="DutyType"] <- "dutytype"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="SimpleAverage"] <- "simpleaverage"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="WeightedAverage"] <- "weightedaverage"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="ImportsValuein1000USD"] <- "imports"
colnames(DTA_Mexico19912003_WTO_countries)[colnames(DTA_Mexico19912003_WTO_countries)=="tradeyear"] <- "year"



### Keep only data for Canada and United States
DTA_Mexico19912003_WTO_countries <- subset(DTA_Mexico19912003_WTO_countries, partnername == "Canada" | partnername == "United States")

### Drop unnecessary variables and reorder remaining variables
DTA_Mexico19912003_WTO_countries <- subset(DTA_Mexico19912003_WTO_countries, select = c(year, partner, product, imports))
colnames(DTA_Mexico19912003_WTO_countries)

###save dataset
DTA_Mexico19912003_WTO_countries2<-DTA_Mexico19912003_WTO_countries
# 
# Converting HS2002 to HS1992

# keep only observations for year 2003
DTA_Mexico19912003_WTO_countries <- DTA_Mexico19912003_WTO_countries[DTA_Mexico19912003_WTO_countries$year == 2003,]

# rename variable product to HS2002
#names(DTA_Mexico19912003_WTO_countries)[names(DTA_Mexico19912003_WTO_countries) == "product"] <- "HS2002"
names(DTA_Mexico19912003_WTO_countries)[names(DTA_Mexico19912003_WTO_countries) == "HS2002"] <- "product"

##### Merge datasets

DTA_Mexico19912003_WTO_countries_merged <- merge(DTA_Mexico19912003_WTO_countries, HS92conversion, by= "product" , all.x = TRUE)

#Product-Partner-Year (PPY) do not uniquely identify observation because of change of classification.
#NOTE: this happens because a HS1992 good might correspond to several HS1992 goods
# we have following data for it :imports2003.dta (Computing SUM of trade flows for GOODS with same PPY)

# we already have import 2023 data as "data2003" but also use following codes to import it:
# imports_2003 <- read_dta("imports2003.dta")
# colnames(imports_2003)

DTA_Mexico19912003_WTO_countries_merged <- DTA_Mexico19912003_WTO_countries_merged %>% 
  group_by(product, partner, year) %>% 
  mutate(imports2 = sum(imports)) %>% 
  group_by(product, partner, year, imports2) %>% 
  mutate(dup = row_number()) %>% 
  filter(dup == 1) %>% 
  select(-c(imports, dup)) %>% 
  rename(imports = imports2)


imports__2003<-DTA_Mexico19912003_WTO_countries_merged
##drop also HS1992 variable 

imports__2003 <- subset(imports__2003, select = c("year", "partner", "product","imports"))

#Put both years back together
DTA_Mexico19912003_WTO_countries2 <- DTA_Mexico19912003_WTO_countries2[DTA_Mexico19912003_WTO_countries2$year != 2003,]
DTA_Mexico19912003_WTO_countries2 <- rbind(DTA_Mexico19912003_WTO_countries2, imports__2003)

DTA_Mexico19912003_WTO_countries2$nativenomen[DTA_Mexico19912003_WTO_countries2$year == 2003] <- "H0"

library(tidyr)


# Reshape the data
# Reshape wide
#df_wide <- spread(DTA_Mexico19912003_WTO_countries, key = year, value = imports)

df_wide <- pivot_wider(DTA_Mexico19912003_WTO_countries2, names_from = year, values_from = c(imports))

#df_wide <- pivot_wider(temp_regression_merged, names_from = year, values_from = c(imports, MFN_tariff_MEX))


# NOTE: this reshape generates many missing values in both trade flows.
# This is the case for flows that did not exist in any of the two years.

#  REPLACING IMPORTS 
# Since we know that MFN tariffs are constant within products, we simply replace 
# the mfn for the case in which we do not observe the flow.
# This is because we will use some of these "missing trade flows" as zeros
# trade flows for those goods that we observe "Imports_v2"

# Loop over the years and create new variables
for (t in c(1991, 2003)) {
  df_wide[[paste0("imports", t, "_v2")]] <- df_wide[[paste0("imports", t)]]
  df_wide[[paste0("imports", t, "_v2")]][is.na(df_wide[[paste0("imports", t, "_v2")]])] <- 0
}



df_wide <- df_wide %>% 
  mutate(
    imports1991_v2 = ifelse(is.na(`1991`), 0, `1991`),
    imports2003_v2 = ifelse(is.na(`2003`), 0, `2003`)
  )

attr(temp_regression_merged$imports1991_v2, "label") <- "1991 imports (including zero imports)"
attr(temp_regression_merged$imports2003_v2, "label") <- "2003 imports (including zero imports)"

# STEP # (x) PUT TOGTHER THESE FLOWS WITH THE INFORMATION ON IMPORTS AND TARIFFS FOR OTHER COUNTRIES			

regression_merged <- rbind(temp_regression_merged, new_merged)

# (xi) SAVE THE FINAL DATASET

write_dta(regression_merged, "DTA-tradeflows-tariffs-withNAFTA.dta")






