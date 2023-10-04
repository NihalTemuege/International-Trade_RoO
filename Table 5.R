

#################################### TABLE 5 #################################### 

library(haven)

#Import data on imports at the product-country of origin level, countries of origin include US and CANADA
setwd("~/Downloads/113151-V1-2/data/Trade")
trade <- read_dta("DTA-tradeflows-tariffs-withNAFTA.dta")
View(trade)

#Switch back to the original working directory
setwd("~/Downloads/113151-V1-2/data/Treatments")

placebo_data <- read_dta("DTA-allrules-placebo.dta")
View(placebo_data)

#Merge data with RoO placebos data
roo_data <- merge(trade, placebo_data[, c("product", "roo1", "roo2_treatedBOTH", "roo3_treatedBOTH", "roo_placebo_treatedBOTH", "roo_placebo_beta_treatedBOTH")], by = "product", all.x = TRUE,incomparables=NULL)

View(trade)
View(roo_data)




#Label variables
install.packages("Hmisc")
library(Hmisc)
# Rename the variables
# names(roo_data)[names(roo_data) == "roo1"] <- "RoO_1 in the paper"
# names(roo_data)[names(roo_data) == "roo2_treatedBOTH"] <- "RoO_2 in the paper"
# names(roo_data)[names(roo_data) == "roo3_treatedBOTH"] <- "RoO_3 in the paper"
# names(roo_data)[names(roo_data) == "roo_placebo_treatedBOTH"] <- "RoO1 - RoO2_treatedBOTH"
# names(roo_data)[names(roo_data) == "roo_placebo_beta_treatedBOTH"] <- "RoO2_treatedBOTH - RoO3_treatedBOTH"
View(roo_data)
help(merge)
#(i) drop goods for which we do not have information on imports. These are goods
#for which we have information on RoO but for which Mexican imports were
#zero both in 1991 and 2003

#roo_data <- roo_data[roo_data$._m != 2, ]
#roo_data$._merge <- NULL

#For goods with missing treatments data, replace with 0
roo_data[is.na(roo_data)] <- 0

# Take logs of RoO variables

roo_vars <- grep("^roo", names(roo_data), value = TRUE)
roo_data[paste0("l", roo_vars)] <- log(1 + roo_data[, roo_vars])

library(Hmisc)
# label(roo_data$lroo1) <- expression(paste("RoO", j, "^1"))
# label(roo_data$lroo2_treatedBOTH) <- expression(paste("RoO", j, "^2"))
# label(roo_data$lroo3_treatedBOTH) <- expression(paste("RoO", j, "^3"))

library(dplyr)
library(tidyr)



#take logs also for the tariffs. (1+log) since there are several zeros
#loop over variables starting with "MFN_"
for (k in grep("^MFN_", names(roo_data), value=TRUE)){
  roo_data[paste0("log", k)] <- log(1 + roo_data[, k])
}

#create new variable "logpref_NAFTA_2004" and take its log(1+value)
roo_data$logpref_NAFTA_2004 <- roo_data$pref_NAFTA_2004 + 1
roo_data$logpref_NAFTA_2004 <- log(roo_data$logpref_NAFTA_2004)
#label variables
label(roo_data$logMFN_tariff_MEX1991) <- "log MFN_tariff_MEX1991"
label(roo_data$logMFN_tariff_MEX2003) <- "log MFN_tariff_MEX2003"
label(roo_data$logpref_NAFTA_2004) <- "log pref_NAFTA_2004"


#tariff applied by Mexico to non-NAFTA before and after
roo_data$delta_logtariff <- log(roo_data$logMFN_tariff_MEX2003 + 1) - log(roo_data$logMFN_tariff_MEX1991 + 1)
#tariff applied by Mexico to NAFTA countries before and after
roo_data$delta_logtariff_NAFTA <- log(roo_data$logpref_NAFTA_2004 + 1) - log(roo_data$logMFN_tariff_MEX1991 + 1)
#label variables
#names(roo_data)[names(roo_data) == "delta_logtariff"] <- "log(MFN_tariff_MEX2003+1) - log(MFN_tariff_MEX1991+1)"
#names(roo_data)[names(roo_data) == "delta_logtariff_NAFTA"] <- "log(pref_NAFTA_2004+1) - log(MFN_tariff_MEX1991+1)"

#(vi) compute the variable that we include in most of the specifications "ÆPreferential Tariff_{j,o}"

roo_data$delta_prefmargin_j <- roo_data$delta_logtariff - roo_data$delta_logtariff_NAFTA 

#take logs for the imports variables that contains zeros
for (t in c(1991, 2003)){
  roo_data[[paste0("logimports", t, "_v2")]] <- log(1 + roo_data[[paste0("imports", t, "_v2")]])
}

#Rename variables
#names(roo_data)[grep("logimports", names(roo_data))] <- c("log(1+imports1991_v2)", "log(1+imports2003_v2)")


#
roo_data$delta_logimports_v2 <-  roo_data$logimports2003_v2-roo_data$logimports1991_v2


# collapse imports at the product level. This is: we want a measure of
# imports at the product level for NAFTA = USA + CANADA
##############################
#NOTE: the observations for CANADA and the US in the "DTA-tradeflows-tariffs-withNAFTA.dta"
#    are used in here below. That is why there is no information on tariffs. 
#     We add this information below when doing the merge


# keep US and Canada temporarily
roo_data2 <- subset(roo_data, partnername == "Canada" | partnername == "United States")

# collapse at the product using the sum
roo_data2 <- aggregate(cbind(imports1991_v2, imports2003_v2) ~ product, roo_data2, sum)

# compute the log of (1+x) for imports
roo_data2$logimports1991_v2 <- log(1 + roo_data2$imports1991_v2)
roo_data2$logimports2003_v2 <- log(1 + roo_data2$imports2003_v2)

# take the log difference
roo_data2$delta_logimports_NAFTA <- roo_data2$logimports2003_v2 - roo_data2$logimports1991_v2
#colnames(roo_data)[which(names(roo_data) == "delta_logimports_NAFTA")] <- "logimports2003_v2-logimports1991_v2 (from NAFTA)"

# generate a variable that tells you whether there were positive imports in both years
roo_data2$pos_imports_NAFTA_1991_2003 <- ifelse(roo_data2$imports1991_v2 != 0 & roo_data2$imports2003_v2 != 0, 1, 0)

# keep the variable of interest and indicator for whether imports were positive in both years
roo_data2 <- roo_data2[c("product", "delta_logimports_NAFTA", "pos_imports_NAFTA_1991_2003")]

# save
#write.dta(roo_data2, "temp.dta")

# keep only non-USA and non-Canada partners
roo_data <- filter(roo_data, partnername != "Canada" & partnername != "United States")

# merge product_data and temp_data
merged_data <- left_join(roo_data, roo_data2, by = "product")



#for each product-NON-NAFTA country compute the difference between the log change
#in imports from that country and that from NAFTA
merged_data$delta_imports_triple_alt <- merged_data$delta_logimports_v2 - merged_data$delta_logimports_NAFTA
#label variable
attr(merged_data$delta_imports_triple_alt, "label") <- "delta_logimports_v2-delta_logimports_NAFTA"

names(merged_data)[names(merged_data) == "lroo_placebo_treatedBOTH"] <- "lroo_placebo_treatedBOTH_10"
names(merged_data)[names(merged_data) == "lroo_placebo_beta_treatedBOTH"] <- "lroo_placebo_beta_treatedBOTH_10"
names(merged_data)[names(merged_data) == "lroo3_treatedBOTH"] <- "lroo3_treatedBOTH_10"


# For the placebo we want to slightly change the tables format. In particular,
# we want to put information on the same table from regressions run for the
# following datasets:
  
  
#		- DTA-allrules-placebo (placebo_data)
#		- DTA-allrules-DRdummy-placebo

placebo_data2 <- read_dta("DTA-allrules-DRdummy-placebo.dta")
placebo_DRdummy_data <- read_dta("DTA-allrules-DRdummy-placebo.dta")

# keep variables of interest
placebo_data <- placebo_data[, c("partner", "product", "RTAmex",
                 "lroo_placebo_treatedBOTH", "lroo3_treatedBOTH", "lroo_placebo_beta_treatedBOTH",
                 "delta_imports_triple_alt", "delta_prefmargin_j")]
  

####################################################################


#Merge data with RoO placebos data
roo_data_dummy <- merge(trade, placebo_DRdummy_data[, c("product", "roo1", "roo2_treatedBOTH", "roo3_treatedBOTH", "roo_placebo_treatedBOTH", "roo_placebo_beta_treatedBOTH")], by = "product", all.x = TRUE,incomparables=NULL)


#Label variables
install.packages("Hmisc")
library(Hmisc)
# Rename the variables
# names(roo_data_dummy)[names(roo_data_dummy) == "roo1"] <- "RoO_1 in the paper"
# names(roo_data_dummy)[names(roo_data_dummy) == "roo2_treatedBOTH"] <- "RoO_2 in the paper"
# names(roo_data_dummy)[names(roo_data_dummy) == "roo3_treatedBOTH"] <- "RoO_3 in the paper"
# names(roo_data_dummy)[names(roo_data_dummy) == "roo_placebo_treatedBOTH"] <- "lroo_placebo_treatedBOTH_11"
# names(roo_data_dummy)[names(roo_data_dummy) == "roo_placebo_beta_treatedBOTH"] <- "RoO2_treatedBOTH - RoO3_treatedBOTH"
View(roo_data)
help(merge)
#(i) drop goods for which we do not have information on imports. These are goods
#for which we have information on RoO but for which Mexican imports were
#zero both in 1991 and 2003

#roo_data <- roo_data[roo_data$._m != 2, ]
#roo_data$._merge <- NULL

#For goods with missing treatments data, replace with 0
roo_data_dummy[is.na(roo_data_dummy)] <- 0

# Take logs of RoO variables

roo_vars_dum <- grep("^roo", names(roo_data_dummy), value = TRUE)
roo_data_dummy[paste0("l", roo_vars_dum)] <- log(1 + roo_data_dummy[, roo_vars_dum])

library(Hmisc)
# label(roo_data$lroo1) <- expression(paste("RoO", j, "^1"))
# label(roo_data$lroo2_treatedBOTH) <- expression(paste("RoO", j, "^2"))
# label(roo_data$lroo3_treatedBOTH) <- expression(paste("RoO", j, "^3"))

library(dplyr)
library(tidyr)



#take logs also for the tariffs. (1+log) since there are several zeros
#loop over variables starting with "MFN_"
for (k in grep("^MFN_", names(roo_data_dummy), value=TRUE)){
  roo_data_dummy[paste0("log", k)] <- log(1 + roo_data_dummy[, k])
}

#create new variable "logpref_NAFTA_2004" and take its log(1+value)
roo_data_dummy$logpref_NAFTA_2004 <- roo_data_dummy$pref_NAFTA_2004 + 1
roo_data_dummy$logpref_NAFTA_2004 <- log(roo_data_dummy$logpref_NAFTA_2004)
#label variables
label(roo_data_dummy$logMFN_tariff_MEX1991) <- "log MFN_tariff_MEX1991"
label(roo_data_dummy$logMFN_tariff_MEX2003) <- "log MFN_tariff_MEX2003"
label(roo_data_dummy$logpref_NAFTA_2004) <- "log pref_NAFTA_2004"


#tariff applied by Mexico to non-NAFTA before and after
roo_data_dummy$delta_logtariff <- log(roo_data_dummy$logMFN_tariff_MEX2003 + 1) - log(roo_data_dummy$logMFN_tariff_MEX1991 + 1)
#tariff applied by Mexico to NAFTA countries before and after
roo_data_dummy$delta_logtariff_NAFTA <- log(roo_data_dummy$logpref_NAFTA_2004 + 1) - log(roo_data_dummy$logMFN_tariff_MEX1991 + 1)
#label variables
#names(roo_data)[names(roo_data) == "delta_logtariff"] <- "log(MFN_tariff_MEX2003+1) - log(MFN_tariff_MEX1991+1)"
#names(roo_data)[names(roo_data) == "delta_logtariff_NAFTA"] <- "log(pref_NAFTA_2004+1) - log(MFN_tariff_MEX1991+1)"

#(vi) compute the variable that we include in most of the specifications "ÆPreferential Tariff_{j,o}"

roo_data_dummy$delta_prefmargin_j <- roo_data_dummy$delta_logtariff - roo_data_dummy$delta_logtariff_NAFTA 

#take logs for the imports variables that contains zeros
for (t in c(1991, 2003)){
  roo_data_dummy[[paste0("logimports", t, "_v2")]] <- log(1 + roo_data_dummy[[paste0("imports", t, "_v2")]])
}

#Rename variables
#names(roo_data)[grep("logimports", names(roo_data))] <- c("log(1+imports1991_v2)", "log(1+imports2003_v2)")


#
roo_data_dummy$delta_logimports_v2 <-  roo_data_dummy$logimports2003_v2-roo_data_dummy$logimports1991_v2


# collapse imports at the product level. This is: we want a measure of
# imports at the product level for NAFTA = USA + CANADA
##############################
#NOTE: the observations for CANADA and the US in the "DTA-tradeflows-tariffs-withNAFTA.dta"
#    are used in here below. That is why there is no information on tariffs. 
#     We add this information below when doing the merge


# keep US and Canada temporarily
roo_data_dummy2 <- subset(roo_data_dummy, partnername == "Canada" | partnername == "United States")

# collapse at the product using the sum
roo_data_dummy2 <- aggregate(cbind(imports1991_v2, imports2003_v2) ~ product, roo_data_dummy2, sum)

# compute the log of (1+x) for imports
roo_data_dummy2$logimports1991_v2 <- log(1 + roo_data_dummy2$imports1991_v2)
roo_data_dummy2$logimports2003_v2 <- log(1 + roo_data_dummy2$imports2003_v2)

# take the log difference
roo_data_dummy2$delta_logimports_NAFTA <- roo_data_dummy2$logimports2003_v2 - roo_data_dummy2$logimports1991_v2
#colnames(roo_data)[which(names(roo_data) == "delta_logimports_NAFTA")] <- "logimports2003_v2-logimports1991_v2 (from NAFTA)"

# generate a variable that tells you whether there were positive imports in both years
roo_data_dummy2$pos_imports_NAFTA_1991_2003 <- ifelse(roo_data_dummy2$imports1991_v2 != 0 & roo_data_dummy2$imports2003_v2 != 0, 1, 0)

# keep the variable of interest and indicator for whether imports were positive in both years
roo_data_dummy2 <- roo_data_dummy2[c("product", "delta_logimports_NAFTA", "pos_imports_NAFTA_1991_2003")]

# save
#write.dta(roo_data_dummy2, "temp.dta")

# keep only non-USA and non-Canada partners
roo_data_dummy <- filter(roo_data_dummy, partnername != "Canada" & partnername != "United States")

# merge product_data and temp_data
merged_data_dummy <- left_join(roo_data_dummy, roo_data_dummy2, by = "product")


#for each product-NON-NAFTA country compute the difference between the log change
#in imports from that country and that from NAFTA
merged_data_dummy$delta_imports_triple_alt <- merged_data_dummy$delta_logimports_v2 - merged_data_dummy$delta_logimports_NAFTA
merged_data_dummy$delta_imports_triple_alt <- merged_data_dummy$delta_logimports_v2 - merged_data_dummy$delta_logimports_NAFTA
View(merged_data_dummy$delta_imports_triple_alt)
summary(merged_data)
#label variable
attr(merged_data_dummy$delta_imports_triple_alt, "label") <- "delta_logimports_v2-delta_logimports_NAFTA"

#Rename 

names(merged_data_dummy)[names(merged_data_dummy) == "lroo_placebo_treatedBOTH"] <- "lroo_placebo_treatedBOTH_11"
names(merged_data_dummy)[names(merged_data_dummy) == "lroo_placebo_beta_treatedBOTH"] <- "lroo_placebo_beta_treatedBOTH_11"
names(merged_data_dummy)[names(merged_data_dummy) == "lroo3_treatedBOTH"] <- "lroo3_treatedBOTH_11"





########## MERGE THESE TWO DATASETS ###############
#final_merged_data2 <- merge(merged_data, placebo_data, by = c("product","partner"), all.x = TRUE)
final_merged_data <- left_join(merged_data, merged_data_dummy, by = c("product","partner"))
f_merged<- merge(merged_data_dummy,merged_data, by = c("partner", "product"), all = FALSE, all.x = FALSE, all.y = TRUE, suffixes = c("", "_y"))

##### RENAME 
names(final_merged_data)[names(final_merged_data) == "delta_imports_triple_alt.x"] <- "delta_imports_triple_alt"
names(final_merged_data)[names(final_merged_data) == "delta_prefmargin_j.x"] <- "delta_prefmargin_j"
names(final_merged_data)[names(final_merged_data) == "RTAmex.x"] <- "RTAmex"
names(final_merged_data)[names(final_merged_data) == "lroo_placebo_treatedBOTH_10.x"] <- "lroo_placebo_treatedBOTH_10"
names(final_merged_data)[names(final_merged_data) == "lroo_placebo_beta_treatedBOTH_10.x"] <- "lroo_placebo_beta_treatedBOTH_10"
names(final_merged_data)[names(final_merged_data) == "lroo3_treatedBOTH_10.x"] <- "lroo3_treatedBOTH_10"





#################### REGRESSIONS #################### 
library(plm)
library(sandwich)
# eststo[[1]] <- lm(delta_imports_triple_alt ~ partner + delta_prefmargin_j +
#                     lroo_placebo_treatedBOTH_10 + lroo_placebo_beta_treatedBOTH_10 +
#                     lroo3_treatedBOTH_10, reg1_data = subset(reg1_data, RTAmex == 0))

reg1_data<- subset(merged_data, RTAmex == 0)

reg1<-plm(delta_imports_triple_alt ~ delta_prefmargin_j +lroo_placebo_treatedBOTH_10 + lroo_placebo_beta_treatedBOTH_10 +
           lroo3_treatedBOTH_10 +0,index = c("partner"),reg1_data, model = "within")

eststo <- list()
eststo[[1]]<-reg1        
eststo[[1]]$se <- sqrt(diag(vcovHC(eststo[[1]], type = "HC2", cluster = "HS2")))
eststo[[1]]$N <- sum(!is.na(eststo[[1]]$fitted.values))
eststo[[1]]$r2 <- summary(eststo[[1]])$r.squared





#Run second regression and store results

reg2_data<- subset(merged_data_dummy, RTAmex == 0)

reg2 <- plm(delta_imports_triple_alt ~ partner + delta_prefmargin_j +
                    lroo_placebo_treatedBOTH_11 + lroo_placebo_beta_treatedBOTH_11 +
                    lroo3_treatedBOTH_11 +0, index = c("partner"),reg2_data, model = "within")
eststo[[2]] <- reg2
eststo[[2]]$se <- sqrt(diag(vcovHC(eststo[[2]], type = "HC2", cluster = "HS2")))
eststo[[2]]$N <- sum(!is.na(eststo[[2]]$fitted.values))
eststo[[2]]$r2 <- summary(eststo[[2]])$r.squared

install.packages("stargazer")
library(stargazer)

stargazer(reg1,reg2, type = "text", dep.var.labels = c(),title="Table 5",digits=5,out="Table5.txt")



