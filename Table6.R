
########################### TABLE 6 ###########################


############## TABLE 6_1 ############## 
#Import data on imports at the product-country of origin level, countries of origin include US and CANADA
setwd("~/Downloads/113151-V1-2/data/Trade")
trade <- read_dta("DTA-tradeflows-tariffs-withNAFTA.dta")
View(trade)

#Switch back to the original working directory
setwd("~/Downloads/113151-V1-2/data/Treatments")

allrules_data <- read_dta("DTA-allrules.dta")
View(allrules_data)

#Merge data with RoO placebos data
allrules_merged <- merge(trade, allrules_data[, c("product", "roo1", "roo2_treatedBOTH", "roo3_treatedBOTH")], by = "product", all.x = TRUE,incomparables=NULL)

View(trade)
View(allrules_merged)




#Label variables
install.packages("Hmisc")
library(Hmisc)

#For goods with missing treatments data, replace with 0
allrules_merged[is.na(allrules_merged)] <- 0

# Take logs of RoO variables

roo_all_vars <- grep("^roo", names(allrules_merged), value = TRUE)
allrules_merged[paste0("l", roo_all_vars)] <- log(1 + allrules_merged[, roo_all_vars])

library(Hmisc)
library(dplyr)
library(tidyr)



#take logs also for the tariffs. (1+log) since there are several zeros
#loop over variables starting with "MFN_"
for (k in grep("^MFN_", names(allrules_merged), value=TRUE)){
  allrules_merged[paste0("log", k)] <- log(1 + allrules_merged[, k])
}

#create new variable "logpref_NAFTA_2004" and take its log(1+value)
allrules_merged$logpref_NAFTA_2004 <- allrules_merged$pref_NAFTA_2004 + 1
allrules_merged$logpref_NAFTA_2004 <- log(allrules_merged$logpref_NAFTA_2004)
#label variables
label(allrules_merged$logMFN_tariff_MEX1991) <- "log MFN_tariff_MEX1991"
label(allrules_merged$logMFN_tariff_MEX2003) <- "log MFN_tariff_MEX2003"
label(allrules_merged$logpref_NAFTA_2004) <- "log pref_NAFTA_2004"


#tariff applied by Mexico to non-NAFTA before and after
allrules_merged$delta_logtariff <- log(allrules_merged$logMFN_tariff_MEX2003 + 1) - log(allrules_merged$logMFN_tariff_MEX1991 + 1)
#tariff applied by Mexico to NAFTA countries before and after
allrules_merged$delta_logtariff_NAFTA <- log(allrules_merged$logpref_NAFTA_2004 + 1) - log(allrules_merged$logMFN_tariff_MEX1991 + 1)

#(vi) compute the variable that we include in most of the specifications "ÆPreferential Tariff_{j,o}"

allrules_merged$delta_prefmargin_j <- allrules_merged$delta_logtariff - allrules_merged$delta_logtariff_NAFTA 

#take logs for the imports variables that contains zeros
for (t in c(1991, 2003)){
  allrules_merged[[paste0("logimports", t, "_v2")]] <- log(1 + allrules_merged[[paste0("imports", t, "_v2")]])
}

#
allrules_merged$delta_logimports_v2 <-  allrules_merged$logimports2003_v2-allrules_merged$logimports1991_v2


# collapse imports at the product level. This is: we want a measure of
# imports at the product level for NAFTA = USA + CANADA
##############################
#NOTE: the observations for CANADA and the US in the "DTA-tradeflows-tariffs-withNAFTA.dta"
#    are used in here below. That is why there is no information on tariffs. 
#     We add this information below when doing the merge


# keep US and Canada temporarily
allrules_merged2 <- subset(allrules_merged, partnername == "Canada" | partnername == "United States")

# collapse at the product using the sum
allrules_merged2 <- aggregate(cbind(imports1991_v2, imports2003_v2) ~ product, allrules_merged2, sum)

# compute the log of (1+x) for imports
allrules_merged2$logimports1991_v2 <- log(1 + allrules_merged2$imports1991_v2)
allrules_merged2$logimports2003_v2 <- log(1 + allrules_merged2$imports2003_v2)

# take the log difference
allrules_merged2$delta_logimports_NAFTA <- allrules_merged2$logimports2003_v2 - allrules_merged2$logimports1991_v2

# generate a variable that tells you whether there were positive imports in both years
allrules_merged2$pos_imports_NAFTA_1991_2003 <- ifelse(allrules_merged2$imports1991_v2 != 0 & allrules_merged2$imports2003_v2 != 0, 1, 0)

# keep the variable of interest and indicator for whether imports were positive in both years
allrules_merged2 <- allrules_merged2[c("product", "delta_logimports_NAFTA", "pos_imports_NAFTA_1991_2003")]

# save
#write.dta(roo_data2, "temp.dta")

# keep only non-USA and non-Canada partners
allrules_merged <- filter(allrules_merged, partnername != "Canada" & partnername != "United States")

# merge product_data and temp_data
allrules_merged_both <- left_join(allrules_merged, allrules_merged2, by = "product")



#for each product-NON-NAFTA country compute the difference between the log change
#in imports from that country and that from NAFTA
allrules_merged_both$delta_imports_triple_alt <- allrules_merged_both$delta_logimports_v2 - allrules_merged_both$delta_logimports_NAFTA
#label variable
attr(allrules_merged_both$delta_imports_triple_alt, "label") <- "delta_logimports_v2-delta_logimports_NAFTA"

names(allrules_merged_both)[names(allrules_merged_both) == "lroo3_treatedBOTH_10"] <- "lroo3_treatedBOTH"




#Run regression and store results

reg3_data<- subset(allrules_merged_both, RTAmex == 0)

reg3 <- plm(delta_imports_triple_alt ~ partner + delta_prefmargin_j + lroo3_treatedBOTH +0, index = c("partner"),cluster= "HS2digit",reg3_data, model = "within",effect = "twoway")

summary(reg3)

# save coefficients for lroo3_treatedBOTH and delta_prefmargin_j
coeff_RoO3 <- summary(reg3)$coef["lroo3_treatedBOTH", "Estimate"]
coeff_delta_margin <- summary(reg3)$coef["delta_prefmargin_j", "Estimate"]

# compute means for lroo3_treatedBOTH and delta_prefmargin_j for treated goods
mean_RoO3 <- mean(reg3_data$lroo3_treatedBOTH[reg3_data$lroo3_treatedBOTH > 0 & reg3_data$RTAmex==0 ])
mean_delta_margin <- mean(reg3_data$delta_prefmargin_j[reg3_data$lroo3_treatedBOTH > 0 & reg3_data$RTAmex==0 ])

# compute average effect of both coefficients in log points
av_eff_log_points <- mean_RoO3 * coeff_RoO3
av_eff_log_points_delta_margin <- mean_delta_margin * coeff_delta_margin

# compute mean of delta_imports_triple_alt for treated goods
delta_imp_triple_alt_roo3<-reg3_data$delta_imports_triple_alt[reg3_data$lroo3_treatedBOTH > 0 ]
mean_delta_imp <- mean(delta_imp_triple_alt_roo3, na.rm = T)

# compute fractions
av_eff_log_points_RoO3 <- av_eff_log_points*100 / mean_delta_imp
av_eff_delta_margin_RoO3 <- abs(av_eff_log_points_delta_margin)*100 / mean_delta_imp

table6_1<- list(coeff_RoO3,mean_RoO3,mean_delta_imp,av_eff_log_points_RoO3,av_eff_delta_margin_RoO3)






############## TABLE 6_2 ############## 

#Import data on imports at the product-country of origin level, countries of origin include US and CANADA
setwd("~/Downloads/113151-V1-2/data/Trade")
trade <- read_dta("DTA-tradeflows-tariffs-withNAFTA.dta")
View(trade)

#Switch back to the original working directory
setwd("~/Downloads/113151-V1-2/data/Treatments")

allrules_dummy_data <- read_dta("DTA-allrules-DRdummy.dta")
View(allrules_dummy_data)

#Merge data with RoO placebos data
allrules_dummy_merged <- merge(trade, allrules_dummy_data[, c("product", "roo1", "roo2_treatedBOTH", "roo3_treatedBOTH")], by = "product", all.x = TRUE,incomparables=NULL)

library(Hmisc)

#For goods with missing treatments data, replace with 0
allrules_dummy_merged[is.na(allrules_dummy_merged)] <- 0

# Take logs of RoO variables

roo_all_dummy_vars <- grep("^roo", names(allrules_dummy_merged), value = TRUE)
allrules_dummy_merged[paste0("l", roo_all_dummy_vars)] <- log(1 + allrules_dummy_merged[, roo_all_dummy_vars])


#take logs also for the tariffs. (1+log) since there are several zeros
#loop over variables starting with "MFN_"
for (k in grep("^MFN_", names(allrules_dummy_merged), value=TRUE)){
  allrules_dummy_merged[paste0("log", k)] <- log(1 + allrules_dummy_merged[, k])
}

#create new variable "logpref_NAFTA_2004" and take its log(1+value)
allrules_dummy_merged$logpref_NAFTA_2004 <- allrules_dummy_merged$pref_NAFTA_2004 + 1
allrules_dummy_merged$logpref_NAFTA_2004 <- log(allrules_dummy_merged$logpref_NAFTA_2004)

#label variables
label(allrules_dummy_merged$logMFN_tariff_MEX1991) <- "log MFN_tariff_MEX1991"
label(allrules_dummy_merged$logMFN_tariff_MEX2003) <- "log MFN_tariff_MEX2003"
label(allrules_dummy_merged$logpref_NAFTA_2004) <- "log pref_NAFTA_2004"


#tariff applied by Mexico to non-NAFTA before and after
allrules_dummy_merged$delta_logtariff <- log(allrules_dummy_merged$logMFN_tariff_MEX2003 + 1) - log(allrules_dummy_merged$logMFN_tariff_MEX1991 + 1)
#tariff applied by Mexico to NAFTA countries before and after
allrules_dummy_merged$delta_logtariff_NAFTA <- log(allrules_dummy_merged$logpref_NAFTA_2004 + 1) - log(allrules_dummy_merged$logMFN_tariff_MEX1991 + 1)

#(vi) compute the variable that we include in most of the specifications "ÆPreferential Tariff_{j,o}"

allrules_dummy_merged$delta_prefmargin_j <- allrules_dummy_merged$delta_logtariff - allrules_dummy_merged$delta_logtariff_NAFTA 

#take logs for the imports variables that contains zeros
for (t in c(1991, 2003)){
  allrules_dummy_merged[[paste0("logimports", t, "_v2")]] <- log(1 + allrules_dummy_merged[[paste0("imports", t, "_v2")]])
}

#
allrules_dummy_merged$delta_logimports_v2 <-  allrules_dummy_merged$logimports2003_v2-allrules_dummy_merged$logimports1991_v2


# collapse imports at the product level. This is: we want a measure of
# imports at the product level for NAFTA = USA + CANADA
##############################
#NOTE: the observations for CANADA and the US in the "DTA-tradeflows-tariffs-withNAFTA.dta"
#    are used in here below. That is why there is no information on tariffs. 
#     We add this information below when doing the merge


# keep US and Canada temporarily
allrules_dummy_merged2 <- subset(allrules_dummy_merged, partnername == "Canada" | partnername == "United States")

# collapse at the product using the sum
allrules_dummy_merged2 <- aggregate(cbind(imports1991_v2, imports2003_v2) ~ product, allrules_dummy_merged2, sum)

# compute the log of (1+x) for imports
allrules_dummy_merged2$logimports1991_v2 <- log(1 + allrules_dummy_merged2$imports1991_v2)
allrules_dummy_merged2$logimports2003_v2 <- log(1 + allrules_dummy_merged2$imports2003_v2)

# take the log difference
allrules_dummy_merged2$delta_logimports_NAFTA <- allrules_dummy_merged2$logimports2003_v2 - allrules_dummy_merged2$logimports1991_v2

# generate a variable that tells you whether there were positive imports in both years
allrules_dummy_merged2$pos_imports_NAFTA_1991_2003 <- ifelse(allrules_dummy_merged2$imports1991_v2 != 0 & allrules_dummy_merged2$imports2003_v2 != 0, 1, 0)

# keep the variable of interest and indicator for whether imports were positive in both years
allrules_dummy_merged2 <- allrules_dummy_merged2[c("product", "delta_logimports_NAFTA", "pos_imports_NAFTA_1991_2003")]

# keep only non-USA and non-Canada partners
allrules_dummy_merged <- filter(allrules_dummy_merged, partnername != "Canada" & partnername != "United States")

# merge product_data and temp_data
allrules_dummy_merged_both <- left_join(allrules_dummy_merged, allrules_dummy_merged2, by = "product")



#for each product-NON-NAFTA country compute the difference between the log change
#in imports from that country and that from NAFTA
allrules_dummy_merged_both$delta_imports_triple_alt <- allrules_dummy_merged_both$delta_logimports_v2 - allrules_dummy_merged_both$delta_logimports_NAFTA
#label variable
attr(allrules_dummy_merged_both$delta_imports_triple_alt, "label") <- "delta_logimports_v2-delta_logimports_NAFTA"

names(allrules_dummy_merged_both)[names(allrules_dummy_merged_both) == "lroo3_treatedBOTH_10"] <- "lroo3_treatedBOTH"




#Run second regression and store results

reg4_data<- subset(allrules_dummy_merged_both, RTAmex == 0)

reg4 <- plm(delta_imports_triple_alt ~ partner + delta_prefmargin_j + lroo3_treatedBOTH +0, index = c("partner"),cluster= "HS2digit",reg4_data, model = "within",effect = "twoway")

summary(reg4)

# save coefficients for lroo3_treatedBOTH and delta_prefmargin_j
coeff_RoO3_2 <- summary(reg4)$coef["lroo3_treatedBOTH", "Estimate"]
coeff_delta_margin_2 <- summary(reg4)$coef["delta_prefmargin_j", "Estimate"]

# compute means for lroo3_treatedBOTH and delta_prefmargin_j for treated goods
mean_RoO3_2 <- mean(reg4_data$lroo3_treatedBOTH[reg4_data$lroo3_treatedBOTH > 0 & reg4_data$RTAmex==0 ])
mean_delta_margin_2 <- mean(reg4_data$delta_prefmargin_j[reg4_data$lroo3_treatedBOTH > 0 & reg4_data$RTAmex==0  ])

# compute average effect of both coefficients in log points
av_eff_log_points_2 <- mean_RoO3_2 * coeff_RoO3_2
av_eff_log_points_delta_margin_2 <- mean_delta_margin_2 * coeff_delta_margin_2

# compute mean of delta_imports_triple_alt for treated goods
delta_imp_triple_alt_roo3_2<-reg4_data$delta_imports_triple_alt[reg4_data$lroo3_treatedBOTH > 0 ]
mean_delta_imp_2 <- mean(delta_imp_triple_alt_roo3_2, na.rm = T)

# compute fractions
av_eff_log_points_RoO3_2 <- av_eff_log_points_2*100 / mean_delta_imp_2
av_eff_delta_margin_RoO3_2 <- abs(av_eff_log_points_delta_margin_2)*100 / mean_delta_imp_2

table6_2<- list(coeff_RoO3_2,mean_RoO3_2,mean_delta_imp_2,av_eff_log_points_RoO3_2,av_eff_delta_margin_RoO3_2)
sum_table6<-cbind("(1)"=table6_1,"(2)"=table6_2)
rownames(sum_table6)<- c("beta hat","mean RoO3","delta imports","effect of Roo3(in log points)","effect of Roo3(as percent of deltaimports)")
stargazer(sum_table6, type = "text", dep.var.labels = c(),title="Table 6",digits=3,rownames = T, out="Table6.txt")
stargazer(sum_table6, type = "html", dep.var.labels = c(),title="Table 6",digits=3,rownames = T, out="Table6_final.html")

help(stargazer)


