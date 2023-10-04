################# TABLE 7 ################# 


############## TABLE 7_1 ############## 
#Import data on imports at the product-country of origin level, countries of origin include US and CANADA
setwd("~/Downloads/113151-V1-2/data/Trade")
trade <- read_dta("DTA-tradeflows-tariffs-withNAFTA.dta")
View(trade)

#Switch back to the original working directory
setwd("~/Downloads/113151-V1-2/data/Treatments")

allrules_data <- read_dta("DTA-allrules.dta")
View(allrules_data)

  
#Merge data with RoO placebos data
allrules_merge_7 <- merge(trade, allrules_data[, c("product", "roo1", "roo2_treatedBOTH", "roo3_treatedBOTH","BOTHPref_finalPOS_6_log","sumExports_USA_CAN_v2" )], by = "product", all.x = TRUE,incomparables=NULL)
View(allrules_merge_7)

#For goods with missing treatments data, replace with 0
allrules_merge_7[is.na(allrules_merge_7)] <- 0

# Take logs of RoO variables

roo_all_vars7 <- grep("^roo", names(allrules_merge_7), value = TRUE)
allrules_merge_7[paste0("l", roo_all_vars7)] <- log(1 + allrules_merge_7[, roo_all_vars7])


#take logs also for the tariffs. (1+log) since there are several zeros
#loop over variables starting with "MFN_"
for (k in grep("^MFN_", names(allrules_merge_7), value=TRUE)){
  allrules_merge_7[paste0("log", k)] <- log(1 + allrules_merge_7[, k])
}

#create new variable "logpref_NAFTA_2004" and take its log(1+value)
allrules_merge_7$logpref_NAFTA_2004 <- allrules_merge_7$pref_NAFTA_2004 + 1
allrules_merge_7$logpref_NAFTA_2004 <- log(allrules_merge_7$logpref_NAFTA_2004)
#label variables
label(allrules_merge_7$logMFN_tariff_MEX1991) <- "log MFN_tariff_MEX1991"
label(allrules_merge_7$logMFN_tariff_MEX2003) <- "log MFN_tariff_MEX2003"
label(allrules_merge_7$logpref_NAFTA_2004) <- "log pref_NAFTA_2004"


#tariff applied by Mexico to non-NAFTA before and after
allrules_merge_7$delta_logtariff <- log(allrules_merge_7$logMFN_tariff_MEX2003 + 1) - log(allrules_merge_7$logMFN_tariff_MEX1991 + 1)
#tariff applied by Mexico to NAFTA countries before and after
allrules_merge_7$delta_logtariff_NAFTA <- log(allrules_merge_7$logpref_NAFTA_2004 + 1) - log(allrules_merge_7$logMFN_tariff_MEX1991 + 1)

#(vi) compute the variable that we include in most of the specifications "ÆPreferential Tariff_{j,o}"

allrules_merge_7$delta_prefmargin_j <- allrules_merge_7$delta_logtariff - allrules_merge_7$delta_logtariff_NAFTA 

#take logs for the imports variables that contains zeros
for (t in c(1991, 2003)){
  allrules_merge_7[[paste0("logimports", t, "_v2")]] <- log(1 + allrules_merge_7[[paste0("imports", t, "_v2")]])
}

#
allrules_merge_7$delta_logimports_v2 <-  allrules_merge_7$logimports2003_v2-allrules_merge_7$logimports1991_v2


# collapse imports at the product level. This is: we want a measure of
# imports at the product level for NAFTA = USA + CANADA
##############################
#NOTE: the observations for CANADA and the US in the "DTA-tradeflows-tariffs-withNAFTA.dta"
#    are used in here below. That is why there is no information on tariffs. 
#     We add this information below when doing the merge


# keep US and Canada temporarily
allrules_merge_7_2 <- subset(allrules_merge_7, partnername == "Canada" | partnername == "United States")

# collapse at the product using the sum
allrules_merge_7_2 <- aggregate(cbind(imports1991_v2, imports2003_v2) ~ product, allrules_merge_7_2, sum)

# compute the log of (1+x) for imports
allrules_merge_7_2$logimports1991_v2 <- log(1 + allrules_merge_7_2$imports1991_v2)
allrules_merge_7_2$logimports2003_v2 <- log(1 + allrules_merge_7_2$imports2003_v2)

# take the log difference
allrules_merge_7_2$delta_logimports_NAFTA <- allrules_merge_7_2$logimports2003_v2 - allrules_merge_7_2$logimports1991_v2

# generate a variable that tells you whether there were positive imports in both years
allrules_merge_7_2$pos_imports_NAFTA_1991_2003 <- ifelse(allrules_merge_7_2$imports1991_v2 != 0 & allrules_merge_7_2$imports2003_v2 != 0, 1, 0)

# keep the variable of interest and indicator for whether imports were positive in both years
allrules_merge_7_2 <- allrules_merge_7_2[c("product", "delta_logimports_NAFTA", "pos_imports_NAFTA_1991_2003")]

# save
#write.dta(roo_data2, "temp.dta")

# keep only non-USA and non-Canada partners
allrules_merge_7 <- filter(allrules_merge_7, partnername != "Canada" & partnername != "United States")

# merge product_data and temp_data
allrules_merged_both_7 <- left_join(allrules_merge_7, allrules_merge_7_2, by = "product")



#for each product-NON-NAFTA country compute the difference between the log change
#in imports from that country and that from NAFTA
allrules_merged_both_7$delta_imports_triple_alt <- allrules_merged_both_7$delta_logimports_v2 - allrules_merged_both_7$delta_logimports_NAFTA
#label variable
attr(allrules_merged_both_7$delta_imports_triple_alt, "label") <- "delta_logimports_v2-delta_logimports_NAFTA"

names(allrules_merged_both_7)[names(allrules_merged_both_7) == "lroo3_treatedBOTH_10"] <- "lroo3_treatedBOTH"


#take logs of the sum of exports to US and CAN of outputs for each input
allrules_merged_both_7$log_sumExports_USA_CAN_v2 <- log(1+allrules_merged_both_7$sumExports_USA_CAN_v2)




#Run regression and store results

reg7_data<- subset(allrules_merged_both_7, RTAmex == 0)
colnames(reg7_data)
reg7 <- plm(delta_imports_triple_alt ~ lroo3_treatedBOTH*BOTHPref_finalPOS_6_log + delta_prefmargin_j +0, index = c("partner"),cluster= "HS2digit",reg7_data, model = "within",effect = "twoway")

summary(reg7)

t7_1_N<-nobs(reg7)
t7_1_r2<-summary(reg7)$r.squared[1]
table7_1<- c(unlist(reg7[1]["coefficients"]),t7_1_N,t7_1_r2)
names(table7_1)<- c("RoO3","Average Margin NAFTA","Delta preferential Tariff","RoO3xAverage Margin NAFTA","Observations","R^2")



reg8_data<- subset(allrules_merged_both_7, RTAmex == 0)
colnames(reg8_data)
reg8 <- plm(delta_imports_triple_alt ~ lroo3_treatedBOTH*log_sumExports_USA_CAN_v2 + delta_prefmargin_j +0, index = c("partner"),cluster= "HS2digit",reg8_data, model = "within",effect = "twoway")
summary(reg8)

t7_2_N<-nobs(reg8)
t7_2_r2<-summary(reg8)$r.squared[1]
table7_2<- c(unlist(reg8[1]["coefficients"]),t7_2_N,t7_2_r2)
names(table7_2)<- c("RoO3","Exports NAFTA","Delta preferential Tariff","RoO3xExports NAFTA","Observations","R^2")



reg9 <- plm(delta_imports_triple_alt ~ lroo3_treatedBOTH*BOTHPref_finalPOS_6_log  +lroo3_treatedBOTH*log_sumExports_USA_CAN_v2 + delta_prefmargin_j +0, index = c("partner"),cluster= "HS2digit",reg8_data, model = "within",effect = "twoway")
summary(reg9)


t7_3_N<-nobs(reg9)
t7_3_r2<-summary(reg9)$r.squared[1]
table7_3<- c(unlist(reg9[1]["coefficients"]),t7_3_N,t7_3_r2)
names(table7_3)<- c("RoO3","Average Margin NAFTA","Exports NAFTA","Delta preferential Tariff","RoO3xAverage Margin NAFTA","RoO3xExports NAFTA","Observations","R^2")

table7_1<-matrix(table7_1)
table7_2<-matrix(table7_2)
table7_3<-matrix(table7_3)
sum_table<- cbind("1"=table7_1,"2"=table7_2,"3"=table7_3)
stargazer(sum_table, type = "text", dep.var.labels = c(), out="Table7.txt")

t7_1<-summary(reg7)
t7_2<-summary(reg8)
stargazer(reg7,reg8,reg9, type = "text", dep.var.labels = c(), out="Table7_try.txt")
help(stargazer)

stargazer(reg7,reg8,reg9, type = "text", covariate.labels=c("RoO3","Average Margin NAFTA","Exports NAFTA","Delta preferential Tariff","RoO3xAverage Margin NAFTA","RoO3xExports NAFTA"), dep.var.labels = c(), out="Table7_final.txt")
stargazer(reg7,reg8,reg9, type = "html", covariate.labels=c("RoO3","Average Margin NAFTA","Exports NAFTA","Delta preferential Tariff","RoO3xAverage Margin NAFTA","RoO3xExports NAFTA"), dep.var.labels = c(), out="Table7_final.html")



##################### TABLE 4 ######################
reg1_table4 <- plm(delta_imports_triple_alt ~ lroo1 + delta_prefmargin_j +0, index = c("partner"),cluster= "HS2digit",reg7_data, model = "within",effect = "twoway")
reg2_table4 <- plm(delta_imports_triple_alt ~ lroo2_treatedBOTH + delta_prefmargin_j +0, index = c("partner"),cluster= "HS2digit",reg7_data, model = "within",effect = "twoway")
reg3_table4 <- plm(delta_imports_triple_alt ~ lroo3_treatedBOTH + delta_prefmargin_j +0, index = c("partner"),cluster= "HS2digit",reg7_data, model = "within",effect = "twoway")

stargazer(reg1_table4,reg2_table4,reg3_table4, type = "text",title="Table 4",covariate.labels=c("RoO1","RoO2","RoO1","Delta preferential Tariff"), dep.var.labels = c(), out="Table4.txt")
stargazer(reg1_table4,reg2_table4,reg3_table4, type = "html",title="Table 4",covariate.labels=c("RoO1","RoO2","RoO1","Delta preferential Tariff"), dep.var.labels = c(), out="Table4.html")



