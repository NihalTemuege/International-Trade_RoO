################## EXTENSION ################## 

#import distance data

setwd("~/Downloads/113151-V1-2/data/Treatments")
distance <- read_dta("dist_cepii.dta")
View(distance)

# keep distance data for Mexico 
distance <- subset(distance, iso_o == "MEX")


## import country codes data from "https://public.opendatasoft.com/explore/dataset/countries-codes/export/"

library(readr)
countries_codes <- read_delim("countries-codes.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(countries_codes)

colnames(countries_codes)[4]<-"countrycode"
colnames(countries_codes)[3]<-"iso_d"
merged_country_codes<- merge(distance,countries_codes[,c("iso_d","countrycode")],by = "iso_d", all.x = TRUE,incomparables=NULL)

##deleting zeros at the beginning of country codes to be able to use it when we merge two datasets
merged_country_codes$countrycode <- sub("^0+", "", merged_country_codes$countrycode) 

#### take log of distance
merged_country_codes$distwces<- log(merged_country_codes$distwces)
# change column name to be able to merge two datasets

colnames(merged_country_codes)[15]<-"partner"

merged_distance_table5_reg1 <- merge(reg1_data,merged_country_codes,by = "partner", all.x = TRUE,incomparables=NULL)
merged_distance_table5_reg2 <- merge(reg2_data,merged_country_codes,by = "partner", all.x = TRUE,incomparables=NULL)
merged_distance_table5_reg1$distwces[is.na(merged_distance_table5_reg1$distwces)]

##############  Table 5 ############## 

### now we also have bilateral distance variable in our datasets for regression in table 5 :))
colnames(merged_distance_table5_reg1)

#Run regressions in Table 5 by adding log of distance variable and store results


reg1_ext<- lm(delta_imports_triple_alt ~ lroo_placebo_treatedBOTH + lroo_placebo_beta_treatedBOTH +
                lroo3_treatedBOTH +delta_prefmargin_j+ distwces+0 ,merged_distance_table5_reg1)
reg2_ext <- lm(delta_imports_triple_alt ~ 
                  lroo_placebo_treatedBOTH + lroo_placebo_beta_treatedBOTH +
                  lroo3_treatedBOTH +delta_prefmargin_j + distwces +0, merged_distance_table5_reg2)



stargazer(reg1_ext,reg2_ext, type = "html", dep.var.labels = c(),title="Table 5", covariate.labels = c("RoO Placebo(RoO1-RoO2)","RoO Flexible(RoO2-RoO3)","RoO Strict(RoO3)","delta preferential Tariff","distance"), digits=3, out="Table5_ext3.html")












##############  Table 7 ############## 

#### datasets are the same  for regressions in table 7 : reg7_data = reg8_data & merge it with distance data

merged_distance_table7 <- merge(reg7_data,merged_country_codes,by = "partner", all.x = TRUE,incomparables=NULL)



#Run regression and store results

colnames(merged_distance_table7)
reg7_ext <- plm(delta_imports_triple_alt ~ lroo3_treatedBOTH*BOTHPref_finalPOS_6_log + delta_prefmargin_j + distwces*lroo3_treatedBOTH +0, index = c("partner"),cluster= "HS2digit",merged_distance_table7, model = "within",effect = "twoway")
summary(reg7_ext)


reg8_data<- subset(merged_distance_table7, RTAmex == 0)
colnames(reg8_data)
reg8_ext <- plm(delta_imports_triple_alt ~ lroo3_treatedBOTH*log_sumExports_USA_CAN_v2 + delta_prefmargin_j+distwces*lroo3_treatedBOTH +0, index = c("partner"),cluster= "HS2digit",reg8_data, model = "within",effect = "twoway")
summary(reg8_ext)


reg9_ext <- plm(delta_imports_triple_alt ~ lroo3_treatedBOTH*BOTHPref_finalPOS_6_log  + lroo3_treatedBOTH*log_sumExports_USA_CAN_v2 + delta_prefmargin_j+distwces*lroo3_treatedBOTH +0, index = c("partner"),cluster= "HS2digit",reg8_data, model = "within",effect = "twoway")
summary(reg9)



stargazer(reg7_ext,reg8_ext,reg9_ext, type = "text",title="Table 7", covariate.labels=c("RoO3","Average Margin NAFTA","Exports NAFTA","Delta preferential Tariff","RoO3xAverage Margin NAFTA","RoO3xExports NAFTA"), dep.var.labels = c(), out="Table7_final.txt")
stargazer(reg7_ext,reg8_ext,reg9_ext, type = "html",title="Table 7", covariate.labels=c("RoO3","Average Margin NAFTA","Exports NAFTA","Delta preferential Tariff","RoO3xAverage Margin NAFTA","RoO3xExports NAFTA","RoO3xDistance"), dep.var.labels = c(), out="Table7_final_ext.html")


##############  Table 4 ############## 

# Here,we use the same dataset for Table 7 extension : merged_distance_table7 
colnames(merged_distance_table7)

merged_dist_table4_ext<- subset(merged_distance_table7, RTAmex == 0)
reg1_table4_ext <- lm(delta_imports_triple_alt ~ lroo1 + delta_prefmargin_j + distwces + 0, merged_dist_table4_ext)
reg2_table4_ext <- lm(delta_imports_triple_alt ~ lroo2_treatedBOTH + delta_prefmargin_j + distwces +0,merged_dist_table4_ext)
reg3_table4_ext <- lm(delta_imports_triple_alt ~ lroo3_treatedBOTH + delta_prefmargin_j + distwces +0,merged_dist_table4_ext)


stargazer(reg1_table4_ext,reg2_table4_ext,reg3_table4_ext, type = "text",title="Table 4",covariate.labels=c("RoO1","RoO2","RoO3","Delta preferential Tariff"), dep.var.labels = c(), out="Table4_ext.txt")
stargazer(reg1_table4_ext,reg2_table4_ext,reg3_table4_ext, type = "html",title="Table 4",covariate.labels=c("RoO1","RoO2","RoO3","Delta preferential Tariff"), dep.var.labels = c(), out="Table4_ext.html")





