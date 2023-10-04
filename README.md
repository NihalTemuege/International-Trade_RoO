# International-Trade_RoO
# Replication of “From Final Goods to Inputs: The Protectionist Effect of Rules of Origin” by Paola Conconi, Manuel García-Santana, Laura Puccio and Roberto Venturini


 1) 	Int_Trade_log_data.R file contains log function at the beginning ( its input can be seen in the screenshot ) and then data construction process by using raw datasets: DTA_Mexico19912003-WTO-countries.dta, MFN_MEX.dta, DTA_pref_2004_Mexico_as_importerHS1992.dta, DTA_Mexico19912003-WTO-countries.dta provided by the authors.
		At the end of the codes, "DTA-tradeflows-tariffs-withNAFTA.dta" dataset is created. 

 2)	Table5.R file is to replicate Table5 by using raw datasets DTA-allrules-placebo.dta, DTA-allrules-DRdummy-placebo.dta provided by the authors and dataset DTA-tradeflows-tariffs-withNAFTA.dta that is created in Int_Trade_log_data.R file. 

 3)	Table6.R file is to replicate Table6 by using raw datasets DTA-allrules.dta, DTA-allrules-DRdummy.dta provided by the authors and dataset DTA-tradeflows-tariffs-withNAFTA.dta that is created in Int_Trade_log_data.R file. 

 4)	 Table7&Table4.R file is to replicate Table5 by using dataset DTA-tradeflows-tariffs-withNAFTA.dta that is created in Int_Trade_log_data.R file. 

 5)	Extension file for additional robustness check tables. Here, distance data is imported (dist_cepii.dta) from http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=6 and country codes data from "https://public.opendatasoft.com/explore/dataset/countries-codes/export/" is imported. And they are combined with datasets created in previous files. 
