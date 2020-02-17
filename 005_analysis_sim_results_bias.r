# ==============================
# Analysis of simulation bias (population and strata level)
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================


rm(list=ls()); graphics.off()

library(data.table)

# select site
	site<-"Norrbyn" # sites in c("Asköfjärden", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kvädöfjärden_Warm","Lagnö","Långvindsfjärden","Norrbyn", "Råneå","Torhamn")

# select scenario
	scenario <- "AllocOrigin" # "AllocScenarioWhitefishN" # "AllocScenarioHerringN", "AllocOrigin"

# loads original data			
	dir_inputs<-"001_Inputs/prepared_main/"
	load(file=paste("001_Inputs/prepared_main/",site,".Rdata",sep=""))
 
# loads simulated data (prepared)
	dir_sim_outputs <- paste("002_Sim_Outputs/",scenario,"/prepared/",sep="")
	load(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_pop_prep.rdata", sep="")) 
	load(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_strata_prep.rdata", sep="")) 
		

# ====================
# Analysis of bias
# ====================
	
	# ===================	
	# Population level	
	# ===================
	
		# mean
		tapply(boot_res_pop$boot_mean_bias_perc, boot_res_pop$SampSize, function(x) {mean(x, na.rm=TRUE)})
		do.call("rbind", tapply(boot_res_pop$boot_mean_bias_perc, boot_res_pop$SampSize, function(x) {quantile(x, na.rm=TRUE)}))
		do.call("rbind", tapply(boot_res_pop$boot_mean_bias_perc, paste(boot_res_pop$SampSize, boot_res_pop$variable), function(x) {quantile(x, na.rm=TRUE)}))
			
		# variance 
		tapply(boot_res_pop$boot_var_bias_perc, boot_res_pop$SampSize, function(x) {mean(x, na.rm=TRUE)})
		do.call("rbind", tapply(boot_res_pop$boot_var_bias_perc, boot_res_pop$SampSize, function(x) {quantile(x, na.rm=TRUE)}))
		do.call("rbind", tapply(boot_res_pop$boot_var_bias_perc, paste(boot_res_pop$SampSize, boot_res_pop$variable), function(x) {quantile(x, na.rm=TRUE)}))
			
		# variance of the mean [variance of the bootstrap means]
		tapply(boot_res_pop$boot_var_mean_bias_perc, boot_res_pop$SampSize, function(x) {mean(x, na.rm=TRUE)})
		do.call("rbind", tapply(boot_res_pop$boot_var_mean_bias_perc, boot_res_pop$SampSize, function(x) {quantile(x, na.rm=TRUE)}))
		do.call("rbind", tapply(boot_res_pop$boot_var_mean_bias_perc, paste(boot_res_pop$SampSize, boot_res_pop$variable), function(x) {quantile(x, na.rm=TRUE)}))
			
	# ===================	
	# Strata level	
	# ===================
			
		# mean
		tapply(boot_res_strata$boot_mean_bias_perc, boot_res_strata$SampSize, function(x) {mean(x, na.rm=TRUE)})
		do.call("rbind", tapply(boot_res_strata$boot_mean_bias_perc, boot_res_strata$SampSize, function(x) {quantile(x, na.rm=TRUE)}))
		do.call("rbind", tapply(boot_res_strata$boot_mean_bias_perc, paste(boot_res_strata$SampSize, boot_res_strata$variable), function(x) {quantile(x, na.rm=TRUE)}))
			
		# variance  [mean of the bootstrap variances]
		tapply(boot_res_strata$boot_var_bias_perc, boot_res_strata$SampSize, function(x) {mean(x, na.rm=TRUE)})
		do.call("rbind", tapply(boot_res_strata$boot_var_bias_perc, boot_res_strata$SampSize, function(x) {quantile(x, na.rm=TRUE)}))
		do.call("rbind", tapply(boot_res_strata$boot_var_bias_perc, paste(boot_res_strata$SampSize, boot_res_strata$variable), function(x) {quantile(x, na.rm=TRUE)}))
			
		# variance of the mean  [variance of the bootstrap means]
		tapply(boot_res_strata$boot_var_mean_bias_perc, boot_res_strata$SampSize, function(x) {mean(x, na.rm=TRUE)})
		do.call("rbind", tapply(boot_res_strata$boot_var_mean_bias_perc, boot_res_strata$SampSize, function(x) {quantile(x, na.rm=TRUE)}))
		do.call("rbind", tapply(boot_res_strata$boot_var_mean_bias_perc, paste(boot_res_strata$SampSize, boot_res_strata$variable), function(x) {quantile(x, na.rm=TRUE)}))
