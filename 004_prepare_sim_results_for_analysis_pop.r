# ==============================
# Preparation of simulations results (Population level)
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================

rm(list=ls())

library(data.table)

# source functions
	source("000_Funs/func_do_summary_stratified_mean_time_series.r")

# select site
	site<-"Norrbyn"

# select scenario
	scenario <- "AllocOrigin" # "AllocScenarioHerringN", "AllocOrigin"

# produces datasets
	for (site in c("Asköfjärden", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kvädöfjärden_Warm","Lagnö","Långvindsfjärden","Norrbyn", "Råneå","Torhamn")
	{  
		# use during testing
		# site<-"Asköfjärden"
		
		print(site)
		
		# sets dir
			dir_sim_outputs <- paste("002_Sim_Outputs/",scenario,"/",sep="")

		# loads original data			
			dir_inputs<-"001_Inputs/prepared_main/"
			load(file=paste("001_Inputs/prepared_main/",site,".Rdata",sep=""))
	 
		# loads simulated data (population)
			sim_pop<-readRDS(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_pop.rds", sep="")) 
			load(file=paste(dir_sim_outputs,site, "_", scenario,"_5000sims_settings.Rdata", sep=""))
			
				# deletes unnecessary columns
					sim_pop <- sim_pop[,!c(".id")]
				# creates useful IDs
					sim_pop[, ID:=paste(Year, variable),]
					setkey(sim_pop, ID)

		# =============
		# Present ("true") estimates: point estimates
		# =============
		
		# runs stratified mean on site data set
			res_pop<-c()
			for (i in target_vars)
			{
			res_pop<-rbind(res_pop, do_summary_stratified_mean_time_series(x = dt_site, target_var = i, strata_var = "DepthStratum", strata_size_var = "NStations", period_var = "Year", klab = TRUE)$pop_res)
			}
				# fixes columns
				colnames(res_pop)[colnames(res_pop)=="period"]<-"Year"
				# creates IDs
				res_pop$ID<-paste(res_pop$Year,res_pop$variable); sum(duplicated(res_pop$ID))==0
				# sets to data.table
				res_pop<-data.table(res_pop)
				# sets key
				setkey(res_pop, ID)
				
		
			# a look into the final structure of the objects
				res_pop[,.N, c("ID")]

				
		# =============
		# Present ("true") estimates: Bootstrap CIs
		# =============	
		
		# Calculates bootstrap CIs from replicates
				#bootlow_perc, boothigh_perc = percentile bootstrap centered on true mean
			
			sim_pop[, `:=`(true_mean = res_pop[sim_pop]$st_mean, true_var_x = res_pop[sim_pop]$st_var_x, true_var_mean = res_pop[sim_pop]$st_var_mean, true_clow_mean = res_pop[sim_pop]$clow_mean, true_chigh_mean = res_pop[sim_pop]$chigh_mean, true_rse_mean = res_pop[sim_pop]$st_rse_mean),]

			ls1<-split(sim_pop, paste(sim_pop$Year, sim_pop$SampSize, sim_pop$variable))
			ls2<-lapply(ls1, function(x){
								a<-quantile(x$mean, c(0.025, 0.975), na.rm=T); 
								b<-quantile(x$mean-mean(x$mean, na.rm=T), c(0.025, 0.975), na.rm=T); 
								ci <- x$true_mean[1]+b; 
								data.frame(Area= x$Area[1], Season= x$Season[1], Year = x$Year[1], variable=x$variable[1], SampSize = x$SampSize[1], 
												true_mean = x$true_mean[1], true_var_x =x$true_var_x[1], true_var_mean =x$true_var_mean[1],  true_rse_mean = x$true_rse_mean[1], true_clow_mean = x$true_clow_mean[1], true_chigh_mean = x$true_chigh_mean[1], 
												boot_mean_n = mean(x$n_tot, na.rm=T), boot_mean = mean(x$mean, na.rm=T), boot_var_x = mean(x$var_x, na.rm=T), boot_var_mean=var(x$mean, na.rm=T), boot_se=sqrt(var(x$mean, na.rm=T)), boot_rse = round(sqrt(var(x$mean, na.rm=T))/x$true_mean[1]*100,1),
														bootlow = a[1], boothigh = a[2], 
															bootlow_perc = ci[1], boothigh_perc = ci[2], 
																bootNA=sum(is.na(x$mean))) })
			boot_res_pop <- rbindlist(ls2)
			boot_res_pop$SampSize<-factor(boot_res_pop$SampSize)
			
			# clean up
			gc(); rm(ls1, ls2, sim_pop); gc()
			
			# adds bootstrap ci with normal approx
			boot_res_pop$bootlow_aprox<-boot_res_pop$true_mean-1.96*sqrt(boot_res_pop$boot_var_mean) 
			boot_res_pop$boothigh_aprox<-boot_res_pop$true_mean+1.96*sqrt(boot_res_pop$boot_var_mean)

			# adds boot_mean_bias and boot_mean_bias_perc
			boot_res_pop$boot_mean_bias<-boot_res_pop$boot_mean-boot_res_pop$true_mean
			boot_res_pop$boot_mean_bias_perc<-round(boot_res_pop$boot_mean_bias/boot_res_pop$true_mean*100,1)

			# adds boot_var_bias and boot_var_bias_perc
			boot_res_pop$boot_var_bias<-boot_res_pop$boot_var_x-boot_res_pop$true_var_x
			boot_res_pop$boot_var_bias_perc<-round(boot_res_pop$boot_var_bias/boot_res_pop$true_var_x*100,1)
		
			# adds boot_var_mean_bias and boot_var_mean_bias_perc
			boot_res_pop$boot_var_mean_bias<-boot_res_pop$boot_var_mean-boot_res_pop$true_var_mean
			boot_res_pop$boot_var_mean_bias_perc<-round(boot_res_pop$boot_var_mean_bias/boot_res_pop$true_var_mean*100,1)

				# a look into the final structure of the objects
				table(boot_res_pop$Year, boot_res_pop$SampSize, boot_res_pop$variable)
				#table(sim_samples[repl==1,]$Year, sim_samples[repl==1,]$SampSize)
				tapply(boot_res_pop$true_mean,list(boot_res_pop$Year, boot_res_pop$SampSize, boot_res_pop$variable), mean)

		# =============
		# Save prepared objects
		# =============	
			
			dir_sim_outputs<-paste(dir_sim_outputs,"prepared/",sep=""); dir.create(dir_sim_outputs, showWarnings = FALSE)
			filename<-paste(site,"_",scenario,"_",nsim,"sims_res_pop_prep.rdata",sep="");
			save(boot_res_pop, res_pop, target_vars, nsim, cpus, scenario, site, sampOpt, file = paste(dir_sim_outputs,filename,sep=""))			
			print(paste(dir_sim_outputs,filename,sep=""))
	}
