# ==============================
# Preparation of simulations results (Strata level)
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================

rm(list=ls())

library(data.table)

# source functions
source("000_Funs/func_do_summary_stratified_mean_time_series.r")

# select scenario
	scenario <- "AllocOrigin" # "AllocScenarioHerringN", "AllocOrigin"

# produces datasets
	for (site in c("Asköfjärden", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kvädöfjärden_Warm","Lagnö","Långvindsfjärden","Norrbyn", "Råneå","Torhamn") 
	{ 

		# use during testing
		# site<-"Asköfjärden"
	  
	print(site)

	dir_sim_outputs <- paste("002_Sim_Outputs/",scenario,"/",sep="")

		# loads original data			
			dir_inputs<-"001_Inputs/prepared_main/"
			load(file=paste("001_Inputs/prepared_main/",site,".Rdata",sep=""))
	 
		# loads simulations
			sim_strata<-readRDS(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_strata.rds", sep="")) 
			load(file=paste(dir_sim_outputs,site, "_", scenario,"_5000sims_settings.Rdata", sep=""))
				# deletes unnecessary columns
					sim_strata <- sim_strata[,!c(".id")]
				# creates useful IDs
					sim_strata[, ID:=paste(Year, variable, stratum),]
				# sets key	
					setkey(sim_strata, ID)
		
		# =============
		# Present ("true") estimates: point estimates
		# =============
		
		# runs stratified mean on site data set
			res_strata<-c()
			for (i in target_vars)
			{
			res_strata<-rbind(res_strata, do_summary_stratified_mean_time_series(x = dt_site, target_var = i, strata_var = "DepthStratum", strata_size_var = "NStations", period_var = "Year", klab = TRUE)$stratum_res)
			}
				# fixes columns
				colnames(res_strata)[colnames(res_strata)=="period"]<-"Year"
				# creates IDs
				res_strata$ID<-paste(res_strata$Year, res_strata$variable, res_strata$stratum); sum(duplicated(res_strata$ID))==0
				# sets to data.table
				res_strata<-data.table(res_strata)
				setkey(res_strata, ID)
		
			# a look into the final structure of the objects
				res_strata[,.N, c("ID")]
				

		# =============
		# Present ("true") estimates: Bootstrap CIs
		# =============	
		
		# Calculates bootstrap CIs from replicates
				#bootlow_perc, boothigh_perc = percentile bootstrap centered on true mean
				
			nlevls<-nlevels(sim_strata$SampSize)

		# if nlevls>1 splits series (for memory saving)		
			if(nlevls>1){
			sim_strata1<-sim_strata[SampSize %in% levels(SampSize)[1:(nlevls%/%2)],]	
			sim_strata2<-sim_strata[SampSize %in% levels(SampSize)[(nlevls%/%2+1):nlevls],]	
			
			# cleans up
			rm(sim_strata); gc()

			sim_strata1[, `:=`(true_n = res_strata[sim_strata1]$n, true_mean = res_strata[sim_strata1]$mean_x, true_var_x = res_strata[sim_strata1]$var_x, true_var_mean = res_strata[sim_strata1]$var_mean_x, true_clow_mean = res_strata[sim_strata1]$clow_mean, true_chigh_mean = res_strata[sim_strata1]$chigh_mean, true_rse_mean = res_strata[sim_strata1]$rse_mean_x),]
			
			ls1<-split(sim_strata1, paste(sim_strata1$Year, sim_strata1$SampSize, sim_strata1$variable, sim_strata1$stratum))
			ls2<-lapply(ls1, function(x){
			  a<-quantile(x$mean, c(0.025, 0.975), na.rm=T); 
			  b<-quantile(x$mean-mean(x$mean, na.rm=T), c(0.025, 0.975), na.rm=T); 
			  ci <- x$true_mean[1]+b; 
			  data.frame(Area= x$Area[1], Season= x$Season[1], Year = x$Year[1], variable=x$variable[1], stratum = x$stratum[1], stratum_size = x$stratum_size[1], SampSize = x$SampSize[1], 
						 true_n = x$true_n[1], true_mean = x$true_mean[1], true_var_x =x$true_var_x[1], true_var_mean =x$true_var_mean[1], true_rse_mean = x$true_rse_mean[1], true_clow_mean = x$true_clow_mean[1], true_chigh_mean = x$true_chigh_mean[1], 
						 boot_mean_n = mean(x$n, na.rm=T), boot_mean = mean(x$mean_x, na.rm=T), boot_var_x = mean(x$var_x, na.rm=T), boot_var_mean=var(x$mean_x, na.rm=T), boot_se=sqrt(var(x$mean_x, na.rm=T)), boot_rse = round(sqrt(var(x$mean_x, na.rm=T))/x$true_mean[1]*100,1),
						 bootlow = a[1], boothigh = a[2], 
						 bootlow_perc = ci[1], boothigh_perc = ci[2], 
						 bootNA=sum(is.na(x$mean_x))) })
			boot_res_strata1 <- rbindlist(ls2)
			gc(); rm(ls1, ls2, sim_strata1); gc()		
			
			sim_strata2[, `:=`(true_n = res_strata[sim_strata2]$n, true_mean = res_strata[sim_strata2]$mean_x, true_var_mean = res_strata[sim_strata2]$var_mean_x,true_var_x = res_strata[sim_strata2]$var_x, true_clow_mean = res_strata[sim_strata2]$clow_mean, true_chigh_mean = res_strata[sim_strata2]$chigh_mean, true_rse_mean = res_strata[sim_strata2]$rse_mean_x),]
			
			ls1<-split(sim_strata2, paste(sim_strata2$Year, sim_strata2$SampSize, sim_strata2$variable, sim_strata2$stratum))
			ls2<-lapply(ls1, function(x){
			  a<-quantile(x$mean, c(0.025, 0.975), na.rm=T); 
			  b<-quantile(x$mean-mean(x$mean, na.rm=T), c(0.025, 0.975), na.rm=T); 
			  ci <- x$true_mean[1]+b; 
			  data.frame(Area= x$Area[1], Season= x$Season[1], Year = x$Year[1], variable=x$variable[1], stratum = x$stratum[1], stratum_size = x$stratum_size[1], SampSize = x$SampSize[1], 
						 true_n = x$true_n[1], true_mean = x$true_mean[1], true_var_mean =x$true_var_mean[1], true_var_x =x$true_var_x[1], true_rse_mean = x$true_rse_mean[1], true_clow_mean = x$true_clow_mean[1], true_chigh_mean = x$true_chigh_mean[1], 
						 boot_mean_n = mean(x$n, na.rm=T), boot_mean = mean(x$mean_x, na.rm=T), boot_var_x = mean(x$var_x, na.rm=T), boot_var_mean=var(x$mean_x, na.rm=T), boot_se=sqrt(var(x$mean_x, na.rm=T)), boot_rse = round(sqrt(var(x$mean_x, na.rm=T))/x$true_mean[1]*100,1),
						 bootlow = a[1], boothigh = a[2], 
						 bootlow_perc = ci[1], boothigh_perc = ci[2], 
						 bootNA=sum(is.na(x$mean_x))) })
			boot_res_strata2 <- rbindlist(ls2)
			boot_res_strata<-rbind(boot_res_strata1, boot_res_strata2)
			
			rm(ls1, ls2, sim_strata2, boot_res_strata1, boot_res_strata2); gc()			
				
			} else { # if nlevls==1 uses simpler procedure
			
			  sim_strata[, `:=`(true_n = res_strata[sim_strata]$n, true_mean = res_strata[sim_strata]$mean_x, true_var_x = res_strata[sim_strata]$var_x, true_var_mean = res_strata[sim_strata]$var_mean_x, true_clow_mean = res_strata[sim_strata]$clow_mean, true_chigh_mean = res_strata[sim_strata]$chigh_mean, true_rse_mean = res_strata[sim_strata]$rse_mean_x),]
			  
			  ls1<-split(sim_strata, paste(sim_strata$Year, sim_strata$SampSize, sim_strata$variable, sim_strata$stratum))
			  ls2<-lapply(ls1, function(x){
				a<-quantile(x$mean, c(0.025, 0.975), na.rm=T); 
				b<-quantile(x$mean-mean(x$mean, na.rm=T), c(0.025, 0.975), na.rm=T); 
				ci <- x$true_mean[1]+b; 
				data.frame(Area= x$Area[1], Season= x$Season[1], Year = x$Year[1], variable=x$variable[1], stratum = x$stratum[1], stratum_size = x$stratum_size[1], SampSize = x$SampSize[1], 
						   true_n = x$true_n[1], true_mean = x$true_mean[1], true_var_x =x$true_var_x[1], true_var_mean =x$true_var_mean[1], true_rse_mean = x$true_rse_mean[1], true_clow_mean = x$true_clow_mean[1], true_chigh_mean = x$true_chigh_mean[1], 
						   boot_mean_n = mean(x$n, na.rm=T), boot_mean = mean(x$mean_x, na.rm=T), boot_var_x = mean(x$var_x, na.rm=T), boot_var_mean=var(x$mean_x, na.rm=T), boot_se=sqrt(var(x$mean_x, na.rm=T)), boot_rse = round(sqrt(var(x$mean_x, na.rm=T))/x$true_mean[1]*100,1),
						   bootlow = a[1], boothigh = a[2], 
						   bootlow_perc = ci[1], boothigh_perc = ci[2], 
						   bootNA=sum(is.na(x$mean_x))) })
			  boot_res_strata <- rbindlist(ls2)
			  rm(ls1, ls2, sim_strata); gc()		  
			}
			  
			boot_res_strata$SampSize<-factor(as.character(boot_res_strata$SampSize), levels=sort(unique(boot_res_strata$SampSize))); unique(boot_res_strata$SampSiz)
			
			boot_res_strata<-boot_res_strata[order(SampSize, Year, variable, stratum),]
			
			# adds bootstrap ci with normal approx (See article)
			boot_res_strata$bootlow_aprox<-boot_res_strata$true_mean-1.96*sqrt(boot_res_strata$boot_var_mean) 
			boot_res_strata$boothigh_aprox<-boot_res_strata$true_mean+1.96*sqrt(boot_res_strata$boot_var_mean)
			
			# adds boot_mean_bias and boot_mean_bias_perc
			boot_res_strata$boot_mean_bias<-boot_res_strata$boot_mean-boot_res_strata$true_mean
			boot_res_strata$boot_mean_bias_perc<-round(boot_res_strata$boot_mean_bias/boot_res_strata$true_mean*100,1)
			tapply(boot_res_strata$boot_mean_bias_perc, boot_res_strata$SampSize, function(x) {quantile(x, na.rm=TRUE)})
			
			# adds boot_var_bias and boot_var_bias_perc
			boot_res_strata$boot_var_bias<-boot_res_strata$boot_var_x-boot_res_strata$true_var_x
			boot_res_strata$boot_var_bias_perc<-round(boot_res_strata$boot_var_bias/boot_res_strata$true_var_x*100,1)
			tapply(boot_res_strata$boot_var_bias_perc, boot_res_strata$SampSize, function(x) {quantile(x, na.rm=TRUE)})
			
			
			# adds boot_var_mean_bias and boot_var_mean_bias_perc
			boot_res_strata$boot_var_mean_bias<-boot_res_strata$boot_var_mean-boot_res_strata$true_var_mean
			boot_res_strata$boot_var_mean_bias_perc<-round(boot_res_strata$boot_var_mean_bias/boot_res_strata$true_var_mean*100,1)
			tapply(boot_res_strata$boot_var_mean_bias_perc, boot_res_strata$SampSize, function(x) {mean(x, na.rm=TRUE)})
			tapply(boot_res_strata$boot_var_mean_bias_perc, boot_res_strata$SampSize, function(x) {quantile(x, na.rm=TRUE)})
			tapply(boot_res_strata$boot_var_mean_bias_perc, paste(boot_res_strata$SampSize, boot_res_strata$variable), function(x) {quantile(x, na.rm=TRUE)})
			
			# a look into the final structure of the objects
			table(boot_res_strata$Year, boot_res_strata$SampSize, boot_res_strata$variable)
			#table(sim_samples[repl==1,]$Year, sim_samples[repl==1,]$SampSize)
			tapply(boot_res_strata$true_mean,list(boot_res_strata$Year, boot_res_strata$SampSize, boot_res_strata$variable), mean)
			

		# =============
		# Save prepared objects
		# =============	
			dir_sim_outputs<-paste(dir_sim_outputs,"prepared/",sep=""); dir.create(dir_sim_outputs, showWarnings = FALSE)
			filename<-paste(site,"_",scenario,"_",nsim,"sims_res_strata_prep.rdata",sep="");	
			save(boot_res_strata, res_strata, target_vars, nsim, cpus, scenario, site, sampOpt, file = paste(dir_sim_outputs,filename,sep=""))
			print(paste(dir_sim_outputs,filename,sep=""))	
			gc()
	}
