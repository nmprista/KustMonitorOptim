# ==============================
# Simulations of stratified designs (sites with no repeated measurements)
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================
	
	# ATT: 
		# Only Nordic coastal multimesh gillnet
		# Script may need adaptation on sites with repeated measurements 


rm(list=ls()); gc()

# reads functions
	source("000_Funs/func_do_summary_simple_mean.r")
	source("000_Funs/func_do_summary_stratified_mean.r")
	source("000_Funs/func_sampleFromAGroup.r")
	source("000_Funs/func_sample2.r")
	source("000_Funs/func_do_sampling_stratified.r")
	source("000_Funs/func_do_determine_best_scale.r")
	source("000_Funs/func_do_sampling_stratified_timeseries.r")
	source("000_Funs/func_do_summary_stratified_mean_time_series.r")
	source("000_Funs/func_do_job.r")
	source("000_Funs/func_prn.r")

# settings
	# parallel
	parallel::detectCores(all.tests = FALSE, logical = TRUE)
	parallel_type <- "snowfall" # alternatives: "snowfall", "foreach_mc", "none"
	# name the scenario
	scenario <- "AllocScenario3" # AllocOrigin = Original sample size [use for bootstrap confidence intervals] # Scenario1 = RegularDecrease
	# define number of simulations
	nsim <- 5000
	# define number of cpus
	cpus <- 16	

# loads packages
	library(data.table)
	library(beepr)
  if(parallel_type == "snowfall")
	{
	  library(snowfall)
	}
  if(parallel_type == "foreach_mc")
	{
		library(foreach) # parallel computing
		library(doMC) # parallel computing
		library(doRNG) # Generic Reproducible Parallel Backend for 'foreach' Loops
	}

# set dirs
	dir_inputs_main<-"001_Inputs/prepared_main/"
	dir_inputs_sampsize<-paste("001_Inputs/sampsizes/",scenario, "/", sep="")
	dir_outputs_sampsize<-paste("002_Sim_Outputs/", scenario, "/", sep="")

# objects to keep during cycle [when a final rm() is applied to clear memory]
	keep_objects <- c("parallel_type", "scenario", "keep_objects", "nsim", "cpus",
						"do_summary_stratified_mean","do_summary_stratified_mean_time_series", "do_sampling_stratified_timeseries", "sampleFromAGroup","do_job1","prn",
							"dir_inputs_main","dir_inputs_sampsize","dir_outputs_sampsize")

for (site in c("Asköfjärden", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kvädöfjärden_Warm","Lagnö","Långvindsfjärden","Norrbyn", "Råneå","Torhamn")
{
	# use during testing and site by site runs
	# site<-"Kinnbäcksfjärden" 

cat(paste("#===============================\n", site, "\n#===============================\n"))

# creates time point (for future documentation)
	ptc0<-Sys.time()

# reads data
	load(file=paste(dir_inputs_main,site,".Rdata",sep=""))
	
# defines target variables [different for fike and nordic nets]
	if(site %in% c("Barsebäck","Fjällbacka_Cold","Fjällbacka_Warm","Kullen","Älgöfjorden") ) # fike nets (ryssjor)
		{ 
		target_vars<-c('CodN','EelN','EelpoutN','FlounderN','MesopredatorsN','PiscivoresN')
		} else { # nordic nets
				target_vars<-c('CodN','CyprinidsB','CyprinidsN','FlounderN','HerringN','PerchB','PerchN','PikeN','PikeperchN','PiscivoresN','WhitefishN') 
				} 

# calculates time series of indicators [using "do_summary_stratified_mean_time_series"]		
	# note: it is here that the estimator is set
		res_pop<-c()
		res_strata<-c()
		for (i in target_vars)
			{
			res_pop<-rbind(res_pop, do_summary_stratified_mean_time_series(x = dt_site, target_var = i, strata_var = "DepthStratum", strata_size_var = "NStations", period_var = "Year", klab = TRUE)$pop_res)
			res_strata<-rbind(res_strata, do_summary_stratified_mean_time_series(x = dt_site, target_var = i, strata_var = "DepthStratum", strata_size_var = "NStations", period_var = "Year", klab = TRUE)$stratum_res)
			}
		colnames(res_pop)[colnames(res_pop)=="period"]<-"Year"
		colnames(res_strata)[colnames(res_strata)=="period"]<-"Year"
		res_pop$ID<-paste(res_pop$variable, res_pop$Year); sum(duplicated(res_pop$ID))==0
	
# creates sampling id	
	dt_site$sampId<-1:nrow(dt_site)	
# creates sampling strata
	dt_site[,YearDepthStratum:= paste(Year,DepthStratum),]
		
# set Sampling Design list (sampDes)	
	sampDes <- list (stratified = TRUE, strata_var = "YearDepthStratum", period_var="Year", samp_sizes = data.frame(YearDepthStratum = names(b<-table(dt_site$YearDepthStratum)), OrigSampSizes = c(b), row.names=NULL))

# prepares samp_size_table
	# reads samp size table
		if (scenario %in% c("AllocOrigin","AllocScenario1","AllocScenario2","AllocScenario3"))
			{
			samp_size_table<-read.csv2(paste(dir_inputs_sampsize,site,"_", scenario, ".csv", sep=""), header=T)
			} else stop ("Scenario not defined!")
	# builds table
		samp_size_table<-merge(data.frame(Year=sort(unique(dt_site[,Year]))),samp_size_table)
		samp_size_table$YearDepthStratum<-paste(samp_size_table$Year, samp_size_table$DepthStratum)
		samp_size_table<-cbind(samp_size_table[ncol(samp_size_table)],samp_size_table[-ncol(samp_size_table)])
	
# set Sampling Options list
	sampOpt<-list(n_sims = 1, stratified = TRUE, strata_var = "YearDepthStratum", period_var = "Year", samp_sizes = samp_size_table, 
				replacement = TRUE,  sample_all_available = TRUE, sample_all_available_warning = TRUE)	

# save settings
	save(target_vars, nsim, cpus, scenario, site, sampOpt, ptc0, file=paste(dir_outputs_sampsize,site,"_",scenario,"_",nsim,"sims_settings.Rdata",sep="") )
		
# =======================================				
# Simulations - implemented in "non" or in parallel (alternative snowfall and foreach_mc)
# =======================================

	if (parallel_type == "none")
		{
		
		# initiates time counter
		ptc<-Sys.time()	
		
		# runs sims
		out<-lapply(1:nsim, do_job1)
		
		# prints time counter
		ptc1<-Sys.time()-ptc
		cat(paste("#=================\n", "Elapsed time = ", ptc1, "\n#=================\n"))		
		}

	if (parallel_type == "snowfall")
		{

		# initiates time counter
		ptc<-Sys.time()				

		# initiates parallel
		sfInit(parallel=TRUE, cpus = cpus)  # 5
		sfLibrary(data.table)
		sfExport("nsim", "do_sampling_stratified_timeseries", "do_summary_stratified_mean", "prn", "sampleFromAGroup", "sampDes", "sampOpt", "dt_site", "target_vars")

		# starts the seeds
		sfLapply(1:nsim, prn)

		# runs simulations
		out<-sfLapply(1:nsim, do_job1)
	
		# cleans up workers	
		sfRemoveAll()

		# stops parallel computing 
		sfStop()				

		# prints time counter
		ptc1<-Sys.time()-ptc
		cat(paste("#=================\n", "Elapsed time = ", ptc1, "\n#=================\n"))
		}
 
  	if (parallel_type == "foreach_mc")
		{
		
		# initiates time counter
		ptc<-Sys.time()	
		
		# initiates parallel
		registerDoMC(cpus)
		print(getDoParWorkers())

		# starts the seeds
		set.seed(123)

		# runs simulations
		out <- foreach (i=1:nsim) %dorng% {
											#require(data.table)
											#set.seed(i);
											do_job1(lo=i)
											}
		
		# prints time counter
		ptc1<-Sys.time()-ptc
		cat(paste("#=================\n", "Elapsed time = ", ptc1, "\n#=================\n"))
 	}
		
    
		# process objects
		sim_res_pop<-rbindlist(lapply(out, function(x) {x$sim_res_pop}), idcol=TRUE)
		sim_res_strata<-rbindlist(lapply(out, function(x) {x$sim_res_strata}), idcol=TRUE)
		sim_samples<-rbindlist(lapply(out, function(x) {x$sim_samples}), idcol=TRUE)
	
 		# save
		cat("saving RDS...\n")
		ptc<-Sys.time()		
		saveRDS(sim_res_pop, file = paste(dir_outputs_sampsize,site,"_",scenario,"_",nsim,"sims_res_pop.rds",sep=""))			
		saveRDS(sim_res_strata, file = paste(dir_outputs_sampsize,site,"_",scenario,"_",nsim,"sims_res_strata.rds",sep=""))
		saveRDS(sim_samples, file = paste(dir_outputs_sampsize,site,"_",scenario,"_",nsim,"sims_sim_samples.rds",sep=""))		
		print(Sys.time()-ptc)		
				
		#Sys.sleep(180)
		rm(list=ls()[!ls() %in% keep_objects])
		gc()
  
}
 