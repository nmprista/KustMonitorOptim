# ==============================	
# Simulates time series regressions of indicator on time using simulated bootstraps 
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================
	
	# includes omdrev == 1, omdrev == 2 and omdrev == 3
	
	# ATT: 
		# runs in parallel using foreach_mc 
		# options "none" and "snowfall", not configured
		# nsims and cpus are redefined after load sim data to allow for different values to be used

rm(list=ls()); gc()

# settings
	parallel::detectCores(all.tests = FALSE, logical = TRUE)
	parallel_type <- "foreach_mc" # "foreach_mc" # "none"

# loads packages
	library(data.table)
	if(parallel_type == "foreach_mc")
	{
	  library(foreach) # parallel computing
	  library(doMC) # parallel computing
	  library(doRNG) # Generic Reproducible Parallel Backend for 'foreach' Loops
	}

# select scenario
	scenario <- "AllocScenario3"

# objects to keep during cycle [when a final rm() is applied to clear memory]
	keep_objects <- c("parallel_type", "scenario", "keep_objects", "nsim", "cpus", "omdrev","selected_spp","sim_pop",
						"dir_inputs_main","dir_inputs_sampsize","dir_sim_outputs", "site","ptc0")	

for (site in c("Asköfjärden", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kvädöfjärden_Warm","Lagnö","Långvindsfjärden","Norrbyn", "Råneå","Torhamn")
{

	# use during testing and site by site runs
	# site<-"Kinnbäcksfjärden" 

  ptc0<-Sys.time()	 
  
  cat(paste("#===============================\n", site,"\n#===============================\n"))
  
  # loads simulation data
	  sim_pop<-readRDS(file=paste("002_Sim_Outputs/",scenario,"/", site, "_", scenario,"_5000sims_res_pop.rds", sep="")) 
	  dir_sim_outputs <- paste("002_Sim_Outputs/",scenario,"/prepared/",sep="")
	  load(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_strata_prep.rdata", sep="")) 
	  
  # ATT: Overwrites loaded objects
	  nsim <- 1
	  cpus <- 6
  
  # freq of occurrence and id of frequent species
	  freq_occ<-tapply(res_strata$mean_x, res_strata$variable, function(x) sum(!x==0))/table(res_strata$variable)
	  freq_occ
	  freq_spp_threshold <- 0.5
	  selected_spp <- names(freq_occ[freq_occ>=freq_spp_threshold])
	  
  # prepares dataset
	  dt0 <- sim_pop[variable %in% selected_spp, `:=`(ID = paste(SampSize, variable)),]
	  
	  rm(boot_res_strata, res_strata, freq_occ, freq_spp_threshold, sim_pop)
	  
  for(omdrev in 1:3)
  {  
    
    cat(paste("#===============================\n", omdrev,"\n#===============================\n"))  
    
    # function
    do_job<-function(lo, dt0, years){
      ls1<-split(dt0, dt0$ID) 
      out<-rbindlist(lapply(ls1, function(x, omdrev1 = omdrev) {
        # selects years
			# samples from 1:omdrev1 to select the starting point of the series
			# samples series with the same length [eg, if years 2002:2018 (i.e., 17 years) and omdrev = 2 samples 8 years; if omdrev = 3 samples 5 years
			if(omdrev1 %in% c(2,3)) years<-seq(sample(1:omdrev1,1), length(unique(x$Year))%/%omdrev1*omdrev1, by = omdrev1) else years<-as.numeric(as.factor(unique(x$Year)))
		# samples sim_pop subset
		dt2<-x[Year %in% unique(Year)[years],.SD[sample(.N, 1)],by = Year][,list(mean_x,Year)]
		        
		# fits model
		model <- lm(mean_x~Year, data=dt2)
		        
		# stores results 
			data.table(SampSize = x$SampSize[1], variable = x$variable[1], omdrev = omdrev1, years = paste(years, collapse=","), slope = coef(model)[2], sign = summary(model)[[4]][8]<0.05)
      }))
	  out$repl<-lo
      out	
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
      
	  # does job
		res_model <- foreach (i=1:nsim) %dorng% {
        require(data.table)
        do_job(lo=i, dt0 = dt0, omdrev)
      }
      gc()
      print(ls())
      res_model<-rbindlist(res_model)
      # prints time counter
      ptc1<-Sys.time()-ptc
      cat(paste("#=================\n", "Elapsed time = ", ptc1, "\n#=================\n"))  
      
    }
  
	# saves results
		cat("saving RDS...\n")
		ptc<-Sys.time()  	
		saveRDS(res_model, file = paste(dir_sim_outputs,site,"_",scenario,"_omdrev_",omdrev,"_", nsim,"sims_res_model.rds",sep=""))		
		print(Sys.time()-ptc)			
    
    #Sys.sleep(180)
    rm(list=ls()[!ls() %in% keep_objects])
    gc()
    
  }
  print(Sys.time()-ptc0)
}  