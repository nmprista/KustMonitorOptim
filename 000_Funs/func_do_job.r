do_job <- function(lo){				
			
# snowfall job to simulate sampling
# Nuno Prista, SLU Aqua, 2019 @  Kustmonitoring
			
				
# simulates sampling
	dt_site<-as.data.frame(dt_site)
	sim_sampId <- do_sampling_stratified_timeseries(sampDes, sampOpt, dataset = dt_site, sampId = "sampId")

# fetches sampled rows from dataset based on sampId + creates variable with "SampSizeName"
	sim_samples<-lapply(sim_sampId, function(x, a1 = dt_site) lapply( x, function (y, a2 = a1) {y <- as.data.table(data.frame(SampSize = y[1], a2[y[-1],])) } ) )

# separates replicates into years
		# [[1]] - SampSize being tested
		# [[2]] - Replicate
		# [[3]] - Year
		sim_samples<-lapply(sim_samples, function(x) lapply(x, function(y) split(y, y$Year)))
			# demonstration
			# table(sim_samples[[1]][[1]][[3]][,c("Year","DepthStratum")])
			# table(sim_samples[[2]][[1]][[3]][,c("Year","DepthStratum")])
			# table(sim_samples[[2]][[1]][[1]][,c("Year","DepthStratum")])
	
# calculates the mean and its statistics
		# stratified mean of all stations
		# the following lapply runs "do_summary_stratified_mean" on each of target_vars
		res_stratified<-lapply(sim_samples, function(z) { lapply(z, function(y) {lapply(y, function (y1, target_vars1 = target_vars){
		a<-(lapply(target_vars1, function(x, x1 = y1)  {a<-do_summary_stratified_mean(x = x1, target_var = x, strata_var = "DepthStratum", strata_size_var = "NStations", klab=TRUE); res = list(pop_res = data.table(Area = x1$Area[1], Season = x1$Season[1], Year = x1$Year[1], SampSize = x1$SampSize[1], repl = lo, a$pop_res), stratum_res = data.table(Area = x1$Area[1], Season = x1$Season[1], Year = x1$Year[1], SampSize = x1$SampSize[1], repl = lo, a$stratum_res))} ))
		})})})
	a<-list(res = 	res_stratified, sim_samples = sim_samples)
	
}		

do_job1 <- function(lo){				
			
# forreach_mc job to simulate sampling
# Nuno Prista, SLU Aqua, 2019 @  Kustmonitoring
			
				
# simulates sampling
	dt_site<-as.data.frame(dt_site)
	sim_sampId <- do_sampling_stratified_timeseries(sampDes, sampOpt, dataset = dt_site, sampId = "sampId")

# fetches sampled rows from dataset based on sampId + creates variable with "SampSizeName"
	sim_samples<-lapply(sim_sampId, function(x, a1 = dt_site) lapply( x, function (y, a2 = a1) {y <- as.data.table(data.frame(SampSize = y[1], a2[y[-1],])) } ) )

# separates replicates into years
		# [[1]] - SampSize being tested
		# [[2]] - Replicate
		# [[3]] - Year
		sim_samples<-lapply(sim_samples, function(x) lapply(x, function(y) split(y, y$Year)))
			# demonstration
			# table(sim_samples[[1]][[1]][[3]][,c("Year","DepthStratum")])
			# table(sim_samples[[2]][[1]][[3]][,c("Year","DepthStratum")])
			# table(sim_samples[[2]][[1]][[1]][,c("Year","DepthStratum")])
	rm(sim_sampId, dt_site)
	gc()

# calculates the mean and its statistics

	# stratified mean of all stations
	# the following lapply runs "do_summary_stratified_mean" on each of target_vars
	res_stratified<-lapply(sim_samples, function(z) { z<-lapply(z, function(y) {y<-lapply(y, function (y1, target_vars1 = target_vars){

	a<- lapply(target_vars1, function(x, x1 = y1)  
						{
						a<-do_summary_stratified_mean(x = x1, target_var = x, strata_var = "DepthStratum", strata_size_var = "NStations", klab=TRUE)

						res = list(
								pop_res = data.table(Area = x1$Area[1], Season = x1$Season[1], Year = x1$Year[1], SampSize = x1$SampSize[1], repl = lo, a$pop_res), 
								stratum_res = data.table(Area = x1$Area[1], Season = x1$Season[1], Year = x1$Year[1], SampSize = x1$SampSize[1], repl = lo, a$stratum_res)
								)
						res		
						} 
				)
		list(pop_res = rbindlist(lapply(a, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(a, function(x) x<-x$stratum_res)))

	
	})
	list(pop_res = rbindlist(lapply(y, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(y, function(x) x<-x$stratum_res)))
	})
	list(pop_res = rbindlist(lapply(z, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(z, function(x) x<-x$stratum_res)))
	})
	res_stratified<-list(pop_res = rbindlist(lapply(res_stratified, function(x) x<-x$pop_res)), stratum_res = rbindlist(lapply(res_stratified, function(x) x<-x$stratum_res)))

	res_stratified$pop_res$repl<-lo
	res_stratified$stratum_res$repl<-lo
		
	sim_samples <- rbindlist(lapply(sim_samples, function(y) {w<-rbindlist(y[[1]],idcol=TRUE); w}),idcol=TRUE)
	sim_samples$repl<-lo
	
	list(sim_res_pop = res_stratified$pop_res, 	sim_res_strata = res_stratified$stratum_res, sim_samples = sim_samples)
	
}
