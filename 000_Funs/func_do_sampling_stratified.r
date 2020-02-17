do_sampling_stratified<-function(sampDes, sampOpt, dataset, sampId){

	# resamples stations from strata
	# Nuno Prista, SLU Aqua, Sweden
	# Developed @ KustMonitoring Review 2019 based on extensive work done under WKBIOPTIM1 and WKBIOPTIM2	
	# 2019-05-01 # seriously?! this is shameful and not to repeat - long live May day [https://en.wikipedia.org/wiki/May_Day]
		
		
# creates storage object	
ls_out_sims<-sapply(as.character(colnames(sampOpt$samp_sizes)[grepl(colnames(sampOpt$samp_sizes), pat="SampSize")]), function(x) NULL)

strata_var<-sampDes$strata_var

# check compatibility between sampling_design and sampling_options
	if (sampDes$stratified == TRUE & (sampOpt$stratified==FALSE | (sampOpt$stratified==TRUE & sampOpt$strata_var!=sampDes$strata_var)))
		{
		stop (cat("\n ATT: sampling_design not compatible with sampling_options \n\n "))
		}

# check on columns
	if(sampOpt$stratified==TRUE)
		{
		if(!strata_var %in% colnames(dataset)) {stop (cat("\n ATT: check strata_var - not found in colnames(dataset) \n\n "))}
		}	

# check on sample sizes
	if(sampOpt$replacement==FALSE)
		{
		if(sampOpt$stratified==TRUE){if (any(sampDes$samp_sizes$OrigSampSizes<sampOpt$samp_sizes[,grepl(colnames(sampOpt$samp_sizes), pat="SampSize")]) & sampOpt$sample_all_available==FALSE) {stop (cat("\n ATT: samp_sizes >  strata size with replace = FALSE AND sample_all_available = FALSE\n\n"))}}
		}		

		
	for (j in 2:ncol(sampOpt$samp_sizes))
	{

	print(paste("--- Simulating samples: ", colnames(sampOpt$samp_sizes)[j]," ---", sep=""))
	print(sampOpt$samp_sizes[,c(1,j)])		
						
	# creates a list to hold results of simulations (each leaf is a simulation of size j)	
												
		out<-sapply(as.character(1:sampOpt$n_sims), function(x) NULL)		
	
	# extracts vector of sample sizes
		samp_size_vector <-	sampOpt$samp_sizes[[j]]
		names(samp_size_vector)<-sampOpt$samp_sizes[[1]]
		
	for(i in 1:sampOpt$n_sims)
		{
		if(i==1) cat("sims:"); cat(i,",", sep=""); if(i==max(sampOpt$n_sims)) cat("\n")
			if(sampOpt$stratified==FALSE)
				{
				stop("sampOpt$stratified==FALSE: not defined")
				}	
			if(sampOpt$stratified==TRUE)
				{
				# note: 1st element is sampSize name
				out[[i]]<-c(colnames(sampOpt$samp_sizes)[j], sampleFromAGroup(x=dataset[[sampId]], y=dataset[,strata_var], nsize = samp_size_vector, samp_options = sampOpt))
				}
		}
		cat("\n")	
			ls_out_sims[[colnames(sampOpt$samp_sizes)[j]]]<-out					
	}
	
ls_out_sims	

}	
