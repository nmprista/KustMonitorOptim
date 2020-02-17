do_summary_stratified_mean_time_series <- function (x, target_var, strata_var, strata_size_var, period_var, klab){

	# calculates stratified mean statistics
	# Nuno Prista, SLU Aqua, Sweden
	# Developed @ KustMonitoring Review 2019
				
	#x is a data.frame values with 
		# target values in target_var variable
		# a strata_var variable
		# a strata_size variable
		# period_var is a variable with the time series (e.g., Year, Year*Quarter)
		# strata_to_keep is a vector of strata to be used in computation of target_var
		
	# 2019-05-11: created from do_summary_stratified_mean
	# 2019-05-11: adapted to presence of missing values
	# 2019-05-11: adapted argument klab to configure strata according to indicator (email jens 09-05-2019)
	# 2019-05-11: added n_strata sampled to output - in case only disturbed stations are sampled a strata may end up not being sampled (overall mean will be biased)
	# 2019-05-15: correction of st_var_mean
	# 2019-05-15: added stratum ci (based on t distribution)
	# 2019-08-11: improvement of klab condition
		
	require(data.table)

	if(!is.data.table(x) & !is.data.frame(x)) stop ("x must be data.frame or data.table")
	if(!is.data.table(x)) x<-data.table(x)
				
# ==========================
# Stratified (st) results
# ==========================
	values<-parse(text=target_var)
	stratum<-parse(text=strata_var)
	size<-parse(text=strata_size_var)
	period<-parse(text=period_var)
		
	if(klab==TRUE)
		{
		# check on variable definition
		if (!target_var %in% c("CyprinidsB","CyprinidsN","EelN","EelpoutN","MesopredatorsN","PerchB","PerchN","PikeN","PiscivoresN", "CodN","FlounderN","HerringN", "PikeperchN", "WhitefishN")) 
			{
			stop ("var stratification not defined - edit code")
			} else {
					if (target_var %in% c("CyprinidsB","CyprinidsN","EelN","EelpoutN","MesopredatorsN","PerchB","PerchN","PikeN","PiscivoresN")) { strata_to_keep <- c("0-3m", "3-6m", "6-10m") } 
					if (target_var %in% c("CodN","FlounderN","HerringN", "PikeperchN", "WhitefishN") ) {	strata_to_keep <- c("0-3m", "3-6m", "6-10m", "10-20m") } 
					x<-droplevels(x[eval(stratum) %in% strata_to_keep,])
					}
		} else strata_to_keep<-unique(x[,eval(stratum)])
		
	if (is.null(period_var)) { stop("must define period var\n")}

	if (!is.null(period_var))
		{
		# stratified variance
			# strata summary
				b1<-x[,list(stratum_size=eval(size)[1], variable = target_var, n=sum(!is.na(eval(values))), mean_x = mean(eval(values), na.rm=TRUE ), var_x = var(eval(values), na.rm=TRUE ), var_mean_x = var(eval(values), na.rm=TRUE )/sum(!is.na(eval(values))), se_mean_x = sd(eval(values), na.rm=TRUE )/sqrt(sum(!is.na(eval(values))))),list(eval(period), eval(stratum))][order(period, stratum)]
				b1[, `:=`(rse_mean_x = round(se_mean_x/mean_x*100,1), clow_mean = mean_x-qt(0.975,n)*se_mean_x, chigh_mean = mean_x+qt(0.975,n)*se_mean_x),]

			# stratified mean and stratified variance of the mean
				b2<-b1[, list( N=sum(stratum_size), n=sum(n), expect_strata = paste(strata_to_keep, collapse=","), samp_strata = paste(unique(stratum), collapse=","), n_strata = paste(n, collapse=","), st_mean = sum(mean_x*stratum_size/sum(stratum_size) ), st_var_mean = sum( 1*(var_mean_x)* ( (stratum_size/sum(stratum_size))^2) )   ), list(period)]
					# alternative calc (same results)
						# see above
			# stratified se of the mean
				b2[, st_se_mean:=sqrt(st_var_mean), period]
				b2[, st_rse_mean:=round(st_se_mean/st_mean*100,1), period]
				
		out<-list(pop_res = data.frame(type="stratified", variable = target_var, b2, clow_mean=b2$st_mean-2*b2$st_se_mean, chigh_mean=b2$st_mean+2*b2$st_se_mean), stratum_res = b1, type="stratified")
		
		}

out		
}		
