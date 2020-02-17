do_summary_simple_mean <- function (x, target_var){

	# calculates simple (i.e., not stratified) mean statistics
	# Nuno Prista, SLU Aqua, Sweden
	# Developed @ KustMonitoring Review 2019

	#x is a dataframe or data.table with
		# # target values in target_var variable


	# 2019-05-06: added clow_mean and chigh_mean (confidence intervals)

		
# ==========================
# Non-stratified (Nst) results
# ==========================

	values<-x[[target_var]]
	# simple mean
		Nst_mean<-mean(values)
	# simple_var_mean
		Nst_var_mean<-var(values)/length(values)
	# simple se of the mean
		Nst_se_mean<-sd(values)/sqrt(length(values))
	# simple cv/rse of the mean 
			Nst_rse_mean<-round(Nst_se_mean/Nst_mean*100,1)

	list(pop_res = data.frame(type = "simple", variable = target_var,  n_tot = length(values), n_strata = NA, mean = Nst_mean, var_mean = Nst_var_mean, se_mean = Nst_se_mean, rse_mean = Nst_rse_mean, clow_mean=Nst_mean-2*Nst_se_mean, chigh_mean=Nst_mean+2*Nst_se_mean), stratum_res = NULL, type="stratified")

}
