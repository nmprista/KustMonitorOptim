# ==============================
# Graphs and tables of simulated regressions
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================

rm(list=ls()); graphics.off()

library(data.table)

# select scenario
scenario <- "AllocScenarioOriginal"

for (site in c("Asköfjärden", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kvädöfjärden_Warm","Lagnö","Långvindsfjärden","Norrbyn", "Råneå","Torhamn")
{

	# use during testing and site by site runs
	# site<-"Kinnbäcksfjärden" 
	
	# site specific output vars
	if (site == "Asköfjärden") output_vars <- c("CyprinidsN","HerringN","PerchN","FlounderN","WhitefishN")
	if (site == "Kvädöfjärden_Warm") output_vars <- c("CyprinidsN","HerringN","PerchN","FlounderN","PikeperchN")
	if (site == "Norrbyn") output_vars <- c("CyprinidsN","HerringN","PerchN","WhitefishN")
	if (site == "Galtfjärden") output_vars <- c("CyprinidsN","HerringN","PerchN","PikeperchN","WhitefishN")
	if (site == "Gaviksfjärden") output_vars <- c("CyprinidsN","HerringN","PerchN","WhitefishN")
	if (site == "Holmön") output_vars <- c("CyprinidsN","HerringN","PerchN","WhitefishN")
	if (site == "Kinnbäcksfjärden") output_vars <- c("CyprinidsN","HerringN","PerchN","WhitefishN")
	if (site == "Lagnö") output_vars <- c("CyprinidsN","HerringN","PerchN","WhitefishN")
	if (site == "Långvindsfjärden") output_vars <- c("CyprinidsN","HerringN","PerchN","WhitefishN")
	if (site == "Torhamn") output_vars <- c("CyprinidsN","HerringN","PerchN","FlounderN")
	if (site == "Råneå") output_vars <- c("CyprinidsN","HerringN","PerchN","WhitefishN")

	# =======================	
	# Loading
	# =======================

	# loads original data			
		dir_inputs<-"001_Inputs/prepared_main/"
		load(file=paste("001_Inputs/prepared_main/NordicGillNets/",site,".Rdata",sep=""))
 
 	# loads simulations (Asköfjärden)
		dir_sim_outputs <- paste("002_Sim_Outputs/",scenario,"/prepared/",sep="")
		load(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_pop_prep.rdata", sep="")) 
		load(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_strata_prep.rdata", sep=""))
 
	# loads regression simulations
		dir_sim_outputs <- paste("002_Sim_Outputs/",scenario,"/prepared/",sep="")
		res_omdrev1<-data.table(readRDS(file=paste(dir_sim_outputs, site, "_", scenario,"_omdrev_1_1000sims_res_model.rds", sep="")))
		res_omdrev2<-data.table(readRDS(file=paste(dir_sim_outputs, site, "_", scenario,"_omdrev_2_1000sims_res_model.rds", sep="")))
		res_omdrev3<-data.table(readRDS(file=paste(dir_sim_outputs, site, "_", scenario,"_omdrev_3_1000sims_res_model.rds", sep="")))

	
	# =======================	
	# Data prep
	# =======================
	
		dir_results <- paste("003_Results/008_Regression_Results/",site,"/", scenario, "/",sep=""); dir.create(dir_results, recursive=T)
	
		# assigns true slope
		
		ls1<-split(res_pop, res_pop$variable)
		ls2<-lapply(ls1, function(x, site1 = site)
				{
				true_model<-lm(st_mean~Year,data=x)
				x<-data.frame(Area = site1, variable = x$variable[1], true_slope = coef(true_model)[2], true_sign = summary(true_model)[[4]][8]<0.05)
				x
				})
		res_regr<-do.call("rbind",ls2)
		write.csv2(res_regr, file=paste(dir_results, site, "_origin_regr.csv", sep=""))
			
			res_omdrev1$true_slope<-res_regr$true_slope[match(res_omdrev1$variable, res_regr$variable)]
			res_omdrev1$true_sign<-res_regr$true_sign[match(res_omdrev1$variable, res_regr$variable)]
			
			res_omdrev2$true_slope<-res_regr$true_slope[match(res_omdrev2$variable, res_regr$variable)]
			res_omdrev2$true_sign<-res_regr$true_sign[match(res_omdrev2$variable, res_regr$variable)]

			res_omdrev3$true_slope<-res_regr$true_slope[match(res_omdrev3$variable, res_regr$variable)]
			res_omdrev3$true_sign<-res_regr$true_sign[match(res_omdrev3$variable, res_regr$variable)]
	
    
    
	# =======================	
	# Exploratory results
	# =======================
	
		# omdrev1
			dt1 <- res_omdrev1
			table(dt1$SampSize)
		
			# biases and variances
				dt1$slope_negative <- dt1$slope<0
				dt1$true_slope_negative <- dt1$true_slope<0
				dt1$slope_dif<-dt1$slope-dt1$true_slope
				dt1$slope_dif_perc<-dt1$slope_dif/dt1$true_slope*100
				dt1$slope_dif_perc_abs<-abs(dt1$slope_dif/dt1$true_slope*100)
							
				dt1[, mean(slope_dif_perc), SampSize]
				dt1[, mean(slope_dif_perc_abs), SampSize]
				dt1[, var(slope_dif_perc), SampSize]
				
				# same slope sign
				res1 <- dt1[variable %in% output_vars, sum(slope_negative==true_slope_negative), list(SampSize, variable)]; res1
				dcast(res1, SampSize~variable)
				
				# same slope sign and significance
				res2 <- dt1[variable %in% output_vars, sum(slope_negative==true_slope_negative & sign==true_sign), list(SampSize, variable)]; res2
				dcast(res2, SampSize~variable)
				write.csv2( dcast(res2, SampSize~variable), file = paste(dir_results, site, "_", scenario,"_identical_omdrev1.csv", sep=""))

				# same slope sign and significance and slope within 10%
				res3 <- dt1[variable %in% output_vars, sum(slope_negative==true_slope_negative & sign==true_sign & slope_dif_perc_abs<50), list(SampSize, variable)]; res3			
				dcast(res3, SampSize~variable)			

					
		# omdrev2
			dt2 <- res_omdrev2
			table(dt2$SampSize)

			# biases and variances		
				dt2$slope_negative <- dt2$slope<0
				dt2$true_slope_negative <- dt2$true_slope<0
				dt2$slope_dif<-dt2$slope-dt2$true_slope
				dt2$slope_dif_perc<-dt2$slope_dif/dt2$true_slope*100
				dt2$slope_dif_perc_abs<-abs(dt2$slope_dif/dt2$true_slope*100)
				
				dt2[, mean(slope_dif_perc), SampSize]
				dt2[, mean(slope_dif_perc_abs), SampSize]
				dt2[, var(slope_dif_perc), SampSize]
				dt2[, list(same_slope=sum(slope<0), same_sign = sum(sign==true_sign)), SampSize]

			# same slope sign
				res1 <- dt2[variable %in% output_vars, sum(slope_negative==true_slope_negative), list(SampSize, variable)]; res1
				dcast(res1, SampSize~variable)
				
				# same slope sign and significance
				res2 <- dt2[variable %in% output_vars, sum(slope_negative==true_slope_negative & sign==true_sign), list(SampSize, variable)]; res2
				dcast(res2, SampSize~variable)
				write.csv2( dcast(res2, SampSize~variable), file = paste(dir_results, site, "_", scenario,"_identical_omdrev2.csv", sep=""))

				# same slope sign and significance and slope within 10%
				res3 <- dt2[variable %in% output_vars, sum(slope_negative==true_slope_negative & sign==true_sign & slope_dif_perc_abs<50), list(SampSize, variable)]; res3			
				dcast(res3, SampSize~variable)		

			
		# omdrev3
			dt3 <- res_omdrev3
			table(dt3$SampSize)

			# biases and variances		
				dt3$slope_negative <- dt3$slope<0
				dt3$true_slope_negative <- dt3$true_slope<0
				dt3$slope_dif<-dt3$slope-dt3$true_slope
				dt3$slope_dif_perc<-dt3$slope_dif/dt3$true_slope*100
				dt3$slope_dif_perc_abs<-abs(dt3$slope_dif/dt3$true_slope*100)
				
				dt3[, mean(slope_dif_perc), SampSize]
				dt3[, mean(slope_dif_perc_abs), SampSize]
				dt3[, var(slope_dif_perc), SampSize]
		
			# same slope sign
				res1 <- dt3[variable %in% output_vars, sum(slope_negative==true_slope_negative), list(SampSize, variable)]; res1
				dcast(res1, SampSize~variable)
				
				# same slope sign and significance
				res2 <- dt3[variable %in% output_vars, sum(slope_negative==true_slope_negative & sign==true_sign), list(SampSize, variable)]; res2
				dcast(res2, SampSize~variable)
				write.csv2( dcast(res2, SampSize~variable), file = paste(dir_results, site, "_", scenario,"_identical_omdrev3.csv", sep=""))

				# same slope sign and significance and slope within 10%
				res3 <- dt3[variable %in% output_vars, sum(slope_negative==true_slope_negative & sign==true_sign & slope_dif_perc_abs<50), list(SampSize, variable)]; res3			
				dcast(res3, SampSize~variable)		
}		
		
	# =======================		
	# exploratory "power" analysis
	# =======================
		
		dt1[, list(same_slope=sum(c(slope<0)==c(true_slope<0)), same_signif = sum(sign==true_sign)), SampSize]
		dt2[, list(same_slope=sum(c(slope<0)==c(true_slope<0)), same_signif = sum(sign==true_sign)), SampSize]
		dt3[, list(same_slope=sum(c(slope<0)==c(true_slope<0)), same_signif = sum(sign==true_sign)), SampSize]

		
		
		
		