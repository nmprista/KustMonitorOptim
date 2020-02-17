# ==============================
# Graphs and tables of simulation results - Original data (population and strata level)
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================

rm(list=ls()); graphics.off()

library(data.table)

# select site
	site<-"Asköfjärden" # sites in c("Asköfjärden", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kvädöfjärden_Warm","Lagnö","Långvindsfjärden","Norrbyn", "Råneå","Torhamn")

# select scenario
	scenario <- "AllocOrigin" # "AllocScenarioWhitefishN" # "AllocScenarioHerringN", "AllocOrigin"

# option for Swedish captions
	sve=TRUE

# table spp conversion eng to sve
	spp_name_conv<-data.frame(eng = c('CodN','CyprinidsB','CyprinidsN','FlounderN','HerringN','PerchB','PerchN','PikeN','PikeperchN','PiscivoresN','WhitefishN'), 
			sve = c('TorskN','CypriniderB','CypriniderN','SkrubbskäddaN','StrömmingN','AbborreB','AbborreN','GäddaN','GösN','PiscivorerN','SikN'))

for (site in c("Asköfjärden", "Norrbyn","Kvädöfjärden_Warm", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Lagnö","Långvindsfjärden","Råneå","Torhamn")
{

	# loads original data			
		dir_inputs<-"001_Inputs/prepared_main/"
		load(file=paste("001_Inputs/prepared_main/",site,".Rdata",sep=""))
 
	# loads simulations
		dir_sim_outputs <- paste("002_Sim_Outputs/",scenario,"/prepared/",sep="")
		load(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_pop_prep.rdata", sep="")) 
		load(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_strata_prep.rdata", sep="")) 

# ====================
# Present ("true") estimates: Plot of Bootstrap CIs
# ====================
	
	# ===================	
	# Population level	
	# ===================
	
		dir_graph <- paste("003_Results/005_Origin_estimates_with_cis/",scenario,"/",sep=""); dir.create(dir_graph, showWarnings = FALSE)
	
		if(sve==TRUE) {boot_res_pop$variable<-spp_name_conv$sve[match(boot_res_pop$variable, spp_name_conv$eng)]
						target_vars<-spp_name_conv$sve[match(target_vars, spp_name_conv$eng)]
						}
	
		for (target_var in target_vars)
		{
		print(target_var)
			
			# subsets
			b<-boot_res_pop[variable==target_var,list(Year, SampSize, true_mean, true_clow_mean, true_chigh_mean, boot_mean, bootlow_aprox, boothigh_aprox, bootlow_perc, boothigh_perc)]
			b$id<-as.integer(as.character(b$Year))
			
			# graphs
				if(sum(!is.na(b$true_mean))>0) ylimite <- c(min(c(b$bootlow_perc), na.rm=T), max(c(b$boothigh_perc), na.rm=T)*1.3)
					windows(7,5)
					if(sve==TRUE) {
						if(site=="Kvädöfjärden_Warm") site2 <- "Kvädöfjärden" else site2 <- site
						tit<-paste(site2, target_var)
								} else {tit<-paste(site, target_var)}
					plot(b$boothigh_perc~b$id, type="n", ylim=ylimite, ylab="Indikatorvärde", xlab="", las=2, main=tit, xlim=c(min(boot_res_pop$Year), max(boot_res_pop$Year)+1), cex.axis=1.2, cex.lab=1.2)
				counter<-0
				for(sampsize in c("N"))
				{
					print(sampsize)
					a<-b[SampSize==sampsize,]
					
					if(sum(!is.na(a$true_mean))>0)
					{
					
					#inside_ids <- a[as.numeric(eval(paste(true_mean))) %between% list(clow_mean, chigh_mean),]$id
					#outside_ids <- a$id[!a$id %in% inside_ids]
					
					segments(x0 = a$id+counter, y0 = a$bootlow_perc, x1 = a$id+counter, y1 = a$boothigh_perc, col="blue", lwd=2)
					#points(a$boot_mean~c(a$id+counter), col="red", pch=19, cex=0.8)
					points(a$boot_mean~c(a$id+counter), col="blue", pch=19, cex=0.8)
					points(a$true_mean~c(a$id+counter), col="black", pch=4, cex=0.8)
					counter <- counter+0.08
					} else print(".no data")
				}

				legend("topright", legend=c("bootstrap medelvärde","uppmätt medelvärde","bootstrap konfidensintervall"), lty=c(0,0,1), lwd = c(0,0,2), col=c("blue","black","blue"), pch=c(19, 4, -1),cex=0.9)
			# save plot	
				if(sve==TRUE) {
						savePlot(paste(dir_graph, paste(site, target_var, scenario, sep="_"), "_N_sve.png", sep=""), type="png")
							} else	{savePlot(paste(dir_graph, paste(site, target_var, scenario, sep="_"), "_N.png", sep=""), type="png")}
		
			# save bootstrap results
				if(sve==TRUE) {
							write.csv2(b, file=paste(dir_graph, paste(site, target_var, scenario, sep="_"), "_N_sve.csv", sep=""), row.names=FALSE)
								} else	{write.csv2(b, file=paste(dir_graph, paste(site, target_var, scenario, sep="_"), "_N.csv", sep=""), row.names=FALSE)}	
				}
		graphics.off()
	
			
	# ===================	
	# Strata level	[single graph]
	# ===================
			
		dir_graph <- paste("003_Results/005_Origin_estimates_with_cis/",scenario,"/",sep=""); dir.create(dir_graph, showWarnings = FALSE)

		if(sve==TRUE) {boot_res_strata$variable<-spp_name_conv$sve[match(boot_res_strata$variable, spp_name_conv$eng)]
						#target_vars<-spp_name_conv$sve[match(target_vars, spp_name_conv$eng)]
						}
		
		for (target_var in target_vars)
		{
		print(target_var)
			# subsets
			b<-boot_res_strata[variable==target_var,list(Year, SampSize, stratum, true_mean, true_clow_mean, true_chigh_mean, boot_mean, bootlow_aprox, boothigh_aprox, bootlow_perc, boothigh_perc)]
			b$id<-as.numeric(as.character(b$Year))
			
			# graphs
				if(sum(!is.na(b$true_mean))>0) ylimite <- c(min(c(b$bootlow_perc), na.rm=T), max(c(b$boothigh_perc), na.rm=T)*1.4)
					windows(7,5)
					if(sve==TRUE) {
						if(site=="Kvädöfjärden_Warm") site2 <- "Kvädöfjärden" else site2 <- site
						tit<-paste(site2, target_var)
								} else {tit<-paste(site, target_var)}
					plot(b$boothigh_perc~b$id, type="n", ylim=ylimite, ylab="Indikatorvärde", xlab="", las=2, main=tit, xlim=c(min(boot_res_pop$Year), max(boot_res_pop$Year)+1), cex.axis=1.2, cex.lab=1.2)
				for(sampsize in c("N"))
				{
					print(sampsize)
					b1<-b[SampSize==sampsize,]
						counter<-0
						for (stratum1 in unique(b1$stratum))
						{
						print(stratum1)
						a <- b1[stratum==stratum1,]
						# defines colours
						if(stratum1=="0-3m") color<-"black"
						if(stratum1=="3-6m") color<-"darkgreen"
						if(stratum1=="6-10m") color<-"blue"
						if(stratum1=="10-20m") color<-"brown"
						
						if(sum(!is.na(a$true_mean))>0)
						{
						
						#inside_ids <- a[as.numeric(eval(paste(true_mean))) %between% list(clow_mean, chigh_mean),]$id
						#outside_ids <- a$id[!a$id %in% inside_ids]
						
						segments(x0 = a$id+counter, y0 = a$bootlow_perc, x1 = a$id+counter, y1 = a$boothigh_perc, col=color, lwd=2)
						points(a$boot_mean~c(a$id+counter), col="black", pch=21, cex=0.7, bg=color)
						points(a$true_mean~c(a$id+counter), col="black", pch=4, cex=0.7)
						counter <- counter+0.1
						print(counter)
						} else print(".no data")
						}
						
				# save bootstrap results
					b1$id<-NULL
					if(sve==TRUE) {
					write.csv2(b1, file=paste(dir_graph, paste(site, target_var, scenario, sep="_"), "_N_strata_sve.csv", sep=""), row.names=FALSE)
								} else { write.csv2(b1, file=paste(dir_graph, paste(site, target_var, scenario, sep="_"), "_N_strata.csv", sep=""), row.names=FALSE)}
				
				}
				
				legend("topright", ncol=2, legend=c("bootstrap_medelvärde","uppmätt medelvärde","","","0-3m","3-6m","6-10m","10-20m"), lty=c(0,0,0,0,1,1,1,1), lwd = c(0,0,0,0,2,2,2,2), col=c("black","black","white","white","black","darkgreen", "blue","brown"), pch=c(1, 4, -1,-1,-1, -1, -1, -1),cex=0.8)
				
				# save plot	
					if(sve==TRUE) {
						savePlot(paste(dir_graph, paste(site, target_var, scenario, sep="_"), "_N_strata_1_sve.png", sep=""), type="png")
							} else	{savePlot(paste(dir_graph, paste(site, target_var, scenario, sep="_"), "_N_strata_1.png", sep=""), type="png")}
					
		}
		graphics.off()
}			

	# ===================	
	# Strata level	[panel with 4 graphs]
	# ===================
			
		dir_graph <- paste("003_Results/005_Origin_estimates_with_cis/",scenario,"/",sep=""); dir.create(dir_graph, showWarnings = FALSE)
		
		for (target_var in target_vars)
		{
		print(target_var)

			# subsets
			b<-boot_res_strata[variable==target_var,list(Year, SampSize, stratum, true_mean, true_clow_mean, true_chigh_mean, boot_mean, bootlow_aprox, boothigh_aprox, bootlow_perc, boothigh_perc)]
			b$id<-as.numeric(as.character(b$Year))
			
			# graphs
			if(sum(!is.na(b$true_mean))>0) ylimite <- c(min(c(b$bootlow_perc), na.rm=T), max(c(b$boothigh_perc), na.rm=T))
				windows(15,10); par(mfrow=c(2,2))
			for(sampsize in c("N"))
			{
				print(sampsize)
				b1<-b[SampSize==sampsize,]
					for (stratum1 in unique(b1$stratum))
					{
					print(stratum1)
					a <- b1[stratum==stratum1,]
					# defines colours
					color<-"blue"
					
					if(sum(!is.na(a$true_mean))>0)
					{
					plot(a$boothigh_perc~a$id, type="n", ylim=ylimite, ylab="indicator value", xlab="year", las=2, main=paste(site, target_var, stratum1), xlim=c(min(boot_res_pop$Year), max(boot_res_pop$Year)+1))
					segments(x0 = a$id+counter, y0 = a$bootlow_perc, x1 = a$id+counter, y1 = a$boothigh_perc, col=color)
					points(a$boot_mean~c(a$id+counter), col="red", pch=17, cex=0.5)
					points(a$true_mean~c(a$id+counter), col="black", pch=4, cex=0.5)
					legend("topright", legend=c("bootstrap_sample_mean","true_sample_mean", "boot_ci_perc"), lty=c(0,0,1), col=c("red","black","blue"), pch=c(19, 4, -1),cex=0.5)
					print(counter)
					} else print(".no data")
					}
			}
			
			# save plot	
				savePlot(paste(dir_graph, paste(site, target_var, scenario, sep="_"), "_N_strata_2.png", sep=""), type="png")
		}
		
		graphics.off()





