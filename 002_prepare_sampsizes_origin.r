# ==============================	
# Preparation of sample size files (AllocOrigin)
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================

	# Only Nordic coastal multimesh gillnet

rm(list=ls())

library(data.table)



# select input and output dirs
	dir_inputs <- "001_Inputs/prepared_main/NordicGillNets/"
	dir_outputs <- "001_Inputs/sampsizes/AllocOrigin/"

# prepares and saves sample size files	
	
	#Nordic coastal multimesh gillnet
	
	for (site in c("Asköfjärden", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kvädöfjärden_Warm","Lagnö","Långvindsfjärden","Norrbyn", "Råneå","Torhamn")	
		{
		
		# use during testing and site by site runs
		# site<-"Kinnbäcksfjärden"		
		
		print(site)	
		
		# load data
		load(file=paste(dir_inputs,site,".Rdata",sep=""))		
			
		# QCA should be 0
		teste<-sum(!dt_site[,.N, list(Year, DepthStratum)][order(Year, DepthStratum)]==dt_site[,length(unique(Station)), list(Year, DepthStratum)][order(Year, DepthStratum)])
		if (teste!=0)stop("check code")
			
		out<-dt_site[,.N, list(Year, DepthStratum)][order(Year, DepthStratum)]
		
		# save (csv2)
		write.table(out, file=paste(dir_outputs, site, "_AllocOrigin.csv", sep=""), row.names=FALSE, sep=";")
		
		}

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
		
	#fyke nets
	
		# for (site in c("Barsebäck","Fjällbacka_Cold","Fjällbacka_Warm","Kullen","Älgöfjorden"))
			# {
		# load(file=paste(dir_inputs,site,".Rdata",sep=""))		
		# print(table(dt_site$Gear))	
		# browser()	
		# # QCA should be 0
		# print(sum(!dt_site[,.N, list(Year, DepthStratum)][order(Year, DepthStratum)]==dt_site[,length(unique(Station)), list(Year, DepthStratum)][order(Year, DepthStratum)]))
			
		# out<-dt_site[,.N, list(Year, DepthStratum)][order(Year, DepthStratum)]

		# write.table(out, file=paste(dir_outputs, site, ".txt", sep=""))
		
			# }		
		
		
		

	
	

	

	
	
	
	
	table(dt_site$Station, dt_site$DepthStratum, dt_
	
