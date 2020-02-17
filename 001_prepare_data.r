# ==============================
# General data preparation
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================

	# Only Nordic coastal multimesh gillnet

rm(list=ls())
library(xlsx)
library(data.table)

# select site 
	# Askofjarden, Kinnbacksfjarden, Lagno, KvadofjardenWarm, Norrbyn
	# Galtfjarden, Gaviksfjarden, Holmon, Langvindsfjarden, Ranea, Torhamn, KvadofjardenCold, Graso, Hanobukten, Herrvik, Musko, Stavstensudde

# sets dirs
	if( site %in% c("Asköfjärden","Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kvädöfjärden_Warm","Lagnö","Långvindsfjärden","Norrbyn","Råneå","Torhamn","KvadofjardenCold", "Graso", "Hanobukten", "Herrvik", "Musko", "Stavstensudde")) 
		{
		dir_inputs <- "001_Inputs\\raw\\NordicGillNets/"
		site <- "Askofjarden"
		dir_save <- "001_Inputs\\prepared_main\\NordicGillNets/"	
		} else {stop ("define")}
	dir_results <- "003_Results\\001_Preparation\\"

# reads data
	a<-fread(file=paste(dir_inputs,"Data ", site,".txt",sep=""), header=T, na.strings=c("NA"," ",""), dec=",")

# updates site name
	site<-a$Area[[1]]
	if(site == "Fjällbacka") {site<-paste(a$Area[[1]],a$Season[1], sep="_")} 

# QCA check on colnames: should yield TRUE
	expect_colnames<-c('Area','Year','Season','Gear','GearCode','Repeated_Day','Depthstratum','Depth, m','Disturbance','Station','Cod>38N','CodN','CyprinidsB','CyprinidsN','Eel>45N','EelN','EelpoutN','Fish>30N','Fish>40N','Flounder>30N','FlounderN','HerringN','MesopredatorsN','Perch>25N','PerchB','PerchN','PikeN','Pikeperch>40N','PikeperchN','Piscivores>30N','PiscivoresN','TotalcatchN','WhitefishN')
	all.equal(colnames(a), expect_colnames)
	#colnames(a)[!colnames(a) %in% expect_colnames]

	# makes consistent colnames
		colnames(a)[colnames(a) == 'Repeated_Day'] <- 'RepeatedDay'
		colnames(a)[colnames(a) == 'Depthstratum'] <- 'DepthStratum'
		colnames(a)[colnames(a) == 'Depth, m'] <- 'Depth'
		colnames(a)[colnames(a) == 'Cod>38N'] <- 'Cod38N'
		colnames(a)[colnames(a) == 'Eel>45N'] <- 'Eel45N'
		colnames(a)[colnames(a) == 'Fish>30N'] <- 'Fish30N'
		colnames(a)[colnames(a) == 'Fish>40N'] <- 'Fish40N'
		colnames(a)[colnames(a) == 'Flounder>30N'] <-'Flounder30N'
		colnames(a)[colnames(a) == 'Perch>24N'] <- 'Perch25N' # check
		colnames(a)[colnames(a) == 'Perch>25N'] <- 'Perch25N'
		colnames(a)[colnames(a) == 'Pikeperch>40N'] <- 'Pikeperch40N'
		colnames(a)[colnames(a) == 'Piscivores>30N'] <- 'Piscivores30N'

	expect_colnames<-c('Area','Year','Season','Gear','GearCode','RepeatedDay','DepthStratum','Depth','Disturbance','Station','Cod38N','CodN','CyprinidsB','CyprinidsN','Eel45N','EelN','EelpoutN','Fish30N','Fish40N','Flounder30N','FlounderN','HerringN','MesopredatorsN','Perch25N','PerchB','PerchN','PikeN','Pikeperch40N','PikeperchN','Piscivores30N','PiscivoresN','TotalcatchN','WhitefishN')
	all.equal(colnames(a), expect_colnames)	# should yield TRUE
		
# check on gears
	table(a$Gear,useNA="al")
		
# some formating	
	a$Station<-as.character(a$Station)	
	a$RepeatedDay<-as.integer(a$RepeatedDay)	
	a$Disturbance[a$Disturbance=="YES"]<-"TRUE"
	a$Disturbance[a$Disturbance=="NO"]<-"FALSE"
	a$Disturbance<-as.logical(a$Disturbance)
			
	charVar <-c("Area","Season","Gear","GearCode","DepthStratum","Station")
	for(i in 1:length(charVar))
	{
	 a[[charVar[i]]]<-as.character(a[[charVar[i]]])
	}

	numVar <-colnames(a)[11:33]
	for(i in 1:length(numVar))
	{
	print(numVar[i])
	 a[[numVar[i]]]<-as.numeric(a[[numVar[i]]])		
	}	

		
# add stratum size
	# fix to match
	if(site %in% c("Kinnbäcksfjärden","Norrbyn","Barsebäck","Fjällbacka_Warm","Gaviksfjärden","Råneå")){a$DepthStratum[a$DepthStratum=="1-3m"]<-"0-3m"}
	
	# Factorizes DepthStratum
	a$DepthStratum<-factor(a$DepthStratum, levels = c("0-3m","3-6m","6-10m","10-20m"))
	
	# SurfaceArea_Km2 [Note: Discontinued]
		auxSurfaceArea<-read.table(paste("001_Inputs\\auxAreaDjup.txt",sep=""), dec=",", header=T)
		auxSurfaceArea$ID<-paste(auxSurfaceArea$Area,auxSurfaceArea$DepthStratum)
		a$ID<-paste(a$Area,a$DepthStratum)
		dim(a)
		a<-merge(a, auxSurfaceArea[c("ID","SurfaceArea_Km2")], by="ID", all.x=T)
		unique(a[,list(DepthStratum,SurfaceArea_Km2)][order(DepthStratum)])
	
# No of stations (email exchange with Jens, 2019-05-09/10)
	table(a$Year, a$DepthStratum)
	tapply(a$Station, list(a$Year, a$DepthStratum), function(x)length(unique(x)))
	
# Some tweaks	
	a$NStations<-as.numeric(NA)
	a$Area[[1]]
	if(site=="Asköfjärden"){a$NStations <- 12}
	if(site %in% c("Torhamn")){
				a$NStations[a$DepthStratum=="0-3m"] <- 21 # note: 27 on 2002!
				a$NStations[a$DepthStratum=="3-6m"] <- 14
				a$NStations[a$DepthStratum=="6-10m"] <- 5
				a$NStations[a$DepthStratum=="10-20m"] <- 0
					}
	if(site %in% c("Råneå")){
				a$NStations[a$DepthStratum=="0-3m"] <- 15
				a$NStations[a$DepthStratum=="3-6m"] <- 20
				a$NStations[a$DepthStratum=="6-10m"] <- 10
				a$NStations[a$DepthStratum=="10-20m"] <- 0
					}
	if(site %in% c("Långvindsfjärden")){
				a$NStations[a$DepthStratum=="0-3m"] <- 14
				a$NStations[a$DepthStratum=="3-6m"] <- 13
				a$NStations[a$DepthStratum=="6-10m"] <- 13
				a$NStations[a$DepthStratum=="10-20m"] <- 5
					}
	if(site %in% c("Holmön")){
				a$NStations[a$DepthStratum=="0-3m"] <- 20
				a$NStations[a$DepthStratum=="3-6m"] <- 10
				a$NStations[a$DepthStratum=="6-10m"] <- 0
				a$NStations[a$DepthStratum=="10-20m"] <- 0
					}
	if(site %in% c("Galtfjärden")){
				a$NStations[a$DepthStratum=="0-3m"] <- 7
				a$NStations[a$DepthStratum=="3-6m"] <- 7
				a$NStations[a$DepthStratum=="6-10m"] <- 8
				a$NStations[a$DepthStratum=="10-20m"] <- 8
					}
	if(site %in% c("Gaviksfjärden")){
				a$NStations[a$DepthStratum=="0-3m"] <- 11
				a$NStations[a$DepthStratum=="3-6m"] <- 14
				a$NStations[a$DepthStratum=="6-10m"] <- 15
				a$NStations[a$DepthStratum=="10-20m"] <- 5
					}
	if(site %in% c("Kinnbäcksfjärden", "Norrbyn")){
				a$NStations[a$DepthStratum=="0-3m"] <- 14
				a$NStations[a$DepthStratum=="3-6m"] <- 12
				a$NStations[a$DepthStratum=="6-10m"] <- 14
				a$NStations[a$DepthStratum=="10-20m"] <- 5
					}
	if(site=="Lagnö"){
				a$NStations[a$DepthStratum=="0-3m"] <- 13
				a$NStations[a$DepthStratum=="3-6m"] <- 13
				a$NStations[a$DepthStratum=="6-10m"] <- 14
				a$NStations[a$DepthStratum=="10-20m"] <- 5
					}							
	if(paste(a$Area[[1]],a$Season[1], sep="_") == "Kvädöfjärden_Warm"){
				a$NStations[a$DepthStratum=="0-3m"] <- 12
				a$NStations[a$DepthStratum=="3-6m"] <- 15
				a$NStations[a$DepthStratum=="6-10m"] <- 12
				a$NStations[a$DepthStratum=="10-20m"] <- 6
					}					
	if(paste(a$Area[[1]],a$Season[1], sep="_") == "Kvädöfjärden_Cold"){
				a$NStations[a$DepthStratum=="0-3m"] <- 5
				a$NStations[a$DepthStratum=="3-6m"] <- 10
				a$NStations[a$DepthStratum=="6-10m"] <- 20
				a$NStations[a$DepthStratum=="10-20m"] <- 20
					}
	if(site=="Gräsö"){
				a$NStations[a$DepthStratum=="0-3m"] <- 13
				a$NStations[a$DepthStratum=="3-6m"] <- 13
				a$NStations[a$DepthStratum=="6-10m"] <- 13
				a$NStations[a$DepthStratum=="10-20m"] <- 5
					}
	if(site=="Hanöbukten"){
				a$NStations[a$DepthStratum=="0-3m"] <- 10
				a$NStations[a$DepthStratum=="3-6m"] <- 10
				a$NStations[a$DepthStratum=="6-10m"] <- 15
				a$NStations[a$DepthStratum=="10-20m"] <- 15
					}
	if(site=="Herrvik"){
				a$NStations[a$DepthStratum=="0-3m"] <- 11
				a$NStations[a$DepthStratum=="3-6m"] <- 12
				a$NStations[a$DepthStratum=="6-10m"] <- 11
				a$NStations[a$DepthStratum=="10-20m"] <- 11
					}
	if(site=="Muskö"){
				a$NStations[a$DepthStratum=="0-3m"] <- 8
				a$NStations[a$DepthStratum=="3-6m"] <- 12
				a$NStations[a$DepthStratum=="6-10m"] <- 16
				a$NStations[a$DepthStratum=="10-20m"] <- 18
					}
	if(site=="Stavstensudde"){
				a$NStations[a$DepthStratum=="0-3m"] <- 5
				a$NStations[a$DepthStratum=="3-6m"] <- 8
				a$NStations[a$DepthStratum=="6-10m"] <- 18
				a$NStations[a$DepthStratum=="10-20m"] <- 17
					}


# saves prepared dara
	dt_site<-a
	save(dt_site, file=paste(dir_save, site,".Rdata",sep=""))
	
		