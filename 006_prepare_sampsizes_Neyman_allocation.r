# ==============================	
# Preparation of sample size files (Neyman Scenarios)
# Nuno Prista, SLU Aqua, 2019 @ KustMonitoring project
# ==============================
	
	# Includes simplified compromise Neyman allocation (based on Cochran, 1977)
		# at indicator level (optimizing across strata*years)
		# at multi-indicator level (optimizing across strata*years*variables)
	
	# Note: this script uses simulation results to produce sample size tables for a range of allocation scenarios
		# after running it, the sample size tables are used in new runs of scripts 003, 004 and 005 to produce the results fot these new allocations
	
rm(list=ls()); graphics.off()

library(data.table)
library("PerformanceAnalytics")

# auxiliary function			
	source("000_Funs/func_chart_Correlation2.r")

# select site
	site<-"Lagnö" # sites in in c("Asköfjärden", "Norrbyn","Kvädöfjärden_Warm", "Galtfjärden","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Lagnö","Långvindsfjärden","Råneå","Torhamn")

# select input scenario
	scenario <- "AllocOrigin"

# loads original data			
	dir_inputs<-"001_Inputs/prepared_main/NordicGillNets/"
	load(file=paste("001_Inputs/prepared_main/NordicGillNets/",site,".Rdata",sep=""))

# loads simulations
	dir_sim_outputs <- paste("002_Sim_Outputs/",scenario,"/prepared/",sep="")
	load(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_pop_prep.rdata", sep="")) 
	load(file=paste(dir_sim_outputs, site, "_", scenario,"_5000sims_res_strata_prep.rdata", sep="")) 

# option for Swedish captions
	sve=TRUE

# table spp conversion eng to sve
	spp_name_conv<-data.frame(eng = c('CodN','CyprinidsB','CyprinidsN','FlounderN','HerringN','PerchB','PerchN','PikeN','PikeperchN','PiscivoresN','WhitefishN'), 
			sve = c('TorskN','CypriniderB','CypriniderN','SkrubbskäddaN','StrömmingN','AbborreB','AbborreN','GäddaN','GösN','PiscivorerN','SikN'))


# ====================
# Analysis of frequency of occurrence (in strata*year combinations)
# ====================
	
	# rare variables tend to be very variable and pull the design to the few strata where they happened to have occurred
	# rare variables may however be important (e.g., invasive species will be null and then rare at early stages)

	dir_output <- paste("003_Results/006_Freq_Occurrence/", sep=""); dir.create(dir_output, showWarnings = FALSE)
		
	# freq of occurrence
		mydata<-res_strata
		freq_occ<-tapply(mydata$mean_x, mydata$variable, function(x) sum(!x==0))/table(mydata$variable)
		freq_occ
		windows(4,3)
		barplot(freq_occ, las=1, cex.axis=0.7, cex.names=0.7, cex.lab=0.7, col="blue", xlab="freq. occurrence", main = paste(site, ": freq. occurrence", sep=""), horiz=T, space=.85, cex.main=0.9)
		savePlot(paste(dir_output, site, "_freq_occ.png", sep=""), type="png")
		write.csv2(freq_occ, file=paste(dir_output,site,"_freq_occ.csv",sep=""))	

		if(sve==TRUE) 
			{
			windows(4,3); par(oma=c(0,2,0,0), mar=c(4,3.5,2,2))
			freq_occ_sve<-freq_occ
			names(freq_occ_sve)<-spp_name_conv$sve[match(names(freq_occ_sve), spp_name_conv$eng)]
			barplot(freq_occ_sve, las=1, cex.axis=0.7, cex.names=0.7, cex.lab=0.7, col="blue", xlab="", main = paste("Indikator förekomstfrekvens", sep=""), horiz=T, space=.85, cex.main=0.9)
			mtext("förekomstfrekvens", side=1, line=2.5, cex=0.7)	
			savePlot(paste(dir_output, site, "_freq_occ_sve.png", sep=""), type="png")
			write.csv2(freq_occ_sve, file=paste(dir_output,site,"_freq_occ_sve.csv",sep=""))	
			}

	graphics.off()
	
# ====================
# Analysis of correlations
# ====================

	# very correlated variables will tend to pull the allocation to their optimal allocation
		# if one wants the design to perform "reasonably" (as in compromise) for the diversity of variables variables that display large correlation should only be present once
		# If wants the design to perform "reasonably" (as in compromise) for most variables, it may be worth maintaining correlated variables

	dir_graph <- paste("003_Results/006_Correlations/", sep=""); dir.create(dir_graph, showWarnings = FALSE)
	
	# pop level correlations
		mydata<-dcast(res_pop[,c("Year","variable","st_mean"), with=F],  Year~variable, fun=mean,drop=FALSE)
		
		round(cor(mydata[,target_vars, with=F], use="complete.obs", method="spearman"),2)
		windows()		
		chart.Correlation(mydata[,target_vars, with=F], use="complete.obs", method="spearman", histogram=TRUE, pch=19)
		mtext(paste(site,"- pop level Spearman correlation"),3,3)
		
		windows()		
		chart.Correlation(mydata[,target_vars, with=F], use="complete.obs", method="kendall", histogram=TRUE, pch=19)
		mtext(paste(site,"- pop level Kendall correlation"),3,3)
	
	# strata level correlations
		mydata<-dcast(res_strata[,c("Year","stratum","variable","mean_x"), with=F],  Year+stratum~variable, fun=mean,drop=FALSE)
			
		res <- round(cor(mydata[,target_vars, with=F], use="complete.obs", method="spearman"),2)
		windows() 
		chart.Correlation(mydata[,target_vars, with=F], use="complete.obs", method="spearman", histogram=TRUE, pch=19)
		mtext(paste(site,"- strata level Spearman correlation"),3,3)
		savePlot(paste(dir_graph, site,"_correl_Pearson.png",sep=""), type="png")
		write.csv2(res, file=paste(dir_graph, site,"_correl_Pearson.csv",sep=""))
		
		res <- round(cor(mydata[,target_vars, with=F], use="complete.obs", method="kendall"),2)
		windows()
		chart.Correlation(mydata[,target_vars, with=F], use="complete.obs", method="kendall", histogram=TRUE, pch=19)
		mtext(paste(site,"- strata level Kendall correlation"),3,3)
		savePlot(paste(dir_graph, site,"_correl_Kendall.png",sep=""), type="png")
		write.csv2(res, file=paste(dir_graph, site,"_correl_Kendall.csv",sep=""))

		if(sve==TRUE) {
					colnames(mydata)[!colnames(mydata)%in%c("Year","stratum")]<-as.character(spp_name_conv$sve[match(colnames(mydata)[!colnames(mydata)%in%c("Year","stratum")], spp_name_conv$eng)])
					mydata[, ,!colnames(mydata) %in% names(freq_occ_sve[freq_occ_sve<0.5])]
					
					res <- round(cor(mydata[,names(freq_occ_sve[freq_occ_sve>=0.5]), with=F], use="complete.obs", method="spearman"),2)
					windows(9,7) 
					chart.Correlation2(mydata[,names(freq_occ_sve[freq_occ_sve>=0.5]), with=F], use="complete.obs", method="spearman", histogram=FALSE, pch=19, cex=1)
					if(site=="Kvädöfjärden_Warm") site2 <- "Kvädöfjärden" else site2 <- site
					mtext(paste(site2,"- Spearman korrelation (strata medeltal)"),3,3)
					savePlot(paste(dir_graph, site,"_correl_Pearson_sve.png",sep=""), type="png")
					write.csv2(res, file=paste(dir_graph, site,"_correl_Pearson_sve.csv",sep=""))
					 }
		
	# fishing site level correlations
		mydata<-dt_site[,target_vars, with=F]
		
		round(cor(mydata[,target_vars, with=F], use="complete.obs", method="spearman"),2)
		windows()
		chart.Correlation(mydata, use="complete.obs", method="pearson", histogram=TRUE, pch=19)
		chart.Correlation(mydata, use="complete.obs", method="spearman", histogram=TRUE, pch=19)
		mtext(paste(site,"- sample level Spearman correlation"),3,3)
		
		windows()
		chart.Correlation(mydata, use="complete.obs", method="kendall", histogram=TRUE, pch=19)
		mtext(paste(site,"- sample level Kendall correlation"),3,3)
		
	graphics.off()

		
# ====================		
# Production of sample_size table for different allocation scenarios (settings based on above analysis)
# ====================

	# ============
	# selection of output scenario
	# ============	

	# site-specific scenarios
		# Torhamn	AllocScenarioCypNHerNPerNFloN
		# Råneå	AllocScenarioCypNHerNPerNWhiN
		# Långvindsfjärden	AllocScenarioCypNHerNPerNWhiN
		# Lagnö	AllocScenarioCypNHerNPerNWhiN
		# Kinnbäcksfjärden	AllocScenarioCypNHerNPerNWhiN
		# Holmön	AllocScenarioCypNHerNPerNWhiN
		# Hanöbukten	AllocScenarioCodNHerNFloN
		# Gräsö	AllocScenarioCypNHerNPerNWhiN
		# Gaviksfjärden	AllocScenarioCypNHerNPerNWhiN
		# Galtfjärden	AllocScenarioCypNHerNPerNPikperNWhiN
		
	# select scenarion
		scenario <- "AllocScenarioCypNHerNPerNWhiN"
		dir_table<-paste("002_Sim_Inputs/", scenario,"/", sep=""); dir.create(dir_table, showWarnings=FALSE)
		dir_results<-paste("003_Results/006_Neyman_Sample_Sizes/", sep=""); dir.create(dir_results, showWarnings=FALSE)
		
	# monoindicator scenarios
		if (scenario %in% c('AllocScenarioCodN','AllocScenarioCyprinidsB','AllocScenarioCyprinidsN','AllocScenarioFlounderN','AllocScenarioHerringN','AllocScenarioPerchB','AllocScenarioPerchN','AllocScenarioPikeN','AllocScenarioPikeperchN','AllocScenarioPiscivoresN','AllocScenarioWhitefishN'))
			{
			selected_spp<-freq_occ[gsub("AllocScenario","", scenario)]
			if(selected_spp<0.5) print("Attention: rare species selected!")
			}
	
	# Other scenarios
		# baseline restricted on frequency 'AllocScenario3'
		if (scenario == c('AllocScenario3'))
			{
			# restriction to frequent species
			threshold <- 0.5
			selected_spp <- freq_occ[freq_occ>=threshold]
			}			
		# baseline restricted on frequency 'AllocScenario3', no PiscivoresN
			# test for "Asköfjärden", "Norrbyn","Kvädöfjärden_Warm"
		if (scenario == c('AllocScenario3a'))
			{
			# restriction to frequent species
			threshold <- 0.5
			selected_spp <- freq_occ[freq_occ>=threshold]
			selected_spp<-selected_spp[selected_spp!="PiscivoresN"]
			}			
		# baseline restricted on frequency 'AllocScenario3', no PiscivoresN, N diversity
			# test for "Asköfjärden", "Norrbyn","Kvädöfjärden_Warm"		
		if (scenario == c('AllocScenario4'))
			{
			# restriction to frequent species
			threshold <- 0.5
			selected_spp <- freq_occ[freq_occ>=threshold]
			selected_spp<-selected_spp[names(selected_spp)!="PiscivoresN"]
			selected_spp<-selected_spp[grepl(names(selected_spp), pat="N")]
			}

		# k-lab suggested scenarios	
		if (scenario %in% c('AllocScenarioCypNHerNPerN'))
			{
			selected_spp<-freq_occ[c("CyprinidsN","HerringN","PerchN")]
			if(any(selected_spp<0.5)) print("Attention: rare species selected!")
			}
		if (scenario %in% c('AllocScenarioCypNHerNPerNFloN'))
			{
			# only Asköfjärden and Kvädöfjärden_Warm and Torhamn
			selected_spp<-freq_occ[c("CyprinidsN","HerringN","PerchN","FlounderN")]
			if(any(selected_spp<0.5)) print("Attention: rare species selected!")
			}
		if (scenario %in% c('AllocScenarioCypNHerNPerNFloNWhiN'))
			{
			# only Asköfjärden
			selected_spp<-freq_occ[c("CyprinidsN","HerringN","PerchN","FlounderN","WhitefishN")]
			if(any(selected_spp<0.5)) print("Attention: rare species selected!")
			}
		if (scenario %in% c('AllocScenarioCodNHerNFloN'))
			{
			# only Hanöbukten
			selected_spp<-freq_occ[c("CodN","HerringN","FlounderN")]
			if(any(selected_spp<0.5)) print("Attention: rare species selected!")
			}	
		if (scenario %in% c('AllocScenarioCypNHerNPerNFloNPikperN'))
			{
			# only Kvädöfjärden_Warm
			selected_spp<-freq_occ[c("CyprinidsN","HerringN","PerchN","FlounderN","PikeperchN")]
			if(any(selected_spp<0.5)) print("Attention: rare species selected!")
			}	
		if (scenario %in% c('AllocScenarioCypNHerNPerNWhiN'))
			{
			# several areas
			selected_spp<-freq_occ[c("CyprinidsN","HerringN","PerchN","WhitefishN")]
			if(any(selected_spp<0.5)) print("Attention: rare species selected!")
			}	
		if (scenario %in% c('AllocScenarioCypNHerNPerNPikperNWhiN'))
			{
			# only Galtfjärden
			selected_spp<-freq_occ[c("CyprinidsN","HerringN","PerchN","PikeperchN","WhitefishN")]
			if(any(selected_spp<=0.5)) print("Attention: rare species selected!")
			}	
		# just for compatibility purposes (will not be used)
		if (scenario %in% c('AllocScenarioOriginal'))
			{
			selected_spp<-freq_occ[c("CyprinidsN","HerringN","PerchN")]
			}
			
		selected_spp		

	# ============
	# Compromise Neyman allocation (based on Cochran)
	# ============	
		if(nlevels(boot_res_strata$stratum)==4)
			{
			out <- data.frame(Year=sort(unique(res_pop$Year)), variable=rep(target_vars, length(sort(unique(res_pop$Year)))), a=NA, b=NA, c=NA, d=NA)
			colnames(out)[3:6] <- levels(boot_res_strata$stratum)
			}
		if(nlevels(boot_res_strata$stratum)==3)
			{
			out <- data.frame(Year=sort(unique(res_pop$Year)), variable=rep(target_vars, length(sort(unique(res_pop$Year)))), a=NA, b=NA, c=NA)
			colnames(out)[3:5] <- levels(boot_res_strata$stratum)
			}
		if(nlevels(boot_res_strata$stratum)==2)
			{
			out <- data.frame(Year=sort(unique(res_pop$Year)), variable=rep(target_vars, length(sort(unique(res_pop$Year)))), a=NA, b=NA)
			colnames(out)[3:4] <- levels(boot_res_strata$stratum)
			}
		
		N <- max(res_pop$N)

		for (i in target_vars)
		{
			for (j in 2002:2018)
			{
			tmp<-res_strata[variable == i & Year == j,][order(stratum)]	
			tmp1<-round(N*tmp$stratum_size*sqrt(tmp$var_x)/ sum(tmp$stratum_size*sqrt(tmp$var_x)),1)
			names(tmp1)<-tmp$stratum
			if(nlevels(boot_res_strata$stratum)==4)	out[out$Year == j & out$variable== i,3:6] <-tmp1[match(colnames(out)[3:6], names(tmp1))]
			if(nlevels(boot_res_strata$stratum)==3)	out[out$Year == j & out$variable== i,3:5] <-tmp1[match(colnames(out)[3:5], names(tmp1))]
			if(nlevels(boot_res_strata$stratum)==2)	out[out$Year == j & out$variable== i,3:4] <-tmp1[match(colnames(out)[3:4], names(tmp1))]
			#out<-rbind(out, round(N*tmp$stratum_size*sqrt(tmp$true_var_mean)/ sum(tmp$stratum_size*sqrt(tmp$true_var_mean)),1))
			}
		}	
		
		out<-out[order(out$variable, out$Year),]
		res_neyman <- out

		res_neyman_tmp0 <- res_neyman
	
		# restriction to scenario
			res_neyman_tmp <- droplevels(res_neyman_tmp0[res_neyman_tmp0$variable %in% names(selected_spp),])
		
		# sets to 0 strata where variable is not used in estimate
			res_neyman_tmp[res_neyman_tmp$variable %in% c("CyprinidsB","CyprinidsN","EelN","EelpoutN","MesopredatorsN","PerchB","PerchN","PikeN","PiscivoresN"),"10-20m"]<-0

		# handling of the missing year*variables
			# assumes the best way is to sample according to the average neyman of the variable
			ls1 <- split(res_neyman_tmp, res_neyman_tmp$variable)
			ls2 <- lapply(ls1, function(x, N1=N){ 
						if(sum(is.na(x$'0-3'))>0) { 
							table_values <- prop.table(apply(x[!is.na(x$'0-3'),c("0-3m","3-6m","6-10m","10-20m")],2,mean))*N1
							x[is.na(x$'0-3m'),"0-3m"]<-table_values[["0-3m"]]
							x[is.na(x$'3-6m'),"3-6m"]<-table_values[["3-6m"]]
							x[is.na(x$'6-10m'),"6-10m"]<-table_values[["6-10m"]]
							x[is.na(x$'10-20m'),"10-20m"]<-table_values[["10-20m"]]
							x
							} else x})
			res_neyman_tmp2<-rbindlist(ls2)

			# quick-check	
			table(res_neyman_tmp2$variable)
			table(res_neyman_tmp2$Year)

		# determines and displays original compromise allocation	
			compromise_alloc<-apply(res_neyman_tmp2[,3:c(nlevels(boot_res_strata$stratum)+2)],2,mean,na.rm=T)/sum(apply(res_neyman_tmp2[,3:c(nlevels(boot_res_strata$stratum)+2)],2,mean,na.rm=T))*N
			compromise_alloc

		if (scenario %in% c('AllocScenarioOriginal'))
			{
			compromise_alloc<-prop.table(tapply(res_strata$stratum_size, res_strata$stratum, function(x) unique(x)))*N
			}

		# corrects for <5 strata (if any)
			compromise_alloc_corr<-compromise_alloc
			tmp <- compromise_alloc_corr[round(compromise_alloc_corr)<=5]
			# assings 5 as minimum per strata
			compromise_alloc_corr[names(tmp)]<-5
			# redistributes the remaining maintaining optimum allocation
			tmp <- names(compromise_alloc_corr)[compromise_alloc_corr>5]
	
		# aligns with sampling goals	
			# original distribution
			compromise_alloc_corr_original<-compromise_alloc_corr
			compromise_alloc_corr_original[tmp]<-round(prop.table(compromise_alloc[tmp])* (N-sum(compromise_alloc_corr[!names(compromise_alloc_corr) %in% tmp])))
			if(sum(compromise_alloc_corr_original)==N){
				compromise_alloc_corr<-compromise_alloc_corr_original
				}
			if(sum(compromise_alloc_corr_original)>N) {
				print("case minus1 needed")
				compromise_alloc_corr_minus1<-compromise_alloc_corr_original
				v1<-which(prop.table(compromise_alloc[tmp]) == min(prop.table(compromise_alloc[tmp])))
				compromise_alloc_corr_minus1[v1] <- compromise_alloc_corr_minus1[v1]-1
				compromise_alloc_corr<-compromise_alloc_corr_minus1
				}
			if(sum(compromise_alloc_corr_original)<N) {
				print("case plus1 needed")
				compromise_alloc_corr_plus1<-compromise_alloc_corr_original
				v1<-which(prop.table(compromise_alloc[tmp]) == max(prop.table(compromise_alloc[tmp])))
				compromise_alloc_corr_plus1[v1] <- compromise_alloc_corr_plus1[v1]+1
				compromise_alloc_corr<-compromise_alloc_corr_plus1
				}			
		
		# QCA (should yield TRUE)
		sum(compromise_alloc_corr) == N
		
		# displays final compromise allocation		
		origin_alloc<-tapply(res_strata$stratum_size, res_strata$stratum, function(x) unique(x))
		out<-rbind(origin_alloc, compromise_alloc, compromise_alloc_corr)
		out

		# saves
		write.csv2(out, file=paste(dir_results, site, "_", scenario,".csv", sep=""), row.names=T)		

		
	# ============
	# Production of final table (includes OriginAlloc ("N") for sake of simplifying subsequent graph production)
	# ============			

		# series of compromise_alloc_corr

            # attention: there is an exception for Lägnö asked by Magnus - email 2019-10-07 10:54

		compromise_alloc_corr_series<-compromise_alloc_corr
		for(i in 1:10)
		{

		if(i==1) b<-compromise_alloc_corr_series else b<-compromise_alloc_corr_series[i,]
		if(i==2) rownames(compromise_alloc_corr_series)[i-1]<-paste("n",N, sep="")
		b[tmp]<-round(prop.table(compromise_alloc[tmp])* ( (N-5*i-sum(b[!names(b) %in% tmp]))))
        if(any(b[tmp]<5) & !site=="Lagnö") break()
		if(any(b[tmp]<4 )) break() 
		compromise_alloc_corr_series <- rbind(compromise_alloc_corr_series, b)
		rownames(compromise_alloc_corr_series)[i+1]<-paste("n",N-5*i,sep="")
		}

		# adds original allocation
		compromise_alloc_corr_series <- rbind(origin_alloc, compromise_alloc_corr_series)
		rownames(compromise_alloc_corr_series)[1]<-"N"

		# displays final compromise allocation_corr_series
		compromise_alloc_corr_series
	
		# builds samp_size_table 
			# note on warning: no problem if strata and years are ordered correctly in both objects]
			samp_size_table <- data.frame(unique(res_strata[,c("Year","stratum"), with=F])[order(Year,stratum)], t(compromise_alloc_corr_series))
			colnames(samp_size_table)[2]<-"DepthStratum"
			samp_size_table
		# saves to inputs
		write.csv2(samp_size_table, file=paste(dir_table, site, "_", scenario,".csv", sep=""), row.names=F)

