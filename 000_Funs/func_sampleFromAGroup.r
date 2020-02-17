sampleFromAGroup<-function(x, y, nsize, samp_options=list(replacement=FALSE, sample_all_available=FALSE, sample_all_available_warning = TRUE)){
# 2015-2016 WP2 FishPI
	
	# Adapted by Nuno Prista from great original work of Liz Clarke, Marine Scotland.

	# 2016-10-17 Nuno Prista: added option sample all when sampling without replacement [see comments]
	# 2016-10-17 Nuno Prista: added sample2 [correction of behaviour of "sample" when only 1 element is being sampled]
	# 2016-10-17 Nuno Prista: improved identification of samples in result
	# 2016-10-18 Nuno Prista: improved code at samp_options level [made independent from position in list - > now easier to add options]
	# 2016-10-18 Nuno Prista: added suppress.warnings to samp_options 
	# 2018-05-29 Nuno Prista: adapted so that one of the group names can be NA (useful in, e.g., stratifying by maturity) 

nGroup<-length(nsize)
xSamp<-NULL
for (i in 1:nGroup) {
#print(i)

	if(!is.na(names(nsize)[i]))
		{
		indx<-unique(x[which(y==names(nsize)[i])]) 
		} else {indx <- x [which(is.na(y))] } # nuno 20180529 [handles NAs in stratification]
    # nuno 20161017: condicao para amostrar todos os disponiveis without replacement
    if(samp_options$replacement == FALSE & samp_options$sample_all_available == TRUE & nsize[i]>length(indx)){
            nsize[i] <- length(indx)
            if(samp_options$sample_all_available_warning==TRUE) {print(paste("sampling all available in group",names(nsize)[i]))}
            }
    # nuno 20161017: condicao para existencia de 1 unico elemento a amostrar (corrige comportamento de funcao sample)
    if(length(indx)>1){
        samp<-sample(indx, size=nsize[i], replace = samp_options$replacement)
        } else {samp<-sample2(indx, size=nsize[i], replace = samp_options$replacement)}
    # nuno 20161017: atribui nomes
	names(samp)<-rep(names(nsize)[i], length(samp))
    if(i == 1) {
    xSamp <-samp
    } else {
        xSamp <- c(xSamp, samp)
    }
    }
    return(xSamp)
}
