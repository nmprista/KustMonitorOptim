prn <- function(seed){
  # function to set seed in multiple cores (More details in Jochen Knaus, Christine Porzelius Parallel computing using R package snowfall)
  # applied first time during fishpi simulations
  set.seed(seed)
  
   #demonstracao
   #sfLapply(1:10, prn)
   #b<-sfLapply(1:10, function(x){rnorm(2)});b 
  }
