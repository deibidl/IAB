#In this R file we combine the results of the simulations. 
setwd("C:/Users/david/Desktop")
setwd("C:/Users/david/Desktop/PSMT")

##########################################################################################################################################
########################################################                       ###########################################################
######################################################## LOADING SIMULATIONS   ###########################################################
########################################################                       ###########################################################  
##########################################################################################################################################

n_scenarios = 3

scenario = c("1", "2", "3")
multiplier = c("1", "2", "3", "4", "5", "6")
combined  = list()

for (a in scenario) {
  for (b in multiplier) {
  names         = paste(paste("ATT", a, b,  sep="_"))
  
  archive_names = dir()[grep(names, dir())]
  
  load(archive_names[1])
  
  simul         = results

  for(i in archive_names[-1]) {
    print(i)
    load(i)
    
    simul       = rbind(simul, results)
    
  } 
  
  name = paste("simul",a, b, sep="_")
  assign (name, simul)
  combined[[which(a==scenario)+n_scenarios*(which(b==multiplier)-1)]] = simul  
  
}
}


save(combined, file=paste("combined",".Rdata",sep=""))


rm(list=setdiff(ls(), "combined"))


