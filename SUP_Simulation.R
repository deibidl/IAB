
library(sampling)
library(MatchIt)
library(data.table)
library(survey)
library(Hmisc)


setwd("/users/dlenis/Liz/PSM2")
load("allo.RData")

temp       = commandArgs(TRUE)
f          = as.numeric(temp[1])
set.seed(f)  



for (scenario in 1:length(data_sets)){
  
  #Creating the sample
  data          = data_sets[[scenario]]
  data$ones     = 1
  data$id       = 1:nrow(data)
  Scenario      = mean(data$Scenario)
  Multiplier    = mean(data$Multiplier)
  
  
  rerun = FALSE
  outfile = paste(paste("ATT", Scenario, Multiplier, f, sep="_"),".Rdata",sep="")
  if (!file.exists(outfile) | rerun){ 
    
  ##########################################################################################################################
  # POPULATION PARAMETES
  ##########################################################################################################################
  
  att  = mean(data$y1[data$z==1])-mean(data$y0[data$z==1])
  ate  = mean(data$y1)-mean(data$y0)
  
  size1 = rep(100000,10)
  size2 = rep(5,10)
  size3 = c(rep(750/5,5), rep(700/5,5), rep(650/5,5), rep(600/5,5), rep(550/5,5), 
            rep(450/5,5), rep(400/5,5), rep(350/5,5), rep(300/5,5), rep(250/5,5))
  
  
  
  
  ##########################################################################################################################
  # CREATING THE SAMPLE
  ##########################################################################################################################
  
  
  set.seed(f)   
  s    = mstage(data, stage    = list("stratified","cluster",""), 
                varnames = list("Strata","Cluster","ones"),
                size     = list( size1, size2,size3),
                method   = list("","srswor","srswor"))
  
  
  sample  = getdata(data,s)[[3]]
  
  atts = mean(sample$y1[sample$z==1])-mean(sample$y0[sample$z==1])
  ratiosp = (atts/att)-1
  sample$s.wt = 1/sample$Prob
  
  ##########################################################################################################################
  # MATCHING FUNCTION
  ##########################################################################################################################
  
  #Arguments of the function
  
  # data       : sample
  # sample.wt  : name of sampling weights
  # treatment  : name of the treatment indicator
  # outcome    : outcome of interest
  # missing    : are there missing values? 4 options: (1) "No"   No Missing data
  #                                                   (2) "MCAR" Missing Completely at Random
  #                                                   (3) "MAR"  Missing at Random (the treatment doesnt affect the response rate)
  #                                                   (3) "MART" Missing at Random (the treatment affects the response rate)
  # method_PS  : how are the weights used in the estimation the PS Model? 3 options (1) "U" Unweighted (2) "W" Weighted (3) "C" Covariate
  # method_OM  : how are the weights used in the estimation the Outcome Model? 2 options (1) "U" Unweighted (2) "W" Weighted 
  # method     : what matching method should be used? (same option as in MatchIt)
  # ratio      : how many controls should be matched to each treated?
  # covariates : what covariates should be included in (1) the PSM and (2) the adjusted OM
  # sigma_SMD  : common SD to use in the computation of the SMD so they are comparable
  # patt       : population value of the ATT (to compute coverage)
  
  
  
  MwCSD = function(data         , sample.wt    , treatment  ,  outcome    ,
                   missing      , method_PS    , method_OM  ,  w.transfer ,
                   method       , ratio        ,  
                   covariatesPS , covariatesOM , covariatesR, sigma_SMD  ,
                   patt         , sp) {
    
    #Name of the Estimator
    name = paste(paste(method_PS,"PS",sep="."), paste(method_OM,"OM",sep = "."), w.transfer, missing, sep = "|")
    
    #Renaming the variables
    names(data[,sample.wt]) = "s.wt" 
    names(data[,treatment]) = "z" 
    names(data[,outcome])   = "y" 
    
    
    
    #Missing Data?
           if (missing == "No")   {
     
      sd_w      = sd(data$s.wt)       
      data      = data[data$r1==1,]
  
    }else  if (missing == "MAR")  {
      
      design.r  = svydesign(ids=~1, data=data)
      formula_R = as.formula(paste("r2", paste(covariatesR, collapse=" + "), sep=" ~ "))
      pr.R      = svyglm(formula_R, design.r, family = "binomial")
      data$ps.r = predict(pr.R  , data=data, type = "response")
      data$s.wt = data$s.wt*(1/data$ps.r)
      sd_w      = sd(data$s.wt)
      data      = data[data$r2==1,]
      
    
    }else  if (missing == "MARX")  {
      
      design.r  = svydesign(ids=~1, data=data)
      formula_R = as.formula(paste("r3", paste(c(covariatesR,"x7"), collapse=" + "), sep=" ~ "))
      pr.R      = svyglm(formula_R, design.r, family = "binomial")
      data$ps.r = predict(pr.R  , data=data, type = "response")
      data$s.wt = data$s.wt*(1/data$ps.r)
      sd_w      = sd(data$s.wt)
      data      = data[data$r3==1,]  
    
    }else  if (missing == "MART")  {
      
      design.r  = svydesign(ids=~1, data=data)
      formula_R = as.formula(paste("r4", paste(c(covariatesR,"z"), collapse=" + "), sep=" ~ "))
      pr.R      = svyglm(formula_R, design.r, family = "binomial")
      data$ps.r = predict(pr.R  , data=data, type = "response")
      data$s.wt = data$s.wt*(1/data$ps.r)
      sd_w      = sd(data$s.wt)
      data      = data[data$r4==1,]
    
    }else {stop("Missing must be 'No', 'MAR','MARX' or 'MART'")}
    
    
    #How Should the propensity score be estimated?
          if (method_PS %in% c("W","U")) {formula_PSM = as.formula(paste("z", paste(covariatesPS          , collapse=" + "), sep=" ~ "))
    }else if (method_PS %in% c("C"))     {formula_PSM = as.formula(paste("z", paste(c(covariatesPS,"s.wt"), collapse=" + "), sep=" ~ "))
    }else {stop("Method_PS must be 'W', 'U' or 'C'")}
    
          if (method_PS %in% c("C","U")) {design_PSM = svydesign(ids=~1,                                      data=data)
    }else if (method_PS %in% c("W"))     {design_PSM = svydesign(ids=~Cluster, strata=~Strata, weights=~s.wt, data=data)
    }else {stop("Method_PS must be 'W', 'U' or 'C'")}
    
    
    #Matching
    mt.w      = svyglm(formula_PSM, design_PSM, family = "quasibinomial")
    data$ps.w = predict(mt.w  , data=data, type = "response")
    m         = matchit(formula = formula_PSM, data=data, method = method, ratio = ratio, distance = data$ps.w, replace = F)
    
    #Should we transfer the sampling weigths?
    if (w.transfer=="WT") {
      
      m.data    = match.data(m) 
      m.data$id = rownames(m.data)
      
      matched    = data.frame(m$match.matrix)
      matched$id = rownames(matched)
      
      m.data                           = merge(m.data,matched,by="id", all.x=TRUE)
      m.data$X1[m.data[,"z"]==0] = m.data$id[m.data[,"z"]==0]
      
      dt       = data.table(m.data)
      dt       = dt[order(dt$X1,dt$z),]
      dt[,s.wt2:=shift(s.wt,1, type="lead"),by=X1]
      
      m.data = data.frame(dt)
      m.data$s.wt[m.data[,"z"]==0]=m.data$s.wt2[m.data[,"z"]==0]
    
    }else if (w.transfer=="OW") { m.data = match.data(m) 
    
    }else {stop("W.Transfer must be 'WT' or 'OW'")}
    
    #SMD
    smd_l     = list()
    names_smd = rep(NA,(length(covariatesOM)+1))
    
    smd_weights = ifelse (method_PS=="U" & method_OM =="U", "ones", "s.wt")
    
    smd_varlist = c(covariatesOM,"s.wt")
    
    for(vars in 1:length(smd_varlist)){ 
      
             if (smd_varlist[vars]!="s.wt"){
      smd             = diff(tapply(m.data[,smd_varlist[vars]] * m.data[,smd_weights], m.data[,"z"], sum)/tapply(m.data[,smd_weights], m.data[,"z"], sum))/ sigma_SMD[vars]
      smd_l[[vars]]   = smd
      names_smd[vars] = paste("SMD", smd_varlist[vars], name, sep = ".")
      
      } else if (smd_varlist[vars]=="s.wt"){
      smd             = diff(tapply(m.data[,smd_varlist[vars]] * m.data[,"ones"], m.data[,"z"], sum)/tapply(m.data[,"ones"], m.data[,"z"], sum))/ sd_w
      smd_l[[vars]]   = smd
      names_smd[vars] = paste("SMD", smd_varlist[vars], name, sep = ".")
    }}
    
    smd        = unlist(smd_l)
    names(smd) = names_smd
    
    
    #How Should the outcome model be estimated?
          if (method_OM %in% c("U"))      {design_OM = svydesign(ids=~1, data=m.data)
    }else if (method_OM %in% c("W")) {design_OM = svydesign(ids=~Cluster, strata=~Strata, weights=~s.wt, data=m.data)
    }else {stop("Method_OM must be 'U' or 'W'")} 
    
    formula_OMA = as.formula(paste("y", paste(c(covariatesOM,"z"), collapse=" + "), sep=" ~ "))
    formula_OMU = as.formula(paste("y", "z", sep=" ~ "))
    
    
    #ATT - Estimation
    fit_model = function(formula, design, var){
      fit     = svyglm(formula, design)
      index   = names(coef(fit)) == var
      return(as.numeric(c(coef(fit)[index],sqrt(diag(vcov(fit))[index]))))
    }
    
    att_adj = list()
    att_una = list()
    
    #Storing Results
    att_lm = fit_model(formula_OMA, design_OM, var="z")
    
    att_adj[[1]] = att_lm[1]
    att_adj[[2]] = att_lm[2]
    att_adj[[3]] = ifelse(att_lm[1]-1.96*att_lm[2] < patt & att_lm[1]+1.96*att_lm[2] > patt, 1, 0)
    
    att_lm = fit_model(formula_OMU, design_OM, var="z")
    
    att_una[[1]] = att_lm[1]
    att_una[[2]] = att_lm[2]
    att_una[[3]] = ifelse(att_lm[1]-1.96*att_lm[2] < patt & att_lm[1]+1.96*att_lm[2] > patt, 1, 0)
    
    att.a = unlist(att_adj)
    att.u = unlist(att_una)
    
    names_att.a = c(paste("ATT|A", name,  sep = "|"), paste("SE|A", name,  sep= "|"), paste("Cov|A", name,  sep= "|") )
    names_att.u = c(paste("ATT|U", name,  sep = "|"), paste("SE|U", name,  sep= "|"), paste("Cov|U", name,  sep= "|") )
    
    names(att.a) = names_att.a
    names(att.u) = names_att.u
    
    results = c(smd,att.a,att.u)
    return(results)
    
  }
  
  
  
  covariatesOM = c("x1","x2","x3","x4","x5","x6")
  covariatesPS = c("x1","x2","x3","x4","x5","x6")
  covariatesR  = c("x1","x2","x3","x4","x5","x6")
  sigma      = rep(1,length(covariatesOM))
  
  
  
  a = c(
    MwCSD(sample, "s.wt", "z", "y", "No", "U", "U","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "No", "U", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "No", "U", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "No", "W", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "No", "W", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "No", "C", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "No", "C", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ), 
    
    MwCSD(sample, "s.wt", "z", "y", "MAR", "U", "U","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MAR", "U", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MAR", "U", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MAR", "W", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MAR", "W", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MAR", "C", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MAR", "C", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    
    MwCSD(sample, "s.wt", "z", "y", "MARX", "U", "U","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MARX", "U", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MARX", "U", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MARX", "W", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MARX", "W", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MARX", "C", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MARX", "C", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    
    MwCSD(sample, "s.wt", "z", "y", "MART", "U", "U","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MART", "U", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MART", "U", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MART", "W", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MART", "W", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MART", "C", "W","WT", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ),
    MwCSD(sample, "s.wt", "z", "y", "MART", "C", "W","OW", "nearest", 1, covariatesPS, covariatesOM, covariatesR, sigma, att, ratiosp ))
  
  results = c(a, Scenario, Multiplier, ratiosp)
  names(results)[(length(results)-2):length(results)] = c("Scenario", "Multiplier", "RatioSP")
  save(results, file=paste(paste("ATT", Scenario, Multiplier, f, sep="_"),".Rdata",sep=""))
  
}
}             
