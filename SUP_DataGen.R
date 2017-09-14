#Data Genarating Process

set.seed(1357)
library(data.table)
#setwd("/users/dlenis/Liz/PSM2")


############################################################################################################################
# PARAMETERS
############################################################################################################################
N           = 1000000             # Population Size
n           = N*0.005             # Sample Size
n_covar     = 7                   # Number of Covariates
n_strata    = 10                  # Number of Strata    
n_cluster   = 20                  # Number of Clusters by Strata
n_scenarios = 3                   # Number of scenarios considered
tau_strata  = 0.35                # SD associated with the mean strata of the covariates across strata
tau_cluster = c(0.25, 0.15, 0.05) # SD associated with the mean strata of the covariates across clusters  
s.m         = 6

############################################################################################################################
# MEANS
############################################################################################################################

data_sets = list()

for(scenario in 1:n_scenarios) {
  
  # Means in Matrix
  mu_strata  = matrix(rnorm(n_strata*n_covar , mean = 0 , sd = tau_strata)           , nrow = n_strata , ncol = n_covar)
  mu_cluster = matrix(rnorm(n_cluster*n_covar, mean = 0 , sd = tau_cluster[scenario]), nrow = n_cluster, ncol = n_covar)
  
  
  #Identifying the STRATA EFFECT by stratum and covaraites
  #Rows
  strata_row = rep(NA,n_strata)
  for (strata in 1:n_strata){
    strata_row[strata] = paste("Strata", strata, sep = "_")
  }
  rownames(mu_strata)=strata_row
  
  #Columns
  strata_col = rep(NA,n_covar)
  for (covariate in 1:n_covar){
    strata_col[covariate] = paste("Covariate", covariate, sep = "_")
  }
  colnames(mu_strata)=strata_col
  
  
  #Identifying the CLUSTER EFFECT by cluster and covaraites
  #Rows
  cluster_row = rep(NA,n_cluster)
  for (cluster in 1:n_cluster){
    cluster_row[cluster] = paste("Cluster", cluster, sep = "_")
  }
  rownames(mu_cluster)=cluster_row
  
  #Columns
  cluster_col = rep(NA,n_covar)
  for (covariate in 1:n_covar){
    cluster_col[covariate] = paste("Covariate", covariate, sep = "_")
  }
  colnames(mu_cluster)=cluster_col
  
  ############################################################################################################################
  # CREATING THE COVARIATES
  ############################################################################################################################
  
  cluster_l  = list()
  strata_l   = list()
  variable_l = list()
  
  for (variable in 1:n_covar){
    for (strata in 1:n_strata){
      for(cluster in 1:n_cluster){
        
        data    = cbind(rep((strata-1)*n_cluster+cluster,N/(n_strata*n_cluster)),
                        rep(strata                     , N/(n_strata*n_cluster)),
                        rnorm(N/(n_strata*n_cluster), mean = mu_cluster[cluster,variable]+mu_strata[strata,variable]))
        
        
        colnames(data) = c("Cluster","Strata",paste("X",variable,sep = ""))
        
        cluster_l[[cluster]] = data
      }
      
      strata_l[[strata]] = do.call("rbind",cluster_l) 
    }
    
    variable_l[[variable]]=data.frame(do.call("rbind",strata_l))
  }
  
  
  data = variable_l[[1]]
  for(variable in 2:n_covar){
    data = cbind(data,variable_l[[variable]][,3])
  }
  
  var_col = rep(NA,n_covar)
  for (covariate in 1:n_covar){
    var_col[covariate] = paste("x", covariate, sep = "")
  }
  
  colnames(data)=c("Cluster","Strata",var_col)
  
  ############################################################################################################################
  # PROPENSITY SCORE MODEL
  ############################################################################################################################
  
  expit = function(x) {exp(x)/(1+exp(x))}
  
  # Coefficients PS Model
  
  a0 = log(0.0329/0.9671)
  a1 = log(1.10)
  a2 = log(1.25)
  a3 = log(1.50)
  a4 = log(1.75)
  a5 = log(2.00)
  a6 = log(2.50)
  
  formula = a0 + a1*data$x1 + a2*data$x2 + a3*data$x3 + a4*data$x4 + a5*data$x5 + a6*data$x6
  
  prob_treat = expit(formula)
  
  data$z = rbinom(N,1,prob_treat)
  
  
  ############################################################################################################################
  # NON-RESPONSE MODELS
  ############################################################################################################################
  
  #NO MISING DATA (R1)
  data$r1   = 1
  data$w.r1 = 1 
  
  #MISSING AT RANDOM (NO Z - R2)
  d0 = -log(0.030)
  d1 = -log(1.10)
  d2 = -log(1.25)
  d3 = -log(1.50)
  d4 = -log(1.75)
  d5 = -log(2.00)
  d6 = -log(2.50)
  
  prob_r2   = expit(d0 + d1*data$x1 + d2*data$x2 + d3*data$x3 + d4*data$x4 + d5*data$x5 + d6*data$x6)
  data$r2   = rbinom(N,1,prob_r2)
  data$w.r2 = 1/prob_r2
  
  #MISSING AT RANDOM (NO Z - X7 R3) - MARX
  d7 = -log(2.50)
  
  prob_r3   = expit(d0 + d1*data$x1 + d2*data$x2 + d3*data$x3 + d4*data$x4 + d5*data$x5 + d6*data$x6 + d7*data$x7)
  data$r3   = rbinom(N,1,prob_r3)
  data$w.r3 = 1/prob_r3
  
  #MISSING AT RANDOM (WITH Z - R4)
  delta = -2 
  
  prob_r4   = expit(d0 + d1*data$x1 + d2*data$x2 + d3*data$x3 + d4*data$x4 + d5*data$x5 + d6*data$x6 + delta*data$z)
  data$r4   = rbinom(N,1,prob_r4)
  data$w.r4 = 1/prob_r4
  
  
  ############################################################################################################################
  # OUTCOME MODEL BINARY (D)
  ############################################################################################################################
  
  #D(0)
  
  c0 =  log(0.05/0.95) 
  c1 =  log(2.50)
  c2 = -log(2.00)
  c3 =  log(1.75)
  c4 = -log(1.25)  
  c5 =  log(1.50)
  c6 =  log(1.10)
  
  prob_d0 = expit(c0 + c1*data$x1 + c2*data$x2 + c3*data$x3 + c4*data$x4 + c5*data$x5 + c6*data$x6)
  data$d0 = rbinom(N,1,prob_d0)
  
  
  #D(1)
  gamma1 = -log(2.00)
  gamma2 = 0.2
  
  prob_d1 = expit(c0 + c1*data$x1 + c2*data$x2 + c3*data$x3 + c4*data$x4 + c5*data$x5 + c6*data$x6 +
                    gamma1 + gamma2*c1*data$x1 + gamma2*c2*data$x2 + gamma2*c3*data$x3)
  
  data$d1 = rbinom(N,1,prob_d1)
  
  #Observed D
  
  data$d = data$d1*data$z + data$d0*(1-data$z)
  
  ############################################################################################################################
  # OUTCOME MODEL CONTINUOUS (Y)
  ############################################################################################################################
  
  # Y(0)
  b0 =  0.00
  b1 =  2.50
  b2 = -2.00
  b3 =  1.75
  b4 = -1.25
  b5 =  1.50
  b6 =  1.10
  
  data$y0 = b0 + b1*data$x1 + b2*data$x2 + b3*data$x3 + b4*data$x4 + b5*data$x5 + b6*data$x6 + rnorm(N)
  
  #Y(1)
  delta1 = 1
  delta2 = 0.2
  
  #Creating Strata Dummy Variables
  for (i in 1:n_strata){
    name = paste("S",i, sep = "")
    data[,name]=0
    data[,name][data$Strata==i]=1
    
  }
  
  
  for (sm in 1:s.m){
    if (scenario %in% c(1)){
      if (sm==1) { 
        svector = c(rep(0.00, 5), rep(0.00, 5))
        data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  -
          svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
          svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
        data$y  = data$z*data$y1 + (1-data$z)*data$y0
        data$Scenario   = scenario
        data$Multiplier = sm
        data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==2) {svector = c(rep( 0.25, 5), rep(1.50, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }    
      
      if (sm==3) {svector = c(rep(-0.25, 5), rep(3.50, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==4) {svector = c(rep(-1.00, 5), rep(3.50, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==5) {svector = c(rep(-1.50, 5), rep(3.00, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==6) {svector = c(rep(-2.00, 5), rep(3.50, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }}
    
    if (scenario %in% c(2)){
      if (sm==1) { 
        svector = c(rep(0.00, 5), rep(0.00, 5))
        data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  -
          svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
          svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
        data$y  = data$z*data$y1 + (1-data$z)*data$y0
        data$Scenario   = scenario
        data$Multiplier = sm
        data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==2) {svector = c(rep( 0.25, 5), rep(1.50, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }    
      
      if (sm==3) {svector = c(rep(-0.25, 5), rep(3.00, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==4) {svector = c(rep(-1.00, 5), rep(3.00, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==5) {svector = c(rep(-1.50, 5), rep(3.00, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==6) {svector = c(rep(-2.00, 5), rep(3.00, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }}
    
    if (scenario %in% c(3)){
      if (sm==1) { 
        svector = c(rep(0.00, 5), rep(0.00, 5))
        data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  -
          svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
          svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
        data$y  = data$z*data$y1 + (1-data$z)*data$y0
        data$Scenario   = scenario
        data$Multiplier = sm
        data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==2) {svector = c(rep( 0.75, 5), rep(1.50, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }    
      
      if (sm==3) {svector = c(rep( 0.25, 5), rep(3.00, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==4) {svector = c(rep(-1.50, 5), rep(3.50, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==5) {svector = c(rep(-2.50, 5), rep(3.50, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }
      
      if (sm==6) {svector = c(rep(-3.50, 5), rep(3.50, 5))
      data$y1 = data$y0            + delta1             + delta2*b1*data$x1  + delta2*b2*data$x2   + delta2*b3*data$x3  +
        svector[2]*data$S2 + svector[3]*data$S3 + svector[4]*data$S4 + svector[5]*data$S5  + svector[6]*data$S6 + 
        svector[7]*data$S7 + svector[8]*data$S8 + svector[9]*data$S9 + svector[10]*data$S10
      data$y  = data$z*data$y1 + (1-data$z)*data$y0
      data$Scenario   = scenario
      data$Multiplier = sm
      data_sets[[scenario+n_scenarios*(sm-1)]] = data
      }}
    
  }  
}




rm(list=setdiff(ls(), c("data_sets")))  
#save.image("allo.RData")

data1 = list()

for (i in 1:18){
  data_set   = data_sets[[i]]
  data_set$ones = 1
  
  scenario=i
  
  smd.1      = diff(tapply(data_set$x1, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x1)
  smd.2      = diff(tapply(data_set$x2, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x2)
  smd.3      = diff(tapply(data_set$x3, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x3)
  smd.4      = diff(tapply(data_set$x4, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x4)
  smd.5      = diff(tapply(data_set$x5, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x5)
  smd.6      = diff(tapply(data_set$x6, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x6)
  smd.s.wt1  = diff(tapply(data_set$w.r1, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$w.r1)
  smd.s.wt2  = diff(tapply(data_set$w.r2, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$w.r2)
  smd.s.wt3  = diff(tapply(data_set$w.r3, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$w.r3)
  smd.s.wt4  = diff(tapply(data_set$w.r4, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$w.r4)
  data1[[i]] = cbind(scenario,smd.1,smd.2,smd.3,smd.4,smd.5,smd.6,smd.s.wt1,smd.s.wt2,smd.s.wt3,smd.s.wt4)
}

data1 = data.frame(matrix(unlist(data1), nrow=length(data1), byrow=T))
colnames(data1) = c("Scenario","SMD1","SMD2","SMD3","SMD4","SMD5","SMD6")
data1
