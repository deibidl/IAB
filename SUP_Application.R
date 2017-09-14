
#In this code I am trying to replicate Reardon's analysis
library(foreign)
library(MatchIt)
library(survey)
library(data.table)
library(Hmisc)
library(xtable)
setwd("C:/Users/david/Google Drive/DataM")

########################################
# WE TRY TO REPLICATE KELLER's RESULTS #
########################################

#data = read.table("28023-0001-Data.tsv", sep="\t",header=T)   #STUDENTS' AND SCHOOLS' CHARACTERISTI Kinder
load("C:/Users/david/Google Drive/DataM/data.1.RData")



VarsToKeep = c(
  #WEIGHTS 
  "C1_6FC0",
  
  #ID SCHOOL AND CHILD
  "CHILDID",
  "S1_ID"  , 
  "S2_ID"  ,
  
  #TREATMENT
  "P2SPECND",
  
  #DEMOGRAPHIC 
  "GENDER" , #Male 
  "WKWHITE", #White
  "WKSESL" ,#Socioeconomic status
  
  #ACADEMIC
  
  "C1R4RSCL", #Kindergarten reading score
  "C1R4MSCL", #Kindergarten math score
  "S2KPUPRI", #Public school
  "P1EXPECT", #Parental expectations Integers
  "P1FIRKDG", #First-time kindergartener
  "P1AGEENT", #Child's age at K entry (months)
  "T1LEARN" , #Approaches to learning rating 
  "P1HSEVER", #Attended head start
  "FKCHGSCH", #Ever changed schools
  
  
  #SCHOOL COMPOSITION 
  "S2KMINOR", #Percent minority students
  
  #FAMILY CONTEXT 
  "P1FSTAMP"  , #Received food stamps
  "P1HFAMIL"  , #Family type
  "P1HPARNT"  , #Classification of the focal child's parents 
  "P1NUMSIB"  , #Number of siblings 
  "P1HMAFB"   , #Mother's age at first birth Years
  "WKCAREPK"  , #Nonparental pre-K child care 
  
  
  #HEALTH 
  "P1EARLY" , #Number of days premature
  "P1WEIGHO", #Birth weight (ounces) 
  "C1FMOTOR", #Fine motor skills Integers
  "C1GMOTOR", #Gross motor skills Integers
  
  #PARENT RATING OF CHILD 
  "P1HSCALE", #Overall health 
  "P1SADLON", #Sad/lonely 
  "P1IMPULS", #Impulsive 
  "P1ATTENI", #Attentive 
  "P1SOLVE" , #Problem solving
  "P1PRONOU", #Verbal communication
  "P1DISABL", #Child has disability
  
  #OUTCOME VARIABLE 
  "C6R4MSCL") #Fifth-grade math score


data.t        = subset(data.1, select=VarsToKeep)
data.t$sample = complete.cases(data.t)
data.t        = subset(data.t, sample==T)
data.t        = subset(data.t,data.t$C1_6FC0>0)
data.t        = subset(data.t, data.t[,5] >=0)
data.t        = subset(data.t, data.t[,7] >=0)
data.t        = subset(data.t, data.t[,9] >=0)
data.t        = subset(data.t, data.t[,10]>=0)
data.t        = subset(data.t, data.t[,13]>=0)
data.t        = subset(data.t, data.t[,14]>=0)
data.t        = subset(data.t, data.t[,15]>=0)
data.t        = subset(data.t, data.t[,16]>=0)
data.t        = subset(data.t, data.t[,18]>=0)
data.t        = subset(data.t, data.t[,19]>=0)
data.t        = subset(data.t, data.t[,23]>=0)
data.t        = subset(data.t, data.t[,24]>=0)
data.t$P1EARLY[data.t$P1EARLY==-1] = 0 
data.t        = subset(data.t, data.t[,25]>=0)
data.t        = subset(data.t, data.t[,26]>=0)
data.t        = subset(data.t, data.t[,27]>=0)
data.t        = subset(data.t, data.t[,28]>=0)
data.t        = subset(data.t, data.t[,28]>=0)
data.t        = subset(data.t, data.t[,30]>=0)
data.t        = subset(data.t, data.t[,31]>=0)
data.t        = subset(data.t, data.t[,32]>=0)
data.t        = subset(data.t, data.t[,33]>=0)
data.t        = subset(data.t, data.t[,34]>=0)
data.t        = subset(data.t, data.t[,36]>=0)


#Recoding some variables
#1) Treatment
data.t$TREAT  = ifelse(data.t$P2SPECND == 1,1,0)

#2) Demographics
data.t$FEMALE = ifelse(data.t$GENDER   == 2,1,0)
data.t$WHITE  = ifelse(data.t$WKWHITE  == 1,1,0)


#3) Public School?
data.t$S2KPUPRI = ifelse(data.t$S2KPUPRI == 1,1,0)

#4) Parent expectations
data.t$P1ELHS = ifelse(data.t$P1EXPECT == 1,1,0) #Less than High School
data.t$P1EHS  = ifelse(data.t$P1EXPECT == 2,1,0) #High School
data.t$P1ESC  = ifelse(data.t$P1EXPECT == 3,1,0) #Some College
data.t$P1EC   = ifelse(data.t$P1EXPECT == 4,1,0) #College
data.t$P1EMS  = ifelse(data.t$P1EXPECT == 5,1,0) #Masters
data.t$P1EPHD = ifelse(data.t$P1EXPECT == 6,1,0) #PhD

#5) First time KG?
data.t$P1FIRKDG = ifelse(data.t$P1FIRKDG == 1,1,0)

#6) Head-Start ever?
data.t$P1HSEVER = ifelse(data.t$P1HSEVER == 1,1,0)

#7) Food stamps in the last 12 months?
data.t$P1FSTAMP = ifelse(data.t$P1FSTAMP == 1,1,0)

#8) Family type
data.t$SGLPAR = ifelse(data.t$P1HFAMIL==3|data.t$P1HFAMIL==4,1,0)
data.t$TWOPAR = ifelse(data.t$P1HFAMIL==1|data.t$P1HFAMIL==2,1,0)

#9) Types of parents
data.t$A1BIOPAR = ifelse(data.t$P1HPARNT<=5,1,0)
data.t$A1ADPPAR = ifelse(data.t$P1HPARNT==6|data.t$P1HPARNT==7,1,0)
data.t$RELGARD  = ifelse(data.t$P1HPARNT==8,1,0)
data.t$URELGARD = ifelse(data.t$P1HPARNT==9,1,0)

#10) Non-parental care
data.t$WKCAREPK = ifelse(data.t$WKCAREPK==1,1,0)

#11) Child is disable?
data.t$P1DISABL = ifelse(data.t$P1DISABL==1,1,0)

#12) Average scores acrross schools
data.t$AVG4RSCL = ave(data.t$C1R4RSCL, data.t$S2_ID)
data.t$AVG4MSCL = ave(data.t$C1R4MSCL, data.t$S2_ID)
data.t$AVGWKSES = ave(data.t$WKSESL  , data.t$S2_ID)

#12) Vector of ones
data.t$ones = 1
data.t$w    = data.t$C1_6FC0


fit_model = function(formula, design, var){
  fit     = svyglm(formula, design)
  index   = names(coef(fit)) == var
  return(as.numeric(c(coef(fit)[index],sqrt(diag(vcov(fit))[index]))))
}

samp = data.t

formula_AO = C6R4MSCL ~ TREAT    + FEMALE   + WHITE    + WKSESL   + C1R4RSCL + C1R4MSCL + S2KPUPRI + P1ELHS   + P1EHS    +
                        P1ESC    + P1EC     + P1EMS    + P1EPHD   + P1FIRKDG + P1AGEENT + T1LEARN  + P1HSEVER + 
                        FKCHGSCH + S2KMINOR + P1FSTAMP + SGLPAR   + TWOPAR   + P1NUMSIB + P1HMAFB  + WKCAREPK + P1EARLY  + P1WEIGHO + C1FMOTOR + C1GMOTOR + 
                        P1HSCALE + P1SADLON + P1IMPULS + P1ATTENI + P1SOLVE  + P1PRONOU + P1DISABL + AVG4RSCL +
                        AVG4MSCL + AVGWKSES 

formula_UO = C6R4MSCL ~ TREAT 

formula_PS =  TREAT    ~ FEMALE   + WHITE    + WKSESL   + C1R4RSCL + C1R4MSCL + S2KPUPRI + P1ELHS   + P1EHS    +
                         P1ESC    + P1EC     + P1EMS    + P1EPHD   + P1FIRKDG + P1AGEENT + T1LEARN  + P1HSEVER + 
                         FKCHGSCH + S2KMINOR + P1FSTAMP + SGLPAR   + TWOPAR   + P1NUMSIB + P1HMAFB  + WKCAREPK + P1EARLY  + P1WEIGHO + C1FMOTOR + C1GMOTOR + 
                         P1HSCALE + P1SADLON + P1IMPULS + P1ATTENI + P1SOLVE  + P1PRONOU + P1DISABL + AVG4RSCL +
                         AVG4MSCL + AVGWKSES 


formula_PSC =  TREAT    ~ FEMALE   + WHITE    + WKSESL   + C1R4RSCL + C1R4MSCL + S2KPUPRI + P1ELHS   + P1EHS    +
                          P1ESC    + P1EC     + P1EMS    + P1EPHD   + P1FIRKDG + P1AGEENT + T1LEARN  + P1HSEVER + 
                          FKCHGSCH + S2KMINOR + P1FSTAMP + SGLPAR   + TWOPAR   + P1NUMSIB + P1HMAFB  + WKCAREPK + P1EARLY  + P1WEIGHO + C1FMOTOR + C1GMOTOR + 
                          P1HSCALE + P1SADLON + P1IMPULS + P1ATTENI + P1SOLVE  + P1PRONOU + P1DISABL + AVG4RSCL +
                          AVG4MSCL + AVGWKSES + C1_6FC0 


#############################################################################################################################
## 1. Naive
#############################################################################################################################
#MATCHING
m.1 = matchit(formula = formula_PS, data = data.t, method = "nearest", ratio = 1, distance="logit" )

m.1_data    = match.data(m.1) 
m.1_data$id = rownames(m.1_data)

design.1  = svydesign(ids=~1, data=m.1_data)

#CORRECTLY SPECIFED OUTCOME MODEL
lm.1c     = fit_model(formula_UO, design.1, var = "TREAT")
c.1c      = lm.1c[1]
se.1c     = lm.1c[2]
ci.1cl    = c.1c-1.96*se.1c
ci.1cu    = c.1c+1.96*se.1c


##############################################################################################################################
## 2.UPS|OW| 
##############################################################################################################################

#MATCHING
m.2 = matchit(formula = formula_PS, data = data.t, method = "nearest", ratio = 1, distance="logit" )


m.2_data    = match.data(m.2) 
m.2_data$id = rownames(m.2_data)

design.2  = svydesign(ids=~1, weights=~C1_6FC0, data=m.2_data)

#CORRECTLY SPECIFED OUTCOME MODEL
lm.2c     = fit_model(formula_UO, design.2, var = "TREAT")
c.2c      = lm.2c[1]
se.2c     = lm.2c[2]
ci.2cl    = c.2c-1.96*se.2c
ci.2cu    = c.2c+1.96*se.2c

##############################################################################################################################
## 3.UPS|WT| 
##############################################################################################################################

#MATCHING
m.3 = matchit(formula = formula_PS, data = data.t, method = "nearest", ratio = 1, distance="logit" )

m.3_data    = match.data(m.3) 
m.3_data$id = rownames(m.3_data)

matched    = data.frame(m.3$match.matrix)
matched$id = rownames(matched)

m.3_data                       = merge(m.3_data,matched,by="id", all.x=TRUE)
m.3_data$X1[m.3_data$TREAT==0] = m.3_data$id[m.3_data$TREAT==0] 

dt       = data.table(m.3_data)
dt       = dt[order(dt$X1,dt$TREAT),]
dt[,C1_6FC02:=shift(C1_6FC0,1, type="lead"),by=X1]
m.3_data = data.frame(dt)
m.3_data$C1_6FC0[m.3_data$TREAT==0]=m.3_data$C1_6FC02[m.3_data$TREAT==0]

design.3  = svydesign(ids=~1, weights=~C1_6FC0, data=m.3_data)

#CORRECTLY SPECIFED OUTCOME MODEL
lm.3c     = fit_model(formula_UO, design.3, var = "TREAT")
c.3c      = lm.3c[1]
se.3c     = lm.3c[2]
ci.3cl    = c.3c-1.96*se.3c
ci.3cu    = c.3c+1.96*se.3c


##############################################################################################################################
## 4.CPS|OW  
##############################################################################################################################

#MATCHING
m.4 = matchit(formula = formula_PSC, data = data.t, method = "nearest", ratio = 1, distance="logit" )

m.4_data    = match.data(m.4) 
m.4_data$id = rownames(m.4_data)

design.4 = svydesign(ids=~1, weights=~C1_6FC0, data=m.4_data)

#CORRECTLY SPECIFED OUTCOME MODEL
lm.4c     = fit_model(formula_UO, design.4, var = "TREAT")
c.4c      = lm.4c[1]
se.4c     = lm.4c[2]
ci.4cl    = c.4c-1.96*se.4c
ci.4cu    = c.4c+1.96*se.4c


##############################################################################################################################
## 5.CPS|WT
##############################################################################################################################

#MATCHING
m.5 = matchit(formula = formula_PSC, data = data.t, method = "nearest", ratio = 1, distance="logit" )

m.5_data    = match.data(m.5) 
m.5_data$id = rownames(m.5_data)

matched    = data.frame(m.5$match.matrix)
matched$id = rownames(matched)

m.5_data                        = merge(m.5_data,matched,by="id", all.x=TRUE)
m.5_data$X1[m.5_data$TREAT==0] = m.5_data$id[m.5_data$TREAT==0] 

dt       = data.table(m.5_data)
dt       = dt[order(dt$X1,dt$TREAT),]
dt[,C1_6FC02:=shift(C1_6FC0,1, type="lead"),by=X1]
m.5_data = data.frame(dt)
m.5_data$C1_6FC0[m.5_data$TREAT==0]=m.5_data$C1_6FC02[m.5_data$TREAT==0]


design.5  = svydesign(ids=~1, weights=~C1_6FC0, data=m.5_data)

#CORRECTLY SPECIFED OUTCOME MODEL
lm.5c     = fit_model(formula_UO, design.5, var = "TREAT")
c.5c      = lm.5c[1]
se.5c     = lm.5c[2]
ci.5cl    = c.5c-1.96*se.5c
ci.5cu    = c.5c+1.96*se.5c

##############################################################################################################################
## 6.WPS|OW 
##############################################################################################################################

design.w = svydesign(ids=~1, weights=~C1_6FC0, data=samp)

m.tw = svyglm(formula_PS, design.w, family = "quasibinomial")

ps.w     = m.tw$fitted.values


#MATCHING
m.6 = matchit(formula = formula_PS, data = data.t, method = "nearest", ratio = 1, distance=ps.w )


m.6_data    = match.data(m.6) 
m.6_data$id = rownames(m.6_data)

design.6  = svydesign(ids=~1, weights=~C1_6FC0, data=m.6_data)

#CORRECTLY SPECIFED OUTCOME MODEL
lm.6c     = fit_model(formula_UO, design.6, var = "TREAT")
c.6c      = lm.6c[1]
se.6c     = lm.6c[2]
ci.6cl    = c.6c-1.96*se.6c
ci.6cu    = c.6c+1.96*se.6c

##############################################################################################################################
##7.WPS|WT
##############################################################################################################################

#MATCHING

m.7 = matchit(formula = formula_PS, data = data.t, method = "nearest", ratio = 1, distance=ps.w )


m.7_data    = match.data(m.7) 
m.7_data$id = rownames(m.7_data)

matched    = data.frame(m.7$match.matrix)
matched$id = rownames(matched)

m.7_data                   = merge(m.7_data,matched,by="id", all.x=TRUE)
m.7_data$X1[m.7_data$TREAT==0] = m.7_data$id[m.7_data$TREAT==0] 

dt       = data.table(m.7_data)
dt       = dt[order(dt$X1,dt$TREAT),]
dt[,C1_6FC02:=shift(C1_6FC0,1, type="lead"),by=X1]
m.7_data = data.frame(dt)
m.7_data$C1_6FC0[m.7_data$TREAT==0]=m.7_data$C1_6FC02[m.7_data$TREAT==0]

design.7  = svydesign(ids=~1, weights=~C1_6FC0, data=m.7_data)

#CORRECTLY SPECIFED OUTCOME MODEL
lm.7c   = fit_model(formula_UO, design.7, var = "TREAT")

c.7c      = lm.7c[1]
se.7c     = lm.7c[2]
ci.7cl    = c.7c-1.96*se.7c
ci.7cu    = c.7c+1.96*se.7c

######################################################################################################################################
#SMD (STANDARDIZED MEAN DIFFERENCES)
#####################################################################################################################################

SMD = function(data, variable)  {
  
  sigma_SMD =  sqrt(wtd.var(data[[variable]], data[["w"]]))
  m.diff    =  diff(tapply(data[[variable]] * data[["C1_6FC0"]], data[["TREAT"]], sum)/tapply(data[["C1_6FC0"]], data[["TREAT"]], sum))
  SMD       =  round(abs(m.diff/sigma_SMD),3)
  return(SMD)
}


data_sets = list(m.1_data,  m.2_data , m.3_data , m.4_data , m.5_data , m.6_data , m.7_data)

variablesl = c("TREAT"    , "FEMALE"   ,   "WHITE"  , "WKSESL"   , "C1R4RSCL" , "C1R4MSCL" , "S2KPUPRI" , "P1ELHS"   , "P1EHS",
               "P1ESC"    , "P1EC"     , "P1EMS"    , "P1EPHD"   , "P1FIRKDG" , "P1AGEENT" , "T1LEARN"  , "P1HSEVER" , 
               "FKCHGSCH" , "S2KMINOR" , "P1FSTAMP" , "SGLPAR"   , "TWOPAR"   , "P1NUMSIB" , "P1HMAFB"  , "WKCAREPK" , "P1EARLY"  , "P1WEIGHO" , "C1FMOTOR" , "C1GMOTOR" , 
               "P1HSCALE" , "P1SADLON" , "P1IMPULS" , "P1ATTENI" , "P1SOLVE"  , "P1PRONOU" , "P1DISABL" , "AVG4RSCL" ,
               "AVG4MSCL" , "AVGWKSES" , "C1_6FC0" )




SMD_Table = matrix(NA,length(variablesl),7)

for (i in 1:length(data_sets)) {
  for (j in 1:length(variablesl)){
    
    SMD_Table[j,i] = SMD(data_sets[[i]],variablesl[j])
  }
}

rownames(SMD_Table) = variablesl
colnames(SMD_Table) = c("Naive" , 
                        "UPS|OW", "UPS|WT" , 
                        "CPS|OW", "CPS|WT" ,  
                        "WPS|OW", "WPS|WT" )

table_SMD = xtable((SMD_Table))
table_SMD



d = c(c.1c  , c.2c  , c.3c  , c.4c  , c.5c  , c.6c  , c.7c  )
e = c(ci.1cl, ci.2cl, ci.3cl, ci.4cl, ci.5cl, ci.6cl, ci.7cl)
f = c(ci.1cu, ci.2cu, ci.3cu, ci.4cu, ci.5cu, ci.6cu, ci.7cu)

a=d
b=e
c=f

table3 = (rbind(a,b,c,d,e,f))
rownames(table3) = c("Unadjusted", "95% CI", "95% CI", "Adjusted", "95% CI", "95% CI" )
colnames(table3) = c("Naive" , 
                     "UPS|OW", "UPS|WT" , 
                     "CPS|OW", "CPS|WT" ,  
                     "WPS|OW", "WPS|WT" ) 

table3_t = xtable(t(table3))
table3_t


