setwd("C:/Users/david/Google Drive/PSM/PSM")

library(ggplot2)
library(data.table)
library(xtable)



load("combined.Rdata") #SIULATION RESULTS (test)

datal = list()
for (i in 1:length(combined)){ 
data = data.frame((combined[[i]]))
datal[[i]] = data
} 

load("allo.Rdata")              

for (i in 1:length(data_sets)){ 
  data_set = data_sets[[i]]
  data     = datal[[i]]
  data$att = mean(data_set$y1[data_set$z==1]) - mean(data_set$y0[data_set$z==1])
  datal[[i]] = data
} 

test = do.call("rbind", datal)
  
# #HERE WE COMPUTE HTE VALUES FOR THE X-AXIS (I.E. [(SATT/PATT)-1]*100)
df              = data.table(test)
colsToKeep      = grep("Ratio", names(df), value=TRUE)
df2c            = df[,lapply(.SD,function(x) round(mean(x)*100,digits = 0)),
                     by = list(Scenario, Multiplier), .SDcols = colsToKeep]
xaxis           = data.frame(df2c)
names(xaxis)[3] = "XAxis"
test_XA         = merge(test,xaxis, by=c("Scenario","Multiplier"))
test_XA$Multiplier = NULL

table(test_XA$XAxis[test_XA$Scenario==1])
table(test_XA$XAxis[test_XA$Scenario==2])
table(test_XA$XAxis[test_XA$Scenario==3])


#HERE WE COMPUTE THE SMD FROM EACH OF THE GENERATED POPULATIONS

#HERE WE SUMMARIZE TEH RESULTS FROM THE SIMULATION STUDIES
df  = data.table(test)

names_col = c("Scenario"   , "Multiplier" , "Naive|NM"   , "UPS|WT|NM"  , "UPS|OW|NM"  , "WPS|WT|NM"  , "WPS|OW|NM"  , 
              "CPS|WT|NM"  , "CPS|OW|NM"  , "Naive|MAR"  , "UPS|WT|MAR" , "UPS|OW|MAR" , "WPS|WT|MAR" , "WPS|OW|MAR" , 
              "CPS|WT|MAR" , "CPS|OW|MAR" , "Naive|MARX" , "UPS|WT|MARX", "UPS|OW|MARX", "WPS|WT|MARX", "WPS|OW|MARX", 
              "CPS|WT|MARX", "CPS|OW|MARX", "Naive|MART" , "UPS|WT|MART", "UPS|OW|MART", "WPS|WT|MART", "WPS|OW|MART", 
              "CPS|WT|MART", "CPS|OW|MART", "Status"     , "Metric")

##############################################################################################
# RMSE
##############################################################################################



#Root MSE (Adjusted Outcome Model)
colsToKeep = c(grep("ATT.A.+", names(df), value=TRUE), "att")
df2c             = df[,lapply(.SD,function(x) round(sqrt(mean((x-mean(att))^2)),digits=3)), 
                      by = list(Scenario,Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c           = data2c[,-ncol(data2c)]
data2c$status    = "Y~T+X"
data2c$metric    = "RMSE"
colnames(data2c) = names_col


#Root MSE (Unadjusted Specified Outcome Model)
colsToKeep = c(grep("ATT.U.+", names(df), value=TRUE), "att")
df2i             = df[,lapply(.SD,function(x) round(sqrt(mean((x-mean(att))^2)),digits=3)), 
                      by = list(Scenario,Multiplier), .SDcols = colsToKeep]
data2i           = data.frame(df2i)
data2i           = data2i[,-ncol(data2i)]
data2i$status    = "Y~T"
data2i$metric    = "RMSE"
colnames(data2i) = names_col

rmse  = rbind(data2c,data2i)

##############################################################################################
# Coverage
##############################################################################################

#Coverage (Adjusted Specified Outcome Model)
colsToKeep       = grep("Cov.A.+", names(df), value=TRUE)
df2c             = df[,lapply(.SD,function(x) round(mean(x-0.95),digits = 2)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c$status    = "Y~T+X"
data2c$metric    = "Coverage - 0.95"
colnames(data2c) = names_col

#Coverage (Unadjusted Outcome Model)

colsToKeep       = grep("Cov.U.+", names(df), value=TRUE)
df2i             = df[,lapply(.SD,function(x) round(mean(x-0.95),digits = 2)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2i           = data.frame(df2i)
data2i$status    = "Y~T"
data2i$metric    = "Coverage - 0.95"
colnames(data2i) = names_col

coverage  = rbind(data2c,data2i)


##############################################################################################
# Bias
##############################################################################################

#Bias (Adjusted Outcome Model)
colsToKeep       = c(grep("ATT.A.+", names(df), value=TRUE), "att")
df2c             = df[,lapply(.SD,function(x) round(abs(mean(x-mean(att))), digits = 3)), 
                      by = list(Scenario,Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c           = data2c[,-ncol(data2c)]
data2c$status    = "Y~T+X"
data2c$metric    = "|Bias|"
colnames(data2c) = names_col

#Bias (Unadjusted Outcome Model)
colsToKeep       = c(grep("ATT.U.+", names(df), value=TRUE), "att")
df2i             = df[,lapply(.SD,function(x) round(abs(mean(x-mean(att))), digits = 3)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2i           = data.frame(df2i)
data2i           = data2i[,-ncol(data2i)]
data2i$status    = "Y~T"
data2i$metric    = "|Bias|"
colnames(data2i) = names_col

bias  = rbind(data2c,data2i)

##############################################################################################
# PLOTS FOR THE ATT ESTIMATION
##############################################################################################


sc = "Scenario 3"

test2          = rbind(rmse, coverage,bias)
test2          = melt(test2, id.vars = c("Scenario","Multiplier", "Metric","Status"))
names(test2)[5]= "Method"
# 
# test2$Groups[grepl("^Naive",test2$Method)==TRUE] = "I - Naive"
# test2$Groups[grepl("^UPS"  ,test2$Method)==TRUE] = "II - Unweighted"
# test2$Groups[grepl("^WPS"  ,test2$Method)==TRUE] = "III - Weighted"
# test2$Groups[grepl("^CPS"  ,test2$Method)==TRUE] = "IV - Covariable"

test2$Groups[grepl("NM$"  ,test2$Method)==TRUE] = "I - No Missing"
test2$Groups[grepl("MAR$" ,test2$Method)==TRUE] = "II - MAR"
test2$Groups[grepl("MARX$",test2$Method)==TRUE] = "III - MARX"
test2$Groups[grepl("MART$",test2$Method)==TRUE] = "IV - MART"

test2$XA[test2$Scenario==1 & test2$Multiplier==1] =  -1
test2$XA[test2$Scenario==1 & test2$Multiplier==2] = -10
test2$XA[test2$Scenario==1 & test2$Multiplier==3] = -22
test2$XA[test2$Scenario==1 & test2$Multiplier==4] = -31
test2$XA[test2$Scenario==1 & test2$Multiplier==5] = -40
test2$XA[test2$Scenario==1 & test2$Multiplier==6] = -51

test2$XA[test2$Scenario==2 & test2$Multiplier==1] =  -1
test2$XA[test2$Scenario==2 & test2$Multiplier==2] = -10
test2$XA[test2$Scenario==2 & test2$Multiplier==3] = -20
test2$XA[test2$Scenario==2 & test2$Multiplier==4] = -29
test2$XA[test2$Scenario==2 & test2$Multiplier==5] = -38
test2$XA[test2$Scenario==2 & test2$Multiplier==6] = -50

test2$XA[test2$Scenario==3 & test2$Multiplier==1] =  -6
test2$XA[test2$Scenario==3 & test2$Multiplier==2] = -13
test2$XA[test2$Scenario==3 & test2$Multiplier==3] = -20
test2$XA[test2$Scenario==3 & test2$Multiplier==4] = -32
test2$XA[test2$Scenario==3 & test2$Multiplier==5] = -40
test2$XA[test2$Scenario==3 & test2$Multiplier==6] = -52


test2$Scenario[test2$Scenario==1] = "Scenario 1"
test2$Scenario[test2$Scenario==2] = "Scenario 2"
test2$Scenario[test2$Scenario==3] = "Scenario 3"


test2$M<-substr(test2$Method,1,6)
test2$M[grepl("Naive" , test2$Method)==TRUE] = "Naive"

test2$Markers[grepl("^CPS"  ,test2$Method)==TRUE] = "CPS"
test2$Markers[grepl("^WPS"  ,test2$Method)==TRUE] = "WPS"
test2$Markers[grepl("^UPS"  ,test2$Method)==TRUE] = "UPS"
test2$Markers[grepl("^Naive",test2$Method)==TRUE] = "Naive"

test2$Colors[grepl("WT"     ,test2$Method)==TRUE] = "WT"
test2$Colors[grepl("OW"     ,test2$Method)==TRUE] = "OW"
test2$Colors[grepl("Naive"  ,test2$Method)==TRUE] = "Naive"

values_def = c("Naive"  = "solid" ,
               "CPS|OW" = "solid", "CPS|WT" = "solid" , 
               "UPS|OW" = "solid", "UPS|WT" = "solid" ,
               "WPS|OW" = "solid", "WPS|WT" = "solid" )  

values_col = c("WPS|OW" = "#A1A1A1", "WPS|WT" = "#000000", "UPS|OW" = "#A1A1A1",
               "UPS|WT" = "#000000", "CPS|OW" = "#A1A1A1", "CPS|WT" = "#000000",  
               "Naive"  = "#000000")

values_shp = c("WPS|OW" = 16, "WPS|WT" = 16, "UPS|OW" = 17,
               "UPS|WT" = 17, "CPS|OW" = 15 , "CPS|WT" = 15,  
               "Naive"  = 8)



breaks = c("UPS|OW", "UPS|WT", "WPS|OW", "WPS|WT", "CPS|OW", "CPS|WT", "Naive")  

test2$M = factor(test2$M, levels = breaks)

scenario1 = subset(test2, test2$Scenario==sc) 


p = qplot(XA               , value        , data=scenario1, facets = Groups~Status+Metric, 
          geom =c("point")    ,  ylab = ""     , color = Colors, shape=Markers)


p + facet_grid(Groups~Status+Metric , scales="free_y" )           +
    geom_point(aes(shape=Markers, color=Colors))                  +
    theme(panel.background = element_rect(fill = "#F5F5F5"))      +              
    theme(legend.position="bottom")                               +
    scale_colour_manual(values=c('#000000',"#A1A1A1", '#000000')) +
    theme(axis.text.x = element_text (angle=90, 
                                      hjust = 0.5, vjust=0.5))    +
    scale_shape_manual(values = c(15, 8, 17, 16))                 +  
    geom_line(size=0.05)                                          +
    geom_point(size=2.00)                                          +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x     = element_blank())         





