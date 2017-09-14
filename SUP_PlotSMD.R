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

load("allo.RData")              

for (i in 1:length(data_sets)){ 
  data_set = data_sets[[i]]
  data     = datal[[i]]
  data$att = mean(data_set$y1[data_set$z==1]) - mean(data_set$y0[data_set$z==1])
  datal[[i]] = data
} 


test = do.call("rbind", datal)

#HERE WE COMPUTE THE SMD FROM EACH OF THE GENERATED POPULATIONS

data1 = list()

for (i in 1:3) {
  for (j in 1:6){
    data_set   = data_sets[[i+3*(j-1)]]
    data_set$ones = 1
    
    scenario=i
    multiplier=j
    
    smd.1      = diff(tapply(data_set$x1, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x1)
    smd.2      = diff(tapply(data_set$x2, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x2)
    smd.3      = diff(tapply(data_set$x3, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x3)
    smd.4      = diff(tapply(data_set$x4, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x4)
    smd.5      = diff(tapply(data_set$x5, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x5)
    smd.6      = diff(tapply(data_set$x6, data_set$z, sum)/tapply(data_set$ones, data_set$z, sum))/ sd(data_set$x6)
    
    data1[[i+3*(j-1)]] = cbind(scenario,multiplier,smd.1,smd.2,smd.3,smd.4,smd.5,smd.6)
  }
}

data1 = data.frame(matrix(unlist(data1), nrow=length(data1), byrow=T))
colnames(data1) = c("Scenario","Multipler","SMD1","SMD2","SMD3","SMD4","SMD5","SMD6")

#HERE WE SUMMARIZE TEH RESULTS FROM THE SIMULATION STUDIES
df  = data.table(test)


names_smd =c("Scenario"   , "Multiplier" , "Naive|NM"   , "UPS|WT|NM"  , "UPS|OW|NM"  , "WPS|WT|NM"  , "WPS|OW|NM"  , 
             "CPS|WT|NM"  , "CPS|OW|NM"  , "Naive|MAR"  , "UPS|WT|MAR" , "UPS|OW|MAR" , "WPS|WT|MAR" , "WPS|OW|MAR" , 
             "CPS|WT|MAR" , "CPS|OW|MAR" , "Naive|MARX" , "UPS|WT|MARX", "UPS|OW|MARX", "WPS|WT|MARX", "WPS|OW|MARX", 
             "CPS|WT|MARX", "CPS|OW|MARX", "Naive|MART" , "UPS|WT|MART", "UPS|OW|MART", "WPS|WT|MART", "WPS|OW|MART", 
             "CPS|WT|MART", "CPS|OW|MART", "Metric"     , "Variable")

##############################################################################################
# SMD
##############################################################################################


#X1
colsToKeep       = c(grep("SMD.x1+", names(df), value=TRUE))
df2c             = df[,lapply(.SD,function(x) round(abs(mean(x)), digits = 3)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c$Metric    = "SMD.X1"
data2c$Variable  = "X1"
colnames(data2c) = names_smd
smd1             = data2c  

#X2
colsToKeep       = c(grep("SMD.x2+", names(df), value=TRUE))
df2c             = df[,lapply(.SD,function(x) round(abs(mean(x)), digits = 3)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c$Metric    = "SMD.X2"
data2c$Variable  = "X2"
colnames(data2c) = names_smd
smd2             = data2c

#X3
colsToKeep       = c(grep("SMD.x3+", names(df), value=TRUE))
df2c             = df[,lapply(.SD,function(x) round(abs(mean(x)), digits = 3)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c$Metric    = "SMD.X3"
data2c$Variable  = "X3"
colnames(data2c) = names_smd
smd3             = data2c

# X4
colsToKeep       = c(grep("SMD.x4+", names(df), value=TRUE))
df2c             = df[,lapply(.SD,function(x) round(abs(mean(x)), digits = 3)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c$Metric    = "SMD.X4"
data2c$Variable  = "X4"
colnames(data2c) = names_smd
smd4             = data2c

# X5
colsToKeep       = c(grep("SMD.x5+", names(df), value=TRUE))
df2c             = df[,lapply(.SD,function(x) round(abs(mean(x)), digits = 3)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c$Metric    = "SMD.X5"
data2c$Variable  = "X5"
colnames(data2c) = names_smd
smd5             = data2c

# X6
colsToKeep       = c(grep("SMD.x6+", names(df), value=TRUE))
df2c             = df[,lapply(.SD,function(x) round(abs(mean(x)), digits = 3)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c$Metric    = "SMD.X6"
data2c$Variable  = "X6"
colnames(data2c) = names_smd
smd6             = data2c

# Survey Weights
colsToKeep       = c(grep("SMD.s.wt+", names(df), value=TRUE))
df2c             = df[,lapply(.SD,function(x) round(abs(mean(x)), digits = 3)), 
                      by = list(Scenario, Multiplier), .SDcols = colsToKeep]
data2c           = data.frame(df2c)
data2c$Metric    = "SMD.S.WT"
data2c$Variable  = "Survey Weights"
colnames(data2c) = names_smd
smd7             = data2c

smdt = rbind(smd1,smd2,smd3,smd4,smd5,smd6,smd7)


##############################################################################################
# PLOTS FOR THE SMD
##############################################################################################

sc = "Scenario 3"

test2            = smdt
test2            = smdt[test2$Multiplier==1,]
test2$Multiplier = NULL
test2             = melt(test2, id.vars = c("Scenario", "Metric", "Variable"))
names(test2)[4]= "Method"

test2$Groups[grepl("NM$"  ,test2$Method)==TRUE] = "I - No Missing"
test2$Groups[grepl("MAR$" ,test2$Method)==TRUE] = "II - MAR"
test2$Groups[grepl("MARX$",test2$Method)==TRUE] = "III - MARX"
test2$Groups[grepl("MART$",test2$Method)==TRUE] = "IV - MART"


test2$M<-substr(test2$Method,1,6)
test2$M[grepl("Naive" , test2$Method)==TRUE] = "Naive"

test2$Markers[grepl("^CPS"  ,test2$Method)==TRUE] = "CPS"
test2$Markers[grepl("^WPS"  ,test2$Method)==TRUE] = "WPS"
test2$Markers[grepl("^UPS"  ,test2$Method)==TRUE] = "UPS"
test2$Markers[grepl("^Naive",test2$Method)==TRUE] = "Naive"

test2$Colors[grepl("WT"     ,test2$Method)==TRUE] = "WT"
test2$Colors[grepl("OW"     ,test2$Method)==TRUE] = "OW"
test2$Colors[grepl("Naive"  ,test2$Method)==TRUE] = "Naive"


test2$Scenario[test2$Scenario==1] = "Scenario 1"
test2$Scenario[test2$Scenario==2] = "Scenario 2"
test2$Scenario[test2$Scenario==3] = "Scenario 3"



values_col = c("WPS|OW" = "#A1A1A1", "WPS|WT" = "#000000", "UPS|OW" = "#A1A1A1",
               "UPS|WT" = "#000000", "CPS|OW" = "#A1A1A1", "CPS|WT" = "#000000",  
               "Naive"  = "#000000")

values_shp = c("WPS|OW" = 16, "WPS|WT" = 16, "UPS|OW" = 17,
               "UPS|WT" = 17, "CPS|OW" = 15 , "CPS|WT" = 15,  
               "Naive"  = 8)



breaks = c("UPS|OW", "UPS|WT", "WPS|OW", "WPS|WT", "CPS|OW", "CPS|WT", "Naive")  

scenario1 = test2
scenario1 = subset(test2, test2$Scenario==sc) 


p = ggplot(scenario1, aes(x = M, y = value, group=Markers))
 
p + geom_point(aes(shape=Markers, color=Colors), size=2.5)                  +
    facet_grid(Groups~Variable)                                   +
    scale_shape_manual(values=c(15, 8, 17, 16))                   +  
    scale_color_manual(values=c('#000000',"#A1A1A1", '#000000'))  +
    xlab("")                                                      +
    ylab("")                                                      +
    theme(axis.title.x=element_blank(),
          axis.text.x =element_blank() ,
          axis.ticks.x=element_blank())                           +
    theme(legend.position="bottom")                               + 
    theme(panel.background = element_rect(fill = "#F5F5F5"))    +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())                     +
    labs(fill="")                                                 +
    geom_hline(yintercept=0.20, color="black")                    +
    scale_x_discrete(limits = breaks)

