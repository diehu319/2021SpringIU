library(magrittr)
library(tidyverse)
library(ggbiplot)
library(ggrepel)
library(plyr)
library(dplyr)
library(GGally)
library(MASS)
library(broom)


# data cleaning and generate a new dataset
# original_appl = read.csv("application_data.csv")
# original_pre_appl = read.csv("previous_application.csv")
# 
# original_appl=original_appl[, c(1,2,4,5,6,7,8,9,10,11,17,18,19)]
# original_pre_appl=original_pre_appl[, c(2,4,5,17)]
# colnames(original_pre_appl)[2]="PRE_AMT_ANNUITY"
# joined=join(original_appl, original_pre_appl, by="SK_ID_CURR")
# defaulter=filter(joined, joined$TARGET=="1")
# nondefaulter=filter(joined,joined$TARGET=="0")
# 
# defaulter=defaulter[1:100,]
# nondefaulter=nondefaulter[1:100,]
# 
# newdataset = rbind(defaulter,nondefaulter)
# write.csv(newdataset,'newdataset.csv', row.names=FALSE)

dataset=read.csv("newdataset.csv")


# recode
#dataset[is.na(dataset)]=0
dataset$NAME_CONTRACT_STATUS=recode(dataset$NAME_CONTRACT_STATUS, 
                                    Approved=1, Refused=0, Canceled=0, "Unused offer"=0)
dataset$FLAG_OWN_CAR=recode(dataset$FLAG_OWN_CAR, 
                            N=0, Y=1)
dataset$FLAG_OWN_REALTY=recode(dataset$FLAG_OWN_REALTY, 
                               N=0, Y=1)
dataset$CODE_GENDER=recode(dataset$CODE_GENDER, 
                           F=0, M=1)
dataset[is.na(dataset)]=0
