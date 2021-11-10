library(tidyverse)
library(stringr)
library(dplyr)
df <- read.csv('application_data.csv')
dat <-data.frame(df[1:1500,])
dat$row_num <- seq.int(nrow(dat)) 
defaulters <- dat$TARGET==1
nondefaulters <- dat$TARGET==0
dat$row_num

#Correction
ggplot(dat , aes(x=TARGET, y=row_num, colour = TARGET))+geom_bar(stat = "identity")+theme_minimal()+
  ggtitle('Distribution of Target Variables')+xlab('Target Variables')+ylab('Count of Target Variables')



x = colSums(is.na(dat))
sum(is.na(dat))
x <- data.frame(x)
percentage <- (colSums(is.na(dat))/sum(is.na(dat)))*100
y <- list(data.frame(percentage))

y<- ifelse(y$percentage<1.00,y$percentage,1)
y
