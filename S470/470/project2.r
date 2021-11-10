library(magrittr)
library(tidyverse)
library(ggbiplot)
library(ggrepel)
library(plyr)
library(dplyr)

#1
vote_1989 = read.csv("1989votes.csv")
member_1989 = read.csv("1989members.csv")
vote_2014 = read.csv("2014votes.csv")
member_2014 = read.csv("2014members.csv")

joined_1989 = join(member_1989, vote_1989, by = "id")
joined_2014 = join(member_2014, vote_2014, by = "id")

vote_1989 = joined_1989[,c(-1, -2, -3, -4, -5, -6)]
member_1989 = joined_1989[,1:6]

vote_2014 = joined_2014[,c(-1, -2, -3, -4, -5, -6)]
member_2014 = joined_2014[,1:6]

recode = function(x) {
  if(is.na(x)) {
    return(0)
  } else if(x == "Yea") {
    return(1)
  } else if(x == "Nay") {
    return(-1)
  } else {
    return(0)
  }
}
votes_1989_recoded = apply(vote_1989, 1:2, recode)
votes_2014_recoded = apply(vote_2014, 1:2, recode)

votes_1989_prcomp = prcomp(votes_1989_recoded, scale. = FALSE)
votes_2014_prcomp = prcomp(votes_2014_recoded, scale. = FALSE)

ggbiplot(votes_1989_prcomp, scale = 0, var.axes = FALSE, group = member_1989$party)  +
  scale_color_manual(values = c("D" = "darkblue", "R" = "firebrick"))
ggbiplot(votes_2014_prcomp, scale = 0, var.axes = FALSE, group = member_2014$party)  +
  scale_color_manual(values = c("D" = "darkblue", "R" = "firebrick", "I"="purple"))

#2
variance_pca=function(x){
  votes=read.csv(paste0("/Users/liuyi/Desktop/stat-s670/project 2/congress/",x,"/votes.csv"))
  members=read.csv(paste0("/Users/liuyi/Desktop/stat-s670/project 2/congress/",x,"/members.csv"))
  joined = join(members, votes, by = "id")
  votes = joined[,c(-1, -2, -3, -4, -5, -6)]
  members = joined[,1:6]
  votes_recoded = apply(votes, 1:2, recode)
  pca = prcomp(votes_recoded, scale. = FALSE)
  variance_pcs=pca$sdev^2/sum(pca$sdev^2)
  return(variance_pcs[1])
}

year=seq(1989,2014,by=1)
var=sapply(year,variance_pca)
variance_1pc=mutate(data.frame(year), variance=var)
ggplot(variance_1pc, aes(x=year, y=variance))+geom_point()+
  stat_smooth(method = "lm", se= FALSE)

#3
# the common senators between 1989 and 1990
member_1990=read.csv("/Users/liuyi/Desktop/stat-s670/project 2/congress/1990/members.csv")
common_senator_0=inner_join(member_1989, member_1990)

# find the common senators from 1989 to 2014
for (x in year){
  members=read.csv(paste0("/Users/liuyi/Desktop/stat-s670/project 2/congress/",x,"/members.csv"))
  common_senator_0=inner_join(common_senator_0, members)
}

# relative position betw S174 and R party
distance_R=function(x){
  votes=read.csv(paste0("/Users/liuyi/Desktop/stat-s670/project 2/congress/",x,"/votes.csv"))
  members=read.csv(paste0("/Users/liuyi/Desktop/stat-s670/project 2/congress/",x,"/members.csv"))
  joined = join(members, votes, by = "id")
  votes = joined[,c(-1, -2, -3, -4, -5, -6)]
  members = joined[,1:6]
  votes_recoded = apply(votes, 1:2, recode)
  pca = prcomp(votes_recoded, scale. = FALSE)
  members=mutate(members, scores=pca$x[,1])
  members=mutate(members, relative_position=abs(scores-members[which(members$id=="S174"), 7]))
  members_R=subset(members, party=="R")
  avg_R=mean(members_R$relative_position)
  return(avg_R)
  
}

# relative position betw S174 and D party
distance_D=function(x){
  votes=read.csv(paste0("/Users/liuyi/Desktop/stat-s670/project 2/congress/",x,"/votes.csv"))
  members=read.csv(paste0("/Users/liuyi/Desktop/stat-s670/project 2/congress/",x,"/members.csv"))
  joined = join(members, votes, by = "id")
  votes = joined[,c(-1, -2, -3, -4, -5, -6)]
  members = joined[,1:6]
  votes_recoded = apply(votes, 1:2, recode)
  pca = prcomp(votes_recoded, scale. = FALSE)
  members=mutate(members, scores=pca$x[,1])
  members=mutate(members, relative_position=abs(scores-members[which(members$id=="S174"), 7]))
  members_D=subset(members, party=="D")
  avg_D=mean(members_D$relative_position)
  return(avg_D)
  
}

year=seq(1989,2014,by=1)
Relp_R=sapply(year,distance_R)
Relpos=mutate(data.frame(year), relative_position_R=Relp_R)
Relp_D=sapply(year,distance_D)
Relpos=mutate(Relpos, relative_position_D=Relp_D)
ggplot(Relpos,aes(x=year, y=relative_position_R))+geom_point()+ylim(c(0,10))+
  stat_smooth(method = "lm", se= FALSE)
ggplot(Relpos,aes(x=year, y=relative_position_D))+geom_point()+ylim(c(10,40))+
  stat_smooth(method = "lm", se= FALSE)

# relative position on pc1
# average score of the parties


