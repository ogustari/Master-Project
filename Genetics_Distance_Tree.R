#Genetics_Distance_Tree
#Author: Olivier Gustarini
#Master Project
rm(list=ls())
setwd()

install.packages("ape")
install.packages("RColorBrewer")

library(ape)
library(plyr)


#Import data set
Distance <- read.table("Global_list.txt", col.names = c("Isolate1", "Isolate2", "Distance"))

#Delete isolates replicates
for(i in c(1,2)){
  Split_Isolate <- do.call(rbind, strsplit(as.character(Distance[,i]), "-"))
  Distance[,i] <- Split_Isolate[,1]
}

#Define isolate to look for 
Find_Isolate <- unique(Distance$Isolate1)

Isolate1_selected <- NULL
Distance_selected <- NULL

#Find the isolate
for(i in Find_Isolate){
  Table_iso1 <- Distance[Distance$Isolate1 == i,]
  Isolate1_selected <- rbind(Isolate1_selected, data.frame(Table_iso1))
  Table_iso2 <- Isolate1_selected[Isolate1_selected$Isolate2 == i,]
  Distance_selected <- rbind(Distance_selected, data.frame(Table_iso2))
}

#Make distance mean between the same isolate
Distance_selected$Iso1_Iso2 <- paste(Distance_selected$Isolate1, Distance_selected$Isolate2, sep = "-")
Distance_selected_mean <- aggregate (Distance_selected[,3], list(Distance_selected$Iso1_Iso2), mean) 
colnames(Distance_selected_mean) <- c("Iso1_Iso2", "Mean")
Separate <- do.call(rbind,strsplit(Distance_selected_mean$Iso1_Iso2 , "-"))
Distance_no_separate <- Distance_selected_mean
Distance_selected_mean$Iso1_Iso2 <- NULL 
Distance_selected_mean$Isolate1 <- Separate[,1]
Distance_selected_mean$Isolate2 <- Separate[,2]

#Make square matrix with diagonal = 0
Matrix_Distance <- matrix(0, nrow = length(Find_Isolate), length(Find_Isolate))
dimnames(Matrix_Distance) <- list(Find_Isolate, Find_Isolate)

for (i in 1:length(Find_Isolate)){
  Matrix_Distance[i,i:length(Find_Isolate)] <- Distance_selected_mean[Distance_selected_mean$Isolate1 == Find_Isolate[i],]$Mean
  Matrix_Distance[i:length(Find_Isolate),i] <- Distance_selected_mean[Distance_selected_mean$Isolate1 == Find_Isolate[i],]$Mean
  diag(Matrix_Distance) <- 0
}

#Make unrooted tree
arbol <- nj(as.dist(Matrix_Distance))
plot(arbol, type = "unrooted", edge.width = 1, font = 4, lab4ut = "axial")

#Make barplot with distance mean
Distance_no_separate <- Distance_no_separate[order(Distance_no_separate$Mean),]
barplot(Distance_no_separate$Mean, names.arg = Distance_no_separate$Iso1_Iso2, ylim=c(0, max(Distance_no_separate$Mean)+0.05),cex.names = 0.5, space = 0.3)

