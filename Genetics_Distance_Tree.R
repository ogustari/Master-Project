#Author: Olivier Gustarini
#Master Project
rm(list=ls())
setwd("/Users/Olivier/Documents/UNIL/Master/Master_project/Big_Exp/Script/R_Code/Distance_Isolate")

install.packages("ape")
library(ape)

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
Trimmed_Matrix_Distance <- Matrix_Distance
To_Trim <- c("B1", "B3", "B4", "B10", "D3") #to make the tree more readable
for(i in To_Trim){
  Trimmed_Matrix_Distance <- Trimmed_Matrix_Distance[!rownames(Trimmed_Matrix_Distance) %in% i, ] 
  Trimmed_Matrix_Distance <- Trimmed_Matrix_Distance[,!colnames(Trimmed_Matrix_Distance) %in% i] 
}

arbol <- nj(as.dist(Trimmed_Matrix_Distance))

color.tree <- rep("black", times = 15)
color.tree[c(1,6,11,13)] <- "red"
pdf("Unrooted_distance_trhee.pdf")
plot.phylo(arbol, type = "unrooted", edge.width = 1, font = 1,lab4ut = "axial", show.tip.label = FALSE)
tiplabels(text = c("","",arbol$tip.label[3],rep("",times = 4),arbol$tip.label[8], "", arbol$tip.label[10], rep("", times =5)), cex = .6, col = color.tree, frame = "none", adj = 0)
tiplabels(text = c("","","",arbol$tip.label[4:5], rep("",times = 5),"", "", arbol$tip.label[13:15]), cex = .6, col = color.tree, frame = "none", adj = 1)
tiplabels(text = c(arbol$tip.label[1], rep("",times = 14)), cex = .6, col = color.tree, frame = "none", adj = 1, srt = 50)
tiplabels(text = c(rep("", times = 5),arbol$tip.label[6:7], rep("",times = 8)), cex = .6, col = color.tree, frame = "none", adj = 1, srt = 90)
tiplabels(text = c("",arbol$tip.label[2],rep("", times = 6),arbol$tip.label[9],"",arbol$tip.label[11:12],"","",""), cex = .6, col = color.tree, frame = "none", adj = 1, srt = -90)
dev.off()


#Make barplot with distance mean
for(i in as.character(c("B3", "B4", "B10", "D3"))){
 Trim <- as.numeric(grep(i, Distance_no_separate$Iso1_Iso2))
 Distance_no_separate <- Distance_no_separate[-Trim,]
}
Distance_no_separate <- Distance_no_separate[-as.numeric(grep("\\bB1\\b", Distance_no_separate$Iso1_Iso2)),]

Distance_no_separate <- Distance_no_separate[order(Distance_no_separate$Mean),]
barplot(Distance_no_separate$Mean, names.arg = Distance_no_separate$Iso1_Iso2, ylim=c(0, max(Distance_no_separate$Mean)+0.05),
        cex.names = 0.5, space = 0.3, xlab = "Isolates", ylab = "Distance")
