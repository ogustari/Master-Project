#Ramdomization/selection for analysis
#Author: Olivier Gustarini
#Master project

install.packages("openxlsx")
install.packages("readxl")
library(readxl)
library(openxlsx)
#Table contain a column "ID" that is formed by 'initial 
#letter of the plant'_'number of the plant', 
#ex for tomato, replicate 2 "P_2"

Big_Exp <- read_excel("~/Documents/UNIL/Master/Master_project/Bix_Exp/Table/Big_Exp.xlsx", 
                      na = "NA")

#Cleaning data frame
Big_Exp$Potting <- NULL 
Big_Exp$Fertilized <- NULL 
Big_Exp$Inoculated <- NULL 

#Randomization, this function allow to select/randomize the table for the analysis.
#Here a is the initial letter of the plant ("T", "P" or "L" for tomato, plantago or leek), 
#need to put the letter between quote. The second value, b is the name which the new table will be saved,
#the extansion has to be specified and the name has to be put between quote. 
Choice_Random <- function (a, b, Random = F) { 
  Split_ID <- do.call(rbind, strsplit(Big_Exp$ID, "_"))
  Big_Exp_m <- Big_Exp
  Big_Exp_m$Plant_type <- Split_ID[,1] 
  Big_Exp_m$Number <- Split_ID[,2]
  Big_Exp_m <- Big_Exp_m[complete.cases(Big_Exp_m$Survived),] #use to delete NA
  Plant <- Big_Exp_m[Big_Exp_m$Plant_type == a & Big_Exp_m$Survived == "YES",]
  Plant$Survived <- NULL
  Plant$Plant_type <- NULL
  Plant$Number <- NULL
  if (a == "P"){ #for plantago is useless to measure the height of the shoor 
    Plant$Shoot_Height <- NULL
  }
  Treatement <- unique(sort(Plant$Isolates))
  Table_Rdm <- NULL
  
    
  if (Random == F) { #this part is used only to be able to select one plant
    for(i in as.character(Treatement)) {
    is <- Treatement[i]
    Rdm <- Plant[Plant$Isolates == i, ]
    Table_Rdm <- rbind(Table_Rdm, data.frame(Rdm))
    }
  } else { #allow the randomization between the analysis (whole root vs section of roots)
    for (i in as.character(Treatement)){
      is <- Treatement[i]
      Rdm <- Plant[Plant$Isolates == i, ]
      n <- nrow(Rdm)
      Rdms <- data.frame(Rdm)
      Type_of_analysis <- c(rep(c("Whole","Section"), time = c(4,(n-4))))
      Rdms$Type_of_analysis <- sample(Type_of_analysis)
      Table_Rdm <- rbind(Table_Rdm, Rdms)
    }
  }
  
  Split_format <- unlist(strsplit(b, "[.]")) #two different format possible to save the data
  Format <- Split_format[2]
  
  if (Format == "txt"){
    write.table(Table_Rdm, file = b, col.names = T)  
  } else {
    write.xlsx(Table_Rdm, file = b)
  }
  return(head(Table_Rdm)) #to show if the table was generate correctly
}

Choice_Random("P", "~/Documents/UNIL/Master/Master_project/Bix_Exp/Table/Plantago_Table.xlsx", Random = T)
