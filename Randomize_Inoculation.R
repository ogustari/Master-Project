#Treatement randomization
#Author: Olivier Gustarini
#Master project

#Randomization the tratement bwetween plants. Here a is the number of plant inoculated for each tretement 
#and b the plant that do will not contain any fungi

Random <- function (a, b) {
  rdm <- c(rep(c("A1","B12", "C3", "C5", "Free", "A1/B12", "A1/C3", "A1/C5"), time = c(a,a,a,a,b,a,a,a)))
  treatement <- sample(rdm)
  n.treat <- 1:length(rdm)
  table <- data.frame(Treatement = treatement, Plant_id = n.treat)
  write.table(table, file = "Treatement.xlsx", col.names = T)
  return (table)
  
}