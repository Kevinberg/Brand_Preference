install.packages("dplyr")
library("dplyr")


random1 <- c("Alex","Sergi","Kevin","Fabi","Laura","Alejandro","Alessandro","David","Oriol","Jasper")


Groupclass <- c(1,1,2,2,3,3,4,4,5,5)

test <- sample(random1)
split(test, Groupclass)
