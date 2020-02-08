Data <- data.frame(
  X = sample (1:4),
  Y = sample(c("mon", "tues","wed","thurs", "fri","sat","sun"), 100, replace = TRUE),
  Z= sample (c("a","b","c"), 100, replace = TRUE),
  #ZZ = sample(c("yes", "no"), 100, replace = TRUE),
  ZZZ=runif(100, min=1, max=10000)
) 

library(questionr)
wtd.table(Data$X,Data$Z,Data$ZZZ)
library(Hmisc)
wtd.table(Data$X,Data$Z,Data$ZZZ)
detach("package:Hmisc", unload = TRUE)
wtd.table(Data$X,Data$Z,Data$ZZZ)
library(Hmisc)
wtd.table(x=Data$X,y=Data$Z,weights=Data$ZZZ)
