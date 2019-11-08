##big comparison
library(readstata13)
read.dta13("/mnt/phs/group/bdomingu/data_clean.dta")->x


## 13421 and 12950 have data from eyfsp pre 2013 (edcont_academicyear = ‘2011/2012’)
## 23557 and 25539 have data from eyfsp post 2013 (edcont_academicyear = ‘2015/2016’)

c(13421,12950,23557,25539)->ids
x[x$childid %in% ids,]->z
z$ks1
z$eyfsp


