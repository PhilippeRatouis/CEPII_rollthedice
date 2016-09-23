library(plotrix)
library(pdfetch)
library(networkD3)
library(knitr)
library(Rgraphviz)

GROWTH <- sfc.model("GROWTH.txt",modelName="GROWTH_MODEL")
dataGROWTH <- simulate(GROWTH)
plot(GROWTH$time,dataGROWTH$baseline[,"Y"],type="l",xlab="",ylab="",lty=2)
kable(round(t(dataGROWTH$baseline[c(1,2,3,4,5),c("phi","phi_T")]), digits=4))
                                                                  