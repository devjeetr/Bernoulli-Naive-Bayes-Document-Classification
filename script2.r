
source("script.r");

load("dictionary2.rdata")
load("Sports.rdata")


spm.2 <- prepareSparseDocumentMatrix(Sports[1:1000], dictionary2);


save(spm.2, file="spm2.rdata");