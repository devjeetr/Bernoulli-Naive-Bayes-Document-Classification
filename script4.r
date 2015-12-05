source("script.r");

load("dictionary2.rdata")
load("Business.rdata")


spm.4 <- prepareSparseDocumentMatrix(Business[1:1000], dictionary2);

save(spm.4, file="spm4.rdata");