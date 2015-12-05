source("script.r");

load("dictionary2.rdata")
load("Obituaries.rdata")


spm.5 <- prepareSparseDocumentMatrix(Obituaries[1:1000], dictionary2);

save(spm.5, file="spm5.rdata");