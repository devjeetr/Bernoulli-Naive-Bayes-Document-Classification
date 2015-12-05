source("script.r");

load("dictionary2.rdata")
load("World.rdata")


spm.3 <- prepareSparseDocumentMatrix(World[1:1000], dictionary2);


save(spm.3, file="spm3.rdata");