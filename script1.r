

source("script.r");

load("dictionary2.rdata")
load("Arts.rdata")


spm.1 <- prepareSparseDocumentMatrix(Arts[1:1000], dictionary2);


save(spm.1, file="spm1.rdata");


# load("spm1.rdata")
# load("spm2.rdata")
# load("spm3.rdata")
# load("spm4.rdata")
# load("spm5.rdata")

# load("spmv1.rdata")
# load("spmv2.rdata")
# load("spmv3.rdata")
# load("spmv4.rdata")
# load("spmv5.rdata")
