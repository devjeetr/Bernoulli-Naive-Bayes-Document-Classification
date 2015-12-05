source("script.r")
load("articles.rdata")
load("dict3.rdata")

testSet <- prepareTestSet(articles.list)
test1 <- prepareTestData(testSet[[1]], dict2)

test1 <- test
testSet1 <- testSet[[1]]
save(testSet1, file="testSet1.rdata");
save(test1, file="test1.rdata");

test2 <- prepareTestData(testSet[[2]], dict3)
testSet2 <- testSet[[2]]
save(testSet2, file="testSet2.rdata");
save(test2, file="test1.rdata");

test3 <- prepareTestData(testSet[[3]], dict3)

testSet3 <- testSet[[3]]
save(testSet3, file="testSet3.rdata");
save(test3, file="test3.rdata");


test4 <- prepareTestData(testSet[[4]], dict3)

testSet4 <- testSet[[4]]
save(testSet4, file="testSet4.rdata");
save(test4, file="test4.rdata");

test5 <- prepareTestData(testSet[[5]], dict3)
testSet5 <- testSet[[5]]
save(testSet5, file="testSet5.rdata");
save(test5, file="test5.rdata");

load("test1.rdata");
load("test2.rdata");
load("test3.rdata");
load("test4.rdata");
load("test5.rdata");