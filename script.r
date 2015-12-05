library(rjson);
library(rvest);
library(MASS);
library(lubridate);
library(parallel);

source("nytimes_scraper.r");
source("preprocess.r");
source("classifier.r");


sections <- list("Arts", "Sports", "World", "Business", "Obituaries");
#articles.list <- loadArticles(sections);



