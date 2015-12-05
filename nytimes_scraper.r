# Proessess a list of a list of articles
# removes stop words and punctuation
processArticles <- function(articles.list){
	
	articles.list = mclapply(X=articles.list, mc.cores=4,FUN=function(x){
			return (processSingleArticle(x));
		});

	return (articles.list);
}
# Proessess a list of articles
# removes stop words and punctuation
processSingleArticle <- function(articles){
	stopwords = read.table("stopwords.txt", col.names=c("Word"), stringsAsFactors=FALSE);
	stopwords = unlist(mclapply(X=stopwords, mc.core=4,FUN=function(x){
		return (paste0("(\\b", x, "\\b)"))
	}));
	stopwords = Reduce(RegexPatternFromList, stopwords); 

	articles.list = mclapply(X=articles, mc.cores=4, FUN=function(article, stopwords){
					return (tryCatch({
						article$Text = gsub(stopwords, "",tolower(article$Text));
						article$Text = gsub("*\\b[[:alpha:]]{1}\\b* | *\\b[[:alpha:]]{2}\\b* | \\b[[:punct:]]\\b", "", article$Text);
						
						return (article);
						},
						error=function(e){
							print("error processing article. skipping...");
							return (NA)
						}))
				
				
		}, stopwords);

	return (articles.list);
}


#fetches urls for n articles for the given section, using the given time
#interval for queries to the nytimes api
#interval is an object of type "interval" from the lubridate library
fetchUrls <- function(n, section, interval, removeOldSave=FALSE){
	if(removeOldSave){
		file.remove(paste(section, "_urls_progress",".txt", sep=""));
		file.remove(paste(section, "_urls", ".txt", sep=""));
		save.file = paste(section, "_urls_progress",".txt", sep="");
		page_number = 0;
		offset = 1;
		N = 0;
		endDate = mdy(format(Sys.time(), "%m%d%y"));
		startDate = endDate - interval;
	}else{
		save.file = paste(section, "_urls_progress",".txt", sep="");
		progress = read.table(save.file, col.names=c("Page", "Offset","N", "Date"), stringsAsFactors=FALSE);
		page_number = unlist(progress$Page);
		offset = unlist(progress$Offset);
		N = as.numeric(unlist(progress$N))

		startDate = mdy(unlist(progress$Date));
		endDate = startDate + interval;
	}

	api_key = "6a2953191973c2ecbd66dace80be613f:0:68912737"

	#fetch only the headline and web url of the given article
	base_request = "http://api.nytimes.com/svc/search/v2/articlesearch.json?fl=headline,web_url";
	hits.total = 0;
	
	startTime = proc.time();
	print(startDate);
	
	while(page_number <= floor(n/10) && N <= n){
		#generate the date filter
		dateArg = paste("&begin_date=",year(startDate), format(startDate, "%m%d"), "&end_date=", year(endDate), format(endDate, "%m%d"), sep="");
		if(section != "Business"){
			request = paste(base_request, "&fq=section_name:(\"",section,"\") AND document_type:(\"article\")&page=",  page_number, dateArg, "&api-key=", api_key, sep="");
		
		}else{
			request = paste(base_request, "&fq=news_desk:(\"",section,"\") AND document_type:(\"article\")&page=",  page_number, dateArg, "&api-key=", api_key, sep="");
		
		}
		request = URLencode(request);
		print(request);
		response = fromJSON(file=request);

		#keep track of how many hits are eft in the current request
		hits.total = response$response$meta$hits - (page_number * 10 + offset - 1);
		
		if(offset <= 10 && N <= n && hits.total > 0)
		print(paste("Processing urls", N,"-", N+10));

		while(offset <= 10 && N <= n && hits.total > 0){
			web_url = response$response$docs[[offset]]$web_url;
			
			if(response$status == "OK"){
				new.entry = data.frame(Url = web_url, stringsAsFactors=FALSE);
				#write new entry to file
				write.table(new.entry, paste(section, "_urls", ".txt", sep=""), col.names=FALSE, row.names=FALSE, append=TRUE);
				N = N + 1;

			}
			#update hits remaining
			hits.total = hits.total - 1;
			if(hits.total == 0){
				break;
			}

			#offset keeps track of the article number on the given page
			offset = offset + 1;
			write.table(data.frame(Page=page_number, Offset=offset, N=N, Date=paste0(month(startDate), day(startDate), year(startDate))), save.file, row.names=FALSE, col.names=FALSE);
			
		}
		
		print(paste0("Urls fetched: ", N, ", Page:Offset : ",page_number, ":", offset));
		
		#if we run out of hits we need to make a new request
		if(hits.total <= 0){
			endDate = startDate;
			startDate = startDate - interval;
			write.table(data.frame(Page=page_number, Offset=offset, N=N, Date=paste0(month(startDate), day(startDate), year(startDate))), save.file, row.names=FALSE, col.names=FALSE);
			page_number = 0;
			offset = 1;
		}else{
			if(N <= n){
				#reset offset for next
				offset = 1;
				page_number = page_number + 1;	
			}else{
				write.table(data.frame(Page=page_number, Offset=offset, N=N, Date=paste0(month(startDate), day(startDate), year(startDate))), save.file, row.names=FALSE, col.names=FALSE);
			}

		}
		
		
		time.elapsed = proc.time() - startTime;
		print(paste("Time elapsed:", time.elapsed[3]));
		
	}
	#print(N);

}




getArticlesFromUrlFile <- function(section, url_file, Append=TRUE){
	urls = read.table(file=url_file, col.names=c("Url"), stringsAsFactors=FALSE);
	urls = unlist(urls[[1]])[1:1500];

	file.name = paste0(section, ".txt");
	

	if(!Append){
		if(file.exists(file.name)){
			file.remove(file.name);
		}
	}
	articles.previous = list();
	
	if(file.exists(file.name)){
		articles.previous = loadFromFile(file.name);
		urls = urls[length(articles.previous):length(urls)];	
	}

	articles = mclapply(X=urls, mc.cores=4,	FUN=prepDataFrame, section);

	return (c(articles.previous, articles));
}

prepDataFrame <- function(url, section){
	# pattern = ( word )|( word )
	file.name = paste0(section, ".txt");

	return (tryCatch(
		{
			web_page = read_html(url);

			text = web_page %>% html_nodes(".article-body")%>%html_text();
			if(length(text) == 0){
				text = web_page %>% html_nodes(".articleBody")%>%html_text();
			}

			title = web_page %>% html_nodes(".headline")%>%html_text();
			

			if(length(text) > 0){
				#remove punctuation and stop words
				new.entry = data.frame(Url=url, Title=title, Text=text, stringsAsFactors=FALSE);
				
				#add entry to file
				write.table(new.entry, file=file.name, row.names=FALSE, col.names=FALSE, append=TRUE, sep="\t");
				
				return (new.entry);
			}else{
				return (NA);
			}
		}, 
		error = function(e){
			print("Warning! url not found!");
			return (NA);
		}));

	
}	

prepArticle <- function(articles){
	stopwords = read.table("stopwords.txt", col.names=c("Word"), stringsAsFactors=FALSE);
	stopwords = unlist(lapply(X=stopwords, FUN=function(x){
		return (paste0("(\\b", x, "\\b)"))
	}));
	stopwords = Reduce(RegexPatternFromList, stopwords); 
	

	articles = lapply(X=articles, FUN=function(x, stopwords){
			
			return (tryCatch({

							x$Text = gsub(stopwords, "",tolower(x$Text));
							x$Text = gsub("*\\b[[:alpha:]]{1}\\b*|*\\b[[:alpha:]]{2}\\b*", "", x$Text);
							#x$Text = gsub("[0-9]", "", x$Text);
							return (x);
						},
						error=function(e){
							print("Warning: Erorr in Text, skipping entry");
							print(e);
							return (NA);
						}));
			
	

	}, stopwords);
	return (articles[!is.na(articles)]);
}


loadFromFile <- function(file.name, nrows = -1){
	articles = read.table(file = file.name, nrows=nrows, sep="\t", col.names = c("Url", "Title", "Text"), stringsAsFactors = FALSE, quote="", fill=T)
	articles = split(articles, seq(nrow(articles)));
	# #create regex pattern out of stop words
	# stopwords = read.table("stopwords.txt", col.names=c("Word"), stringsAsFactors=FALSE);
	# stopwords = unlist(lapply(X=stopwords, FUN=function(x){
	# 	return (paste0("(\\b", x, "\\b)"))
	# }));
	# stopwords = Reduce(RegexPatternFromList, stopwords); 
	

	articles = lapply(X=articles, FUN=function(x){
			if(nchar(x$Text) <= 10){
				return (NA);
			}
			return (x);
			
	 });

	return (articles[!is.na(articles)]);
}
#articles <- getArticlesFromUrlFile("Arts", "Arts_urls.txt")





















