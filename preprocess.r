
createTDIF <- function(articles.list, word.dictionary){
	tdif = lapply(X=word.dictionary, )
}

#prepares the word dictionary


getSubset <- function(articles.list, n){
	subset = lapply(X=articles.list, FUN=function(x, n){return (x[1:n])}, n);
	return (subset);
}

extractWordsFromArticleList <- function(articles.list){
	
	word.dictionary = list()

	for(i in 1:length(articles.list)){
		word.dictionary = unlist(unique(c(word.dictionary, strsplit(articles.list[[i]]$Text, "[ ]"))));
		i = i + 1;
	}
	word.dictionary = lapply(X=word.dictionary, FUN=function(x){ if( nchar(x) <= 3 || x == " " || x=="") return (NA) else return (x)});
	
	return (as.list(word.dictionary[!is.na(word.dictionary)]));
}



loadArticles <- function(sections, n=-1){
	articles = lapply(X=sections, FUN=function(x, n){loadFromFile(paste0(x, ".txt"), n)}, n);
	
	return (articles);
}

RegexPatternFromList <- function(a, b){
	return (paste(a, "|", b, sep=""));
}

#load("dictionary.rdata")
#load("articles.rdata")
#spm.1<- prepareSparseDocumentMatrix(articles.list[[1]], word.dictionary)

# prepares a sparse document matrix for the given
# list of articles from a single section
prepareSparseDocumentMatrix <- function(articles, word.dictionary){
	len = length(word.dictionary);
	sparseDocumentMatrix = matrix(0, nrow=len, ncol=length(articles));
	sparseDocumentMatrix = apply(X=sparseDocumentMatrix, FUN=function(column, word){
			return (word);
		},MARGIN=2, word.dictionary);
	#return (sparseDocumentMatrix);
	#process column by column
	startTime = proc.time();
	print(dim(sparseDocumentMatrix))
	# sparseDocumentMatrix = apply(sparseDocumentMatrix, articles, FUN=function(column, article){
	# 		column = lapply(X=column, FUN=function(word, text){
	# 						if(grepl(word, text)){
	# 							return (1);
	# 						}
	# 						return (0);
	# 					}, article);
	# 		return (unlist(column));
	# 	} ,MARGIN=2);
	
	i = 1
	while(i <= length(articles)){
		print(i);
		currentColumn = sparseDocumentMatrix[, i];
		currentColumn = mclapply(X=currentColumn, mc.cores=4, FUN=function(word, text){
							if(grepl(word, text)){
								return (1);
										}
							return (0);
						}, articles[[i]]$Text);
		print(length(currentColumn));
		sparseDocumentMatrix[, i] = unlist(currentColumn);

		if(i %% 10 == 0){
			elapsed = proc.time() - startTime;
			print(paste0(i, " documents processed in: ", elapsed[[3]], " seconds"));
		}
		i = i + 1;
	}

	return (sparseDocumentMatrix);
}


wrap <- function(p) {
	return (paste0("(", p,")"));
}
