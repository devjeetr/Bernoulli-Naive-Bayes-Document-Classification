#load("Arts_sparse.rdata");
#load("Business_sparse.rdata");
#load("Sports_sparse.rdata");
#load("World_sparse.rdata");
#spd.list <- list(Arts_sparse, Business_sparse, Sports_sparse, World_sparse);
#8 minutes/4 sections
trainClassifier <- function(spd.list, alpha, beta){
	start.time = proc.time();
	
	#calculate theta and theta_c for base class
	n_w = dim(spd.list[[1]])[1];
	n_c = dim(spd.list[[1]])[2];
	
	n = Reduce('+', unlist(mclapply(X=spd.list, mc.cores=4,FUN=function(x){return (dim(x)[2])})));
	
	theta_base =  matrix((alpha-1)/(alpha+beta-2), ncol=1, nrow=n_w);
	
	#print(dim(theta_base))
	
	theta_base_c = n_c/n;

	weights = mclapply(X=spd.list, mc.cores=4,FUN=train, n, alpha, beta, theta_base, theta_base_c);

	
	print(paste0("Time elapsed: ", (proc.time() - start.time)[[3]]));

	return (weights);
}

prepareTestSet <- function(articles.list){

	articles.list = lapply(X=articles.list, FUN=function(x){
			return (x[sample(seq(1001, 2000), 1000)]);
		})
	return (articles.list);
}

test <- function(articles, weights, dictionary, section){
	time = proc.time();
	results = mclapply(X=articles, FUN=function(article, weights, dict, section){
			result = predict(article$Text, weights, dict);
			if(result == section){
				return (1);
			}
			return (0);
		}, weights, dictionary, section);

	n_corect = Reduce('+', results);
	n_total = length(articles);
	print(paste0("Time taken: ", (proc.time()-time)[[3]]));
	print(paste0("Accuracy is ", (n_corect/n_total*100), "%"));

	return (results);
}

test.sparse <- function(articles.sparse.list, weights, dictionary, sections){
	n_c = length(articles.sparse.list)
	confusion.table = matrix(0, n_c, n_c);
	time = proc.time();
	for(i in 1:n_c){
		confusion.table = test.sparse.single(articles.sparse.list[[i]], weights, dictionary, i, confusion.table);
		print(i);
	}

	rownames(confusion.table) = sections;
	colnames(confusion.table) = sections;
	print(paste0("Time taken: ", (proc.time()-time)[[3]]));
	return (confusion.table);
}
sumConfTable <- function(table){
	sum = 0;
	for(i in 1:dim(table)[1]){
		sum = sum + table[i, i];
	}
	return (sum);
}

testAlphaBeta <- function(articles.sparse.list, spm.list, dictionary, sections){
	
	

	#vary a
	a = list(2, 3, 4, 6, 8, 10);
	b = list(2, 3, 4, 6, 8, 10);

	#b = 3, vary a
	weights.a <- mclapply(X=a, mc.cores=4, FUN=function(x, b, articles, dictionary, sections){
			w = trainClassifier(spm.list, x, b);
			res = test.sparse(articles, w, dictionary, sections);
			return (sumConfTable(res));
		}, 3, articles.sparse.list, dictionary, sections);

	#a = 2, vary b
	weights.b <- mclapply(X=a, mc.cores=4, FUN=function(x, a, articles, dictionary, sections){
			w = trainClassifier(spm.list, a, x)
			res = test.sparse(articles, w, dictionary, sections);
			return (sumConfTable(res));
		}, 2,articles.sparse.list, dictionary, sections);



	return (list(weights.a, weights.b));
	
}

test.sparse.single <- function(articles, weights, dictionary, section, table){

	for(i in 1:length(articles)){
		result = predict.sparse(articles[[i]], weights, dictionary);
		table[section, result] = table[section, result] + 1;
	}
	return (table);
}



predict.sparse <- function(text , weights.list, word.dictionary){

	result = mclapply(FUN=predictSingle.sparse, mc.cores=4, X=weights.list, word.dictionary, text);
	elements = order(unlist(result), decreasing=T)[1];
	#print(result);
	return (elements[[1]]);
}

predictSingle.sparse <- function(weights, word.dictionary, x){
	
	result =  sum(weights$Weight * x) + unique(weights$Weight_c);
	#result = unlist(result);
	return (result);
}



train <- function(sparse.document.matrix, documents.total, alpha, beta, theta_base, theta_base_c){
	n_w = dim(sparse.document.matrix)[1];
	n_c = dim(sparse.document.matrix)[2];
	#print(n_c);
	
	theta_jc = apply(X=sparse.document.matrix, FUN=function(slice, n_c, a, b){
			#print(slice);
			slice = as.numeric(slice);
			n_jc = Reduce('+', slice);
			
			return ((n_jc + a - 1)/(n_c + a + b - 2));
			}, n_c, alpha, beta, MARGIN=1);

	theta_c = n_c/documents.total;
	
	weights = data.frame(Weight = theta_jc, Weight_c=theta_c);
	
	return (weights);
}

prepDictionary <- function(word.frequencyTable){
	
	word.frequencyTable= word.frequencyTable[which(word.frequencyTable <= 3000)];
	word.frequencyTable= word.frequencyTable[which(word.frequencyTable >= 3)];
	dictionary = row.names(word.frequencyTable);

	return (dictionary[nchar(dictionary) > 3]);
}


getTextFromArticleList <- function(articles.list){
	text = mclapply(X=articles.list,mc.cores=4, FUN=function(articles){
					articles = mclapply(X=articles[1:1000], mc.cores=4,FUN=function(article){
								return (article$Text);
							});

					x = Reduce(paste, articles);
					return (x);
				});
	text = Reduce(paste0, text);

	return (text);
}



calculateWordFrequency <- function(complete.text){
	complete.text = gsub("[0-9]", "", complete.text);
	word.list = strsplit(complete.text, "\\W");
	word.frequency = table(word.list);
	return (word.frequency);
}


printMostImportant <- function(sections, weights.list, word.dictionary){
	mostImp = list();

	for(i in 1:length(sections)){
		elements = order(weights.list[[i]]$Weight, decreasing=T)[1:20];
		
		print(sections[i]);
		mostImp[[i]] = as.matrix(unlist(word.dictionary[elements]), nrow=5, ncol=5);
	}
	return (mostImp);
}


#sparse <- prepareSparseDocumentMatrix(loadFromFile("Business.txt"), dictionary.combined)
#sparse <- prepareSparseDocumentMatrix(loadFromFile("Arts.txt"), dictionary.combined)
#sparse <- prepareSparseDocumentMatrix(loadFromFile("World.txt"), dictionary.combined)
#sparse <- prepareSparseDocumentMatrix(loadFromFile("Sports.txt"), dictionary.combined)

prepareTestData <- function(articles, dictionary){

	sparse.list = mclapply(X=articles,mc.cores=4, FUN=function(article, dict){
		return (getFeatureMatrix(article$Text, dict));

	}, dictionary);

	return (sparse.list);
}

getFeatureMatrix <- function(text, dictionary){
	#text.list = strsplit(text, "\\W");
	start = proc.time();
	
	x = mclapply(X=dictionary,mc.cores=4, FUN=function(word, text){
			pattern = paste0("\\b",word,"\\b");
			if(grepl(pattern, text)){
				return (1);
			}
			return (0);
		}, text);

	elapsed = proc.time() - start;
	print(paste0("Time taken: ", elapsed[3]));

	return (unlist(x));
}



predict <- function(text , weights.list, word.dictionary){

	result = lapply(FUN=predictSingle, X=weights.list, word.dictionary, text);
	elements = order(unlist(result), decreasing=T)[1];
	#print(result);
	return (elements[[1]]);
}


predictSingle <- function(weights, word.dictionary, article){
	#build feature matrix
	#
	x = lapply(X=word.dictionary, FUN=function(y, article){
			pattern = paste0("\\b",y,"\\b");
			if(grepl(pattern, article)){
				return (1);
			}
			return (0);
		}, article );

	x = unlist(x);
	#print(paste0("x", dim(x)));
	#print(paste0("weight", length(weights$Weight)));

	result =  sum(weights$Weight * x) + unique(weights$Weight_c);
	#result = unlist(result);
	return (result);
}