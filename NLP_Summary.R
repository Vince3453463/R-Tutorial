#NLP Summary

### used libraries ###
library(tm) #for stopwords
library(stringr)
library(jsonlite)
library(textstem) #for lemmatize
library(tosca) #for LDA
library(data.table)
library(seededlda)
library("dplyr")
library("tidytext")
library(stringr)
library(XML)

####################################################### settings ########################################################################

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #current .R location
setwd("C:\\Users\\Helga\\Desktop\\Text as Data\\sheet 2") #manually
#setwd("C:\\Users\\xschroeder\\Downloads\\sheet x") #manually in uni pc

options(scipen=999) #avoid scientific notation

#text is a single document, could be linewise vector
#tests is a set of document, usually list or vector or list of vectors
#words and tokenized_list are seperated words into a list or vector
#df is a dataframe

#examples:
setwd(dirname(rstudioapi::getSourceEditorCondf()$path))
df = officer::read_docx("110723EUParl.docx")
df = officer::docx_summary(df)


##########################################################################################################################################
#################################################  loading data ###########################################################################
##########################################################################################################################################

## .txt
#.txt, list of 7 documents, vector of lines per listelement
texts = lapply(1:7, function(x) readLines(paste0("filename", x, ".txt"), encoding = "utf-8", warn = F)) #utf-16

# single document linewise
text = readLines(file("filename.txt"), warn=FALSE, encoding = "utf-8")

#other
text = read.delim("reviews1.txt", header = F)


## .csv
text = read.csv("filename.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")

# .csv directly to table
emotions = data.table::fread("emotion_dataset.csv") #Similar to read.table but faster and more convenient


## .xlsx
readxl::read_xlsx("filename.xlsx", skip = 0, sheet = NULL, na = "", n_max = Inf)


## .docx
text = officer::read_docx("110723EUParl.docx")
text = officer::docx_summary(text)


## .json
text = jsonlite::fromJSON("trek.json")

## .xml
texts = XML::xmlToDataFrame("trump.xml") #need to be equal length of columns

# XML to list
xml_data = xmlTreeParse("trump.xml", useInternalNodes = TRUE) #works despite error?
texts_list = xmlToList(xml_data)


######################################################### random string operations #############################################################

text = strsplit(text, split = "\n") #splits text on new line character
text = stringr::str_squish(text) # removes whitespace at the start and end, and replaces all internal whitespace with a single space.

#alternative to Regex
stringr::str_locate("aaa12xxx333", "[0-9]+")
stringr::str_locate_all("aaa12xx3333x", "[0-9]+")
stringr::str_extract_all("aaa12xx33x1", "[0-9]+")
stringr::str_extract("aaa12xx33x1", "[0-9]+")

#tokenize
token_list = strsplit(text, " ") #works vectorized

#flatten list
texts = unlist(text, recursive = TRUE)

#sentence splitting (careful for 1. FC Koeln stuff)
text = "This is a sample sentence. It contains multiple sentences! How exciting, right?"
sentences = unlist(strsplit(text, "[.:?!]\\s")) #returns vector of characters

##########################################################################################################################################
###############################################################  preprocessing ################################################################
##########################################################################################################################################

#order: lowercase, remove punctuation and numbers, tokenize, lemmatize > stemming, stopwordremoval but apply same preprocess steps to stop words

# prepocessing function (old)
preprocess = function(text) {
  
  
  text = gsub("Copyright", " ", text) #remove author information, ..., manually, right in the begginning
  text = gsub("@user", " ", text, fixed = T) #need fixed = T so that @d or & does not get treated as a regex expression
  text = gsub("&amp;", "", text, fixed = T)
  
  text = tolower(text)
  text = gsub("\\s+", " ", text) #replace all whitespace characters with single space
  text = gsub("http\\S+", " ", text) #replace links
  text = gsub("[^a-z ]", "", text) #remove! non-characters like numbers or special characters
  text = stringr::str_squish(text) #remove bad whitespaces
  
  #add more stuff often used
  return(text)
}
text = preprocess(text)

#alternative preprocess
texts = tm::removeNumbers(texts)
texts = tm::removePunctuation(texts)

### REGEX functions
txt = "aaa12xxx33"
txtDate = "Tabling deadlines:- Amendments: 11 July 2023 at 13.00- Requests for separate votes and split votes: 11 July 2023 at 19.00."

ind = grep(pattern = "\\d?\\d \\w+ \\d\\d\\d\\d", x = df$text) #returns row indices of matches
rowMatch = grep(pattern = "\\d?\\d \\w+ \\d\\d\\d\\d", x = df$text, value = T)[1] #returns [1]st actual matches (full strings)

sub(pattern = "[0-9]+", replacement = "insertThis", x = txt) #to sub first match
gsub(pattern = "[0-9]+", replacement = "insertThis", x = txt) #to sub all
gsub(pattern = "[0-9]+", replacement = "insertThis", x = "asasd11f 1sdfsdf[0-9]+324", fixed = T) #to ignore regexexpression style patterns

regmatches(txt, regexpr("[0-9]+",txt)) #returns actual regexMatch eg to extract dates
regmatches(txt, regexpr("[0-9]+",txt), invert = T) #returns everything except the first match, split by the match

regmatches(txt, gregexpr("[0-9]+",txt)) #gregexpr returns all matches unlikle regexpr

grepl(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)


#split df by rows using regex match , e.g. per chapter which starts with 1.1, .., 8.4 etc
ind = grep(pattern = "^\\d+\\.", x = df$text) 
df_list = split(df, cumsum(1:nrow(df) %in% ind))
df_list[1]
#or unlist text first
overallText = unlist(df$text)
texts_split = split(overallText, cumsum(1:length(overallText) %in% ind))



#Apply functions
text_list = lapply(texts, FUN = function(x) str_split(x, pattern = " ")) #not sure


#stopwords, important to preprocess these in the same way as the data!!
stopwords = preprocess(tm::stopwords()) #creates a character vector of preprocessed stopwords like i, me, my, ...
#additional lemmatizing
stopwords = textstem::lemmatize_words(preprocess(tm::stopwords()))
#additional stemming
stopwords = tm::stemDocument(preprocess(tm::stopwords()))
token_list = lapply(token_list, function(x) x[!(x %in% stopwords)])


#lemmatize
text = lemmatize_strings(texts[1:3]) #expects a vector of strings
token_list = lapply(token_list, function(x) textstem::lemmatize_words(x)) #Lemmatize a vector of words. ????
text_lemma = lapply(texts, FUN = function (x) textstem::lemmatize_words(x)) #lemmatize a list of list of tokens


#stemming
text = "Everything is fine, but is it really Or whatever in this country"
tm::stemDocument(text)
texts = list(text, text)
lapply(texts, FUN = function (x) tm::stemDocument(x)) #stemming on list of list of docs, or a list of vectors containing tokens

#stemming by SnowballC
text = SnowballC::wordStem(text) #text is a character vector of words



#preprocess all listelements with additional lemmatizing
for (i in seq_along(texts)) {
  texts[[i]] = preprocess(
    textstem::lemmatize_strings(
      preprocess(unlist(texts[[i]]))))
  # Preprocess again to remove numbers coming out of lemmatization
}

# dealing with time data
Sys.setlocale("LC_TIME",'us') #set locale time to us
date = grep(pattern = "\\d?\\d \\w+ \\d\\d\\d\\d", x = text$text) #define regex pattern of e.g. 3. march 2022
date = as.POSIXct(text[date[1], "text"], format = "%d %B %Y") #define POSICct pattern
# ?strptime for Posix patterns
# ?gsub for regex functions and arguments
weekdays.POSIXt(date) #Extract the weekday
months.POSIXt(date) #Extract the month
quarters.POSIXt(date) #Extract the quarter
julian.POSIXt(date, origin = as.POSIXct("1970-01-01", tz = "GMT")) #Extract the days since some origin

btc$created = as.POSIXct(btc$created)
btc$created = as.Date(btc$created, "GMT")

#on list of document vectors, read with readLines
for (i in 1:length(texts)) {
  texts[[i]] = texts[[i]][!grepl("(.+) - J.K. Rowling", texts[[i]])] #removes all lines with that specific match
  
  texts[[i]] = texts[[i]][-(1:(grep("[a-z]", tolower(texts[[i]]))[1]-1))] #removes everything until the first line that has characters
  
  texts[[i]] = gsub("\n", " ", texts[[i]]) #replaces newliens with space
  texts[[i]] = paste(texts[[i]], collapse = " ") #collapses lines into single doc
}


#deleting everything except the middle part
first = match("man", words) #match returns a vector of the positions of (first) matches of its first argument in its second
ind = which("feelings" == words) #Give the TRUE indices of a logical object
last = ind[length(ind)] #get the last
text = words[first:last]#filter from first to last

##########################################################################################################################################
#####################################################################  analysis ###############################################################
##########################################################################################################################################

### counting words
words = unlist(strsplit(texts, split = " "))
head(sort(table(words), decreasing = T), 10) #show 10 highest frequ words
#alt on list of tokenized texts, so list of list of words
sort(table(unlist(texts)), decreasing = T)[1:10]


######################################################################## LDA ############################################################

#LDA is bad for overlapping topics and when there are subcategories which
#determine the word distribution more then then topic e.g. in computer science
ldaprep = tosca::LDAprep(text = token_list, vocab = unique(unlist(token_list))) #each element in the list equals one document and containes its tokens
lda = tosca::LDAgen(ldaprep, vocab = unique(unlist(token_list)), K = 10, num.iterations = 200) #K topics, 200 training iterations
tosca::topWords(lda$topics, numWords = 15) #show column wise the top 15 words for each of the K=10 topics 

#or LDA on each chapter of the book seperately 
LDAs = list()
for (i in 1:5) {
  token_list = token_list_list[[i]] #list per chapter, each contains a list of tokens
  ldaprep = tosca::LDAprep(text = token_list, vocab = unique(unlist(token_list)))
  lda = tosca::LDAgen(ldaprep, vocab = unique(unlist(token_list)), K = 10, num.iterations = 50)
  LDAs[[i]] = lda
  tosca::topWords(lda$topics, 5)
}

#seeded LDA
dfm = quanteda::dfm(tokens_obj)
lda = seededlda::textmodel_seededlda(dfm, dictionary = quanteda::dictionary(seed_words))
seededlda::terms(lda, n = 20)


########################################################## tf-idf #####################################################

#tf-idf (w,m) : term frequency inverse document frequency
# = term frequency of word w in doc m multiplied by inverse doc frequ of word m (= log(M/ df(w)))
# the tf-idf (w,m) weights down words that appear in a lot of documents, which is necessary since stop word removal is
# usually too conservative to take care of all very frequent terms.


#use tidytext::bind_tf_idf() or tm::DocumentTermMatrix()

temp = data.frame(book = rep(titles, sapply(books, length)), text = unlist(books)) #create dataframe with document title(book) and their text
temp = tidytext::unnest_tokens(temp, word, text) #flattens column (here: temp$text) into words (new column called word here)
temp = dplyr::count(temp, book, word, sort = T) #counts temp$word, grouped by book

temp = tidytext::bind_tf_idf(temp, word, book, n) #calcs and adds tf and idf and tf-idf for table temp, on terms word, grouped by docs book with n frequs 
# Split the data frame into a list of data frames based on the 'book' column
temp_list = split(temp, f = temp$book) #split data frame into list by column book

for(i in 1:length(temp_list)) {
  sorted_df = arrange(temp_list[[i]], desc(tf_idf))
  top_words = sorted_df$word[1:10]
  cat("Top words for:", names(temp_list)[i], "are:\n", top_words, "\n")
}

#alt dtm tf idf
library("tm")
corpus = Corpus(VectorSource(news$text)) #A vector source interprets each element of the vector x as a document.
dtm = DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf)) #didnt work though
inspect(dtm[1:10,])
as.matrix(dtm)

############################################## Sentiment analysis #####################################################

#just iterate over each word in each comment and try to find it in the dictionary
btc$sentiment = 0
for(i in 1:length(btc$text)) { #takes a while to run
  comment = unlist(strsplit(btc[i, "text"], split = " ")) #str_split, maybe add str_squish here
  comment = comment[!is.na(comment) & comment != ""] #remove NA and empty string elements
  for(j in 1:length(comment)) {
    word = unlist(comment[j]) #need word as string, not list
    wordSentiment = dic[dic$term == word, "sentiment"] #find it in dictionary
    
    if(length(wordSentiment) == 1) {
      btc[i, "sentiment"] = btc[i, "sentiment"] + wordSentiment
      # cat(word, "has sentiment of", wordSentiment, "\n")
    }
  }
}



####################################################################  RF prediction model ############################################################

# Get predictors and target variable
data = iris
target = "Species"
predictors = data[, -which(names(data) == "Species")]  
target = data$Species 

# Split the data into training and testing sets
train_indices = sample(1:nrow(data), 0.7 * nrow(data))  #use 70% for training here
train_data = data[train_indices, ]
test_data = data[-train_indices, ]

#  Train rf
library(randomForest)
rf_model = randomForest::randomForest(Species ~ ., data = train_data)

#  Evaluate the model
predictions = predict(rf_model, newdata = test_data)
predictions_onTrain = predict(rf_model)

# Assess model performance (e.g., accuracy)
accuracy = mean(predictions == test_data$Species)
accuracy_onTrain = mean(predictions_onTrain == train_data$Species)

cat("Accuracy:", accuracy, "\n")
cat("Accuracy on the training data:", accuracy_onTrain, "\n")

library(caret)

# Compute confusion matrix
conf_matrix = caret::confusionMatrix(predictions, test_data$Species)

#alt confusion matrix with manual levels
conf = caret::confusionMatrix(data=factor(emotions$pred, levels = c("anger", "joy", "optimism", "sadness")), 
                              reference = factor(emotions$label, levels = c("anger", "joy", "optimism", "sadness")))
# alt alt 
codes = c("cs" = 1, "eess" = 2, "math" = 3, "stat" = 4) #switch true level names to the prediction output
table(true = codes[test$terms], predicted) #confusion matrx

# Calculate precision, recall, and F1 score
precision = conf_matrix$byClass[, "Precision"]
recall = conf_matrix$byClass[, "Recall"]
f1_score = conf_matrix$byClass[, "F1"]

#macro averages here since we have 3 classes
cat("Precision:", mean(precision), "\n")
cat("Recall:", mean(recall), "\n")
cat("F1 Score:", mean(f1_score), "\n")


########################################################  clustering model #######################################################

clus = kmeans(dtm_matrix, centers = 10, nstart = 1)
#compare with truth
cluster_labels <- clus$cluster
comparison_df <- data.frame(cluster = cluster_labels, label = news$category)

# Evaluate the agreement between clusters and labels
table(comparison_df)




################################################################ word2vec ###################################################

library("word2vec")
# the window size parameter determines the context window around each target word 
# A small size leads to more local context, focussing on the direct neighbours of the target word
# This way, the model captures more  syntactic and morphological information and it leads to better NER for example.
# a larger size finds better semantic information and relationships between words, since it looks at a more global context of the target.
# A smaller windows size is also computationally faster since the models neads to take fewer words into account for each target word.

vec2 = word2vec(unlist(trek), window = 2, dim = 300, stopwords = textstem::lemmatize_strings(preprocess(tm::stopwords())),
                type = "cbow") #cbow is default, or skip-gram
vec10 = word2vec(unlist(trek), window = 10, dim = 300, stopwords = textstem::lemmatize_strings(preprocess(tm::stopwords())))

mat2 = as.matrix(vec2)
mat10 = as.matrix(vec10)

#compare the models by looking at the cosine differences for each character pair
View(word2vec_similarity(mat2[match(figures$Character, rownames(mat2)),], mat2[match(figures$Character, rownames(mat2)),], type = "cosine"))
View(word2vec_similarity(mat10[match(figures$Character, rownames(mat10)),], mat10[match(figures$Character, rownames(mat10)),], type = "cosine"))


#compare sims between series
series = unique(figures$Series)
tvshows2 = matrix(NA, 5, 5)
rownames(tvshows2) = series
colnames(tvshows2) = series
for (i in seq_along(series)) {
  for (j in seq_along(series)) {
    figs1 = figures$Character[figures$Series == series[i]]
    figs2 = figures$Character[figures$Series == series[j]]
    tvshows2[i, j] = mean(word2vec_similarity(mat2[match(figs1, rownames(mat2)),], mat2[match(figs2, rownames(mat2)),], type = "cosine"))
  }
}
View(tvshows2)


#doc2vec by averaging over word2vec and by other solution
library("doc2vec")
data = rbind(train, test)
data$doc_id = 1:nrow(data)
model = doc2vec::paragraph2vec(data, type = "PV-DM", window = 5, dim = 50, threads = 6)

#we can extract the doc embeddings manually, but not for unseen data
emb = as.matrix(model, which = "docs") #M times dim matrix
emb[1:10, 1:5]
#emb = predict(model, newdata = test) #crashes R
docEmbedding = doc2vec(wordemb, newdata = data) #this works?!

#alt average over word2vec

wordemb  = word2vec(data$text,  window = 5, dim = 50)
mat2 = as.matrix(wordemb)
docEmbs = matrix(0, nrow(data), ncol(mat2)) #initialise with 0 vectors for earch doument
for(i in 1:nrow(mat2)) { #iterate over all words
  for(j in 1:length(data)) { #iterate over all docs
    if(rownames(mat2)[i] %in% data[j, "text"]){
      docEmbs[j, ] = docEmbs[j, ] + mat2[i, ]
      #does not work this way, need to gather them all and then take the average
      #not that relavant anyway
    }
    
  }
}



# distances
library(stringdist)
stringdist::stringdist("cax","abcd", q = 1, method = "jaccard") #lv for levenstein, cosine,
stringdist::stringdist("abcde","edcba", method = "lv") #lv for levenstein, cosine,



########### #Interpretation

#PET is a form of unsupervised learning to predict topics
#This article is about the research topic [MASK]
#take argmax over the predictions

#Bert has very high dimention, will overfit with N = 100


#LSA
#LSA is based on the principle that words that are used in similar contexts tend to have similar meanings
# LSA is a task in information retrieval for analyzing relationships between a set of documents and the terms they contain
#LSA applies SVD to the DTM to capture latent semantic structure of the document and terms
# Similar documents and terms are represented by vectors which are close to each other in this reduces space
#This allows for document similarity, term similarity, and document clustering, e.g. by calculating the cosine similarity 
# between documents or term verctors in the reduced space

#pLSA
# Probabilistic Latent Semantic Analysis (pLSA) is a probabilistic extension of Latent Semantic Analysis (LSA) 
# that models the generation of documents in terms of latent topics. Unlike LSA, which uses singular value decomposition
# (SVD) for dimensionality reduction, pLSA is based on a probabilistic generative model.
#pLSA assumes that each document in a corpus is generated by a mixture of latent topics. 
#Each topic is characterized by a probability distribution over the vocabulary 


#Stemming reduces words to their stem/ root form by removing suffixes, which are often, but not always, noise.
#It operates rule based and without a dictionary:
# remove -ed, -ing, -ion, sses -> ss, ational -> ate
# pro: fast, can improve classification and information retrieval by reducing the  size of the vocabulary
# cons: Stemmed words may not always be meaningful or valid in the language, e.g. 
#   "connection" and "connectivity" both stem to "connect", which loses the disctinction
#   resulting words are not always real words, which makes interpretation and communication of results less clear,
#   does not take context into account
# don’t stem for topic modelling (cf., Schofield et al., 2016)

#Lemmatization reduces words to their morphologial base or dictionary form
#It produces valid words which makes interpretation more easy compared to stemming.
#It reduces the size of the vocabulary and provides a more accurate and meaningful representations of words
#am, are, is -> be
# car, cars, car’s, cars’ -> car
# better -> good
# This way, lemmization  makes some tasks e.g. text classification easier to perform, since the feature size and its sparcity 
# is reduced which can improve the performance..
#Also improves sentiment analysis, by making sure that the sentiment-bearing words are properly identified
#It needs a dictionary and is therefore computationally slower compared to stemming which is rule based and needs fewer lookups.
#Lemmatizing is good for all BOW approaches but not for contextual LLM. There, we need the raw form of the words which could carry contextual information


#stopword removal is useful to denoise data by removing common words without semantic meaning.
#These words are too common so they can not be used e.g. to differentiate meaning between documents or classify the document. 
#We also reduce our feature or sample space depending on the task which speeds up computation.