## Author attribution

library(tm)
library(e1071)
library (plyr)
library(caret)
library(doMC)
registerDoMC(cores=detectCores()) 

# Remember to source in the "reader" wrapper function
# it's stored as a Github gist at:
# https://gist.github.com/jgscott/28d9d1287a0c3c1477e2113f6758d5ff

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

## Rolling two directories together into a single corpus
author_dirs = Sys.glob('~/Documents/GitHub/STA380/data/ReutersC50/C50train/*')
#author_dirs = Sys.glob('http://raw.githubusercontent.com/jgscott/STA380/master/data/ReutersC50/C50train/*') 
#author_dirs = author_dirs[1:50]
file_list = NULL
labels = NULL
nchar('~/Documents/GitHub/STA380/data/ReutersC50/C50train/*')
for(author in author_dirs) {
  author_name = substring(author, first=52)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}
#length(file_list)

# Apply readerPlain to file_list, name columns based on names file_list
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

my_corpus = Corpus(VectorSource(all_docs))

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)
DTM 

class(DTM)  

inspect(DTM[1:10,1:20])
DTM = removeSparseTerms(DTM, 0.975) # only want words that are in 97% of docs
DTM

# # Dense matrix
# Xtrain = as.matrix(DTM)
# row.names(Xtrain) = file_list
# # X

# # Multinomial probability vector
# smooth_count = 1/nrow(Xtrain)
# w = colSums(Xtrain + smooth_count)
# w = w/sum(w)

#########
## Now for test data:

author_dirsTest = Sys.glob('~/Documents/GitHub/STA380/data/ReutersC50/C50test/*')
#author_dirsTest = Sys.glob('http://raw.githubusercontent.com/jgscott/STA380/master/data/ReutersC50/C50test/*') 
file_listTest = NULL
labelsTest = NULL
xTest = nchar('~/Documents/GitHub/STA380/data/ReutersC50/C50test/*')
for(author in author_dirsTest) {
  author_name = substring(author, first=xTest)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_listTest = append(file_listTest, files_to_add)
  labelsTest = append(labelsTest, rep(author_name, length(files_to_add)))
}
length(file_listTest)


all_docsTest = lapply(file_listTest, readerPlain) 
names(all_docsTest) = file_listTest
names(all_docsTest) = sub('.txt', '', names(all_docsTest))

my_corpusTest = Corpus(VectorSource(all_docsTest))

# names(my_corpus) = file_list


# Preprocessing
my_corpusTest = tm_map(my_corpusTest, content_transformer(tolower)) # make everything lowercase
my_corpusTest = tm_map(my_corpusTest, content_transformer(removeNumbers)) # remove numbers
my_corpusTest = tm_map(my_corpusTest, content_transformer(removePunctuation)) # remove punctuation
my_corpusTest = tm_map(my_corpusTest, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpusTest = tm_map(my_corpusTest, content_transformer(removeWords), stopwords("SMART"))

# Deal with words that weren't in train set:
freqTrain = f## Author attribution

library(tm)
library(e1071)
library (plyr)
library(caret)
library(doMC)
registerDoMC(cores=detectCores()) 

# Remember to source in the "reader" wrapper function
# it's stored as a Github gist at:
# https://gist.github.com/jgscott/28d9d1287a0c3c1477e2113f6758d5ff

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

## Rolling two directories together into a single corpus
author_dirs = Sys.glob('~/Documents/GitHub/STA380/data/ReutersC50/C50train/*')
#author_dirs = Sys.glob('http://raw.githubusercontent.com/jgscott/STA380/master/data/ReutersC50/C50train/*') 
#author_dirs = author_dirs[1:50]
file_list = NULL
labels = NULL
nchar('~/Documents/GitHub/STA380/data/ReutersC50/C50train/*')
for(author in author_dirs) {
  author_name = substring(author, first=52)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}
#length(file_list)

# Apply readerPlain to file_list, name columns based on names file_list
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

my_corpus = Corpus(VectorSource(all_docs))

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)
DTM 

class(DTM)  

inspect(DTM[1:10,1:20])
DTM = removeSparseTerms(DTM, 0.975) # only want words that are in 97% of docs
DTM

# # Dense matrix
# Xtrain = as.matrix(DTM)
# row.names(Xtrain) = file_list
# # X

# # Multinomial probability vector
# smooth_count = 1/nrow(Xtrain)
# w = colSums(Xtrain + smooth_count)
# w = w/sum(w)

#########
## Now for test data:

author_dirsTest = Sys.glob('~/Documents/GitHub/STA380/data/ReutersC50/C50test/*')
#author_dirsTest = Sys.glob('http://raw.githubusercontent.com/jgscott/STA380/master/data/ReutersC50/C50test/*') 
file_listTest = NULL
labelsTest = NULL
xTest = nchar('~/Documents/GitHub/STA380/data/ReutersC50/C50test/*')
for(author in author_dirsTest) {
  author_name = substring(author, first=xTest)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_listTest = append(file_listTest, files_to_add)
  labelsTest = append(labelsTest, rep(author_name, length(files_to_add)))
}
length(file_listTest)


all_docsTest = lapply(file_listTest, readerPlain) 
names(all_docsTest) = file_listTest
names(all_docsTest) = sub('.txt', '', names(all_docsTest))

my_corpusTest = Corpus(VectorSource(all_docsTest))

# names(my_corpus) = file_list


# Preprocessing
my_corpusTest = tm_map(my_corpusTest, content_transformer(tolower)) # make everything lowercase
my_corpusTest = tm_map(my_corpusTest, content_transformer(removeNumbers)) # remove numbers
my_corpusTest = tm_map(my_corpusTest, content_transformer(removePunctuation)) # remove punctuation
my_corpusTest = tm_map(my_corpusTest, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpusTest = tm_map(my_corpusTest, content_transformer(removeWords), stopwords("SMART"))

# Deal with words that weren't in train set:
freqTrain = findFreqTerms(DTM, 1) # use only words that have appeared in DTM (train)
length((freqTrain)) # 1411 -> same as number terms in DTM train

# now 
DTMtest = DocumentTermMatrix(my_corpusTest, control=list(dictionary = freqTrain))
DTMtest 

class(DTMtest)

inspect(DTMtest[1:10,1:20])
# DTMtest = removeSparseTerms(DTMtest, 0.975)
# DTMtest

# # Now a dense matrix
# Xtest = as.matrix(DTMtest)
# row.names(Xtest) = file_listTest


# Boolean feature Multinomial Naive Bayes

convert_count = function(x) {
  y = ifelse(x>0, 1, 0)
  y = factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB = apply(DTM, 2, convert_count)
trainNB = as.data.frame(data.matrix(trainNB),stringsAsfactors = FALSE)
trainNB = cbind(trainNB,as.factor(labels))
colnames(trainNB)[ncol(trainNB)] = "LabelsTrain"

testNB = apply(DTMtest, 2, convert_count)
testNB = as.data.frame(data.matrix(testNB),stringsAsfactors = FALSE)
testNB = cbind(testNB,labelsTest)
colnames(testNB)[ncol(testNB)] = "LabelsTest"

# Train the classifier
classifier = naiveBayes(LabelsTrain~., data = trainNB, laplace = 1) 

# Use the NB classifier we built to make predictions on the test set.
pred = predict(classifier, testNB)

# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions" = pred,  "Actual" = testLabels)

conf.mat = confusionMatrix(pred, labelsTest)

conf.mat$byClass
conf.mat$overall
conf.mat$overall['Accuracy']




