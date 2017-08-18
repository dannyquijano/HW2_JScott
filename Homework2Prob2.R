


## Author attribution

library(tm)

# Remember to source in the "reader" wrapper function
# it's stored as a Github gist at:
# https://gist.github.com/jgscott/28d9d1287a0c3c1477e2113f6758d5ff

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

## Rolling two directories together into a single corpus
author_dirs = Sys.glob('~/Documents/GitHub/STA380/data/ReutersC50/C50train/*')
#author_dirs = Sys.glob('http://raw.githubusercontent.com/jgscott/STA380/master/data/ReutersC50/C50train/*') 
author_dirs = author_dirs[1:50]
file_list = NULL
labels = NULL
nchar('~/Documents/GitHub/STA380/data/ReutersC50/C50train/*')
for(author in author_dirs) {
  author_name = substring(author, first=52)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}
length(file_list)
# Need a more clever regex to get better names here
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
DTM = removeSparseTerms(DTM, 0.975)
DTM

# Dense matrix
Xtrain = as.matrix(DTM)
row.names(Xtrain) = file_list
# X

# Multinomial probability vector
smooth_count = 1/nrow(Xtrain)
w = colSums(Xtrain + smooth_count)
w = w/sum(w)

#########
## Now for test data:

author_dirsTest = Sys.glob('~/Documents/GitHub/STA380/data/ReutersC50/C50test/*')
#author_dirsTest = Sys.glob('http://raw.githubusercontent.com/jgscott/STA380/master/data/ReutersC50/C50test/*') 
author_dirsTest = author_dirsTest[1:50]
file_listTest = NULL
labelsTest = NULL
nchar('~/Documents/GitHub/STA380/data/ReutersC50/C50test/*')
for(author in author_dirsTest) {
  author_name = substring(author, first=51)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_listTest = append(file_listTest, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}
length(file_listTest)
# Need a more clever regex to get better names here
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

DTMtest = DocumentTermMatrix(my_corpusTest)
DTMtest # some basic summary statistics

class(DTMtest)  # a special kind of sparse matrix format

## You can inspect its entries...
inspect(DTMtest[1:10,1:20])
DTMtest = removeSparseTerms(DTMtest, 0.975)
DTMtest

# Now a dense matrix
Xtest = as.matrix(DTMtest)
row.names(Xtest) = file_listTest

# Predict on Xtest

#DTMtest -> 1437 terms
#DTM (train) -> 1411 terms

#levels(Xtest) = levels(Xtrain) # ignore new features


########


## Lecture notes
# Let's take a specific test document
x_test = X[49,]
head(sort(x_test, decreasing=TRUE), 25)

# Compare log probabilities under the Naive Bayes model
sum(x_test*log(w_AP))
sum(x_test*log(w_AC))

# Another test document
x_test2 = X[99,]
head(sort(x_test2, decreasing=TRUE), 25)
sum(x_test2*log(w_AP))
sum(x_test2*log(w_AC))
########
















