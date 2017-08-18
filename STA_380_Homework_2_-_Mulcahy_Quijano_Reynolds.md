1. Flights at ABIA
------------------

``` r
abia = read.csv('http://raw.githubusercontent.com/jgscott/STA380/master/data/ABIA.csv', header=TRUE)
abia = subset(abia, abia$Origin == "AUS")
#Getting only late flights
late_flights = subset(abia, abia$DepDelay > 0)
```

What kind of analysis can we perform that actually produces actionable results for a traveler. For example, say you have a wedding or you are attending a conference in Atlanta. The decision of where you are going is fixed and there is a small degree of flexibilty in terms of when you can fly. Therefore, finding information like average delay from Austin to Atlanta would not be relevant, because regardless as to the delay, you have to go to Atlanta.

The assumptions we will make, then, are that the traveler can not be flexible in terms of where they go, but they can be reasonably flexible in terms of when they fly.

First, let's try to get an idea about what time of day the traveler should try to fly by looking at how often flights are delayed at certain times of day:

``` r
library(lattice)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
delay_histo = histogram(late_flights$DepTime/100, main = 'Late Flights Departure Times', xlab = 'Hour', breaks = 24)

delay_histo
```

![](STA_380_Homework_2_-_Mulcahy_Quijano_Reynolds_files/figure-markdown_github/unnamed-chunk-2-1.png) From this histogram, we see a normalized histogram displaying the number of total delayed flights that depart at certain times of the day. In general, 11:00 a.m. to 8:00 p.m. are the times with the highest proportion of delayed flights. This seems reasonable as congestion on the runway or near the gate may affect other flights at busy times of the day.

Now that we know at what time of day delays are more likely, let's see how long you can expect to wait at certain times of the day (all flights, not just delayed ones).

``` r
plot(cut(abia[abia$DepDelay >0,]$CRSDepTime/100, seq(0, 24 , by = 1), labels = 1:24), abia[abia$DepDelay > 0,]$DepDelay, xlab = 'Hour', ylab = 'Delay', main = 'Expected Delay at Certain Time of Day', ylim = range(0:200))
```

![](STA_380_Homework_2_-_Mulcahy_Quijano_Reynolds_files/figure-markdown_github/unnamed-chunk-3-1.png) This plot shows us that the median flight delay is not dependent on time of day.

Now let's see how the average wait time looks for all flights compared to delayed ones.

``` r
par(mfrow=c(1,2))
#plot of average delay time by scheduled departure hour
plot(tapply(abia$DepDelay, cut(abia$CRSDepTime, seq(0, 2400, by=100)), mean, na.rm = TRUE), ylim=range(0,60), xlab = 'Hour', ylab = "Expected Delay", main = 'All Flights')
#plot of average delay time by scheduled departure hour (excluding ontime)
plot(tapply(abia[abia$DepDelay > 0,]$DepDelay, cut(abia[abia$DepDelay > 0,]$CRSDepTime, seq(0, 2400, by=100)), mean, na.rm = TRUE), ylim=range(0,60), xlab = 'Hour', ylab = "Expected Delay", main = 'Late Flights')
```

![](STA_380_Homework_2_-_Mulcahy_Quijano_Reynolds_files/figure-markdown_github/unnamed-chunk-4-1.png) Here, we can see that the average amount of time you can expect to wait increases by about 15-20 minutes if you find out your flight is delayed. for this situation, it is probably more useful to use the mean because there do not appear to be a large number of outliers that could be inflating the mean.

Our lesson would be to travel early in the day. There are fewer delayed flights and they are not delayed as much.

2. Author attribution
---------------------

``` r
library(tm)
```

    ## Loading required package: NLP

``` r
library(e1071)
library (plyr)
```

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
library(caret)
```

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:NLP':
    ## 
    ##     annotate

``` r
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }
```

Rolling two directories together into a single corpus

``` r
author_dirs = Sys.glob('~/GitHub/STA380/data/ReutersC50/C50train/*')
file_list = NULL
labels = NULL
x = nchar('C:/Users/Daniel/Documents/GitHub/STA380/data/ReutersC50/C50test/*')
for(author in author_dirs) {
  author_name = substring(author, first=x+1)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}
```

Apply readerPlain to file\_list, name columns based on names file\_list

``` r
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))
my_corpus = Corpus(VectorSource(all_docs))
```

Preprocessing & creating DTM

``` r
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)
DTM 
```

    ## <<DocumentTermMatrix (documents: 2500, terms: 32241)>>
    ## Non-/sparse entries: 473695/80128805
    ## Sparsity           : 99%
    ## Maximal term length: 74
    ## Weighting          : term frequency (tf)

``` r
class(DTM)  
```

    ## [1] "DocumentTermMatrix"    "simple_triplet_matrix"

``` r
inspect(DTM[1:10,1:20])
```

    ## <<DocumentTermMatrix (documents: 10, terms: 20)>>
    ## Non-/sparse entries: 48/152
    ## Sparsity           : 76%
    ## Maximal term length: 11
    ## Weighting          : term frequency (tf)
    ## Sample             :
    ##     Terms
    ## Docs access accounts agencies announced bogus business called character
    ##   1       1        1        1         1     2        2      1         4
    ##   10      4        0        0         0     0        1      0         4
    ##   2       0        0        0         1     0        1      0         4
    ##   3       2        0        0         0     0        0      0         4
    ##   4       0        0        0         1     0        1      0         4
    ##   5       0        0        0         1     0        1      0         4
    ##   6       0        0        0         0     0        0      0         4
    ##   7       0        0        1         0     0        0      1         4
    ##   8       0        0        0         0     0        1      0         4
    ##   9       0        0        1         0     0        1      0         4
    ##     Terms
    ## Docs charged commission
    ##   1        1          2
    ##   10       0          0
    ##   2        0          0
    ##   3        0          0
    ##   4        0          0
    ##   5        0          0
    ##   6        0          0
    ##   7        0          5
    ##   8        1          2
    ##   9        1          2

``` r
DTM = removeSparseTerms(DTM, 0.975) # only want words that are in 97% of docs
DTM
```

    ## <<DocumentTermMatrix (documents: 2500, terms: 1411)>>
    ## Non-/sparse entries: 290499/3237001
    ## Sparsity           : 92%
    ## Maximal term length: 18
    ## Weighting          : term frequency (tf)

Now for test data:

``` r
author_dirsTest = Sys.glob('~/GitHub/STA380/data/ReutersC50/C50test/*')
file_listTest = NULL
labelsTest = NULL
xTest = nchar('C:/Users/Daniel/Documents/GitHub/STA380/data/ReutersC50/C50test/*')
for(author in author_dirsTest) {
  author_name = substring(author, first=xTest)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_listTest = append(file_listTest, files_to_add)
  labelsTest = append(labelsTest, rep(author_name, length(files_to_add)))
}
length(file_listTest)
```

    ## [1] 2500

``` r
all_docsTest = lapply(file_listTest, readerPlain) 
names(all_docsTest) = file_listTest
names(all_docsTest) = sub('.txt', '', names(all_docsTest))

my_corpusTest = Corpus(VectorSource(all_docsTest))
```

Preprocessing & creating Test DTM

``` r
my_corpusTest = tm_map(my_corpusTest, content_transformer(tolower)) # make everything lowercase
my_corpusTest = tm_map(my_corpusTest, content_transformer(removeNumbers)) # remove numbers
my_corpusTest = tm_map(my_corpusTest, content_transformer(removePunctuation)) # remove punctuation
my_corpusTest = tm_map(my_corpusTest, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpusTest = tm_map(my_corpusTest, content_transformer(removeWords), stopwords("SMART"))

# Deal with words that weren't in train set:
freqTrain = findFreqTerms(DTM, 1) # use only words that have appeared in DTM (train)
length((freqTrain)) # 1411 -> same as number terms in DTM train
```

    ## [1] 1411

``` r
DTMtest = DocumentTermMatrix(my_corpusTest, control=list(dictionary = freqTrain))
DTMtest 
```

    ## <<DocumentTermMatrix (documents: 2500, terms: 1411)>>
    ## Non-/sparse entries: 290258/3237242
    ## Sparsity           : 92%
    ## Maximal term length: 18
    ## Weighting          : term frequency (tf)

``` r
class(DTMtest)
```

    ## [1] "DocumentTermMatrix"    "simple_triplet_matrix"

``` r
inspect(DTMtest[1:10,1:20])
```

    ## <<DocumentTermMatrix (documents: 10, terms: 20)>>
    ## Non-/sparse entries: 56/144
    ## Sparsity           : 72%
    ## Maximal term length: 13
    ## Weighting          : term frequency (tf)
    ## Sample             :
    ##     Terms
    ## Docs accounts alliance announced authorities bureau character computer
    ##   1         3        3         1           1      1         1        1
    ##   10        0        1         1           1      0         0        2
    ##   2         0        0         1           0      0         0        2
    ##   3         0        0         0           0      0         1        0
    ##   4         0        0         0           0      0         0        0
    ##   5         0        1         1           0      0         0        0
    ##   6         0        0         0           0      0         1        4
    ##   7         0        0         1           1      0         0        0
    ##   8         0        0         0           0      2         2        0
    ##   9         0        0         0           0      1         2        0
    ##     Terms
    ## Docs consumer cthe description
    ##   1         1    1           4
    ##   10        0    0           4
    ##   2         0    0           4
    ##   3         2    1           4
    ##   4         0    2           4
    ##   5         0    0           4
    ##   6         3    2           4
    ##   7         0    0           4
    ##   8         0    1           4
    ##   9         0    1           4

We will try Naive Bayes and Random Forests
------------------------------------------

First, Naive Bayes: Boolean feature Multinomial Naive Bayes

``` r
convert_count = function(x) {
  y = ifelse(x>0, 1, 0)
  y = factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y}
```

Apply the convert\_count function to get final training and testing DTMs

``` r
trainNB = apply(DTM, 2, convert_count)
trainNB = as.matrix(trainNB)
row.names(trainNB) = file_list
trainNB = as.data.frame(trainNB,stringsAsfactors = FALSE)
trainNB$answer = as.factor(labels)

testNB = apply(DTMtest, 2, convert_count)
testNB = as.matrix(testNB)
testNB = as.data.frame(testNB,stringsAsfactors = FALSE)
row.names(testNB) = file_listTest
testNB$answer = as.factor(labelsTest)
```

Train the classifier

``` r
classifier = naiveBayes(answer ~ ., data = trainNB, laplace = 1)  

# Use the NB classifier we built to make predictions on the test set.
#pred = predict(classifier, testNB)

# Create a truth table by tabulating the predicted class labels with the actual class labels 
# table("Predictions" = pred,  "Actual" = testLabels)
# 
# conf.mat = confusionMatrix(pred, labelsTest)
# 
# conf.mat$byClass
# conf.mat$overall
# conf.mat$overall['Accuracy']
```

Now we move on to Random Forests

``` r
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
#set up train and test datasets using Xtrain and Xtest
Xtrain = as.matrix(DTM) # Dense matrix
TRdataset <- as.data.frame(Xtrain)
TRdataset$answers <- labels #adding a column for the authors
TRdataset$answers = factor(TRdataset$answers) # Encoding the target feature as factor
Xtest = as.matrix(DTMtest) # Dense matrix
TEdataset <- as.data.frame(Xtest)
TEdataset$answers <- labelsTest #adding a column for the authors
TEdataset$answers = factor(TEdataset$answers) # Encoding the target feature as factor

rfcol = ncol(TRdataset)

#train the RF model
RF <- randomForest(x = TRdataset[-rfcol], y = TRdataset$answers, ntree = 10)
#use the RF model to predict on the test set
y_pred <- predict(RF, newdata = TEdataset[-rfcol])



#create a confusion matrix
conf <- RF$confusion
#show our errors errors for each author
RF$confusion[, 'class.error']
```

    ##     AaronPressman        AlanCrosby    AlexanderSmith   BenjaminKangLim 
    ##         0.3600000         0.2600000         0.6122449         0.6800000 
    ##     BernardHickey       BradDorfman  DarrenSchuettler       DavidLawder 
    ##         0.5000000         0.6400000         0.2600000         0.1632653 
    ##     EdnaFernandes       EricAuchard    FumikoFujisaki    GrahamEarnshaw 
    ##         0.6800000         0.6938776         0.3000000         0.3265306 
    ##  HeatherScoffield     JaneMacartney        JanLopatka      JimGilchrist 
    ##         0.1800000         0.6600000         0.3600000         0.2244898 
    ##          JoeOrtiz      JohnMastrini      JonathanBirt    JoWinterbottom 
    ##         0.5600000         0.5000000         0.6734694         0.4489796 
    ##       KarlPenhaul         KeithWeir    KevinDrawbaugh     KevinMorrison 
    ##         0.3877551         0.4600000         0.5000000         0.4893617 
    ##     KirstinRidley KouroshKarimkhany         LydiaZajc    LynneO'Donnell 
    ##         0.6800000         0.4680851         0.3400000         0.3200000 
    ##   LynnleyBrowning   MarcelMichelson      MarkBendeich        MartinWolk 
    ##         0.2600000         0.3600000         0.5918367         0.5400000 
    ##      MatthewBunce     MichaelConnor        MureDickie         NickLouth 
    ##         0.4000000         0.6666667         0.7600000         0.3958333 
    ##   PatriciaCommins     PeterHumphrey        PierreTran        RobinSidel 
    ##         0.5800000         0.3265306         0.4285714         0.3600000 
    ##      RogerFillion       SamuelPerry      SarahDavison       ScottHillis 
    ##         0.2800000         0.6400000         0.6530612         0.7346939 
    ##       SimonCowell          TanEeLyn    TheresePoletti        TimFarrand 
    ##         0.6122449         0.6600000         0.6000000         0.6458333 
    ##        ToddNissen      WilliamKazer 
    ##         0.3600000         0.6200000

``` r
cm <- table(TEdataset[,rfcol], y_pred)
cm['EricAuchard',]
```

    ##     AaronPressman        AlanCrosby    AlexanderSmith   BenjaminKangLim 
    ##                 7                 2                 0                 0 
    ##     BernardHickey       BradDorfman  DarrenSchuettler       DavidLawder 
    ##                 1                 2                 0                 2 
    ##     EdnaFernandes       EricAuchard    FumikoFujisaki    GrahamEarnshaw 
    ##                 0                 0                 1                 4 
    ##  HeatherScoffield     JaneMacartney        JanLopatka      JimGilchrist 
    ##                 3                 1                 0                 1 
    ##          JoeOrtiz      JohnMastrini      JonathanBirt    JoWinterbottom 
    ##                 0                 1                 0                 0 
    ##       KarlPenhaul         KeithWeir    KevinDrawbaugh     KevinMorrison 
    ##                 5                 1                 0                 0 
    ##     KirstinRidley KouroshKarimkhany         LydiaZajc    LynneO'Donnell 
    ##                 0                 0                 0                 0 
    ##   LynnleyBrowning   MarcelMichelson      MarkBendeich        MartinWolk 
    ##                 0                 0                 0                 1 
    ##      MatthewBunce     MichaelConnor        MureDickie         NickLouth 
    ##                 4                 1                 0                 1 
    ##   PatriciaCommins     PeterHumphrey        PierreTran        RobinSidel 
    ##                 0                 0                 1                 5 
    ##      RogerFillion       SamuelPerry      SarahDavison       ScottHillis 
    ##                 2                 1                 0                 0 
    ##       SimonCowell          TanEeLyn    TheresePoletti        TimFarrand 
    ##                 1                 1                 0                 0 
    ##        ToddNissen      WilliamKazer 
    ##                 0                 1

The class errors shown explain our accuracy at predicting each author using the random forest model. Our model best predicts articles for authors like JimGilchrist, LynneO'Donnell, and LydiaZajc (their error rates are 20%, 24%, and 28%, respectively). Our model's worst predictions are for EricAuchard with 79% error. EricAuchard is often misclassified as SamuelPerry (9 out of 50 times).

Q3:Practice with association rule mining
----------------------------------------

``` r
library(tm) 
detach(package:tm, unload=TRUE)
library(arules)  
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
library(splitstackshape)
```

    ## Loading required package: data.table

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
#read in the data
groceries_raw <- read.csv('http://raw.githubusercontent.com/jgscott/STA380/master/data/groceries.txt', sep='\n', header=FALSE)

#head(groceries_raw,12)

#names the basket column
groceries_raw$basket = rownames(groceries_raw)
#split the string of items into individual rows
groceries = cSplit(groceries_raw, "V1", direction='long', ",")
#look at the first few items
head(groceries)
```

    ##                     V1 basket
    ## 1:        citrus fruit      1
    ## 2: semi-finished bread      1
    ## 3:           margarine      1
    ## 4:         ready soups      1
    ## 5:      tropical fruit      2
    ## 6:              yogurt      2

``` r
# Turn basket into a factor
groceries$basket <- factor(groceries$basket)

# First create a list of baskets: vectors of items by consumer
# apriori algorithm expects a list of baskets in a special format
# In this case, one "basket" of items per basket
# First split data into a list of items for each basket
baskets <- split(x=groceries$V1, f=groceries$basket)

## Remove duplicates ("de-dupe")
baskets <- lapply(baskets, unique)

## Cast this variable as a special arules "transactions" class.
baskettrans <- as(baskets, "transactions")
```

``` r
# Now run the 'apriori' algorithm
basketrules <- apriori(baskettrans, parameter=list(support=.01, confidence=.4, maxlen=4))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.4    0.1    1 none FALSE            TRUE       5    0.01      1
    ##  maxlen target   ext
    ##       4  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 98 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [88 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4

    ## Warning in apriori(baskettrans, parameter = list(support = 0.01, confidence
    ## = 0.4, : Mining stopped (maxlen reached). Only patterns up to a length of 4
    ## returned!

    ##  done [0.00s].
    ## writing ... [62 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
# Look at the output
inspect(basketrules)
```

    ##      lhs                        rhs                   support confidence     lift
    ## [1]  {hard cheese}           => {whole milk}       0.01006609  0.4107884 1.607682
    ## [2]  {butter milk}           => {whole milk}       0.01159126  0.4145455 1.622385
    ## [3]  {ham}                   => {whole milk}       0.01148958  0.4414062 1.727509
    ## [4]  {sliced cheese}         => {whole milk}       0.01077783  0.4398340 1.721356
    ## [5]  {oil}                   => {whole milk}       0.01128622  0.4021739 1.573968
    ## [6]  {onions}                => {other vegetables} 0.01423488  0.4590164 2.372268
    ## [7]  {hamburger meat}        => {other vegetables} 0.01382816  0.4159021 2.149447
    ## [8]  {hamburger meat}        => {whole milk}       0.01474326  0.4434251 1.735410
    ## [9]  {sugar}                 => {whole milk}       0.01504830  0.4444444 1.739400
    ## [10] {cream cheese}          => {whole milk}       0.01647178  0.4153846 1.625670
    ## [11] {chicken}               => {other vegetables} 0.01789527  0.4170616 2.155439
    ## [12] {chicken}               => {whole milk}       0.01759024  0.4099526 1.604411
    ## [13] {white bread}           => {whole milk}       0.01708185  0.4057971 1.588147
    ## [14] {frozen vegetables}     => {whole milk}       0.02043721  0.4249471 1.663094
    ## [15] {beef}                  => {whole milk}       0.02125064  0.4050388 1.585180
    ## [16] {curd}                  => {whole milk}       0.02613116  0.4904580 1.919481
    ## [17] {margarine}             => {whole milk}       0.02419929  0.4131944 1.617098
    ## [18] {butter}                => {whole milk}       0.02755465  0.4972477 1.946053
    ## [19] {domestic eggs}         => {whole milk}       0.02999492  0.4727564 1.850203
    ## [20] {whipped/sour cream}    => {other vegetables} 0.02887646  0.4028369 2.081924
    ## [21] {whipped/sour cream}    => {whole milk}       0.03223183  0.4496454 1.759754
    ## [22] {tropical fruit}        => {whole milk}       0.04229792  0.4031008 1.577595
    ## [23] {root vegetables}       => {other vegetables} 0.04738180  0.4347015 2.246605
    ## [24] {root vegetables}       => {whole milk}       0.04890696  0.4486940 1.756031
    ## [25] {yogurt}                => {whole milk}       0.05602440  0.4016035 1.571735
    ## [26] {curd,                                                                      
    ##       yogurt}                => {whole milk}       0.01006609  0.5823529 2.279125
    ## [27] {other vegetables,                                                          
    ##       pork}                  => {whole milk}       0.01016777  0.4694836 1.837394
    ## [28] {pork,                                                                      
    ##       whole milk}            => {other vegetables} 0.01016777  0.4587156 2.370714
    ## [29] {butter,                                                                    
    ##       other vegetables}      => {whole milk}       0.01148958  0.5736041 2.244885
    ## [30] {butter,                                                                    
    ##       whole milk}            => {other vegetables} 0.01148958  0.4169742 2.154987
    ## [31] {domestic eggs,                                                             
    ##       other vegetables}      => {whole milk}       0.01230300  0.5525114 2.162336
    ## [32] {domestic eggs,                                                             
    ##       whole milk}            => {other vegetables} 0.01230300  0.4101695 2.119820
    ## [33] {fruit/vegetable juice,                                                     
    ##       other vegetables}      => {whole milk}       0.01047280  0.4975845 1.947371
    ## [34] {whipped/sour cream,                                                        
    ##       yogurt}                => {other vegetables} 0.01016777  0.4901961 2.533410
    ## [35] {whipped/sour cream,                                                        
    ##       yogurt}                => {whole milk}       0.01087951  0.5245098 2.052747
    ## [36] {other vegetables,                                                          
    ##       whipped/sour cream}    => {whole milk}       0.01464159  0.5070423 1.984385
    ## [37] {whipped/sour cream,                                                        
    ##       whole milk}            => {other vegetables} 0.01464159  0.4542587 2.347679
    ## [38] {other vegetables,                                                          
    ##       pip fruit}             => {whole milk}       0.01352313  0.5175097 2.025351
    ## [39] {pip fruit,                                                                 
    ##       whole milk}            => {other vegetables} 0.01352313  0.4493243 2.322178
    ## [40] {other vegetables,                                                          
    ##       pastry}                => {whole milk}       0.01057448  0.4684685 1.833421
    ## [41] {citrus fruit,                                                              
    ##       root vegetables}       => {other vegetables} 0.01037112  0.5862069 3.029608
    ## [42] {citrus fruit,                                                              
    ##       yogurt}                => {whole milk}       0.01026945  0.4741784 1.855768
    ## [43] {citrus fruit,                                                              
    ##       other vegetables}      => {whole milk}       0.01301474  0.4507042 1.763898
    ## [44] {citrus fruit,                                                              
    ##       whole milk}            => {other vegetables} 0.01301474  0.4266667 2.205080
    ## [45] {bottled water,                                                             
    ##       other vegetables}      => {whole milk}       0.01077783  0.4344262 1.700192
    ## [46] {root vegetables,                                                           
    ##       tropical fruit}        => {other vegetables} 0.01230300  0.5845411 3.020999
    ## [47] {root vegetables,                                                           
    ##       tropical fruit}        => {whole milk}       0.01199797  0.5700483 2.230969
    ## [48] {tropical fruit,                                                            
    ##       yogurt}                => {other vegetables} 0.01230300  0.4201389 2.171343
    ## [49] {tropical fruit,                                                            
    ##       yogurt}                => {whole milk}       0.01514997  0.5173611 2.024770
    ## [50] {rolls/buns,                                                                
    ##       tropical fruit}        => {whole milk}       0.01098119  0.4462810 1.746587
    ## [51] {other vegetables,                                                          
    ##       tropical fruit}        => {whole milk}       0.01708185  0.4759207 1.862587
    ## [52] {tropical fruit,                                                            
    ##       whole milk}            => {other vegetables} 0.01708185  0.4038462 2.087140
    ## [53] {root vegetables,                                                           
    ##       yogurt}                => {other vegetables} 0.01291307  0.5000000 2.584078
    ## [54] {root vegetables,                                                           
    ##       yogurt}                => {whole milk}       0.01453991  0.5629921 2.203354
    ## [55] {rolls/buns,                                                                
    ##       root vegetables}       => {other vegetables} 0.01220132  0.5020921 2.594890
    ## [56] {rolls/buns,                                                                
    ##       root vegetables}       => {whole milk}       0.01270971  0.5230126 2.046888
    ## [57] {other vegetables,                                                          
    ##       root vegetables}       => {whole milk}       0.02318251  0.4892704 1.914833
    ## [58] {root vegetables,                                                           
    ##       whole milk}            => {other vegetables} 0.02318251  0.4740125 2.449770
    ## [59] {other vegetables,                                                          
    ##       soda}                  => {whole milk}       0.01392984  0.4254658 1.665124
    ## [60] {rolls/buns,                                                                
    ##       yogurt}                => {whole milk}       0.01555669  0.4526627 1.771563
    ## [61] {other vegetables,                                                          
    ##       yogurt}                => {whole milk}       0.02226741  0.5128806 2.007235
    ## [62] {other vegetables,                                                          
    ##       rolls/buns}            => {whole milk}       0.01789527  0.4200477 1.643919

This list is our list of rules with support&gt;.01, confidence&gt;.4, and no more than 4 items per set. We see that "staple" items like milk and vegetable are most often predicted. This makes sense because with a support level of 1%, there will be very few combinations that are present in enough baskets to make it into our list with high confidence. Only the staple items are bought together frequently enough to qualify.

``` r
#re-run rules
basketrules <- apriori(baskettrans, parameter=list(support=.004, confidence=.4, maxlen=4))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.4    0.1    1 none FALSE            TRUE       5   0.004      1
    ##  maxlen target   ext
    ##       4  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 39 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [126 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4

    ## Warning in apriori(baskettrans, parameter = list(support = 0.004,
    ## confidence = 0.4, : Mining stopped (maxlen reached). Only patterns up to a
    ## length of 4 returned!

    ##  done [0.02s].
    ## writing ... [432 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
## Choose a subset
inspect(subset(basketrules, subset=lift > 5))
```

    ##     lhs         rhs            support     confidence lift    
    ## [1] {liquor} => {bottled beer} 0.004677173 0.4220183  5.240594

When we re-run the apriori rules with support lowered to .4%, we see many more rules involving items that aren't necessarily considered "staple" items. Some of these rules have a much higher lift since an item that is less common in the overall basket list can be predicted with higher confidence given some other items in the basket. For instance, we see that bottled beer is 5 times more likely to be in a basket given that the basket also contains liquor.
