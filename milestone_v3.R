##H Synopsis

#This is a milestone report for the capstone project of the data sciences specialization. This is an exploratory analysis of the training dataset. This will involve summaries of the 3 files in terms of word counts, line counts, chaacter counts. Histograms will be done to see what are the most frequently occuring words. 


##H Part 1 - Load Files and libraries

# We load the libraries and then set the current directory. We download the file and unzip the contents. We download the bad words from a publicly available URL in case it will be useful. We will read all the three files and calculate the line count and also calculate the character count as these are easy to calculate. Word count is difficult and will require the tm package which we do later on

#```{r eval=TRUE, echo=TRUE}
library (data.table)
library (caret)
library (ggplot2)
library (stringr)
library (wordcloud)
library (tm)
library (RWeka)
library(SnowballC)
library (slam)
library (quanteda)



setwd ("/home/kirana/coursera")
if (!file.exists("Coursera-SwiftKey.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
        destfile = "Coursera-SwiftKey.zip")
	system ("unzip -d swiftkey Coursera-SwiftKey.zip")
}
setwd ("swiftkey/final/en_US")

download.file("http://www.frontgatemedia.com/new/wp-content/uploads/2014/03/Terms-to-Block.csv", 
        destfile = "badwords.csv")

blogLines=readLines(file("en_US.blogs.txt","r"))
twitterLines=readLines(file("en_US.twitter.txt","r"))
newsLines=readLines(file("en_US.news.txt","r"))

googlebadwords <- read.csv ("badwords.csv",header = TRUE, stringsAsFactors = F)
googlebadwords <- googlebadwords[-1,2]
googlebadwords <- unique (googlebadwords)
googlebadwords <- gsub (",", "", googlebadwords)

save.image ('inputs.rData')


#```

##H Part 2 - Preprocess the files and Create Term Document Matrix

# We clean the corpus. THis involves removing punctuation, removing numbers, stripping out the whitespace, converting to lowercase and removing stopwords in English. We then create a corpus for the twitter, blog and news datasets. We then generate 1-gram, 2-gram and 3-gram Term Document Matrix (TDM) from the above corpora

#```{r eval=TRUE, echo=TRUE}

mycorpus_twitter <- corpus (tolower(twitterLines))
mycorpus_blog <- corpus (tolower(blogLines))
mycorpus_news <- corpus (tolower(newsLines))

mycorpus_twitter<- tokenize (mycorpus_twitter, removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE)
mycorpus_twitter <- removeFeatures (mycorpus_twitter, googlebadwords)
mycorpus_blog <- tokenize (mycorpus_blog, removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE)
mycorpus_blog <- removeFeatures (mycorpus_blog, googlebadwords)
mycorpus_news <- tokenize (mycorpus_news, removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE)
mycorpus_news <- removeFeatures (mycorpus_news, googlebadwords)
save.image ('corpus.rData')

mycorpus_twitter_1 <- ngrams (mycorpus_twitter, 1)
mycorpus_blog_1 <- ngrams (mycorpus_blog, 1)
mycorpus_news_1 <- ngrams (mycorpus_news, 1)
tdm_news_1 <- dfm (mycorpus_news_1, verbose=FALSE)
tdm_blog_1 <- dfm (mycorpus_blog_1, verbose=FALSE)
tdm_twitter_1 <- dfm (mycorpus_twitter_1, verbose=FALSE)

rm (mycorpus_twitter_1, mycorpus_blog_1, mycorpus_news_1)
save (tdm_news_1, tdm_blog_1, tdm_twitter_1, file = '1grams.rData')
rm (tdm_news_1, tdm_blog_1, tdm_twitter_1)
gc ()


mycorpus_twitter_2 <- ngrams (mycorpus_twitter, 2)
mycorpus_blog_2 <- ngrams (mycorpus_blog, 2)
mycorpus_news_2 <- ngrams (mycorpus_news, 2)
tdm_news_2 <- dfm (mycorpus_news_2, verbose=FALSE)
tdm_blog_2 <- dfm (mycorpus_blog_2, verbose=FALSE)
tdm_twitter_2 <- dfm (mycorpus_twitter_2, verbose=FALSE)
rm (mycorpus_twitter_2, mycorpus_blog_2, mycorpus_news_2)
save (tdm_news_2, tdm_blog_2, tdm_twitter_2, file = '2grams.rData')
rm (tdm_news_2, tdm_blog_2, tdm_twitter_2)
gc ()


mycorpus_twitter_3 <- ngrams (mycorpus_twitter, 3)
mycorpus_blog_3 <- ngrams (mycorpus_blog, 3)
mycorpus_news_3 <- ngrams (mycorpus_news, 3)
tdm_news_3 <- dfm (mycorpus_news_3, verbose=FALSE)
tdm_blog_3 <- dfm (mycorpus_blog_3, verbose=FALSE)
tdm_twitter_3 <- dfm (mycorpus_twitter_3, verbose=FALSE)
rm (mycorpus_twitter_3, mycorpus_blog_3, mycorpus_news_3)
save (tdm_news_3, tdm_blog_3, tdm_twitter_3, file = '3grams.rData')
rm (tdm_news_3, tdm_blog_3, tdm_twitter_3)
gc ()

mycorpus_twitter_4 <- ngrams (mycorpus_twitter, 4)
mycorpus_blog_4 <- ngrams (mycorpus_blog, 4)
mycorpus_news_4 <- ngrams (mycorpus_news, 4)
tdm_news_4 <- dfm (mycorpus_news_4, verbose=FALSE)
tdm_blog_4 <- dfm (mycorpus_blog_4, verbose=FALSE)
tdm_twitter_4 <- dfm (mycorpus_twitter_4, verbose=FALSE)
rm (mycorpus_news_4, mycorpus_blog_4, mycorpus_twitter_4)
save (tdm_news_4, tdm_blog_4, tdm_twitter_4, file = '4grams.rData')
rm (tdm_news_4, tdm_blog_4, tdm_twitter_4)
gc ()


load ('1grams.rData')
load ('2grams.rData')
load ('3grams.rData')
load ('4grams.rData')

rm (tdm_news_1, tdm_blog_1, tdm_twitter_1)
# Find most frequent terms via rolling up
freqdfnews2 <- data.table (freq = colSums (tdm_news_2), word = colnames (tdm_news_2))
freqdfnews3 <- data.table (freq = colSums (tdm_news_3), word = colnames (tdm_news_3))
freqdfnews4 <- data.table (freq = colSums (tdm_news_4), word = colnames (tdm_news_4))

freqdfblog2 <- data.table (freq = colSums (tdm_blog_2), word = colnames (tdm_blog_2))
freqdfblog3 <- data.table (freq = colSums (tdm_blog_3), word = colnames (tdm_blog_3))
freqdfblog4 <- data.table (freq = colSums (tdm_blog_4), word = colnames (tdm_blog_4))

freqdftwitter2 <- data.table (freq = colSums (tdm_twitter_2), word = colnames (tdm_twitter_2))
freqdftwitter3 <- data.table (freq = colSums (tdm_twitter_3), word = colnames (tdm_twitter_3))
freqdftwitter4 <- data.table (freq = colSums (tdm_twitter_4), word = colnames (tdm_twitter_4))


rm (tdm_news_4, tdm_blog_4, tdm_twitter_4)
rm (tdm_news_3, tdm_blog_3, tdm_twitter_3)
rm (tdm_news_2, tdm_blog_2, tdm_twitter_2)




# sort each of these by the frequency decreasing order
freqdfnews2 <- freqdfnews2[order(freq, decreasing=TRUE),]
freqdfnews3 <- freqdfnews3[order(freq, decreasing=TRUE),]
freqdfnews4 <- freqdfnews4[order(freq, decreasing=TRUE),]

freqdfblog2 <- freqdfblog2[order(freq, decreasing=TRUE),]
freqdfblog3 <- freqdfblog3[order(freq, decreasing=TRUE),]
freqdfblog4 <- freqdfblog4[order(freq, decreasing=TRUE),]

freqdftwitter2 <- freqdftwitter2[order(freq, decreasing=TRUE),]
freqdftwitter3 <- freqdftwitter3[order(freq, decreasing=TRUE),]
freqdftwitter4 <- freqdftwitter4[order(freq, decreasing=TRUE),]

# replace the _ with space
freqdfnews2 [,word := gsub ("_", " ", word)]
freqdfnews3 [,word := gsub ("_", " ", word)]
freqdfnews4 [,word := gsub ("_", " ", word)]

freqdfblog2 [,word := gsub ("_", " ", word)]
freqdfblog3 [,word := gsub ("_", " ", word)]
freqdfblog4 [,word := gsub ("_", " ", word)]

freqdftwitter2 [,word := gsub ("_", " ", word)]
freqdftwitter3 [,word := gsub ("_", " ", word)]
freqdftwitter4 [,word := gsub ("_", " ", word)]


#save (mycorpus_blog, mycorpus_news, mycorpus_twitter, file = 'reqcorpus.rData')
rm (mycorpus_blog, mycorpus_news, mycorpus_twitter)
rm (blogLines, twitterLines, newsLines)
gc ()

save.image ('beforetdms.rData')


# have a target in each
library (stringi)
freqdfnews2 [,target:=stri_extract_last_words (word)]
freqdfnews3 [,target:=stri_extract_last_words (word)]
freqdfnews4 [,target:=stri_extract_last_words (word)]
freqdfnews2 [,word1 := sub(word, pattern = " [[:alpha:]]*$", replacement = "")]
freqdfnews3 [,word1 := sub(word, pattern = " [[:alpha:]]*$", replacement = "")]
freqdfnews4 [,word1 := sub(word, pattern = " [[:alpha:]]*$", replacement = "")]

freqdftwitter2 [,target:=stri_extract_last_words (word)]
freqdftwitter3 [,target:=stri_extract_last_words (word)]
freqdftwitter4 [,target:=stri_extract_last_words (word)]
freqdftwitter2 [,word1 := sub(word, pattern = " [[:alpha:]]*$", replacement = "")]
freqdftwitter3 [,word1 := sub(word, pattern = " [[:alpha:]]*$", replacement = "")]
freqdftwitter4 [,word1 := sub(word, pattern = " [[:alpha:]]*$", replacement = "")]


freqdfblog2 [,target:=stri_extract_last_words (word)]
freqdfblog3 [,target:=stri_extract_last_words (word)]
freqdfblog4 [,target:=stri_extract_last_words (word)]
freqdfblog2 [,word1 := sub(word, pattern = " [[:alpha:]]*$", replacement = "")]
freqdfblog3 [,word1 := sub(word, pattern = " [[:alpha:]]*$", replacement = "")]
freqdfblog4 [,word1 := sub(word, pattern = " [[:alpha:]]*$", replacement = "")]


freqdftwitter2[,word := NULL]
freqdftwitter3[,word := NULL]
freqdftwitter4[,word := NULL]
freqdfblog2[,word := NULL]
freqdfblog3[,word := NULL]
freqdfblog4[,word := NULL]
freqdfnews2[,word := NULL]
freqdfnews3[,word := NULL]
freqdfnews4[,word := NULL]

save.image ('tdms.rData')


# remove non english words from the freq tables
removeNonEnglishWords <- function (dftemp) {
	dftemp [,word1 := iconv (word1, "latin1", "ASCII", sub="")]
	dftemp [,target := iconv (target, "latin1", "ASCII", sub="")]
	dftemp [,word1 := str_trim(word1)]
	dftemp [,target := str_trim(target)]

	dftemp <- dftemp [word1 != "" & target!= "",]
	return (dftemp)
}


freqdftwitter2 <- removeNonEnglishWords (freqdftwitter2)
freqdftwitter3 <- removeNonEnglishWords (freqdftwitter3)
freqdftwitter4 <- removeNonEnglishWords (freqdftwitter4)
freqdfnews2 <- removeNonEnglishWords (freqdfnews2)
freqdfnews3 <- removeNonEnglishWords (freqdfnews3)
freqdfnews4 <- removeNonEnglishWords (freqdfnews4)
freqdfblog2 <- removeNonEnglishWords (freqdfblog2)
freqdfblog3 <- removeNonEnglishWords (freqdfblog3)
freqdfblog4 <- removeNonEnglishWords (freqdfblog4)



# Conditional Probability - get denominator
calc_denominator <- function (dftemp) {
	dftemp <- dftemp [order(word1),]
	temp <- dftemp [,list (denominator = sum(freq)),by=word1]
	dftemp <- merge (dftemp, temp, by = "word1", all.x=TRUE)


	return (dftemp)
}

if (1==0) {
	freqdftwitter2 <- calc_denominator (freqdftwitter2)
	freqdftwitter4 <- calc_denominator (freqdftwitter3)
	freqdftwitter3 <- calc_denominator (freqdftwitter4)
	freqdfnews2 <- calc_denominator (freqdfnews2)
	freqdfnews4 <- calc_denominator (freqdfnews3)
	freqdfnews3 <- calc_denominator (freqdfnews4)
	freqdfblog2 <- calc_denominator (freqdfblog2)
	freqdfblog4 <- calc_denominator (freqdfblog3)
	freqdfblog3 <- calc_denominator (freqdfblog4)
}
save.image ('tdms_withdenom.rData')


# merge all the files into a 2-gram data frame
ngram2df <- rbind (freqdftwitter2, freqdfnews2, freqdfblog2)
rm (freqdftwitter2, freqdfnews2, freqdfblog2)
ngram2df <- ngram2df [,list (freq=sum(freq)),by=list(word1,target)]
ngram2df <- calc_denominator (ngram2df)

# now get the 3-gram data frame
ngram3df <- rbind (freqdftwitter3, freqdfnews3, freqdfblog3)
rm (freqdftwitter3, freqdfnews3, freqdfblog3)
ngram3df <- ngram3df [,list (freq=sum(freq)),by=list(word1,target)]
ngram3df <- calc_denominator (ngram3df)


# and now the 4-gram data frame
ngram4df <- rbind (freqdftwitter4, freqdfnews4, freqdfblog4)
rm (freqdftwitter4, freqdfnews4, freqdfblog4)
ngram4df <- ngram4df [,list (freq=sum(freq)),by=list(word1,target)]
ngram4df <- calc_denominator (ngram4df)

rm (temp1, temp)
gc ()


ngram2df <- ngram2df [order(word1, -freq),]
ngram3df <- ngram3df [order(word1, -freq),]
ngram4df <- ngram4df [order(word1, -freq),]

save.image ('beforediscount.rData')

# Have a discount coefficient for each
calc_discount_coefficient <- function (dftemp) {
	dftemp [,discount := 1]
	for (i in 10:1) 
		dftemp [freq == i, discount := length (which(dftemp$freq==(i+1)))/length(which(dftemp$freq == i)) * (i+1) / i]

	return (dftemp)
}

ngram2df <- calc_discount_coefficient (ngram2df)
ngram3df <- calc_discount_coefficient (ngram3df)
ngram4df <- calc_discount_coefficient (ngram4df)

calc_missing_prob_mass <- function (dftemp) {
	dftemp [,temp := discount * freq/denominator]; 	temp <- dftemp [,list(tempsum = sum(temp)), by = word1]
	dftemp <- merge (dftemp, temp, by = "word1", all.x=TRUE)
	dftemp [,missing_prob_mass := 1-tempsum]
	dftemp [,temp := NULL]; 	dftemp [,tempsum := NULL]
	return (dftemp)
}
ngram2df <- calc_missing_prob_mass(ngram2df)
ngram3df <- calc_missing_prob_mass(ngram3df)
ngram4df <- calc_missing_prob_mass(ngram4df)


# Prediction function
mypredict <- function (x, tgt=-1) {
	x <- tolower (x)
	x <- tokenize (x, removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE)
	x <- removeFeatures (x, googlebadwords)

	# Let us look for match in 4-gram, 3-gram, 2-gram
	mywords3 <- paste (tail (unlist (x), 3), collapse=" ")
	mywords2 <- paste (tail (unlist (x), 2), collapse=" ")
	mywords1 <- tail (unlist (x), 1)
	match4gram <- ngram4df [word1==mywords3,]
	match3gram <- ngram3df [word1==mywords2,]
	match2gram <- ngram2df [word1==mywords1,]

	# Is there a match in 4-gram
	if (nrow(match4gram) > 0) {
		match4gram [,prob := freq * (1-unique(match4gram$missing_prob_mass))/denominator]
		match3gram [,prob := freq * discount * unique(match4gram$missing_prob_mass)/sum(freq*discount) * (1- unique (match3gram$missing_prob_mass))]
		match2gram [,prob := freq * discount * (1-sum(match3gram$prob) - sum(match4gram$prob) )/sum(freq*discount)]
		temp <- rbind (match4gram, match3gram, match2gram)
		temp <- temp [order(-prob),]
		temp <-	temp [,list(prob=sum(prob)),by=target]
		if (tgt != -1) temp <- temp [target %in% tgt,]
		return (temp$target[1:min(15, nrow(temp))])
	}	

	else {
		if (nrow (match3gram) > 0) {
			match3gram [,prob := freq * (1-unique(match3gram$missing_prob_mass))/denominator]
			match2gram [,prob := freq * discount * unique(match3gram$missing_prob_mass)/sum(freq*discount)]
			temp <- rbind (match3gram, match2gram)
			temp <-	temp [,list(prob=sum(prob)),by=target]
			temp <- temp [order(-prob),]
			if (tgt !=  -1) temp <- temp [target %in% tgt,]
			return (temp$target[1:min(15, nrow(temp))])
		}
		else {
			if (nrow (match2gram) > 0) {
				match2gram [,prob := freq * (1-unique(match2gram$missing_prob_mass))/denominator]
				temp <- match2gram
				temp <- temp [order(-prob),]
				if (tgt !=  -1) temp <- temp [target %in% tgt,]
				return (temp$target[1:min(15, nrow(temp))])
			}
			else {
				return ("the")
			}
		}
	}

}

kiranpredict ("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
kiranpredict ("Every inch of you is perfect from the bottom to the")
kiranpredict ("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")
# We use a variation of the Katz backoff where we look at all 4-gram, 3-gram and 2-gram
# First search in the 4-gram table
# Whatever we get as the for the 4-gram, we take the list of (word, target, freq, denom) pairs
# We look at the 3-gram and take the list of (word, target, freq, denom) pairs
# similarly we look at the 2-gram and take the list of the (word, target, freq, denom) pairs
# all the above are candidates for us


mypredict ("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd", c("sleep", "eat", "die", "give"))
mypredict ("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his", c("financial", "marital", "spiritual", "horticultural"))
mypredict ("I'd give anything to see arctic monkeys this", c("weekend", "decade", "month", "morning"))
mypredict ("Talking to your mom has the same effect as a hug and helps reduce your", c("happiness", "stress", "sleepiness", "hunger"))
mypredict ("When you were in Holland you were like 1 inch away from me but you hadn't time to take a", c("picture", "look", "minute", "walk"))
mypredict ("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the", c("matter", "case", "incident", "account"))
mypredict ("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each", c("hand", "toe", "arm", "finger"))
mypredict ("Every inch of you is perfect from the bottom to the", c("top", "side", "center", "middle"))
mypredict ("I’m thankful my childhood was filled with imagination and bruises from playing", c("outside", "daily", "inside", "weekly"))
mypredict ("I like how the same people are in almost all of Adam Sandler's", c("pictures", "stories", "movies", "novels"))
mypredict ("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", c("beer", "pretzels", "cheese", "soda"))

mypredict ("You're the reason why I smile everyday. Can you follow me please? It would mean the", c("best", "universe", "world", "most"))

mypredict ("Hey sunshine, can you follow me and make me the", c("happiest", "saddest", "bluest", "smelliest")) 
mypredict ( "Offense still struggling but the", c("crowd", "defense", "players", "referees")) 

mypredict ("Go on a romantic date at the", c("mall", "beach", "movies", "grocery"))

mypredict ( "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", c("phone", "way", "motorcycle", "horse"))

mypredict ( "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", c("thing", "years", "weeks", "time"))
mypredict ("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", c("eyes", "toes", "fingers", "ears"))
mypredict( "Be grateful for the good times and keep the faith during the", c("hard", "sad", "worse", "bad"))
mypredict ("If this isn't the cutest thing you've ever seen, then you must be", c("callous", "insane", "asleep", "insensitive"))


save.image ('predicted.rData')
save (googlebadwords, file = 'googlebadwords.rData')

# Unfortunately what happens is that the file becomes very unwieldy for shiny - we cannot load such a big file. So let us make the .rData file small.
ngram2df <- ngram2df [ngram2df$freq > 5,]
ngram3df <- ngram3df [ngram3df$freq > 2,]
ngram4df <- ngram4df [ngram4df$freq > 2,]
rm (bigtemp, mywords1, mywords3, mywords2, prevwords, temp, dftemp)
gc ()

# remove instances where the target is a stopword
ngram2df <- ngram2df [!(ngram2df$target %in% stopwords("english")),]
ngram3df <- ngram3df [!(ngram3df$target %in% stopwords("english")),]
ngram4df <- ngram4df [!(ngram4df$target %in% stopwords("english")),]
ngram3df <- ngram3df [ngram3df$freq > 5,]
ngram4df <- ngram4df [ngram4df$freq > 5,]

save.image ('smaller.rData')






