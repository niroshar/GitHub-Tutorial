
library(lubridate)
library(wordcloud)
library(tm)

## Reading the data file
text <- readLines("https://www.nytimes.com/2018/03/17/us/florida-bridge-collapse-crack.html")

# Creating data file as a corpus and inspect it
docs <- Corpus(VectorSource(text))
inspect(docs)


# Replacing special characters with space:
f.rem <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, ">")
docs <- tm_map(docs, toSpace, "(")
docs <- tm_map(docs, toSpace, ")")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, ">")
docs <- tm_map(docs, toSpace, "(")
docs <- tm_map(docs, toSpace, ")")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ",")

# Removing specific words
docs <- tm_map(docs, removeWords, c("span","meta","nytimescom","https","li","blabla1", "blabla2","div","css", "srchttps","html","body","script","href","http","hrefhttps","wpcontent", "footer","head")) 


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers,
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
inspect(docs)
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)



set.seed(1234)
wordcloud(words = d$word, freq = d$freq, scale=c(4,0.5), min.freq = 3,
          max.words=150, random.order=FALSE, rot.per=0.5, 
          colors=brewer.pal(8, "Dark2"))


head(d, 10)