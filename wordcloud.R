
lib <- c("lubridate","wordcloud", "tm")
lapply(lib, require, character.only = TRUE)

## Reading the data file
text <- readLines("https://www.nytimes.com/2018/03/17/us/florida-bridge-collapse-crack.html")

# Creating data file as a corpus and inspect it
Dat_text <- Corpus(VectorSource(text))
inspect(Dat_text)


# Replacing special characters with space:
f.rem <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
Dat_text <- tm_map(Dat_text, f.rem, ">")
Dat_text <- tm_map(Dat_text, f.rem, ")")
Dat_text <- tm_map(Dat_text, f.rem, "-")
Dat_text <- tm_map(Dat_text, f.rem, "/")
Dat_text <- tm_map(Dat_text, f.rem, "@")
Dat_text <- tm_map(Dat_text, f.rem, "\\|")
Dat_text <- tm_map(Dat_text, f.rem, ">")
Dat_text <- tm_map(Dat_text, f.rem, ")")
Dat_text <- tm_map(Dat_text, f.rem, "-")
Dat_text <- tm_map(Dat_text, f.rem, ",")


# Convert the text to lower case text
Dat_text <- tm_map(Dat_text, content_transformer(tolower))
# Remove numbers, english common stopwords, punctuations, extra white spaces
Dat_text <- tm_map(Dat_text, removeNumbers)
Dat_text <- tm_map(Dat_text, removeWords, stopwords("english"))
Dat_text <- tm_map(Dat_text, removePunctuation)
Dat_text <- tm_map(Dat_text, stripWhitespace)

# Instpect the content after cleaning the data file
inspect(Dat_text)

# Removing specific words
Dat_text <- tm_map(Dat_text, removeWords, c("span","meta","nytimescom","wwwnytimescom","https","li","blabla1", "blabla2","div","css", "srchttps","html","body","script","href","http","hrefhttps","wpcontent", "footer","head")) 

## Save in a matrix
text_mat <- TermDocumentMatrix(Dat_text)
t.matrix <- as.matrix(text_mat)
# Sort in decreasing order
ord.matrix <- sort(rowSums(t.matrix),decreasing=TRUE)
# Save as a data frame
df <- data.frame(word = names(ord.matrix),freq=ord.matrix)


## Create the wordcloud
set.seed(1)
wordcloud(words = df$word, freq = df$freq, scale=c(4,0.5), min.freq = 3,
          max.words=150, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

## Display first 15 words by frequency
head(df, 15)
