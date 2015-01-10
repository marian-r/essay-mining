
corpus <- rawCorpus
#inspect(corpus[1:10])

#
# Transforming Text
#

# Remove placeholders
corpus = tm_map(corpus, content_transformer(function(x) {
  gsub("@[A-Z]+[0-9]+", "", x, perl = T)
}))

# Change letters to lower case
corpus <- tm_map(corpus, tolower)

# Remove punctuations
corpus <- tm_map(corpus, removePunctuation)

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Remove stopwords (these are some of the most common, short function words, such as the, is, at, which, etc.)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

# Read the custom stopwords list
conn = file("data/english.stop.txt", open="r")
mystopwords = readLines(conn)
close(conn)

# Inspect our custom stopwords list
#mystopwords

# Remove stopwords
corpus <- tm_map(corpus, removeWords, mystopwords)

# the previous transformations changed the internal representation of our corpus,
# so we have to transform it back to PlainTextDocument 
corpus <- Corpus(VectorSource(corpus))

# Stem words to retrieve their radicals, so that various forms derived from a stem would be taken as the same 
# when counting word frequency
corpus <- tm_map(corpus, stemDocument)

# Strip extra whitespace from text documents
corpus <- tm_map(corpus, stripWhitespace)

# again, we have to transform our corpus representation back to PlainTextDocument
corpus <- Corpus(VectorSource(corpus))

# Have a look at the first 10 documents in the corpus
#inspect(corpus[1:10])
