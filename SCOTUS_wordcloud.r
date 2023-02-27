
library(tm)
library(wordcloud)

# Input the data into a corpus & TDM
corpus = Corpus(VectorSource(list_control))

# Remove stopwords, including judge names 
corpus <- tm_map(corpus, removeWords, c(tm::stopwords(), 'justice', 'thank', 'you', 'counsel',
                                        tolower(gsub('JUSTICE ', '', topics1))))

# Create TDM
tdm = TermDocumentMatrix(corpus)

# Convert to matrix
tdm = as.matrix(tdm)

# Relabel column names of the matrix for comparison cloud
colnames(tdm) <- gsub('JUSTICE ', '', topics1) %>% stringr::str_to_title()

# Set the filesize & adjust margins to look better than plotting within RStudio
png("comparison_cloud3.png", width = 1000, height = 1000) 
par(mar = c(0,0,2,0)) # rep(0, 4))

# Create the comparison cloud which is saved to file above
comparison.cloud(tdm, scale = c(6, .7), max.words = 1250, random.order = FALSE, title.size = 2.5) + 
    title('Comparison Wordcloud: Gonzales v Google Feb. 21 Oral Argument')

# End the graphics
dev.off()
