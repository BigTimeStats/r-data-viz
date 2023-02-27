
library(tm)
library(wordcloud)

opinion_clean1 <- readr::read_csv('https://raw.githubusercontent.com/BigTimeStats/r-data-viz/main/SCOTUS-Wordcloud/oral_argument_gonzalez_v_google_feb_21.csv')

topics1 <- unique(opinion_clean1$speaker)[unique(opinion_clean1$clean_text) != ''] %>% 
    grep('JUSTICE', ., value = TRUE) 

opinion_clean2 <- opinion_clean1 %>% 
    filter(speaker %in% topics1)

list_control <- vector()

for(i in 1:length(topics1)){
    
    list_control[[i]] <- opinion_clean2 %>% filter(speaker == topics1[i]) %>% .[['clean_text']] %>% paste(., collapse = ' ')
    
}

# Input the data into a corpus & TDM
corpus <- Corpus(VectorSource(list_control))

# Remove stopwords, including judge names 
corpus <- tm_map(corpus, removeWords, c(tm::stopwords(), 'justice', 'thank', 'you', 'counsel',
                                        tolower(gsub('JUSTICE ', '', topics1))))

# Create TDM
tdm <- TermDocumentMatrix(corpus)

# Convert to matrix
tdm <- as.matrix(tdm)

# Relabel column names of the matrix for comparison cloud
colnames(tdm) <- gsub('JUSTICE ', '', topics1) %>% stringr::str_to_title()

# Set the filename, size, & adjust margins to look better than plotting within RStudio
png("comparison_cloud3.png", width = 1000, height = 1000) 
par(mar = c(0,0,2,0)) # leave a little margin at the top for a title

# Create the comparison cloud which is saved to file above
comparison.cloud(tdm, scale = c(6, .7), max.words = 1250, random.order = FALSE, title.size = 2.5) + 
    title('Comparison Wordcloud: Gonzales v Google Feb. 21 Oral Argument')

# End the graphics connection
dev.off()
