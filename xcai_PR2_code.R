#Load the necessary R packages.
library(stringr)
library(dplyr)
library(wordcloud)

#Load the dataset.
dataset <- read.csv("product_data_df_cleaned.csv",stringsAsFactors=FALSE)

#Calculate the number of observations in the dataset_all
paste("Until now, there are",nrow(dataset),"observations in the dataset.")

# Replace some values in the "KEYWORDS".
dataset$KEYWORDS <- dataset$KEYWORDS %>% 
  gsub("\\(", " ", .) %>%
  gsub("\\)", " ", .) %>%
  gsub("\\[", " ", .) %>%
  gsub("\\]", " ", .)


# Create a list of keywords.
keywords_list <- str_split(dataset$KEYWORDS, ",")

# Concert the list into a dataframe. 
keywords_string <- unlist(keywords_list)
keywords_df <- data.frame(word = keywords_string)

# Trim the spaces at both side of the terms.
keywords_df$word <- str_trim(keywords_df$word, side = "both")

#Analysis of word frequency
frequency <- keywords_df %>% 
  count(word, sort = TRUE) %>% 
  ungroup()


# Remove some meaningless terms in the dataframe.
frequency_cleaned <- subset(frequency,  !(word %in% c("","|","w/","-","W/","/","–")))

# Visualise the results.
wordcloud(words = frequency_cleaned$word, freq = frequency_cleaned$n,min.freq = 1,           
          max.words=300, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
