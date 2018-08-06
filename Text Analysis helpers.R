#---------- Load Libraries ----------#
library(tidyverse)
library(tidytext)
library(stringr)

###################################
#---------- Text Mining ----------#
###################################

# Text mining can also be thought of as text analytics. The goal is to discover relevant information that is possibly unknown or buried beneath the obvious. 
# Natural Language Processing (NLP) is one methodology used in mining text. It tries to decipher the ambiguities in written language by tokenization, 
# clustering, extracting entity and word relationships, and using algorithms to identify themes and quantify subjective information. You'll begin by breaking 
# down the concept of lexical complexity.

# Lexical complexity can mean different things in different contexts, but for now, assume that it can be described by a combination of these measures:

      # Word Frequency: number of words per song
      # Word Length: average length of individual words in a text - use nchar()
      # Lexical Diversity: number of unique words used in a text (song vocabulary) - use n_distinct()
      # Lexical Density: the number of unique words divided by the total number of words (word repetition) - use n_distinct()

# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
df$text <- sapply(df$text, fix.contractions)


# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
df$text <- sapply(df$text, removeSpecialChars)


# convert everything to lower case
df$text <- sapply(df$text, tolower)

# create a plot theme
theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}


#---------- Calculate TF-IDF ----------#

# TF-IDF
# The method that you've been using so far looks at the entire dataset, but it has not addressed how to quantify just how important various 
# terms are in a document with respect to an entire collection. You have looked at term frequency, and removed stop words, but this may not 
# be the most sophisticated approach.

# Enter TF-IDF. TF is "term frequency". IDF is "inverse document frequency", which attaches a lower weight for commonly used words and a higher weight 
# for words that are not used much in a collection of text. When you combine TF and IDF, a term's importance is adjusted for how rarely it is used. 
# The assumption behind TF-IDF is that terms that appear more frequently in a document should be given a higher weight, **unless** it also appears in many 
# documents. The formula can be summarized below:
  
# * Term Frequency (TF): Number of times a term occurs in a document
# * Document Frequency (DF): Number of documents that contain each word
# * Inverse Document Frequency (IDF) = 1/DF
# * TF-IDF = TF * IDF

# The IDF of any term is therefore a higher number for words that occur in fewer of the documents in the collection. You can use this new approach to examine 
# the most important words per chart level with the `bind_tf_idf()` function provided by `tidytext`. This function calculates and binds the TF and IDF, along 
# with the TF*IDF product. It takes a tidy text dataset as input with one row per token (word), per document (song). You'll see the results in new columns.


popular_tfidf_words <- df %>%
  unnest_tokens(word, text) %>%
  distinct() %>%
  filter(nchar(word) > 3) %>% #this is to remove shorter words... won't always do this
  count(facet_filter, word, sort = TRUE) %>% #where 'facet_filter' is what you want to group by
  ungroup() %>%
  bind_tf_idf(word, facet_filter, n)

head(popular_tfidf_words)


#---------- Plotting: ----------#

# first create a df that makes plotting in order easier when faceting
top_popular_tfidf_words <- popular_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(facet_filter) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(facet_filter, tf_idf) %>%
  mutate(row = row_number())

# then make the plot
top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, 
             fill = facet_filter)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by facet_filter") +
  theme_lyrics() +  
  facet_wrap(~facet_filter, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
    labels = top_popular_tfidf_words$word) +
  coord_flip()


