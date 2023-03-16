library(openNLP)
library(tidytext)

# Load the data

# Split the reviews into sentences
sentences <- papers$text %>%
  get_sentences()

# Load the AFINN sentiment lexicon
data("afinn")

# Calculate the sentiment score for each sentence
sentiment_scores <- data.frame(sentence = sentences) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(afinn) %>%
  group_by(sentence) %>%
  summarize(sentiment_score = sum(value))

# Preview the sentiment scores
head(sentiment_scores)