library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(SnowballC)
library(slam)
library(quanteda)
library(stopwords)
library(ggplot2)

data <- read.csv('nodes_df_conflict.csv')

data$transcript

ids <- c(10000:99999)

ids <- sample(ids, nrow(data))

data <- data %>% 
  mutate(id = ids)

library(cld3)

# Add a new column with detected language
data <- data %>%
  mutate(language = cld3::detect_language(transcript))

# Filter to only English
data <- data %>%
  filter(language == "en")

corpus_data <- quanteda::corpus(data, text_field = "transcript", docid_field = "id")

tokens <- corpus_data %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE,
    remove_url = TRUE,
    split_hyphens = TRUE
  ) %>%
  tokens_select(pattern = "[A-Za-z]", valuetype = "regex") %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en"))

dfm <- dfm(tokens)

dfm <- dfm_trim(
  dfm,
  min_docfreq = 0.05,
  max_docfreq = 0.95,
  docfreq_type = "prop",
  verbose = TRUE
)

dfm_dtm <- tidy(dfm)

dfm_dtm_lda <- dfm_dtm %>%
  cast_dtm(document, term, count)

num_topics <- 5
ldamodel <- LDA(dfm_dtm_lda, k = num_topics, control = list(seed = 1234))

print(ldamodel)

topics <- tidy(ldamodel , matrix =  'beta')

top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

print(top_terms)

print(topics)

# Split by depth
library(purrr)

#Model the different topics by depth/search query
models_by_query <- data %>%
  split(.$depth) %>%
  map(~ {
    corpus_q <- quanteda::corpus(.x, text_field = "transcript", docid_field = "id")
    tokens_q <- corpus_q %>% 
      tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("en")) %>% 
      tokens_select(pattern = "[A-Za-z]", valuetype = "regex")
    dfm_q <- dfm(tokens_q) %>%
      dfm_trim(min_docfreq = 0.05, max_docfreq = 0.95, docfreq_type = "prop")
    dtm_q <- tidy(dfm_q) %>% cast_dtm(document, term, count)
    LDA(dtm_q, k = 5, control = list(seed = 1234))
  })

depth3_topics <- tidy(models_by_query$'3')

# Extract top 10 terms per topic for each depth
top_terms_by_depth <- map2_dfr(
  models_by_query,
  names(models_by_query),
  ~ {
    tidy(.x, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      mutate(depth = .y)
  }
)

top_terms_by_depth <- top_terms_by_depth %>%
  arrange(as.numeric(depth), topic, -beta)

top_terms_by_depth %>% 
  count(depth)

print(top_terms_by_depth)

library(textdata)
nrc <- get_sentiments("nrc")

emotion_scores <- top_terms_by_depth %>%
  inner_join(nrc, by = c("term" = "word"))

emotion_scores %>% 
  count(depth)
