library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(SnowballC)
library(slam)
library(quanteda)
library(stopwords)
library(ggplot2)
library(textdata)


#first is "israel palestine war", the second was "israel hamas war", and the third was "gaza war"
data <- read.csv('nodes_df_graph_hamas.csv') %>% 
  mutate(search_query = 'israel palestine war')

data2 <- read.csv('nodes_df_graph_hamas_2.csv') %>% 
  mutate(search_query ='israel hamas war')

data3 <- read.csv('nodes_df_graph_hamas_3.csv') %>% 
  mutate(search_query = 'gaza war')

data <- bind_rows(data, data2, data3)

ids <- c(10000:99999)

ids <- sample(ids, nrow(data))

data <- data %>% 
  mutate(id = ids)

library(cld3)

# Add a new column with detected language
data <- data %>%
  mutate(language = cld3::detect_language(transcript))

data %>% count(language)

# Filter to only English
data <- data %>%
  filter(language == "en")

#Filter out '[Music]' and 'uh', 'um' from the transcripts
data <- data %>%
  mutate(transcript = gsub("\\[Music\\]|\\[Applause\\]|\\buh\\b|\\bum\\b", "", transcript, ignore.case = TRUE))

data %>% 
  count(transcript == 'Empty')
# # Basic data inspection -------------------------------------------------

data %>%
  group_by(search_query) %>%
  summarise(
    n_videos = n(),
    avg_transcript_length = mean(nchar(transcript), na.rm = TRUE),
    median_transcript_length = median(nchar(transcript), na.rm = TRUE),
    empty_transcripts = sum(is.na(transcript) | transcript == ""),
    .groups = "drop"
  )

# Look at a few sample transcripts
data %>%
  filter(nchar(transcript) > 100) %>%
  slice_sample(n = 3) %>%
  select(search_query, transcript) %>%
  mutate(transcript = str_trunc(transcript, 200))

# Check transcript lengths
transcript_stats <- data %>%
  mutate(
    word_count = str_count(transcript, "\\w+"),
    char_count = nchar(transcript)
  ) %>%
  summarise(
    n_videos = n(),
    mean_words = mean(word_count, na.rm = TRUE),
    median_words = median(word_count, na.rm = TRUE),
    min_words = min(word_count, na.rm = TRUE),
    max_words = max(word_count, na.rm = TRUE),
    videos_over_1000_words = sum(word_count >= 1000, na.rm = TRUE),
    videos_over_500_words = sum(word_count >= 500, na.rm = TRUE)
  )

print(transcript_stats)

# Look at distribution
data %>%
  mutate(word_count = str_count(transcript, "\\w+")) %>%
  ggplot(aes(x = word_count)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Video Transcript Lengths",
       x = "Word Count", y = "Number of Videos")

# within video topic modeling ---------------------------------------------

# Filter to longer videos and model topics within each video
long_videos <- data %>%
  mutate(word_count = str_count(transcript, "\\w+")) %>% 
  filter(word_count >= 500)
# Function to model topics within a single video transcript
# model_video_topics <- function(transcript_text, video_id, n_topics = 3) {
#   
#   # Split transcript into sentences/paragraphs as "documents"
#   sentences <- str_split(transcript_text, "(?<=[.!?])\\s+")[[1]]
#   sentences <- sentences[nchar(sentences) > 50]  # Keep only substantial sentences
#   
#   if(length(sentences) < 10) return(NULL)  # Need minimum sentences
#   
#   # Create corpus from sentences
#   sentence_df <- tibble(
#     doc_id = paste0(video_id, "_", 1:length(sentences)),
#     text = sentences
#   )
#   
#   corpus_video <- quanteda::corpus(sentence_df, text_field = "text", docid_field = "doc_id")
#   
#   tokens_video <- corpus_video %>%
#     tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
#     tokens_tolower() %>%
#     tokens_remove(stopwords("en")) %>%
#     tokens_select(pattern = "[A-Za-z]{3,}", valuetype = "regex")
#   
#   dfm_video <- dfm(tokens_video) %>%
#     dfm_trim(min_termfreq = 2, min_docfreq = 2)
#   
#   if(nfeat(dfm_video) < 20) return(NULL)  # Need minimum vocabulary
#   
#   dtm_video <- tidy(dfm_video) %>% cast_dtm(document, term, count)
#   
#   tryCatch({
#     LDA(dtm_video, k = n_topics, control = list(seed = 1234))
#   }, error = function(e) NULL)
# }

#Function to create pseudo-sentences by splitting text into fixed size chunks
split_into_chunks <- function(text, chunk_size = 50) {
  words <- unlist(str_split(text, "\\s+"))
  chunks <- split(words, ceiling(seq_along(words) / chunk_size))
  sapply(chunks, paste, collapse = " ")
}

model_video_topics <- function(transcript_text, video_id, n_topics = 3) {
  
  sentences <- split_into_chunks(transcript_text, chunk_size = 50)
  sentences <- sentences[nchar(sentences) > 50]
  
  if(length(sentences) < 10) return(NULL)
  
  sentence_df <- tibble(
    doc_id = paste0(video_id, "_", 1:length(sentences)),
    text = sentences
  )
  
  corpus_video <- quanteda::corpus(sentence_df, text_field = "text", docid_field = "doc_id")
  
  tokens_video <- corpus_video %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("en")) %>%
    tokens_select(pattern = "[A-Za-z]{3,}", valuetype = "regex")
  
  dfm_video <- dfm(tokens_video) %>%
    dfm_trim(min_termfreq = 2, min_docfreq = 2)
  
  if(nfeat(dfm_video) < 20) return(NULL)
  
  dtm_video <- tidy(dfm_video) %>% cast_dtm(document, term, count)
  
  tryCatch({
    LDA(dtm_video, k = n_topics, control = list(seed = 1234))
  }, error = function(e) NULL)
}


# Apply to each long video
video_topics <- long_videos %>%
  rowwise() %>%
  mutate(
    lda_model = list(model_video_topics(transcript, id, n_topics = 3))
  ) %>%
    filter(!is.null(lda_model))

# Extract topics from each video
video_topic_terms <- video_topics %>%
  rowwise() %>%
  mutate(
    topics = list({
      if(!is.null(lda_model)) {
        tidy(lda_model, matrix = "beta") %>%
          group_by(topic) %>%
          slice_max(beta, n = 5) %>%
          summarise(top_terms = paste(term, collapse = ", "), .groups = "drop")
      } else {
        NULL
      }
    })
  ) %>%
  select(id, search_query, topics) %>%
  unnest(topics)

# Extract beta values from each video
video_betas<- video_topics %>%
  rowwise() %>%
  mutate(
    topics = list({
      if(!is.null(lda_model)) {
        tidy(lda_model, matrix = "beta") %>%
          group_by(topic) %>%
          slice_max(beta, n = 5)%>%
          ungroup() %>% 
          mutate(video_id = id)  
      } else {
        NULL
      }
    })
  ) %>%
  # select(id, search_query, topics) %>%
  unnest(topics)

print(video_topic_terms)


# Emotional intensity -----------------------------------------------------

#create a corpus 
corpus_data <- quanteda::corpus(data, text_field = "transcript", docid_field = "id")

#clean adn tokenize the transcripts
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
  #tokens_wordstem(language = 'en') %>% this doesn't really work
  tokens_remove(stopwords("en"))

dfm <- dfm(tokens)

tokens_df <- tidy(dfm)#Create a dataframe with all tokens 

lexicon_nrc_eil <- textdata::lexicon_nrc_eil() # Emotional intensity lexicon

lexicon_nrc_eil %>% 
  count(AffectDimension)

tokens_df %>% 
  count(term %in% lexicon_nrc_eil$term)

tokens_df <- tokens_df %>% 
  left_join(lexicon_nrc_eil, by='term')#Join with lexicon to get scores

#Compute weighted average emotional intensity for each video
tokens_df <- tokens_df %>%
  mutate(weighted_score = count * score)

doc_scores <- tokens_df %>%
  group_by(document) %>%
  summarise(
    total_score = sum(weighted_score, na.rm = TRUE),
    total_words_with_scores = sum(count[!is.na(score)]),
    avg_weighted_emotional_intensity = total_score / total_words_with_scores,
    avg_emotional_intensity = sum(score, na.rm = T) / total_words_with_scores,
    fear = sum(count[AffectDimension == 'fear'], na.rm = T),
    anger = sum(count[AffectDimension == 'anger'], na.rm = T),
    sadness = sum(count[AffectDimension == 'sadness'], na.rm = T),
    joy = sum(count[AffectDimension == 'joy'], na.rm = T)
  ) %>% 
  mutate(document = as.numeric(document))

#Join doc_scores with video data
doc_scores <- left_join(doc_scores, data, by=c('document'='id'))

#doc_scores %>% write.csv('doc_scores.csv', row.names = F)
#Test simple correlation
cor.test(doc_scores$avg_weighted_emotional_intensity, doc_scores$depth)

doc_scores <- doc_scores %>% 
  mutate(transcript_length = str_count(transcript, "\\w+"))

doc_scores$views <- as.numeric(gsub(",", "", doc_scores$views))

#Demean depth for interaction term
doc_scores <- doc_scores %>% 
  mutate(ddepth = depth - mean(depth))

#OLS regression with videolength and search query and depth
model <- lm(avg_weighted_emotional_intensity ~ depth + transcript_length + search_query + depth:search_query, data = doc_scores) 

summary(model)

#diagnostic plots
par(mfrow = c(2, 2))
plot(model)

car::vif(model) #Variance inflation factor

#export
library(broom)
library(modelsummary)

tidy(model) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>% 
  mutate(across(where(is.numeric), round, 3))

israel <- doc_scores %>% 
  filter(search_query == 'israel hamas war')

cor.test(israel$avg_weighted_emotional_intensity, israel$depth)

palestine <- doc_scores %>% 
  filter(search_query == 'gaza war')

cor.test(palestine$avg_weighted_emotional_intensity, palestine$depth)

neutral <- doc_scores %>% 
  filter(search_query == 'israel palestine war')

cor.test(neutral$avg_weighted_emotional_intensity, neutral$depth)

#express affect dimensions as proportions for each video
doc_scores <- doc_scores %>% 
  rowwise() %>% 
  mutate(tot_emot = sum(fear, anger, joy, sadness),
         prop_fear = fear / tot_emot,
         prop_anger = anger / tot_emot,
         prop_joy = joy / tot_emot, 
         prop_sadness = sadness / tot_emot)

#doc_scores %>% write.csv('doc_scores.csv')
#Fit model on predominant emotion vs. depth
model2 <- lm(prop_joy ~ depth + search_query, data = doc_scores)

summary(model2)

plot(model2)

library(patchwork)

ggplot(doc_scores, aes(x = depth, y = avg_weighted_emotional_intensity)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Fear")

# Define the individual plots
plot_fear <- ggplot(doc_scores, aes(x = depth, y = prop_fear)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Fear")

plot_anger <- ggplot(doc_scores, aes(x = depth, y = prop_anger)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Anger")

plot_sadness <- ggplot(doc_scores, aes(x = depth, y = prop_sadness)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Sadness")

plot_joy <- ggplot(doc_scores, aes(x = depth, y = prop_joy)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Joy")

# Combine them in a 2x2 layout
combined_plot <- (plot_fear | plot_anger) /
  (plot_sadness | plot_joy)

# Save as JPG
#ggsave("prop_affect_by_depth.jpg", plot = combined_plot, dpi = 340, width = 10, height = 8)

plot(model2)

#Group by search query and compute average emot. int
doc_scores_by_query <- doc_scores %>% 
  group_by(search_query) %>% 
  summarise(avg_emot = mean(avg_weighted_emotional_intensity))

#Group by search query and depth, compute the preponderance of emotions in the groups proportionally
affect_by_query <- doc_scores %>% 
  group_by(depth, search_query) %>% 
  summarise(total = sum(fear, anger, joy, sadness),
              fear = sum(fear) / total, 
              anger = sum(anger) / total, 
              joy = sum(joy) / total, 
              sadness = sum(sadness) / total
            )


rm(list = c('data2', 'data3', 'dfm', 'israel', 'neutral', 'palestine', 'tokens'))
# Topic and emotion analysis ----------------------------------------------

doc_scores <- doc_scores %>% 
  left_join(select(video_topic_terms, -search_query), by =c('document'='id'))

#doc_scores %>% write.csv()

notopics <- doc_scores %>% filter(is.na(topic))

#Most frequent affect dimension 

doc_scores %>% summarise(fear = sum(fear), anger = sum(anger), joy = sum(joy), sadness = sum(sadness))

#Most frequent affect dimension by search query

affect_by_query <- doc_scores %>% 
  group_by(search_query) %>% 
  summarise(fear = sum(fear), anger = sum(anger), joy = sum(joy), sadness = sum(sadness))

#Most frequently occuring topic

topic_freqs <- doc_scores %>% 
  mutate(top_terms = str_split(top_terms, ',')) %>% 
  unnest(top_terms) %>%                             
  mutate(top_terms = str_trim(top_terms))

#Most frequent topic terms
topic_freqs %>%
  count(top_terms) %>% 
  top_n(10, n) %>% 
  arrange(top_terms, n)

#Most frequently occuring topic words by depth
tops_depth <- topic_freqs %>% 
  group_by(depth) %>% 
  count(top_terms) %>% 
  top_n(10, n)

#Most frequently occuring topic words by query
tops_query <- topic_freqs %>% 
  group_by(search_query) %>% 
  count(top_terms) %>% 
  top_n(10, n)

# Compute relative frequencies
tops_depth <- topic_freqs %>%
  group_by(depth, top_terms) %>% 
  summarise(n = n(), .groups = "drop") %>%
  group_by(depth) %>%
  mutate(
    total = sum(n),
    rel_freq = n / total
  ) %>%
  slice_max(rel_freq, n = 10) %>%
  ungroup() %>%
  mutate(top_terms = fct_reorder(top_terms, rel_freq))

# Plot relative frequencies
ggplot(tops_depth, aes(x = top_terms, y = rel_freq)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ depth, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Top Topic Words by Depth (Relative Frequency)",
    x = "Topic Word",
    y = "Relative Frequency"
  ) +
  theme_minimal(base_size = 12)

top_words <- topic_freqs %>%
  count(top_terms) %>%
  top_n(5, n) %>%
  pull(top_terms)

freq_by_depth <- topic_freqs %>%
  filter(top_terms %in% top_words) %>%
  count(depth, top_terms) %>%
  group_by(depth) %>%
  mutate(rel_freq = n / sum(n)) %>%
  ungroup()

ggplot(freq_by_depth, aes(x = depth, y = rel_freq, color = top_terms)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Frequency of Top Words Across Depths", x = "Depth", y = "Relative Frequency") +
  theme_minimal()



#How many videos are actually about the topic
keywords <- c('israel', 'gaza', 'hamas', 'palestine', 'zionism', 'zion', 'idf', 'netanyahu', 'genocide', 'war')

# keywords <- c(
#   'israel', 'gaza', 'hamas', 'palestine', 'zionism', 'zion', 'idf', 'netanyahu',
#   'jerusalem', 'west bank', 'tel aviv', 'hezbollah', 'intifada',
#   'occupation', 'settlements', 'apartheid', 'resistance', 'ceasefire',
#   'two-state', 'genocide', 'bombing', 'airstrike', 'hostage', 'refugees',
#   'blockade', 'siege', 'war', 'protest', 'martyr', 'displacement'
# )
affect_by_topic <- topic_freqs %>% 
  filter(top_terms %in% keywords)

affect_by_topic %>% count(!duplicated(document))

affect_by_topic <- affect_by_topic %>% 
  group_by(top_terms) %>% 
  summarise(emot_int = mean(avg_weighted_emotional_intensity),
            fear = sum(fear),
            anger = sum(anger), 
            joy = sum(joy), 
            sadness = sum(sadness))

affect_by_topic <- affect_by_topic %>% 
  group_by(top_terms) %>% 
  mutate(total = sum(fear, anger, joy, sadness),
         prop_fear = fear /total,
         prop_anger = anger / total,
         prop_joy = joy / total,
         prop_sadness = sadness / total)


library(tidyr)
library(ggplot2)

# Convert to long format for heatmap
affect_long <- affect_by_topic %>%
  select(top_terms, prop_fear, prop_anger, prop_joy, prop_sadness) %>%
  pivot_longer(
    cols = starts_with("prop_"),
    names_to = "affect",
    values_to = "proportion"
  ) %>%
  mutate(affect = gsub("prop_", "", affect))

# Heatmap plot

ggplot(affect_long, aes(x = affect, y = reorder(top_terms, -proportion), fill = proportion)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(
    title = "Affect Proportions by Topic",
    x = "Affect Dimension", y = "Topic Term"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot = affect_prop_by_topic, 'affect_prop_by_topic.jpg', dpi = 340)

ggplot(affect_by_topic, aes(x = top_terms, y = emot_int)) +
  geom_bar(stat = 'identity')

#Frequency of keywords across depth

#keywords <- c('israel', 'gaza', 'hamas', 'palestine', 'genocide', 'war')


freq_by_depth <- topic_freqs %>%
  count(depth, top_terms, search_query) %>%
  group_by(depth) %>%
  mutate(rel_freq = n / sum(n)) %>%
  filter(top_terms %in% keywords) %>%
  ungroup()

rel_freq_keyword_by_depth <- ggplot(freq_by_depth, aes(x = depth, y = rel_freq, color = top_terms)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ search_query) +
  labs(title = "Frequency of Key Topic Terms Across Depths by Search Query", x = "Depth", y = "Relative Frequency", color = 'Keyword') +
  theme_minimal()

#ggsave(plot = rel_freq_keyword_by_depth, 'rel_freq_keyword_by_depth2.jpg', dpi = 340)

# What about looking only at videos related to the conflict, did their emotional intensity, 
#fear, anger or sadness proportions increase?

# Step 1: Find documents that contain any keywords
docs_with_keywords <- tokens_df %>%
  filter(term %in% keywords) %>%
  distinct(document)

# Step 2: Filter out those from doc_scores — keep only documents with no keyword terms
conflict_videos <- doc_scores %>%
  filter(document %in% docs_with_keywords$document)

conflict_videos <- conflict_videos %>%
  distinct(document, .keep_all = TRUE)
#98 no conflict videos, 290 conflict vids (many of them not directly related to conflict)

model2 <- lm(avg_weighted_emotional_intensity ~ depth + search_query, data = conflict_videos)

summary(model2)

plot(model2)

#Is this sound filtering criteria? 
#If the video has no topic 1-3 containing one of the keywords, it is not related to the conflict

conflict_videos <- topic_freqs %>% 
  group_by(document) %>% 
  filter(top_terms %in% keywords)

conflict_videos <- doc_scores %>%
  filter(document %in% conflict_videos$document)

conflict_videos <- conflict_videos %>%
  distinct(document, .keep_all = TRUE)

model2 <- lm(prop_joy ~ depth + search_query, data = conflict_videos)

summary(model2)

plot(model2)


# Add binary indicator to doc_scores
doc_scores <- doc_scores %>%
  mutate(conflict_related = ifelse(document %in% conflict_videos$document, 1, 0)) %>% 
  distinct(document, .keep_all = T)


doc_scores %>% count(conflict_related)

#conflict related based on title
doc_scores <- doc_scores %>%
  mutate(conflict_title = str_detect(str_to_lower(title), str_c(keywords, collapse = "|")))

conf_bytitle <- doc_scores %>% filter(conflict_title == F)

different_verdict <- doc_scores %>% filter(conflict_related == 1 & conflict_title == F)

t.test(avg_weighted_emotional_intensity ~ conflict_related, data = doc_scores)

model <- lm(prop_fear ~ conflict_related + depth + search_query, data = doc_scores)

summary(model)

plot(model)

car::vif(model)

# Load required packages
library(broom)
library(dplyr)
library(tidyr)
library(purrr)

# List of dependent variables
emotion_vars <- c("prop_fear", "prop_anger", "prop_joy", "prop_sadness")

# Run models and tidy results
model_results <- map(emotion_vars, function(dep_var) {
  model <- lm(as.formula(paste(dep_var, "~ depth + conflict_related + search_query")), data = doc_scores)
  tidy(model) %>%
    mutate(dependent = dep_var)
})

# Combine results
all_results <- bind_rows(model_results)

# Round for readability
all_results <- all_results %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Pivot wider for summary by variable
summary_table <- all_results %>%
  select(dependent, term, estimate, p.value) %>%
  pivot_wider(names_from = dependent, values_from = c(estimate, p.value))

# View the table
print(summary_table)

# Optional: Write as tab-separated for copying into Google Sheets
write.table(all_results, sep = "\t", row.names = FALSE, quote = FALSE)

all_results %>% write.csv('summary_table.csv')

# Random stuff ------------------------------------------------------------



dfm <- dfm_trim(
  dfm,
  min_docfreq = 0.05, #Remove words appearing in less than 5% and more than 95% of documents
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

#get top terms by topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

print(top_terms)

print(topics)

library(purrr)

# Test with one query to see vocabulary retention
test_data <- data %>% filter(search_query == "israel palestine war")

corpus_test <- quanteda::corpus(test_data, text_field = "transcript", docid_field = "id")
tokens_test <- corpus_test %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en"))

dfm_test <- dfm(tokens_test)

# Check vocabulary at each step
cat("Original vocabulary size:", nfeat(dfm_test), "\n")
cat("Documents:", ndoc(dfm_test), "\n")
cat("Sparsity:", round(sparsity(dfm_test), 3), "\n")

# Apply your filtering
dfm_filtered <- dfm_trim(dfm_test, min_docfreq = 0.2, max_docfreq = 0.8, docfreq_type = "prop")
cat("After filtering - vocabulary size:", nfeat(dfm_filtered), "\n")
cat("After filtering - sparsity:", round(sparsity(dfm_filtered), 3), "\n")

#Model the different topics by search query
models_by_query <- data %>%
  split(.$search_query) %>%
  map(~ {
    corpus_q <- quanteda::corpus(.x, text_field = "transcript", docid_field = "id")
    tokens_q <- corpus_q %>% 
      tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("en")) %>% 
      tokens_select(pattern = "[A-Za-z]", valuetype = "regex")
    
    dfm_q <- dfm(tokens_q) %>%
      dfm_trim(min_docfreq = 0.01, max_docfreq = 0.99, docfreq_type = "prop")
    
    dtm_q <- tidy(dfm_q) %>% cast_dtm(document, term, count)
    
    LDA(dtm_q, k = 10, control = list(seed = 1234))
  })

# Much more lenient filtering
models_by_query_fixed <- data %>%
  split(.$search_query) %>%
  map(~ {
    corpus_q <- quanteda::corpus(.x, text_field = "transcript", docid_field = "id")
    tokens_q <- corpus_q %>% 
      tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("en")) %>%
      tokens_select(pattern = "[A-Za-z]{3,}", valuetype = "regex")  # Only words 3+ chars
    
    dfm_q <- dfm(tokens_q) %>%
      dfm_trim(min_docfreq = 0.2, min_termfreq = 0.8)  # Very minimal filtering
    
    dtm_q <- tidy(dfm_q) %>% cast_dtm(document, term, count)
    
    LDA(dtm_q, k = 5, control = list(seed = 1234, alpha = 0.1))  # Lower alpha for more focused topics
  })


top_terms_by_query <- map2_dfr(
  models_by_query_fixed,
  names(models_by_query_fixed),
  ~ {
    tidy(.x, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      mutate(query = .y)
  }
)

#Get all topics in one dataframe
topics_by_query <- map2_dfr(models_by_query, names(models_by_query), ~ {tidy(.x) %>% mutate(query = .y)})

topics_by_query <- topics_by_query %>% 
  group_by(topic, query) %>% 
  top_n(50, beta) %>% 
  ungroup()
  


#Emotional intensity lexicon? lexicon_nrc_eil NRC Emotion Intensity Lexicon (aka Affect Intensity Lexicon) v0.5

lexicon_nrc_eil <- textdata::lexicon_nrc_eil() # Emotional intensity lexicon

nrc <- get_sentiments("nrc") # sentiment lexicon

topics_by_query %>% count(term %in% lexicon_nrc_eil$term) 
#Is it better to do the emotional intensity analysis on topic terms or on raw tokens?
#Only 6625/40590 terms appear in lexicon

emotion_intensity <- topics_by_query %>% 
  inner_join(lexicon_nrc_eil, by='term')#Join terms with emotion intensity scores and affect dimension

#Compare emotion intensity scores across topics and queries 
avg_intensity <- emotion_intensity %>% 
  group_by(query, topic) %>%
  summarise(avg_intesity_score = mean(score))

emotion_scores <- topics_by_query %>%
  inner_join(nrc, by = c("term" = "word"))#Join sentiments with terms

emotion_scores <- emotion_scores %>% 
  inner_join(lexicon_nrc_eil, by='term')





















