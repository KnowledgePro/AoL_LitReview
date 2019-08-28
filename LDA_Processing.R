

# Initialize packages and read PDFs ---------------------------------------

require(tm)
require(pdftools)
require(qdapRegex)
require(textreadr)
require(tidyverse)
require(tidytext)
require(wordcloud)
require(igraph)
require(ggraph)
require(topicmodels)

# Route 1 - tidy processing: Use 'textreadr' to capture a corpus - reading by pages-----

alt_Corpus <- read_dir("docs completos/", doc.col = "article")

# Clean the texts using qdapRegex

alt_Corpus$content <- alt_Corpus$content %>%
  rm_url() %>%
  rm_email() %>%
  rm_non_ascii() %>%
  rm_endmark() %>%
  rm_white_punctuation() %>%
  rm_white_endmark() %>%
  rm_number() %>%
  rm_round() %>%
  rm_non_words()

# Route 1 - tokenize with 'tidytext' ----
# The stop_words lexicon from tidytext is extended to remove uninformative results
my_stopwords <-
  add_row(stop_words, word = "al", lexicon = "AoLTeam") %>%
  add_row(word = "s", lexicon = "AoLTeam") %>%
  add_row(word = "th", lexicon = "AoLTeam") %>%
  add_row(word = "de", lexicon = "AoLTeam") %>%
  add_row(word = "la", lexicon = "AoLTeam") %>%
  add_row(word = "Sabana", lexicon = "AoLTeam") %>%
  add_row(word = "journal", lexicon = "AoLTeam") %>%
  add_row(word = "york", lexicon = "AoLTeam") %>%
  add_row(word = "ny", lexicon = "AoLTeam") %>%
  add_row(word = "washington", lexicon = "AoLTeam") %>%
  add_row(word = "dc", lexicon = "AoLTeam") %>%
  add_row(word = "san", lexicon = "AoLTeam") %>%
  add_row(word = "francisco", lexicon = "AoLTeam") %>%
  add_row(word = "ca", lexicon = "AoLTeam") %>%
  add_row(word = "issn", lexicon = "AoLTeam") %>%
  add_row(word = "crossmark", lexicon = "AoLTeam") %>%
  add_row(word = "xxx", lexicon = "AoLTeam") %>%
  add_row(word = "xxxx", lexicon = "AoLTeam") %>%
  add_row(word = "jossey", lexicon = "AoLTeam") %>%
  add_row(word = "josseybass", lexicon = "AoLTeam") %>%
  add_row(word = "houghton", lexicon = "AoLTeam") %>%
  add_row(word = "mifflin", lexicon = "AoLTeam") %>%
  add_row(word = "pearson", lexicon = "AoLTeam") %>%
  add_row(word = "  ok", lexicon = "AoLTeam")  %>%
  add_row(word = "ok", lexicon = "AoLTeam")  %>%
  add_row(word = "nj", lexicon = "AoLTeam")  %>%
  add_row(word = "prentice", lexicon = "AoLTeam")  %>%
  add_row(word = "hall", lexicon = "AoLTeam")  %>%
  add_row(word = "john", lexicon = "AoLTeam")  %>%
  add_row(word = "wiley", lexicon = "AoLTeam")  %>%
  add_row(word = "sons", lexicon = "AoLTeam")  %>%
  add_row(word = "publishing", lexicon = "AoLTeam")  %>%
  add_row(word = "rome", lexicon = "AoLTeam")  %>%
  add_row(word = "italy", lexicon = "AoLTeam")  %>%
  add_row(word = "june", lexicon = "AoLTeam")  %>%
  add_row(word = "mcser", lexicon = "AoLTeam")  %>%
  add_row(word = "national", lexicon = "AoLTeam")  %>%
  add_row(word = "press", lexicon = "AoLTeam") %>%
  add_row(word = "au", lexicon = "AoLTeam") %>%
  add_row(word = "bu", lexicon = "AoLTeam") %>%
  add_row(word = "eu", lexicon = "AoLTeam")

# Get Unigrams -not lemmatized-

alt_unigrams <- unnest_tokens(alt_Corpus, word, content) %>%
  anti_join(., my_stopwords, by = "word")

alt_freq_unigrams <- alt_unigrams %>%
  count(., word, sort = TRUE) %>%
  unique() %>%
  mutate(., rank = row_number())

alt_freq_unigrams <- alt_freq_unigrams %>%
  mutate(., CDF = cumsum(alt_freq_unigrams$n))

# Where is 80% of the cumulative distribution reached?
rank_80 <- length(subset(alt_freq_unigrams$CDF, alt_freq_unigrams$CDF <= length(alt_unigrams$word)*.8))

# Unigram CDF Plot (in two parts) and a borderline
plot(alt_freq_unigrams$rank[1:rank_80], alt_freq_unigrams$CDF[1:rank_80],
     type = "l", col = 2, ylab = "Cumulative Distribution", xlab = "Word Rank",
     xlim = c(0,length(alt_freq_unigrams$n)), ylim = c(0,last(alt_freq_unigrams$CDF)))
lines(alt_freq_unigrams$rank[rank_80:28064], alt_freq_unigrams$CDF[rank_80:28064], type = "l", col = 3)
abline(v = 2464, lty = 2, col = "dark grey")

# Average word reuse in corpus
# How many total words in corpus / unique words in corpus
length(alt_unigrams$word) / length(alt_freq_unigrams$word)

# Parameters
summary(alt_freq_unigrams)

# Bigrams

alt_bigrams <- unnest_tokens(alt_Corpus, bigram, content, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word) %>%
  filter(!word2 %in% my_stopwords$word) %>%
  unite(bigram, word1, word2, sep = " ", remove = TRUE)

alt_freq_bigram <- alt_bigrams %>%
  count(bigram, sort = TRUE) %>%
  mutate(rank = row_number(), CDF = cumsum(.$n))

# Bigram CDF plot, with a comparison borderline
# Where is 80% of the cumulative distribution reached?
rank_80_b <- length(subset(alt_freq_bigram$CDF, alt_freq_bigram$CDF <= length(alt_bigrams$bigram)*.8))

plot(alt_freq_bigram$rank, alt_freq_bigram$CDF, type = "l", col = 2, ylab = "Cumulative Distribution", xlab = "Word Rank",
     main = "Bigram Cumulative Distribution")
abline(v = rank_80_b, lty = 2, col = "dark grey")

# Average bigram frequency
summary(alt_freq_bigram)

# Trigrams

alt_trigrams <- unnest_tokens(alt_Corpus, trigram, content, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word) %>%
  filter(!word2 %in% my_stopwords$word) %>%
  filter(!word3 %in% my_stopwords$word) %>%
  unite(trigram, word1, word2, word3, sep = " ", remove = TRUE)

alt_freq_trigram <- alt_trigrams %>%
  count(trigram, sort = TRUE) %>%
  mutate(rank = row_number(), CDF = cumsum(.$n))

# Route 1 - Trigram CDF plot ----
# Where is 80% of the cumulative distribution reached?
rank_80_t <- length(subset(alt_freq_trigram$CDF, alt_freq_trigram$CDF <= length(alt_trigrams$trigram)*.8))
plot.new() # Commenting this line one gets a comparative graph
plot(alt_freq_trigram$rank, alt_freq_trigram$CDF, type = "l", lty = 2, col = 2, ylab = "Cumulative Distribution", xlab = "Word Rank",
     main = "Trigram Cumulative Distribution")
abline(v = rank_80_t, lty = 2, col = "dark grey")

# Route 1 - Bigram graph ----

bigram_graph <- alt_freq_bigram %>%
      filter(n>50) %>%
      separate(bigram, c("word1", "word2"), sep = " ")

bigram_graph_plot <- bigram_graph %>%
      graph_from_data_frame()

set.seed(2000)
ggraph(bigram_graph_plot, layout = "nicely") +
      geom_edge_fan(aes(edge_alpha = n), hjust =1, vjust =1, arrow = grid::arrow(type = "closed", length = unit(.15, "inches"))) +
      geom_node_point(size = 1) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Route 1 - Trigram graph ----

trigram_graph <- alt_freq_trigram %>%
  filter(n>15) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigram_graph_plot <- trigram_graph %>%
      graph_from_data_frame()

set.seed(2000)
ggraph(trigram_graph_plot, layout = "nicely") +
      geom_edge_fan(aes(edge_alpha = n), hjust =1, vjust =1, arrow = grid::arrow(type = "closed", length = unit(.25, "cm"))) +
      geom_node_point(size = 1) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1)



# Route 1 additional plotting - Zipf law

alt_freq_unigrams %>%
  ggplot(aes(rank, n, color = 1)) +
  geom_line(size = 1.1,
            alpha = 0.8,
            show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  annotation_logticks(colour = "black")

alt_freq_bigram %>%
  ggplot(aes(rank, n, color = 1)) +
  geom_line(size = 1.1,
            alpha = 0.8,
            show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  annotation_logticks(colour = "black")

alt_freq_trigram %>%
  ggplot(aes(rank, n, color = 1)) +
  geom_line(size = 1.1,
            alpha = 0.8,
            show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  annotation_logticks(colour = "black")

# In Which articles is AACSB individually mentioned?
which(alt_unigrams$word == "aacsb") %>% alt_unigrams[.,1] %>% unique()
which("advance collegiate schools" == alt_trigrams$trigram) %>% alt_trigrams[.,1] %>% unique()

# AACSB Percentage of the total articles
aacbs_articles %>% length() / alt_Corpus$article %>% unique() %>% length()

# full article list
articles <- alt_Corpus$article %>% unique()

# In Which articles are 'major field test'&'ets major field' individually mentioned?
# 1.'major field test'
length(which(alt_trigrams$trigram == "major field test"))
which(alt_trigrams$trigram == "major field test") %>% alt_trigrams[.,1] %>% unique()
# 2. 'ets major field'
length(which(alt_trigrams$trigram == "ets major field"))
which(alt_trigrams$trigram == "ets major field") %>% alt_trigrams[.,1] %>% unique()
# 3.'educational testing service'
length(which(alt_trigrams$trigram == "educational testing service"))
which(alt_trigrams$trigram == "educational testing service") %>% alt_trigrams[.,1] %>% unique()
# 4. 'ets' unigrams
length(which(alt_unigrams$word == "ets"))
which(alt_unigrams$word == "ets") %>% alt_unigrams[.,1] %>% unique()
# 5. 'ets major'
length(which(alt_bigrams$bigram == 'ets major'))
which(alt_bigrams$bigram == 'ets major') %>% alt_bigrams[.,1] %>% unique()
# 6. 'major field'
length(which(alt_bigrams$bigram == 'major field'))
which(alt_bigrams$bigram == 'major field') %>% alt_bigrams[.,1] %>% unique()
# 7. 'field test'
length(which(alt_bigrams$bigram == 'field test'))
which(alt_bigrams$bigram == 'field test') %>% alt_bigrams[.,1] %>% unique()
# 8. ‘multiple choice questions’
grep("multiple choice questions", alt_Corpus$content) %>% alt_Corpus[.,1] %>% unique()



# Find "accounting" as a whole word in the Corpus, by article.
grep("accounting", alt_Corpus$content) %>% alt_Corpus[.,1] %>% unique()
# accounting_count <- grepl("accounting", alt_unigrams$word) %>%
#   subset(x = alt_unigrams,subset =  . )
 grep("accounting", alt_unigrams$word) %>% alt_unigrams[.,1] %>% unique()
 grep("accounting", alt_freq_unigrams$word)
grep("accounting", alt_bigrams$bigram)
# accounting_count_bi <- grepl("accounting", alt_bigrams$bigram) %>%
#   subset(x = alt_bigrams,subset =  . ) %>%
#   group_by(article) %>%
#   select(-bigram) %>%
#   unique()
grep("accounting", alt_bigrams$bigram) %>% alt_bigrams[.,1] %>% unique()
# grep("accounting", alt_trigrams$trigram)
# accounting_count_tri <- grepl("accounting", alt_trigrams$trigram) %>%
#   subset(x = alt_trigrams,subset =  . ) %>%
#   group_by(article) %>%
#   select(-trigram) %>%
#   unique()
# grep("accounting", alt_trigrams$trigram) %>% alt_trigrams[.,1] %>% unique()
# length(grep("accounting", head(alt_freq_trigram$trigram,100)))


# Frequencies for 'article'
grep("article", alt_unigrams$word)
grep("article", alt_unigrams$word) %>% alt_unigrams[.,1] %>% unique()

grep("article", alt_trigrams$trigram)
grep("article", alt_trigrams$trigram) %>% alt_trigrams[.,1] %>% unique()

grep("article", alt_Corpus$content) %>% alt_Corpus[.,1] %>% unique()



# Route 1 - Tidy Tf-idf weighting -----
alt_unigrams <- alt_unigrams %>%
    count(article, word, sort = TRUE)
tokens_per_article <- alt_unigrams %>%
    group_by(article) %>%
    summarize(total = sum(n))
alt_unigrams <- left_join(alt_unigrams, tokens_per_article) %>%
    bind_tf_idf(word, article, n) %>%
    arrange(desc(n))
tfidf_alt_unigrams <- alt_unigrams %>%
    arrange(desc(tf))
# Results for tidy tf-idf were less informative than expected
# proceeding with {tm} tf-idf

# Route 2 - {tm} package processing: Read to Document-Text Matrix----

tmCorpus <-
  Corpus(
    DirSource("docs completos/"),
    readerControl = list(reader = readPDF(), language = "en")
  )

# Clean PDF text data using {tm} package and create a Document-Term Matrix

toSpace <-
  content_transformer(function(x, pattern) {
    return (gsub(pattern, " ", x, fixed = TRUE))
  })

fixAACSB <-
  content_transformer(function(x) {
    return (gsub("aacsb", " aacsb ", x, fixed = TRUE))
  })

removeURL <- content_transformer(function(x) {
  return (rm_url(x))
})

removeEmail <-
  content_transformer(function(x) {
    return (rm_email(x))
  })

removeThe <-
      content_transformer(function(x) {
            return (rm_default(x, "the"))
      })

tmCleanCorpus <- tm_map(tmCorpus, removeURL) %>%
  tm_map(., removeEmail) %>%
  tm_map(., fixAACSB) %>%
  tm_map(., removeNumbers) %>%
  tm_map(., toSpace, "/") %>%
  tm_map(., toSpace, "✓") %>%
  tm_map(., removePunctuation) %>%
  tm_map(., toSpace, "/") %>%
  tm_map(., toSpace, "−") %>%
  tm_map(., toSpace, "\\n") %>%
  tm_map(., toSpace, "\a") %>%
  tm_map(., toSpace, "\\u") %>%
  tm_map(., toSpace, "\f") %>%
  tm_map(., toSpace, "<") %>%
  tm_map(., toSpace, ">") %>%
  tm_map(., toSpace, "\n") %>%
  tm_map(., toSpace, "\\") %>%
  tm_map(., toSpace, "*") %>%
  tm_map(., toSpace, "∗") %>%
  tm_map(., toSpace, "´") %>%
  tm_map(., toSpace, "’") %>%
  tm_map(., toSpace, "–") %>%
  tm_map(., toSpace, "—") %>%
  tm_map(., toSpace, "‘") %>%
  tm_map(., toSpace, "“") %>%
  tm_map(., toSpace, "”") %>%
  tm_map(., toSpace, "") %>%
  tm_map(., toSpace, "●") %>%
  tm_map(., toSpace, "•") %>%
  tm_map(., toSpace, " ") %>%
  tm_map(., toSpace, "…") %>%
  tm_map(., toSpace, "○") %>%
  tm_map(., toSpace, "€") %>%
  tm_map(., toSpace, "\003") %>%
  tm_map(., toSpace, "\002") %>%
  tm_map(., toSpace, "\001") %>%
  tm_map(., toSpace, "\u2028") %>%
  tm_map(., toSpace, "the") %>%
  tm_map(., stripWhitespace) %>%
  tm_map(., removeWords, stopwords("english"))

#Create Document-Term Matrices from the {tm} package
dtm <- DocumentTermMatrix(tmCleanCorpus) #Unigrams

# TrigramTokenizer <-  function(x)  {
#   unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = TRUE)
# }
#
# dtm3 <- DocumentTermMatrix(tmCleanCorpus, control = list(tokenize = TrigramTokenizer))

# Tidytext processing from DTM: tokenization 1-, 2-, and 3-grams

# text_base <- tmCleanCorpus %>% tidy()

# unigrams <- text_base %>%
#   unnest_tokens(., word, text) %>%
#   anti_join(., my_stopwords, by = "word")
#
# freq_unigram <- unigrams %>%
#   count(., word, sort = TRUE) %>%
#   mutate(rank = row_number())
#
# bigrams <-  text_base %>%
#   unnest_tokens(., bigram, text, token = "ngrams", n = 2) %>%
#   separate(bigram, c("word1", "word2"), sep = " ") %>%
#   filter(!word1 %in% my_stopwords$word) %>%
#   filter(!word2 %in% my_stopwords$word)
#
# freq_bigram <- bigrams %>%
#   count(word1, word2, sort = TRUE) %>%
#   mutate(rank = row_number())
#
# trigrams <-  text_base %>%
#   unnest_tokens(., trigram, text, token = "ngrams", n = 3) %>%
#   separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
#   filter(!word1 %in% my_stopwords$word) %>%
#   filter(!word2 %in% my_stopwords$word) %>%
#   filter(!word3 %in% my_stopwords$word)
#
# freq_trigram <- trigrams %>%
#   count(word1, word2, word3, sort = TRUE) %>%
#   mutate(rank = row_number())

# Route 2 Plotting ---
#
# freq_unigram %>%
#   ggplot(aes(rank, n, color = 1)) +
#   geom_line(size = 1.1,
#             alpha = 0.8,
#             show.legend = FALSE) +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw() +
#   annotation_logticks(colour = "black")
#
# freq_bigram %>%
#   ggplot(aes(rank, n, color = 1)) +
#   geom_line(size = 1.1,
#             alpha = 0.8,
#             show.legend = FALSE) +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw() +
#   annotation_logticks(colour = "black")
#
#
# freq_trigram %>%
#   ggplot(aes(rank, n, color = 1)) +
#   geom_line(size = 1.1,
#             alpha = 0.8,
#             show.legend = FALSE) +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw() +
#   annotation_logticks(colour = "black")

# Results from the above commented section were congruent with Route 1 results


# From DTM Tf-idf weighting
#
# article_words <- text_base %>%
#   unnest_tokens(word, text) %>%
#   count(id, word, sort = TRUE) %>%
#   ungroup()
#
# total_words <- article_words %>%
#   group_by(id) %>%
#   summarize(total = sum(n))
#
# article_words <- left_join(article_words, total_words) %>%
#     bind_tf_idf(word, id, n) %>%
#     arrange(desc(tf_idf))
#
# Same poor results as with tidy tf-idf


# Route 2 {tm} TF-IDF unigram frequencies ----
tfidf_tm_unigrams <- weightTfIdf(dtm, normalize = TRUE)
tfidf_tm_unigrams  <- removeSparseTerms(tfidf_tm_unigrams , .8)
dim(tfidf_tm_unigrams )
inspect(tfidf_tm_unigrams )
freq_tfidf_tm <- sort(colSums(as.matrix(tfidf_tm_unigrams)), decreasing = T)
write.csv(freq_tfidf_tm,"Results/TF-IDF Unigram Frequencies.csv")

#  Route 2 {tm} TF-IDF trigram frequencies ----
tfidf_tm_trigrams <- weightTfIdf(dtm3, normalize = TRUE)
tfidf_tm_trigrams <- removeSparseTerms(tfidf_tm_trigrams, .8)
dim(tfidf_tm_trigrams)
inspect(tfidf_tm_trigrams)
freq_tfidf_tm_3 <- sort(colSums(as.matrix(tfidf_tm_trigrams)), decreasing = T)
write.csv(freq_tfidf_tm_3,"Results/TF-IDF Trigram Frequencies.csv")



# Print out Route 1 and Route 2 data -----
write.csv(freq_unigram, "Results/TM Unigram Frequency.csv")
write.csv(freq_bigram, "Results/TM Bigram Frequency.csv")
write.csv(freq_trigram, "Results/TM trigram Frequency.csv")
write.csv(alt_freq_unigrams, "Results/Alt-tidy Unigram Frequency.csv")
write.csv(alt_freq_bigram, "Results/Alt-tidy Bigram Frequency.csv")
write.csv(alt_freq_trigram, "Results/Alt-tidy trigram Frequency.csv")

# Wordcloud generation ----


freq_bigram %>%
  unite(bigram, word1, word2, sep = " ") %>%
  top_n(100,n) %>%
  with(wordcloud(bigram, n, random.order = TRUE, random.color = FALSE))

alt_freq_bigram %>%
  top_n(200,n) %>%
  with(wordcloud(bigram, n, random.order = TRUE, random.color = FALSE))

freq_trigram %>%
  unite(trigram, word1, word2, word3, sep = " ") %>%
  top_n(100,n) %>%
  with(wordcloud(trigram, n, random.order = TRUE, random.color = FALSE))

alt_freq_trigram %>%
  top_n(150,n) %>%
  with(wordcloud(trigram, n, random.order = FALSE, random.color = FALSE))

# Route 2 - LDA topic modeling. K parameter =8 from term analysis----

# '_small' means 'remove sparse'
dtm_small <- removeSparseTerms(dtm, 0.8)

unigram_LDA <- LDA(dtm_small, k = 8, control = list(seed = 975) )
Unigram_tfidf_LDA <- LDA(tfidf_tm_unigrams, k = 8, control = list(seed = 975))

# Initial Analysis of non-weighted matrix
initial_topics <- tidy(unigram_LDA, matrix="beta")

top_topic_words <- initial_topics %>%
      group_by(topic) %>%
      top_n(20, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
top_topic_words  %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip()

# Second stage: Analysis of TFIDF-weighted matrix
initial_topics <- tidy(unigram_LDA, matrix="beta")

top_topic_words <- initial_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_topic_words  %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
