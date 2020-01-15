

# 0- Initialize packages and read PDFs ---------------------------------------

require(tm)
require(pdftools)  # requires libpoppler-cpp-dev on system
require(qdapRegex) # requieres libcurl4-openssl-dev on system
require(textreadr) # requires libpoppler-cpp-dev on system
require(tidyverse)
require(tidytext)
require(wordcloud)
require(igraph)
require(ggraph)
require(topicmodels) # requires libxml2-dev on system
require(bibliometrix)

# 1- Scientometric Analysis with Bibliometrix ----

# The original data for this comes from the basic Scopus query ("Assurance of
# Learning"  OR  "curriculum alignment"  OR  "curricular alignment") in Title,
# Keywords or Abstract

BibData <- readFiles("scopus2.bib") %>%
  convert2df( dbsource = "scopus", format = "bibtex")

BibResults <- biblioAnalysis(BibData)

BibResults

summary(object = BibResults)

plot(BibResults)

BibDominance <- dominance(BibResults)

plot(BibDominance)

BibCitation <- localCitations(BibData, sep = ";")

BibCitation$Authors[1:20,]

# Route 1-1- Reading for tidy processing -------

# Use 'textreadr' to capture a corpus - reading by pages

alt_Corpus <- read_dir("docs completos/", doc.col = "article")

# The ligature problem with 'ffi', 'ffl', 'ff', 'fi', 'fl'
# https://superuser.com/questions/375449/why-does-the-text-fi-get-cut-when-i-copy-from-a-pdf-or-print-a-document
# https://tex.stackexchange.com/questions/33476/why-cant-fi-be-separated-when-being-copied-from-a-compiled-pdf
# Depending on the font of the PDF, some documents are missing all words with the ligatures.

#  Route 1-2- Cleaning the texts using qdapRegex -----

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

# Route 1-3- Tokenizing with 'tidytext' ----
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
  add_row(word = "eu", lexicon = "AoLTeam") %>%
  add_row(word = " doi ", lexicon = "AoLTeam") %>%
  add_row(word = "spermissionsnav", lexicon = "AoLTeam")

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

# Trigram CDF plot
# Where is 80% of the cumulative distribution reached?
rank_80_t <- length(subset(alt_freq_trigram$CDF, alt_freq_trigram$CDF <= length(alt_trigrams$trigram)*.8))
plot.new() # Commenting this line one gets a comparative graph
plot(alt_freq_trigram$rank, alt_freq_trigram$CDF, type = "l", lty = 2, col = 2, ylab = "Cumulative Distribution", xlab = "Word Rank",
     main = "Trigram Cumulative Distribution")
abline(v = rank_80_t, lty = 2, col = "dark grey")

# Route 1-4- Bigram graph ----

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

# Route 1-5- Trigram graph ----

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



# Route 1-6- Plotting Zipf law ----

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

# Route 1-7- Tidy TF-IDF weighting -----
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

# Results for tidy tf-idf were less informative than expected.
# Proceeding with {tm} tf-idf

# Route 1-8- Questions Answered ----
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

# Route 2-1- Reading with {tm} package to Document-Text Matrix----

tmCorpus <-
  Corpus(
    DirSource("docs completos/"),
    readerControl = list(reader = readPDF(), language = "en")
  )
# The same ligature problem is present

# Route 2-2- Clean PDF text data using {tm} and {qdapRegex} package ----

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

lowerCASE <-
  content_transformer(function(x) {
    return (tolower(x))
  })

tmCleanCorpus <- tm_map(tmCorpus, removeURL) %>%
  tm_map(., lowerCASE) %>%
  tm_map(., removeEmail) %>%
  tm_map(., fixAACSB) %>%
  tm_map(., toSpace, " the ") %>%
  tm_map(., removeNumbers) %>%
  tm_map(., removePunctuation) %>%
  tm_map(., toSpace, "\\n") %>%
  tm_map(., toSpace, "\n") %>%
  tm_map(., toSpace, "\a") %>%
  tm_map(., toSpace, "\\u") %>%
  tm_map(., toSpace, "\\") %>%
  tm_map(., toSpace, "\f") %>%
  tm_map(., toSpace, "–") %>%
  tm_map(., toSpace, "—") %>%
  tm_map(., toSpace, "⇑") %>%
  tm_map(., toSpace, "©") %>%
  tm_map(., toSpace, "¼") %>%
  tm_map(., toSpace, "±") %>%
  #tm_map(., toSpace, "o") %>%
  tm_map(., toSpace, "\003") %>%
  tm_map(., toSpace, "html") %>%
  tm_map(., toSpace, "the ") %>%
  tm_map(., toSpace, "  ") %>%
  tm_map(., toSpace, " and ") %>%
  tm_map(., toSpace, "/") %>%
  tm_map(., toSpace, "✓") %>%
  tm_map(., toSpace, "√") %>%
  tm_map(., toSpace, "/") %>%
  tm_map(., toSpace, "−") %>%
  tm_map(., toSpace, "–") %>%
  tm_map(., toSpace, "–") %>%
  tm_map(., toSpace, "<") %>%
  tm_map(., toSpace, ">") %>%
  tm_map(., toSpace, "*") %>%
  tm_map(., toSpace, "∗") %>%
  tm_map(., toSpace, "´") %>%
  tm_map(., toSpace, "’") %>%
  tm_map(., toSpace, "‘") %>%
  tm_map(., toSpace, "“") %>%
  tm_map(., toSpace, "”") %>%
  tm_map(., toSpace, "") %>%
  #tm_map(., toSpace, "") %>%
  tm_map(., toSpace, "●") %>%
  tm_map(., toSpace, "•") %>%
  tm_map(., toSpace, " ") %>%
  tm_map(., toSpace, "…") %>%
  tm_map(., toSpace, "○") %>%
  tm_map(., toSpace, "€") %>%
  tm_map(., toSpace, "¡") %>%
  tm_map(., toSpace, "\002") %>%
  tm_map(., toSpace, "\001") %>%
  tm_map(., toSpace, "\u2028") %>%
  tm_map(., toSpace, "\u2439") %>%
  tm_map(., toSpace, " ⳱") %>%
  tm_map(., toSpace, " al. ") %>%
  tm_map(., toSpace, " s ") %>%
  tm_map(., toSpace, " de ") %>%
  tm_map(., toSpace, " also ") %>%
  tm_map(., toSpace, " la ") %>%
  tm_map(., toSpace, " et ") %>%
  tm_map(., toSpace, "universidad") %>%
  tm_map(., toSpace, "sabana") %>%
  tm_map(., toSpace, "march") %>%
  tm_map(., toSpace, "journal") %>%
  tm_map(., toSpace, " level ") %>%
  tm_map(., toSpace, "york") %>%
  tm_map(., toSpace, " ny ") %>%
  tm_map(., toSpace, "washington") %>%
  tm_map(., toSpace, " dc ") %>%
  tm_map(., toSpace, " san ") %>%
  tm_map(., toSpace, "francisco") %>%
  tm_map(., toSpace, " ca ") %>%
  tm_map(., toSpace, " issn ") %>%
  tm_map(., toSpace, "crossmark") %>%
  tm_map(., toSpace, "xxx") %>%
  tm_map(., toSpace, "xxxx") %>%
  tm_map(., toSpace, "jossey") %>%
  tm_map(., toSpace, "josseybass") %>%
  tm_map(., toSpace, "houghton") %>%
  tm_map(., toSpace, "mifflin") %>%
  tm_map(., toSpace, "pearson") %>%
  tm_map(., toSpace, "  ok")  %>%
  tm_map(., toSpace, " ok")  %>%
  tm_map(., toSpace, " nj")  %>%
  tm_map(., toSpace, "prentice")  %>%
  tm_map(., toSpace, "hall")  %>%
  tm_map(., toSpace, "john")  %>%
  tm_map(., toSpace, "wiley")  %>%
  tm_map(., toSpace, " sons")  %>%
  tm_map(., toSpace, " taylor ")  %>%
  tm_map(., toSpace, " francis ")  %>%
  tm_map(., toSpace, " llc ")  %>%
  tm_map(., toSpace, "publishing")  %>%
  tm_map(., toSpace, " rome")  %>%
  tm_map(., toSpace, "italy")  %>%
  tm_map(., toSpace, "june")  %>%
  tm_map(., toSpace, " mcser")  %>%
  tm_map(., toSpace, "national")  %>%
  tm_map(., toSpace, "press") %>%
  tm_map(., toSpace, " print ") %>%
  tm_map(., toSpace, " au ") %>%
  tm_map(., toSpace, " bu ") %>%
  tm_map(., toSpace, " eu ") %>%
  tm_map(., toSpace, " doi ") %>%
  tm_map(., toSpace, "spermissionsnav") %>%
  tm_map(., toSpace, "sagepubcom") %>%
  tm_map(., toSpace, " online ")  %>%
  tm_map(., toSpace, " homepage ")  %>%
  tm_map(., toSpace, " acsjchemedb ")  %>%
  tm_map(., removeWords, stopwords("english")) %>%
  tm_map(., toSpace, " use ")  %>%
  tm_map(., toSpace, " can ")  %>%
  tm_map(., toSpace, " one ")  %>%
  tm_map(., toSpace, " used ")  %>%
  tm_map(., toSpace, " new ")  %>%
  tm_map(., toSpace, " may ")  %>%
  tm_map(., toSpace, " two ")  %>%
  tm_map(., toSpace, " using ")  %>%
  tm_map(., toSpace, " will ")  %>%
  tm_map(., toSpace, " three ")  %>%
  tm_map(., stripWhitespace)


#Create Document-Term Matrices from the {tm} package
dtm <- DocumentTermMatrix(tmCleanCorpus)
# tdm <- TermDocumentMatrix(tmCleanCorpus)
dim(dtm)
inspect(dtm)

# Remove Sparsity
dtm <- dtm %>% removeSparseTerms(.8)
dim(dtm)
inspect(dtm)

# Unigrams
freq_dtm <- colSums(as.matrix(dtm)) %>%
  sort(decreasing = TRUE)

#Getting the top terms from unweighted DTM-matrix
most_frqnt_dtm <- head(freq_dtm,500)  %>%
  sort(decreasing = TRUE)
most_frqnt_dtm
write.csv(most_frqnt_dtm, "Results/Top_Ranking_Unigrams_DTM.csv")

# Route 2-2-A- TFIDF Weighting of dtm ----
dtmTFIDF <- weightTfIdf(dtm, normalize = TRUE)

# Unigrams TFIDF
freq_dtmTFIDF <- colSums(as.matrix(dtmTFIDF)) %>%
  sort(decreasing = TRUE)

#Getting the top 150 from TF-IDF weighted DTM-matrix
most_freq_TFIDF <- head(freq_dtmTFIDF,150)
most_freq_TFIDF
write.csv(most_freq_TFIDF, "Results/Top_Ranking_Unigrams_TF-IDF.csv")

# Finding any term's rank in the most frequent from dtm
match('aacsb', names(most_frqnt_dtm))

# Route 2-3- Pearson's Correlations ----

numNodes_dtm <- 26

cor_dtm <- cor(as.matrix(dtm))
boolean_dtm <- cor_dtm
boolean_dtm[boolean_dtm==1.0000] <- 0 # Removes loops
boolean_dtm[boolean_dtm>0] <- 1
boolean_dtm[boolean_dtm<=0] <- 0
boolean_dtm[1:15,1:15]
netGraph_dtm <- graph_from_adjacency_matrix(boolean[names(head(freq_dtm, numNodes_dtm)),names(head(freq_dtm, numNodes_dtm))], weighted=TRUE, mode="undirected")
# netGraph <- simplify(netGraph)
V(netGraph_dtm)$label <- V(netGraph_dtm)$name
V(netGraph_dtm)$degree <- degree(netGraph_dtm)
# La distribución espacial se basa en algoritmos dirigidos por fuerzas internas
set.seed(100)
layout1_dtm <- layout_with_fr(netGraph_dtm, start.temp = 21)
layout2_dtm <- layout.kamada.kawai(netGraph_dtm)
# Se da peso a cada nodo de acuerdo con sus conexiones:
DegreeScale_dtm <- V(netGraph_dtm)$degree / max(V(netGraph_dtm)$degree)
V(netGraph_dtm)$label.cex <- DegreeScale + .6
#egam <- (log((E(netGraph)$weight)/max(E(netGraph)$weight)))+.5
#E(netGraph)$color <- rgb(.2,.2,.2, egam)
#E(netGraph)$width <- egam
plot(netGraph_dtm,  rescale=T, layout=layout1_dtm*.30, edge.curved=0, vertex.frame.color=rgb(0,0,0,degree(netGraph_dtm)/max(V(netGraph_dtm)$degree)), vertex.color = rgb(1,1,1, degree(netGraph_dtm)/max(V(netGraph_dtm)$degree)), vertex.label.color="black", vertex.size = degree(netGraph_dtm, mode = "all")*2, )
tkplot(netGraph_dtm,  rescale=T, layout=layout1_dtm*.30, edge.curved=0, vertex.frame.color=rgb(0,0,0,degree(netGraph_dtm)/max(V(netGraph_dtm)$degree)), vertex.color = rgb(1,1,1, degree(netGraph_dtm)/max(V(netGraph_dtm)$degree)), vertex.label.color="black", vertex.size = degree(netGraph_dtm, mode = "all")*2)

# Se obtienen las correlaciones de Pearson para
# todos los términos de la matriz ponderada con TF-IDF
cor_2 <- cor(as.matrix(dtmTFIDF))
# A partir de la matriz de correlaciones Se establece una matriz booleana: toda
# correlación positiva es 1 y toda correlación negativa es cero. No es necesario
# generar la matriz producto boolean %*% t(boolean) ya que la función de
# correlación de Pearson lo hace
boolean <- cor_2
boolean[boolean==1.0000] <- 0 # Removes loops
boolean[boolean>0] <- 1
boolean[boolean<=0] <- 0
#Verificamos el resultado
boolean[1:15,1:15]

# Generamos un grafo de correlaciones entre los elementos de mayor rango estadístico usando {igraph}



netGraph <- graph_from_adjacency_matrix(boolean[names(head(freq_dtmTFIDF, numNodes)),names(head(freq_dtmTFIDF, numNodes))], weighted=TRUE, mode="undirected")
# netGraph <- simplify(netGraph)
V(netGraph)$label <- V(netGraph)$name
V(netGraph)$degree <- degree(netGraph)
# La distribución espacial se basa en algoritmos dirigidos por fuerzas internas
set.seed(100)
layout1 <- layout_with_fr(netGraph, start.temp = 12)
layout2 <- layout.kamada.kawai(netGraph)
# Se da peso a cada nodo de acuerdo con sus conexiones:
DegreeScale <- V(netGraph)$degree / max(V(netGraph)$degree)
V(netGraph)$label.cex <- DegreeScale + .6
#egam <- (log((E(netGraph)$weight)/max(E(netGraph)$weight)))+.5
#E(netGraph)$color <- rgb(.2,.2,.2, egam)
#E(netGraph)$width <- egam
plot(netGraph,  rescale=T, layout=layout1*.20, edge.curved=0, vertex.frame.color=rgb(0,0,0,degree(netGraph)/max(V(netGraph)$degree)), vertex.color = rgb(1,1,1, degree(netGraph)/max(V(netGraph)$degree)), vertex.label.color="black", vertex.size = degree(netGraph, mode = "all")*2, )

tkplot(netGraph,  rescale=T, layout=layout1*.30, edge.curved=0, vertex.frame.color=rgb(0,0,0,degree(netGraph)/max(V(netGraph)$degree)), vertex.color = rgb(1,1,1, degree(netGraph)/max(V(netGraph)$degree)), vertex.label.color="black", vertex.size = degree(netGraph, mode = "all")*2)



# Route 2-4 - Trigrams ----
TrigramTokenizer <-  function(x)  {
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = TRUE)
}

dtm3 <- DocumentTermMatrix(tmCleanCorpus, control = list(tokenize = TrigramTokenizer))

inspect(dtm3)

freq_dtm3_trigram <- colSums(as.matrix(dtm3)) %>%
  sort(decreasing = TRUE)

head(freq_dtm3_trigram,200)

small_dtm3 <- removeSparseTerms(dtm3, 0.8)

freq_small_dtm3 <- colSums(as.matrix(dtm3)) %>%
  sort(decreasing = TRUE)

head(freq_small_dtm3,100)

# Route 2-5 - TF-IDF Trigrams ----
tfidf_dtm3_trigrams <- weightTfIdf(dtm3, normalize = TRUE)
#tfidf_dtm3_trigrams <- removeSparseTerms(tfidf_dtm3_trigrams, .8)
dim(tfidf_dtm3_trigrams)
inspect(tfidf_dtm3_trigrams)

freq_tfidf_dtm3 <- sort(colSums(as.matrix(tfidf_dtm3_trigrams)), decreasing = T)
write.csv(freq_tfidf_dtm3,"Results/TF-IDF Trigram Frequencies2.csv")
head(freq_tfidf_dtm3,200)

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

# LDA topic modeling. K parameter varies from term analysis----

unigram_LDA_11 <- LDA(dtm, k = 11, control = list(seed = 975) )
unigram_LDA_10 <- LDA(dtm, k = 10, control = list(seed = 975) )
unigram_LDA_5 <- LDA(dtm, k = 5, control = list(seed = 975) )
unigram_LDA_4 <- LDA(dtm, k = 4, control = list(seed = 975) )
unigram_LDA_2 <- LDA(dtm, k = 2, control = list(seed = 975) )
unigram_LDA_15 <- LDA(dtm, k = 15, control = list(seed = 975) )



# Showing top "beta": per-topic-per-word from LDA model
UnigramTopics_15 <- tidy(unigram_LDA_15, matrix="beta")
UnigramTopics_11 <- tidy(unigram_LDA_11, matrix="beta")
UnigramTopics_10 <- tidy(unigram_LDA_10, matrix="beta")
UnigramTopics_5 <- tidy(unigram_LDA_5, matrix="beta")
UnigramTopics_4 <- tidy(unigram_LDA_4, matrix="beta")
UnigramTopics_2 <- tidy(unigram_LDA_2, matrix="beta")

top_topic_words <- function(ldamtrx, ntop) {
  ldamtrx %>%
    group_by(topic) %>%
    top_n(ntop, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

}

top_15 <- top_topic_words(UnigramTopics_15,10)
top_10 <- top_topic_words(UnigramTopics_10,5)
top_11 <- top_topic_words(UnigramTopics_11, 5)
top_5 <- top_topic_words(UnigramTopics_5, 10)
top_4 <- top_topic_words(UnigramTopics_4, 10)
top_2 <- top_topic_words(UnigramTopics_2, 10)

Plot_LDA_topics <- function(top_topic_words_var) {
  top_topic_words_var  %>%
        mutate(term = reorder(term, beta)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip()
}

Plot_LDA_topics(top_15)

# Topics: Most probable topic for each document ####

gammaDF <- as.data.frame(unigram_LDA_15@gamma)
row.names(gammaDF)
toptopics <- as.data.frame(cbind(document = row.names(gammaDF),
             topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))

toptopics

# Review of tidy term results ----
review <- tidy(dtm) %>%
  count(document, term, sort =TRUE)
