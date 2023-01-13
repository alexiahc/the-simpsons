library(readr)
library(plyr)
library(purrr)
library(readr)
library(tidyverse)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(textdata)
library(reshape2)
library(RWeka)
library(knitr)
library(gridExtra)
library(grid)
library(magick)
library(igraph)
library(ggraph)
library("ggsci")
library(devtools)
library(circlize)
library(radarchart)
library(dplyr)
# library(plyr)
# library(purrr)
# library(readr)
# library(tm)
# library(RColorBrewer)
# library(wordcloud2)
# library(textdata)
# library(RWeka)
# library(knitr)
# library(gridExtra)
# library(magick)
# library(igraph)
# library(ggraph)
# library("ggsci")
# library(devtools)
# library(radarchart)
# library(RCurl)

characters <- read_csv("data/simpsons_characters.csv")
episodes <- read_csv("data/simpsons_episodes.csv")
locations <- read_csv("data/simpsons_locations.csv")
script_lines <- read_csv("data/simpsons_script_lines.csv")
bing <- read_csv("data/Bing.csv")
nrc <- read_csv("data/NRC.csv")
afinn <- read_csv("data/Afinn.csv")

women <- subset(characters, gender=='f')
men <- subset(characters, gender=='m')
ratio_nb_women <- length(women$id) / (length(women$id) + length(men$id))

list_names <- str_replace_all(women$normalized_name, 
                              "ms |miss |mrs |little |female |madam |lady |dr |princess ", "")
list_names <- strsplit(list_names, " ")
list_names_len <- sapply(list_names, length)
sup <- function(x){x>1}
list_names_len <- sapply(list_names_len, sup)
nb_complete_names <- sum(list_names_len)

script_char <- script_lines %>% group_by(character_id)
script_lines <- script_lines %>% rename(line_id = "id")
characters <- characters %>% rename(character_id = "id",
                                    character_name = "name",
                                    character_norm_name = "normalized_name")
data <- merge(script_lines, characters, by= "character_id")
locations <- locations %>% rename(location_id = "id",
                                  location_name = "name",
                                  location_norm_name = "normalized_name")
data <- merge(data, locations, by= "location_id")
episodes <- episodes %>% rename(episode_id = "id",
                                  episode_title = "title")
data <- merge(data, episodes, by= "episode_id")

data <- subset(data, speaking_line == TRUE)
data <- subset(data, select = -c(number, raw_text, raw_character_text,
                                 raw_location_text, spoken_words, character_name,
                                 location_name, speaking_line))

data$word_count <- as.numeric(data$word_count)
data$word_count[is.na(data$word_count)] <- 0 
data_women <- subset(data, gender=='f')
data_men <- subset(data, gender=='m')

# option choose the season ? the year ? 

# time of speaking
ratio_women_time <- sum(data_women$timestamp_in_ms) / (sum(data_women$timestamp_in_ms) 
                                                       + sum(data_men$timestamp_in_ms))

# mean time of speaking per lines
ratio_women_time_mean <- mean(data_women$timestamp_in_ms) / (mean(data_women$timestamp_in_ms) + 
                                                               mean(data_men$timestamp_in_ms))

# number of lines 
ratio_women_nb_lines <- length(data_women$line_id) / (length(data_women$line_id) + 
                                                        length(data_men$line_id))

data_fm <- subset(data, gender=="f" | gender=="m")
data_fm %>% 
  # prepare the table
  count(character_norm_name) %>%
  arrange(desc(n)) %>% 
  slice(1:15) %>%
  merge(characters, by= "character_norm_name") %>%
  arrange(desc(n)) %>% 
  
  # the plot
  ggplot(aes(x=reorder(character_norm_name, n), y=n, color=gender)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=F) +
  geom_label(aes(label=n)) +
  scale_fill_gradient(low="#33D1FF", high="#3383FF") +
  scale_color_manual(values=c("#F8B122", "#5DD241")) +
  labs(x="Character", y="Number of dialogues", title="Most talkative in The Simpsons") +
  coord_flip() +
  theme_bw()


tokens <- data %>% 
  mutate(dialogue = as.character(data$normalized_text)) %>% 
  unnest_tokens(word, dialogue)

tokens %>% head(5) %>% select(character_norm_name, word)

tokens %>% 
  # append the bing sentiment and prepare the data
  inner_join(bing, "word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  
  # wordcloud
  comparison.cloud(colors=c("#991D1D", "#327CDE"), max.words = 100)

to_plot <- tokens %>% 
  # get 'bing' and filter the data
  inner_join(bing, "word") %>% 
  filter(character_norm_name %in% c("homer simpson", "marge simpson", "lisa simpson", "bart simpson")) %>% 
  
  # sum number of words per sentiment and character
  count(sentiment, character_norm_name) %>% 
  group_by(character_norm_name, sentiment) %>% 
  summarise(sentiment_sum = sum(n)) %>% 
  ungroup()

# The Chord Diagram  
circos.clear()
circos.par(gap.after = c(rep(2, length(unique(to_plot[[1]])) - 1), 15,
                         rep(2, length(unique(to_plot[[2]])) - 1), 15), gap.degree=2)

myColors = c("homer simpson" = "#FA8072", "marge simpson" = "#04700A", "lisa simpson" = "#75A9F3", "bart simpson" = "#FADD71", "positive" = "#D7DBDD", "negative" = "#D7DBDD")

chordDiagram(to_plot, grid.col = myColors, transparency = 0.4, annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.01, 0.02))
title("Relationship between mood and the Simpson family")

tokens_women <- data_women %>% 
  mutate(dialogue = as.character(data_women$normalized_text)) %>% 
  unnest_tokens(word, dialogue)

sentiments_women <- tokens_women %>% 
  inner_join(nrc, "word") %>%
  count(sentiment, sort=T)
sentiments_women$gender <- "f"

tokens_men <- data_men %>% 
  mutate(dialogue = as.character(data_men$normalized_text)) %>% 
  unnest_tokens(word, dialogue)

sentiments_men <- tokens_men %>% 
  inner_join(nrc, "word") %>%
  count(sentiment, sort=T)
sentiments_men$gender <- "m"

sentiments <- rbind(sentiments_women, sentiments_men)

p1 <- ggplot(sentiments[sentiments$gender == 'f',], aes(x=sentiment, y= n)) + geom_col(fill='#E69F53') + theme_minimal()+
  coord_flip() + scale_y_reverse(name= "Women",expand = expand_scale(mult= c(c(0.05,0)))) +
  theme(panel.spacing.x = unit(0, "mm")) +theme(plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt"))

p2 <- ggplot(sentiments[sentiments$gender == 'm',], aes(x=sentiment, y= n)) + geom_col(fill='#40B742') + 
  scale_y_continuous(name = "Men", breaks = seq(0.025, 0.125, 0.025) ,expand = expand_scale(mult= c(c(0,0.05)))) +
  coord_flip() +
  theme(panel.spacing.x = unit(0, "mm"))+ theme_minimal() +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.line.y = element_blank(), axis.ticks.y=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, -3.5), "pt"))

grid.newpage()
grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))


tokens <- data %>% 
  mutate(dialogue = as.character(data$normalized_text)) %>% 
  unnest_tokens(word, dialogue)

sentiments <- tokens %>% 
  inner_join(nrc, "word") %>%
  count(sentiment, sort=T)

# The plot:
sentiments %>% 
  ggplot(aes(x=reorder(sentiment, n), y=n)) +
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=F) +
  geom_label(label=sentiments$n) +
  labs(x="Sentiment", y="Frequency", title="How is the overall mood") +
  coord_flip() + 
  theme_bw() 



ratios_nb_word <- sum(data_women$word_count) / (sum(data_women$word_count) 
                                                + sum(data_men$word_count))

df_ratios <- data.frame(criteria=c("nb characters", "nb lines", 
                             "speaking time", "mean speaking time", 
                             "word count"),
                     ratio=c(ratio_nb_women, ratio_women_nb_lines,
                              ratio_women_time, ratio_women_time_mean, 
                              ratios_nb_word))
ggplot(data=df_ratios, aes(x=criteria, y=ratio)) +
  geom_bar(stat="identity", fill ="#E69F53") 

library(seededlda)
library(quanteda)
test <- data %>% 
  group_by(episode_id) %>% 
  mutate(text_by_ep = paste0(normalized_text, collapse = "")) 

toks_news <- tokens(test$text_by_ep,
                    remove_punct = TRUE, 
                    remove_numbers = TRUE, 
                    remove_symbol = TRUE)


toks_news <- tokens_remove(toks_news,
                           pattern = c(stopwords("en"), 'from', 'subject', 're', 'edu', 
                                       'use', 'not', 'would', 'say', 'could', '_', 'be', 
                                       'know', 'good', 'go', 'get', 'do', 'done', 'try', 
                                       'many', 'some', 'nice', 'thank', 'think', 'see', 
                                       'rather', 'easy', 'easily', 'lot', 'lack', 'make', 
                                       'want', 'seem', 'run', 'need', 'even', 'right', 
                                       'line', 'even', 'also', 'may', 'take', 'come', 'ive', 
                                       'thats', 'youve', 'ill', 'one', 'two', 'three', 'four',
                                       'five', 'six', 'seven', 'eight', 'nine', 'ten', 'im', 
                                       'lets', 'whats', 'youre', 'going', 'gonna', 'mr', 
                                       'dont', 'id', 'hes', 'can', 'cant', 'got', 'youll',
                                       'didnt', 'theyre', 'hey', 'ya', 'okay', 'oh', 'yeah', 
                                       'ooh', 'hello', 'uh', 'ah', 'huh', 'youll', 'theres', 
                                       'whoa', 'cmon', 'ha', 'wont', 'shes', 'eh', 'em', 
                                       'ow', 'gotta', 'la', 'woo', 'hoo', 'hi', 'mrs', 
                                       'haw', 'ho', 'da'))

nfeat(dfm(toks_news))
dfmat_news <- dfm(toks_news) %>%
  dfm_trim(min_termfreq = 0.99, termfreq_type = "quantile",
           max_docfreq = 0.10,  docfreq_type = "prop")
nfeat(dfmat_news)
topfeatures(dfmat_news, nfeat(dfmat_news))
tmod_lda <- textmodel_lda(dfmat_news,
                          k = 10)
terms(tmod_lda, 10)
dfmat_news$topic <- topics(tmod_lda)
table(dfmat_news$topic)

library(tm)
dtm <- DocumentTermMatrix(toks_news)
dtm <- removeSparseTerms(dtm, 0.99)
nRows <- apply(dtm , 1, sum)
dtm <- dtm[nRows> 0, ]
dtm_tfxidf <- weightTfIdf(dtm)

m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)

library(caret)
preproc <- preProcess(m)
m_norm <- predict(preproc, m)
cl <- kmeans(m_norm, centers = 3)
table(cl$cluster)
plot(prcomp(m_norm)$x, col=cl$cl)
