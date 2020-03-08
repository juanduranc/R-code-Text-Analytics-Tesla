library(textreadr)
library(dplyr)
library(tidytext)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(twitteR)
library(tm)
library(stringr)
library(plotly)
library(shinyjs)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(wordcloud)
library(textdata)
library(tidyverse)
library(sqldf)

# EXTRACTIND DATA FROM TWITTER: READ THE FOLLOWING EXCEL FILE
brand_tesla <- read_csv("C:/Users/Juanse/Desktop/Hult/classes/semester 2/002 text analytics/z projects/datasets/brand_tesla_db.csv")

# PERFORMING NRC SENTIMET ANALYSIS

# tokenize data for analysis
tidy_tesla <- brand_tesla %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% count(sentiment, sort=T) %>% mutate(sentiment = reorder(sentiment, n))

ggplot(data=tidy_tesla, aes(x= reorder(sentiment, n), y=n, fill=sentiment)) +
  geom_bar(stat="identity") + geom_text(aes(label = n, y = n-0.6), size = 6) + xlab("") +
  ylab("frequency") + ggtitle(paste("Frequency of Emotions:", "Tesla")) +
  theme(legend.title = element_text(size=10, face="bold")) + coord_flip()


# TABLE Details about NRC sentiments
brand_tesla %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort=T)

# Content of each sentiment with regard to specific words
text_column <- brand_tesla %>% select(text)
text_column %>% filter(str_detect(text, "dumb"))
text_column %>% filter(str_detect(text, "trump"))
text_column %>% filter(str_detect(text, "model"))
text_column %>% filter(str_detect(text, "john"))
text_column %>% filter(str_detect(text, "battery"))
text_column %>% filter(str_detect(text, "time"))

# Removing unnecesary words ---------------------------------------------------------------------------------------
custom_stop_words <- bind_rows(tibble(word = c("john", "Elon", "Musk", "@elonmusk", "battery", "coronavirus", "panic", "dumb", "Nikola", "nikola"), lexicon = 'nol'), stop_words)
tesla_filtered <- filter(brand_tesla, !grepl('john|Elon|Musk|@elonmusk|coronavirus|panic|dumb|Nikola|nikola', brand_tesla$text))
tidy_tesla2 <- tesla_filtered %>% unnest_tokens(word, text) %>% anti_join(custom_stop_words) %>%
  inner_join(get_sentiments("nrc")) %>% count(sentiment, sort=T) %>% mutate(sentiment = reorder(sentiment, n))

ggplot(data=tidy_tesla2, aes(x= reorder(sentiment, n), y=n, fill=sentiment)) +
  geom_bar(stat="identity") + geom_text(aes(label = n, y = n-0.6), size = 6) + xlab("") +
  ylab("frequency") + ggtitle(paste("Frequency of Emotions:", "Tesla")) +
  theme(legend.title = element_text(size=10, face="bold")) + coord_flip()


# TABLE Details about NRC sentiments
tf <- tesla_filtered %>% unnest_tokens(word, text) %>% anti_join(custom_stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort=T) %>% mutate(sentiment = reorder(sentiment, n))

sqldf("select count(distinct(word)) from tf")
# Content of each sentiment with regard to specific words
text_column <- tesla_filtered %>% select(text)
text_column %>% filter(str_detect(text, "time"))
text_column %>% filter(str_detect(text, "trump"))
text_column %>% filter(str_detect(text, "model"))
text_column %>% filter(str_detect(text, "john"))
text_column %>% filter(str_detect(text, "battery"))
text_column %>% filter(str_detect(text, "time"))


# Part 2: removing words  ---------------------------------------------------------------------------------------
#custom_stop_words <-""
custom_stop_words <- bind_rows(tibble(word = c("john", "Elon", "Musk", "@elonmusk", "battery", "coronavirus", "panic", "dumb", "Nikola", "nikola", "model","trump", "president"), lexicon = 'nol'), stop_words)
tesla_filtered3 <- filter(brand_tesla, !grepl('john|Elon|Musk|@elonmusk|coronavirus|panic|dumb|Nikola|nikola|model|president|trump', brand_tesla$text))
tidy_tesla3 <- tesla_filtered %>% unnest_tokens(word, text) %>% anti_join(custom_stop_words) %>%
  inner_join(get_sentiments("nrc")) %>% count(sentiment, sort=T) %>% mutate(sentiment = reorder(sentiment, n))

ggplot(data=tidy_tesla3, aes(x= reorder(sentiment, n), y=n, fill=sentiment)) +
  geom_bar(stat="identity") + geom_text(aes(label = n, y = n-16), size = 6) + xlab("") +
  ylab("frequency") + ggtitle(paste("Frequency of Emotions:", "Tesla")) +
  theme(legend.title = element_text(size=10, face="bold")) + coord_flip() + theme_dark()


# TABLE Details about NRC sentiments
tf2 <- tesla_filtered3 %>% unnest_tokens(word, text) %>% anti_join(custom_stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort=T) %>% mutate(sentiment = reorder(sentiment, n))
View(tf2)
sqldf("select count(distinct(word)) from tf2")
# Content of each sentiment with regard to specific words
text_column3 <- tesla_filtered3 %>% select(text)
text_column3 %>% filter(str_detect(text, "worth"))
text_column3 %>% filter(str_detect(text, "damn"))
text_column3 %>% filter(str_detect(text, "dark"))
text_column3 %>% filter(str_detect(text, "hot"))
text_column3 %>% filter(str_detect(text, "center"))
text_column3 %>% filter(str_detect(text, "time"))
text_column3 %>% filter(str_detect(text, "boy"))



# EXTRA AFINN SENTIMENT ANALYSIS  ----------------------------------------------------------------------------------------------

afinn_tesla <- brand_tesla %>% unnest_tokens(word, text) %>% anti_join(custom_stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% count(word, value, sort=T) %>%
  ungroup() %>% mutate(score = n*value) %>% mutate(absolute = abs(n*value))

afinn_tesla$sentiment = case_when(afinn_tesla$score > 0 ~ 'positive', afinn_tesla$score < 0 ~ 'negative')
afinn_tesla <- afinn_tesla %>% arrange(desc(absolute)) %>% slice(1:10)
maxx <- as.integer(max(afinn_tesla$absolute)) + 10

affin_tesla_charts <- afinn_tesla %>% mutate(word = reorder(word, absolute)) %>%
  ggplot(aes(word, absolute, color = sentiment, label = absolute)) +
  geom_segment(aes(x = word, xend = word, y = 0, yend = absolute), show.legend = FALSE) +
  geom_point(aes(x = word, y = absolute, size = 3), show.legend = FALSE) + geom_text(vjust = 0, nudge_y = 2.5, show.legend = FALSE, size = 8) +
  ylim(0, maxx) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "score axis", x = NULL, title = paste("Affin sentiment on:", "Tesla"), caption = "SCORE = FREQUENCY * VALUE") +
  coord_flip() + theme_dark()

# -------
