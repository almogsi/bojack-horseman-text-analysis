library(xml2)
library(rvest)
library(stringr)

#data frame pre-allocation
m <- matrix(nrow = 5*10, ncol = 3)
colnames(m) <- c("season", "episode", "text")
m <- as.data.frame(m)

count = 1
for (i in 1:5){
  for (j in 1:10){
    if (j<10){
      scrappedurl <-paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=bojack-horseman-2014&episode=s0",
                           i, "e0", j)
      html.raw <- read_html(scrappedurl)
      n <- html_nodes(html.raw, "div#content_container")
      txt <- html_text(n)
      txt <- str_replace_all(txt, "[\r\n\t]" , "")
      t <- str_split(txt, "Episode Script", simplify = T)[3]
      m$season[count] <- i
      m$episode[count] <- j
      m$text[count] <- t
    }
    else{
      scrappedurl <-paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=bojack-horseman-2014&episode=s0",
                           i, "e", j)
      html.raw <- read_html(scrappedurl)
      n <- html_nodes(html.raw, "div#content_container")
      txt <- html_text(n)
      txt <- str_replace_all(txt, "[\r\n\t]" , "")
      t <- str_split(txt, "Episode Script", simplify = T)[3]
      m$season[count] <- i
      m$episode[count] <- j
      m$text[count] <- t
    }
    count <-  count+1
  }
}

library(kableExtra)
#let's see the dataframe
kable(m) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px")


library(tidyverse)
library(TATE)
m <- m %>%
  mutate(id = 1:50) %>%
  group_by(id) %>%
  mutate(concrete = concretness(text))

#let's look at the concretness ratings by episode
library(extrafont)
loadfonts()
theme_set(theme_light(base_size = 15, base_family = "Candara"))
g <- ggplot(m) +
  geom_line(aes(id, concrete)) + scale_x_continuous(breaks = seq(5,50,5)) + 
  geom_smooth(aes(id, concrete), method = "lm") + 
  labs(x = "Episode id and use a pretty font", y = "Concreteness Score") +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "Candara", size = 10),
        panel.grid = element_blank())

g

library(tidytext)
library(tm)
season_01 <- m %>%
  ungroup() %>%
  filter(season==1) %>%
  mutate(clean_text = removeNumbers(text),
         clean_text = tolower(clean_text),
         clean_text = removePunctuation(clean_text),
         clean_text = stripWhitespace(clean_text)) %>%
  unnest_tokens(word,clean_text) %>%
  anti_join(get_stopwords()) %>%
  count(word, sort=T) %>%
  mutate(s01.prop = n/sum(n))

season_05 <- m %>%
  ungroup() %>%
  filter(season==5) %>%
  mutate(clean_text = removeNumbers(text),
         clean_text = tolower(clean_text),
         clean_text = removePunctuation(clean_text),
         clean_text = stripWhitespace(clean_text)) %>%
  unnest_tokens(word,clean_text) %>%
  anti_join(get_stopwords()) %>%
  count(word, sort=T) %>%
  mutate(s05.prop = n/sum(n))

merged <- full_join(season_01, season_05, by = "word") %>%
  mutate(s01.prop = ifelse(is.na(s01.prop), 0, s01.prop),
         s05.prop = ifelse(is.na(s05.prop), 0, s05.prop)) %>%
  mutate(diff = s01.prop-s05.prop,
         color = ifelse(diff>0, "blue", "red"),
         abs = abs(diff)) %>%
  arrange(desc(abs))


#plot word cloud to visualize differences
library(wordcloud)
wordcloud(words = merged$word, freq = merged$abs, min.freq =0,
          max.words=40, random.order=FALSE, rot.per=0.35, 
          colors=merged$color, ordered.colors=TRUE)

