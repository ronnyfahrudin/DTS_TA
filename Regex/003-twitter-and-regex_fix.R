# Load packages -----------------------------------------------------------
#install.packages(c('remotes','tidyverse','vroom','lubridate','scale','hrbrthemes'))
library(tidyverse)
library(vroom)
library(lubridate)
library(scales)
library(hrbrthemes)
library(remotes)
#remotes::install_github('gadenbuie/regexplain')
# Pemanasan Regex --------------------------------------------

#1. regex "^[aAeEiloOuU].+"finding vocal prefix 
v1 <- 'abad'
v2 <- '3ra'
v3 <- 'UT|AR4'
v4 <- 'Padi67'
v5 <- 'Bakso'
v6 <- 'Boso'
v7 <- 'Padang'
ls <- c(v1,v2,v3,v4,v5,v6,v7)
ls
o <- str_extract(ls, "^[aAeEiIoOuU].+")
o

#2. mencari Akhiran kata ".+so$"

akh <- ".+so$"
ak <- str_extract(ls, akh)
ak[!is.na(ak)]

# 3. awalan kata ada ".+Pa"
p <- "Pa.+"
aw <-  str_extract(ls,p)
aw

# 4. mencari metacharacter
mc <- ".+\\|+."
m <-  str_extract(ls,mc)
m
# 5. mengganti value 
gsub('\\|','',ls)

# 6. mencari digit angka
s <- c('tjahbsd87','asbd799','hahah')
str_extract(s, '.+\\d?.+')
# 7. mencari huruf aja
str_extract(s,'\\d+.')
s_alpha<-gsub('\\d+.','',s)
s_alpha

#######################  STUDY CASE ################################

# Fetch nuicemedia tweets using twint -----------------------------------

#' Gather tweets from @nuicemedia using twint.

system("twint -u nuicemedia -o /nuicemedia.csv --csv")

# Import data -------------------------------------------------------------

nuicemedia_raw <- vroom("E:/Karir/Mentoring dan Pemateri/Regex/nuicemedia.csv")

# seeing dataset
glimpse(nuicemedia_raw)

# Simplify data by only selecting some columns ----------------------------

nuicemedia <-
  nuicemedia_raw %>%
  distinct(id, .keep_all = TRUE) %>%
  select(created_at, date, time, tweet, ends_with("count")) %>% # pilih column yang dibutuhkan
  mutate(
    created_at = ymd_hms(created_at),
    tweet = str_to_lower(tweet),
    contain_vax = str_detect(tweet, "vax")
  )

glimpse(nuicemedia)

# Some trivia exploration, shall we? --------------------------------------

#' what as @nuicemedia said on its first tweet?

nuicemedia %>%
  filter(created_at == min(created_at)) %>%
  pull(tweet)

#' is that the case? Let's check tweets' length distribution!
nuicemedia %>%
  ggplot(aes(nchar(tweet))) +
  geom_histogram(fill = "#3BC14A", colour = "#3BC14A") +
  geom_vline(xintercept = 280, linetype = "dashed", colour = "#642CA9") +
  annotate(
    geom = "text",
    x = 278,
    y = 10,
    label = "max length\n(280 chars)",
    hjust = "right",
    vjust = "bottom",
    lineheight = 0.8,
    colour = "white",
    fontface = "italic",
    size = 3
  ) +
  labs(
    x = "Number of tweets' characters",
    y = "Frequency",
    title = "@nuicemedia tweets' length distribution"
  ) +
  theme_modern_rc(grid = "Y", ticks = TRUE)

#' Hmm, we find that some tweets are larger that twitter character limit. Why so? Let's those tweets and sort the top 10 tweets in terms of number of characters. Do you find the cause by looking at tweets' text?

nuicemedia %>%
  filter(nchar(tweet) > 280) %>%
  slice_max(order_by = nchar(tweet), n = 10) %>%
  pull(tweet)

#' Next, how about number of daily tweets over time?

nuicemedia %>%
  count(date) %>%
  ggplot(aes(date, n)) +
  geom_col(fill = "#3BC14A", colour = "#3BC14A") +
  labs(
    x = NULL,
    y = NULL,
    title = "@nuicemedia tweets count over time"
  ) +
  theme_modern_rc(grid = "Y", ticks = TRUE)

#' How if you explore the retweets_counts, replies_count, and likes_count?
nuicemedia %>%
  count(date, wt = retweets_count, name = "retweets_count") %>%
  ggplot(aes(date, retweets_count)) +
  geom_col(fill = "#3BC14A", colour = "#3BC14A")

nuicemedia %>%
  group_by(date) %>%
  summarise(
    retweets_count = sum(retweets_count),
    ntweet = n()
  )%>%
  ungroup() %>%
  mutate(retweets_rate = retweets_count/ntweet)%>%
  ggplot(aes(date, retweets_rate)) +
  geom_line(colour = "#3BC14A") +
  labs (
    x = NULL,
    y = NULL,
    title = '@nuicemedia retweets rate count over time'
  ) +
  theme_ipsum(grid = "y")
  
# Extract vaccine data from tweets ----------------------------------------
#' Knowing that @nuicemedia consistently tweeting about vaccination status in Indonesia, it might be interesting for us to extract the data.
head(nuicemedia$tweet)
nuicemedia_vaxrecap <-
  nuicemedia %>%
  filter(contain_vax == TRUE) %>%
  transmute(
    date,
    time,
    tweet,
    vax1 = str_extract(tweet, "(?<=vax\\s?1?\\s?:\\s?)[\\d,\\.]+"),
    vax2 = str_extract(tweet, "(?<=vax\\s?2\\s?:\\s?)[\\d,\\.]+"),
  ) %>%
  drop_na() %>%
  mutate(
    across(vax1:vax2, ~ str_remove_all(.x, ",|\\.")),
    across(vax1:vax2, parse_number)
  ) %>%
  arrange(date)

nuicemedia_vaxrecap 
#' Now let's take a look by visualizing it!

nuicemedia_vaxrecap %>%
  ggplot(aes(date)) +
  geom_line(aes(y = vax1, colour = "Dosage 1")) +
  geom_line(aes(y = vax2, colour = "Dosage 2")) +
  scale_y_continuous(labels = number_format()) +
  labs(
    x = NULL,
    y = 'Dosage',
    colour = NULL,
    title = "@nuicemedia's Indonesian vaccination recap"
  )

#' Something is off, isn't it? Aside off missing data, is there any other possible issues? Data duplication, maybe?

nuicemedia_vaxrecap %>%
  count(date, sort = TRUE)

nuicemedia_vaxrecap %>%
  add_count(date, sort = TRUE) %>%
  filter(n > 1)

#' Now let's fix it and also calculate % of coverage of each dosage. Indonesian vaccination target is 208,265,720 (https://vaksin.kemkes.go.id/#/vaccines).

nuicemedia_vaxrecap <-
  nuicemedia_vaxrecap %>%
  group_by(date) %>%
  filter(time == max(time)) %>%
  ungroup() %>%
  mutate(
    across(vax1:vax2, list(coverage = ~.x / 208265720))
  ) %>%
  select(-time, -tweet)

nuicemedia_vaxrecap %>%
  add_count(date, sort = TRUE) %>%
  filter(n > 1)
#' Now let's re-visualize it! Do you find any differences?

nuicemedia_vaxrecap %>%
  ggplot(aes(date)) +
  geom_line(aes(y = vax1, colour = "Dosage 1")) +
  geom_line(aes(y = vax2, colour = "Dosage 2")) +
  scale_y_continuous(labels = number_format()) +
  labs(
    x = NULL,
    y = 'Dosage',
    colour = NULL,
    title = "@nuicemedia's Indonesian vaccination recap"
  )

#' Challenge: what would you do to make the previous graph better?

# Save the data -----------------------------------------------------------

write_csv(nuicemedia_vaxrecap, "lokasi_datamu/nuicemedia_vaxrecap.csv")
write_rds(nuicemedia_vaxrecap, "lokasi_datamu/nuicemedia_vaxrecap.rds")
