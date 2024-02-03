library(readr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(childesr)

# Total: 6 Corpuses, 58 children
# Providence Corpus, 6 children (3 girls, 3 boys)
pro <- get_utterances(role = "Mother", age = c(12,36), corpus = "Providence")
pro_utterance <- select(pro, corpus_name, id, gloss, target_child_name, target_child_id, target_child_age, target_child_sex, part_of_speech, corpus_id, collection_id, speaker_id, transcript_id)

# Gleason Corpus, 7 children (3 girls, 4 boys)
# Laurel, Martin, Nanette, Patricia, Richard, Victor, William
gleason <- get_utterances(role = "Mother", age = c(12,36), corpus = "Gleason")
gle_utterance <- select(gleason, corpus_name, id, gloss, target_child_name, target_child_id, target_child_age, target_child_sex, part_of_speech, corpus_id, collection_id, speaker_id, transcript_id)

# McCune Corpus, 10 children
mcc <- get_utterances(role = "Mother", age = c(12,36), corpus = "McCune")
mcc_utterance <- select(mcc, corpus_name, id, gloss, target_child_name, target_child_id, target_child_age, target_child_sex, part_of_speech, corpus_id, collection_id, speaker_id, transcript_id)

# Warren Corpus, 9 children (4 girls, 5 boys)
warren <- get_utterances(role = "Mother", age = c(12,36), corpus = "Warren")
wr_utterance <- select(warren, corpus_name, id, gloss, target_child_name, target_child_id, target_child_age, target_child_sex, part_of_speech, corpus_id, collection_id, speaker_id, transcript_id)

# VanHouten Corpus, 20 children (8 girls, 12 boys)
van <- get_utterances(role = "Mother", age = c(12,36), corpus = "VanHouten")
van_utterance <- select(van, corpus_name, id, gloss, target_child_name, target_child_id, target_child_age, target_child_sex, part_of_speech, corpus_id, collection_id, speaker_id, transcript_id)

# Weist Corpus, 6 children (3 girls, 3 boys)
weist <- get_utterances(role = "Mother", age = c(12,36), corpus = "Weist")
wst_utterance <- select(weist, corpus_name, id, gloss, target_child_name, target_child_id, target_child_age, target_child_sex, part_of_speech, corpus_id, collection_id, speaker_id, transcript_id)

# combined utterance
combined_utterance <- bind_rows(wst_utterance, van_utterance, wr_utterance,
                                mcc_utterance, gle_utterance, pro_utterance)

# filter utterances that include adj.
adj_utterance <- combined_utterance %>% 
  filter(grepl("adj", part_of_speech))

# filter utterances that include nouns.
noun_utterance <- combined_utterance %>% 
  filter(grepl("n", part_of_speech))

# export combined tibbles
write.csv(adj_utterance, file = "/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/adj_utterance.csv", row.names = FALSE)
write.csv(noun_utterance, file = "/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/noun_utterance.csv", row.names = FALSE)

## ======================================================================== ##

# tidied data files read-in

adj_utterance <- read_csv("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/tidy_adj_utterance.csv")
noun_utterance <- read_csv("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/tidy_noun_utterance.csv")

right_join()

# female adj. utterance
adj_female <- adj_utterance %>% 
  filter(grepl("female", target_child_sex))
write.csv(adj_female, file = "adj_female.csv", row.names = FALSE)

table(adj_utterance$target_child_sex)
## female adj. utterance:  29,498 obs.
##   male adj. utterance:  17,161 obs.

# female noun. utterance
n_female <- noun_utterance %>% 
  filter(grepl("female", target_child_sex))
write.csv(n_female, file = "n_female.csv", row.names = FALSE)

table(noun_utterance$target_child_sex)
## female noun. utterance:  98,396 obs.
##   male noun. utterance:  70,543 obs.

# check for NAs
which(is.na(adj_utterance$target_child_sex))
which(is.na(adj_utterance$target_child_name))

# what if the columns are blank??
adj_importNA <- read.csv("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/adj_utterance.csv", na = " ")

# female biased nouns: dress, doll, necklace, purse, baby, sweater, girl

# dress
fnoun_dress <- noun_utterance %>% 
  filter(grepl("dress", gloss))
write.csv(fnoun_dress, file = "fnoun_dress.csv", row.names = FALSE)

# doll
fnoun_doll <- noun_utterance %>% 
  filter(grepl("doll", gloss))
write.csv(fnoun_doll, file = "fnoun_doll.csv", row.names = FALSE)

# necklace
fnoun_necklace <- noun_utterance %>% 
  filter(grepl("necklace", gloss))
write.csv(fnoun_necklace, file = "fnoun_necklace.csv", row.names = FALSE)

# purse
fnoun_purse <- noun_utterance %>% 
  filter(grepl("purse", gloss))
write.csv(fnoun_purse, file = "fnoun_purse.csv", row.names = FALSE)

# baby
fnoun_baby <- noun_utterance %>% 
  filter(grepl("baby", gloss))
write.csv(fnoun_baby, file = "fnoun_baby.csv", row.names = FALSE)

# sweater
fnoun_sweater <- noun_utterance %>% 
  filter(grepl("sweater", gloss))
write.csv(fnoun_sweater, file = "fnoun_sweater.csv", row.names = FALSE)

# girl
fnoun_girl <- noun_utterance %>% 
  filter(grepl("girl", gloss))
write.csv(fnoun_girl, file = "fnoun_girl.csv", row.names = FALSE)

# male biased nouns: hammer, truck, firetruck, broom, shovel, motorcycle, train
# hammer
mnoun_hammer <- noun_utterance %>% 
  filter(grepl("hammer", gloss))
write.csv(mnoun_hammer, file = "mnoun_hammer.csv", row.names = FALSE)

# truck
mnoun_truck <- noun_utterance %>% 
  filter(grepl("truck", gloss))
write.csv(mnoun_truck, file = "mnoun_truck.csv", row.names = FALSE)

# firetruck
mnoun_firetruck <- noun_utterance %>% 
  filter(grepl("firetruck", gloss))
write.csv(mnoun_firetruck, file = "mnoun_firetruck.csv", row.names = FALSE)

# broom
mnoun_broom <- noun_utterance %>% 
  filter(grepl("broom", gloss))
write.csv(mnoun_broom, file = "mnoun_broom.csv", row.names = FALSE)

# shovel
mnoun_shovel <- noun_utterance %>% 
  filter(grepl("shovel", gloss))
write.csv(mnoun_shovel, file = "mnoun_shovel.csv", row.names = FALSE)

# motorcycle
mnoun_motorcycle <- noun_utterance %>% 
  filter(grepl("motorcycle", gloss))
write.csv(mnoun_motorcycle, file = "mnoun_motorcycle.csv", row.names = FALSE)

# train
mnoun_train <- noun_utterance %>% 
  filter(grepl("train", gloss))
write.csv(mnoun_train, file = "mnoun_train.csv", row.names = FALSE)

# competence-based adj.
# clever, smart
cadj_clever <- adj_utterance %>% 
  filter(grepl("clever", gloss))
write.csv(cadj_clever, file = "cadj_clever.csv", row.names = FALSE)

cadj_smart <- adj_utterance %>% 
  filter(grepl("smart", gloss))
write.csv(cadj_smart, file = "cadj_smart.csv", row.names = FALSE)

cadj_brilliant <- adj_utterance %>% 
  filter(grepl("brilliant", gloss))
write.csv(cadj_brilliant, file = "cadj_brilliant.csv", row.names = FALSE)

cadj_strong <- adj_utterance %>% 
  filter(grepl("strong", gloss))
write.csv(cadj_strong, file = "cadj_strong.csv", row.names = FALSE)

# warmth-based adj

## friendly
wadj_friendly <- adj_utterance %>% 
  filter(grepl("friendly", gloss))
write.csv(wadj_friendly, file = "wadj_friendly.csv", row.names = FALSE)

# attraction
## pretty
adj_pretty <- adj_utterance %>% 
  filter(grepl("pretty", gloss))
write.csv(adj_pretty, file = "adj_pretty.csv", row.names = FALSE)

## look good
adj_lookgood <- adj_utterance %>% 
  filter(grepl("look good", gloss))
write.csv(adj_lookgood , file = "adj_lookgood.csv", row.names = FALSE)

## beautiful
adj_beaut <- adj_utterance %>% 
  filter(grepl("beaut", gloss))
write.csv(adj_beaut, file = "adj_beaut.csv", row.names = FALSE)

## handsome
adj_handsome <- adj_utterance %>% 
  filter(grepl("handsome", gloss))
write.csv(adj_handsome, file = "adj_handsome.csv", row.names = FALSE)

## cute 
adj_cute <- adj_utterance %>% 
  filter(grepl("cute", gloss))
write.csv(adj_cute, file = "adj_cute.csv", row.names = FALSE)

## lovely
adj_lovely <- adj_utterance %>% 
  filter(grepl("lovely", gloss))
write.csv(adj_lovely, file = "adj_lovely.csv", row.names = FALSE)

## gorgeous
adj_gorgeous <- adj_utterance %>% 
  filter(grepl("gorgeous", gloss))
write.csv(adj_gorgeous, file = "adj_gorgeous.csv", row.names = FALSE)


## Assignment (9)
# 9.1 set working directory of all female-biased nouns
setwd("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/fnoun")

files <- list.files(pattern="*.csv")

all_fnouns <- data.frame()

## extracting "gloss" and "target_child_sex" from all fnoun files
for(file in files) {
  temp_data <- read.csv(file)
  selected_data <- temp_data %>%
    select(gloss, target_child_sex)
  all_fnouns <- rbind(all_fnouns, selected_data)
}

write.csv(all_fnouns, "combined_fnouns.csv", row.names = FALSE)

library(tidyr)

# calculating numbers of each male biased noun in "gloss"
number_fnoun <- read.csv("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/combined_fnouns.csv")
fnoun_words <- c("dress", "doll", "necklace", "purse", "baby", "sweater", "girl")
word_counts <- number_fnoun %>%
  mutate(gloss = tolower(gloss)) %>%
  separate_rows(gloss, sep = "\\s+") %>%
  filter(gloss %in% fnoun_words) %>%
  count(gloss, target_child_sex)
final_data <- pivot_wider(word_counts, names_from = target_child_sex, values_from = n, values_fill = list(n = 0))

# tidy data of female-biased nouns
write.csv(final_data, "counts_fnouns.csv", row.names = FALSE)

# 9.2 set working directory of all female-biased nouns
setwd("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/mnoun")

files <- list.files(pattern="*.csv")

all_mnouns <- data.frame()

## extracting "gloss" and "target_child_sex" from all mnoun files
for(file in files) {
  temp_data <- read.csv(file)
  selected_data <- temp_data %>%
    select(gloss, target_child_sex)
  all_mnouns <- rbind(all_mnouns, selected_data)
}

write.csv(all_mnouns, "combined_mnouns.csv", row.names = FALSE)

# calculating numbers of each male biased noun in "gloss"
number_mnoun <- read.csv("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/combined_mnouns.csv")
mnoun_words <- c("hammer", "truck", "firetruck", "broom", "shovel", "motorcycle", "train")
word_counts <- number_mnoun %>%
  mutate(gloss = tolower(gloss)) %>%
  separate_rows(gloss, sep = "\\s+") %>%
  filter(gloss %in% mnoun_words) %>%
  count(gloss, target_child_sex)
final_mdata <- pivot_wider(word_counts, names_from = target_child_sex, values_from = n, values_fill = list(n = 0))

# tidy data of male-biased nouns
write.csv(final_mdata, "counts_mnouns.csv", row.names = FALSE)

# 9.3 set working directory of all appearance-related adjs
setwd("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/adj./appearance adj")

files <- list.files(pattern="*.csv")

all_app_adj <- data.frame()

## extracting "gloss" and "target_child_sex" from all appearance adj. files
for(file in files) {
  temp_data <- read.csv(file)
  selected_data <- temp_data %>%
    select(gloss, target_child_sex)
  all_app_adj <- rbind(all_app_adj, selected_data)
}

write.csv(all_app_adj, "all_app_adj.csv", row.names = FALSE)

# calculating numbers of each app adj. in "gloss"
number_app_adj <- read.csv("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/adj./all_app_adj.csv")
app_adj <- c("beautiful","cute","gorgeous","handsome","lovely","pretty")
word_counts <- number_app_adj %>%
  mutate(gloss = tolower(gloss)) %>%
  separate_rows(gloss, sep = "\\s+") %>%
  filter(gloss %in% app_adj) %>%
  count(gloss, target_child_sex)
final_app_adj <- pivot_wider(word_counts, names_from = target_child_sex, values_from = n, values_fill = list(n = 0))

# tidy data of male-biased nouns
write.csv(final_app_adj, "final_app_adj.csv", row.names = FALSE)

# 9.4 set working directory of all competence-related adjs
setwd("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/adj./competence adj")

files <- list.files(pattern="*.csv")

all_com_adj <- data.frame()

## extracting "gloss" and "target_child_sex" from all appearance adj. files
for(file in files) {
  temp_data <- read.csv(file)
  selected_data <- temp_data %>%
    select(gloss, target_child_sex)
  all_com_adj <- rbind(all_com_adj, selected_data)
}

write.csv(all_com_adj, "all_com_adj.csv", row.names = FALSE)

# calculating numbers of each com adj. in "gloss"
number_com_adj <- read.csv("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/adj./all_com_adj.csv")
com_adj <- c("clever","brilliant","smart","strong")
word_counts <- number_com_adj %>%
  mutate(gloss = tolower(gloss)) %>%
  separate_rows(gloss, sep = "\\s+") %>%
  filter(gloss %in% com_adj) %>%
  count(gloss, target_child_sex)
final_com_adj <- pivot_wider(word_counts, names_from = target_child_sex, values_from = n, values_fill = list(n = 0))

# tidy data of male-biased nouns
write.csv(final_com_adj, "final_com_adj.csv", row.names = FALSE)

## *NEW:Appearance Related Adj.s Proportion*
library(dplyr)

# read data
appearance_adj_number <- read.csv('/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/tidy_adj_utterance.csv')

# Define words that refer to the appearance description
appearance_words <- c("handsome", "gorgeous", "beautiful", "lovely", "cute")

# Filter gloss that contains the specified word
filtered_appearance <- appearance_adj_number %>% 
  filter(sapply(appearance_words, function(word) grepl(word, gloss, ignore.case = TRUE)) %>% rowSums() > 0)

grouped_adj_number <- filtered_appearance %>%
  group_by(target_child_name) %>%
  summarise(gloss_count = n())

write.csv(grouped_adj_number, '/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/Proportion data/without_pretty_adj_number.csv', row.names = FALSE)


# PLOT
library(ggplot2)
proportion_appearance <- read.csv("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/Proportion data/adj_number_proportions.csv")
ggplot(proportion_appearance, aes(x = target_child_sex, y = appearance_proportion, fill = target_child_sex)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("female" = "lightpink", "male" = "lightblue")) +
  coord_cartesian(ylim = c(0, 0.1)) +
  labs(x = "Gender", y = "Proportion", title = "Appearance Related Adj. Proportion") +
  theme_minimal()

ggplot(proportion_appearance, aes(x = target_child_sex, y = without_pretty_proportion, fill = target_child_sex)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("female" = "lightpink", "male" = "lightblue")) +
  labs(x = "Gender", y = "Proportion", title = "Without Pretty Proportion") +
  theme_minimal()

## Female-biased Nouns Proportion

# read data
fnoun_number <- read.csv('/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/tidy_noun_utterance.csv')

# Define words that refer to female-biased nouns
fnoun_words <- c("dress", "doll", "necklace", "purse", "baby", "sweater", "girl")

# Filter gloss that contains the specified Female-Biased Nouns
filtered_fnoun <- fnoun_number %>% 
  filter(sapply(fnoun_words, function(word) grepl(word, gloss, ignore.case = TRUE)) %>% rowSums() > 0)

grouped_fnoun_number <- filtered_fnoun %>%
  group_by(target_child_name) %>%
  summarise(gloss_count = n())

write.csv(grouped_mnoun_number, '/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/Proportion data/grouped_fnoun_number.csv', row.names = FALSE)

## 02/03/2024 *NEW: Male-biased Nouns Proportion*
mnoun_number <- read.csv('/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/tidy_noun_utterance.csv')

# Define words that refer to male-biased nouns
mnoun_words <- c("broom", "firetruck", "truck", "train", "hammer", "motorcycle", "shovel")

# Filter gloss that contains the specified Male-Biased Nouns
filtered_mnoun <- mnoun_number %>% 
  filter(sapply(mnoun_words, function(word) grepl(word, gloss, ignore.case = TRUE)) %>% rowSums() > 0)

grouped_mnoun_number <- filtered_mnoun %>%
  group_by(target_child_name) %>%
  summarise(gloss_count = n())

write.csv(grouped_mnoun_number, '/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/Proportion data/grouped_mnoun_number.csv', row.names = FALSE)

## *Proportion Calculation*
noun_number_data <- read.csv("/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/Proportion data/noun_number_data.csv")

# 计算 mnoun_proportion 并添加到数据框中
noun_number_data <- noun_number_data %>%
  mutate(mnoun_proportion = mnoun_count / gloss_count) %>%
  mutate(fnoun_proportion = fnoun_count / gloss_count)


# 导出修改后的数据框到新的 CSV 文件
write.csv(noun_number_data, "/Users/katia/Desktop/D2M/CHILDES_utterance/Childes dataset/Proportion data/noun_proportion_data.csv", row.names = FALSE)
