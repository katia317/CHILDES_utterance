---
output:
  html_document: default
  pdf_document: default
---
# CHILDES_utterance
Class (public) repo for Data to Manuscript in R

## Dataset Description
This dataset contains transcripts of 6 corpuses (Providence, Warren, Gleason, MeCune, VanHouten and Weist) selected from the *CHILDES* database, which record naturalistic conversations between 58 monolingual Native English-speaking children (1-3 years old) and their mothers. 

## Planned Project 

**Research Question**: It is expected that mothers will employ more female-biased nouns and appearance-related adjectives when addressing female infants compared to male infants, and employ more male-biased nouns and competence-related adjectives when addressing male infants.

$Gender-biased nouns$: 14 Nouns (7 for each gender) extracted from the two lists “Top 10 most female/male biased words in English (America) for comprehension data” of the Wordbank data report (2016) will be treated as gender-biased language data.  

$Appearance-related adjectives$: "beautiful","cute","gorgeous","handsome","lovely","pretty" 

$Competence-related adjectives$: "clever","brilliant","smart","strong"


## File Tree
```
--[CHILDES-Utterances] 
   -- README.md 
   -- .gitignore
   -- My_Script.R  
-- Data Analysis
   -- My Notebook.Rmd
   -- My-Notebook.pdf
   -- data-prep.R
-- Dataset
   -- /Childes dataset  
        -- /adj.  
        -- /fnoun 
        -- /mnoun 
        -- /Proportion data  
        -- /tidy data
        -- adj_utterance.csv
        -- noun_utterance.csv
```
        
 
