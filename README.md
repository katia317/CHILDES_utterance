---
output:
  html_document: default
  pdf_document: default
---
# CHILDES_utterance
Class (public) repo for Data to Manuscript in R

## Dataset Description
This dataset comes from the Phonbank English Providence Corpus from 2002-2005 by Katherine Demuth and her research assistants at Brown University. (https://phon.talkbank.org/access/Eng-NA/Providence.html)   
  The section I selected contains utterances from longitudinal audio/video transcripts of 6 monolingual English-speaking children’s language development from 1-3 years during spontaneous interactions with their parents (usually their mothers) at home. The participants included 3 boys (Alex, Ethan, William) and 3 girls (Lily, Naima, Violet). Each child was recorded for 1 hour every 2 weeks beginning at the onset of first words. 

## Planned Project 
*Temporary, may be changed in future*  
**Research Question**: It is expected that mothers will employ more nouns linked to female stereotypes and warmth-based adjectives when addressing female infants compared to male infants.  
Gender-biased noun. Nouns extracted from the two lists “Top 10 most female/male biased words in English (America) for comprehension data” of the Wordbank data report (2016) will be treated as gender-biased language data.  
Competence and warmth adjectives. Based on the SCM’s categorization (2008), warmth scales include “good‐natured”, “trustworthy”, “tolerant”, “friendly”, and “sincere”, whereas competence scales include “capable”, “skillful”, “intelligent”, and “confident”.

## File Tree
project/  
│  
├── README.md  
├── scripts/  
│   └── My_Script.R  
├── notebook/  
│   └── My_Notebook.Rmd  
└── data_file/  
    └── Dataset_Providence/  
        ├── Alex  
        ├── Ethan  
        ├── Lily  
        ├── Naima  
        ├── Violet  
        └── William  
 
