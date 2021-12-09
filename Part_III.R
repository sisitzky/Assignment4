source("Parts_I_and_II.R")

# devtools::install_github("Truenumbers/tnum/tnum")
library(knitr)
library(kableExtra)
library(magrittr)
library(gutenbergr)
library(tidyverse)
library(tnum)
library(sentimentr)



tnum.authorize("mssp1.bu.edu")
tnum.setSpace("test3")

scarlet_letter2 <- gutenberg_download(gutenberg_id=25344)

scarlet_letter2 <- readLines("pg25344.txt")

source("Book2TN-v6A-1.R")

## tnBooksFromLines(scarlet_letter2, "sisitzky/scarlet")
tnum.getDBPathList(taxonomy="subject", levels=2)

scarlet_letter2 %>% 
  get_sentences() %>% 
  sentiment_by(by = NULL) %>% #View()
  ggplot() + geom_density(aes(ave_sentiment))




