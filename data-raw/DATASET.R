## code to prepare `Frankenstein` dataset goes here

#install.packages("gutenbergr")
library(gutenbergr)
library(dplyr)
library(tidytext)
library(stringr)

my_mirror <- "http://mirror.csclub.uwaterloo.ca/gutenberg/"
frankenstein_raw <- gutenberg_download(84, mirror = my_mirror)

Frankenstein <- frankenstein_raw |>
  mutate(line = row_number())

Frankenstein$section_type <- NA
Frankenstein$section <- NA
num_lines <- nrow(Frankenstein)
curr_section <- 0
type <- "misc"
for (i in 1:num_lines) {
  if (str_detect(Frankenstein$text[i], "^Letter")) {
    curr_section <- as.numeric(str_replace_all(Frankenstein$text[i], "\\D", ""))
    type <- "letter"
  }
  
  if (str_detect(Frankenstein$text[i], "^Chapter")) {
    curr_section <- as.numeric(str_replace_all(Frankenstein$text[i], "\\D", ""))
    type <- "chapter"
  }
  
  Frankenstein$section[i] <- curr_section
  Frankenstein$section_type[i] <- type
  
}


Frankenstein <- tibble(line = Frankenstein$line, 
                       text = Frankenstein$text, 
                       section = Frankenstein$section,
                       section_type = Frankenstein$section_type
)
usethis::use_data(Frankenstein, overwrite = TRUE)

anno_Frankenstein <- readRDS("data-raw/anno_frankenstein.Rds")
usethis::use_data(anno_Frankenstein, overwrite = TRUE)



