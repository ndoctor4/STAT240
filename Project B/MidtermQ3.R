# Project B: Album Ratings

library(rvest)
library(stringr)


pitchfork <- function(url) {
  x = read_html(url)
  
  # The text of the review
  text = x %>% 
    html_nodes(".contents.dropcap") %>%   # css selector
    html_text() %>%
    str_trim() %>%
    unlist()
  
  text = gsub("\n"," ", text)
  text = gsub("( Buy: ).*", "", text)     # replace last sentence
  
  # The rating, decimal value, of the review
  rating = x %>% 
    html_nodes(".score") %>%              # css selector
    html_text() %>% 
    as.numeric()
  
  review = data.frame(text=c(text), rating=c(rating))
  return(review)
}

