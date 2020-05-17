## Lab 06
## March 09-2020

## Section 1
course_url = 'https://www.sfu.ca/outlines.html?2020/spring/stat/240/d100'
course_page = readLines(course_url)

length(course_page)

# arbitrary sets of 10 lines of html
trimws(course_page[240:250])

grep('<h3', course_page)

trimws(grep('<h3', course_page, value = T))

heading_index = grep('<h3', course_page) 
# note this gives us the same result as using grep(, value = T) 
trimws(course_page[heading_index])

heading = course_page[(heading_index[1]-1):(heading_index[2]+1)]

## Problem 1
# 1A
headings3 = trimws(grep('<h3', course_page, value = T))
headings3 = gsub("<.*?>","",headings3)

# 1B

# Precondition: url is a string describing a url from SFU course outline
# Postcondition: return value a character of the class number
class_number <- function(course_url) {
  course_page = readLines(course_url)
  
  classnum = trimws(grep('<h3 id=\"class-number\">', course_page, value = T))
  classnum = gsub("<.*?>","",classnum)
  return(classnum)
}

class_number(course_url) # Test 1: works with Stat240
class_number('https://www.sfu.ca/outlines.html?2020/summer/math/310/d100') # Test 2: works with MATH310
class_number('https://www.sfu.ca/outlines.html?2019/fall/math/345/d100') # Test 3: works with MATH345

# 1C

# Precondition: url is a string describing a url from SFU course outline
# Postcondition: return value a list containing the class number and instructor name
course <- function(course_url) {
  course_page = readLines(course_url)

  classnum = trimws(grep('<h3 id=\"class-number\">', course_page, value = T))
  classnum = gsub("(<.*?>)","",classnum)
  classnum = gsub("Class Number: ","",classnum)

  instructor = trimws(course_page[240:250])
  instructor = gsub("<.*?>","", instructor)
  
  i = 1
  while (i != length(instructor)) {
    if (instructor[i] == "Instructor:") {
      instructor = instructor[i+1]
      i = length(instructor)
    } else if (i+1 == length(instructor)) {
      instructor = "TBD"
      i = length(instructor)
    } else {
      i = i + 1
    }
  }
  
  return(list("Class Number" = classnum, "Instructor" = instructor))
}

course(course_url) # Test 1: works with Stat240
course('https://www.sfu.ca/outlines.html?2020/summer/math/310/d100') # Test 2: works with MATH310
course('https://www.sfu.ca/outlines.html?2019/fall/math/345/d100') # Test 3: works with MATH345


## Section 2
library(rvest)
library(tidyverse)

movie_url = "https://www.imdb.com/chart/boxoffice"

movie_table = read_html("https://www.imdb.com/chart/boxoffice") 
length(html_nodes(movie_table, "table"))

zz = html_table(html_nodes(movie_table, "table")[[1]])

zz[1,c(2,4)]

## Problem 2
# 2A
boxoffice <- function() {
  movie_html = read_html("https://www.imdb.com/chart/boxoffice")
  movie_table = html_table(html_nodes(movie_html, "table")[[1]])
  
  movie_table$Weekend = as.numeric(gsub("[$M]", "", movie_table$Weekend))
  movie_table$Gross = as.numeric(gsub("[$M]", "", movie_table$Gross))
  
  boxoffice_data = tibble(Name = movie_table$Title,
                          Gross = movie_table$Gross, 
                          Weeks = as.numeric(movie_table$Weeks))
  
  boxoffice_data = boxoffice_data %>% 
    mutate('PerWeek ($M)' = Gross / Weeks) %>% 
    select(Name, Gross,'PerWeek ($M)') %>% 
    rename('BoxOffice ($M)' = Gross)
  
  return(boxoffice_data)
}

(dat = boxoffice()) # Call function

