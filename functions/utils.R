extract_years <- function(year_input) {
  
  # test
  #year_input <- file_lists[30]
  
  # target
  target_nums <- c(2000:2022)
  
  # wrangling
  out <- year_input %>%  
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("[[:alpha:]]", " ") %>%
    str_split(" +") %>%
    pluck(1) %>%
    stri_remove_empty() %>%
    parse_number()
  
  out <- out[out %in% target_nums]
  
  if (length(out) > 2) {
    
    out <- c(out[1], out[2])
    
  } 
  
  out <- data.frame(
    "begin" = out[1],
    "end" = out[2],
    "source" = year_input)
  
  return(out)
  
}

extract_url <- function(url) {

  pg <- read_html(url)
  
  # extract links
  
  hrefs <- html_attr(html_nodes(pg, "a"), "href")
  
  return(hrefs)
  
}

# Action plans

extract_pdf <- function(url) {

  # adapted from this code: https://stackoverflow.com/questions/31517121/using-r-to-scrape-the-link-address-of-a-downloadable-file-from-a-web-page
  pdf_links <-  read_html(url) %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     # get the url
    str_subset("\\.pdf") 
  
  unique_pdf <- pdf_links %>%
    unique()
  
  action_list <- str_detect(tolower(unique_pdf), "action")
  
  unique_pdf <- unique_pdf[action_list]

  if (!is_empty(unique_pdf)) {
  
      unique_pdf <- data.frame(
        pdf_link = unique_pdf,
        college = str_remove_all(url, "https://allinchallenge.org/campuses/|/"))
      
  } else 
    
  { unique_pdf <- data.frame(
    pdf_link = NA,
    college = str_remove_all(url, "https://allinchallenge.org/campuses/|/")) }
  
  return(unique_pdf)
}

# NSLVE

extract_stat <- function(url) {
  
  # adapted from this code: https://stackoverflow.com/questions/31517121/using-r-to-scrape-the-link-address-of-a-downloadable-file-from-a-web-page
  pdf_links <-  read_html(url) %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     # get the url
    str_subset("\\.pdf") 
  
  unique_pdf <- pdf_links %>%
    unique()
  
  nslve_list <- str_detect(tolower(unique_pdf), "nslve")
  
  unique_pdf <- unique_pdf[nslve_list]
  
  if (!is_empty(unique_pdf)) {
    
    unique_pdf <- data.frame(
      pdf_link = unique_pdf,
      college = str_remove_all(url, "https://allinchallenge.org/campuses/|/"))
    
  } else 
    
  { unique_pdf <- data.frame(
    pdf_link = NA,
    college = str_remove_all(url, "https://allinchallenge.org/campuses/|/")) }
  
  return(unique_pdf)
}