extract_url <- function(url) {

  pg <- read_html(url)
  
  # extract links
  
  hrefs <- html_attr(html_nodes(pg, "a"), "href")
  
  return(hrefs)
  
}

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