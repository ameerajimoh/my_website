---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: Scaping for Positions- Consulting Jobs
draft: false
image: scrapit.jpeg
keywords: ""
slug: scraping
title: Web Scaping 
---
Scaping for Positions: Consulting Jobs
``` r
#Checking if it is okay to scrape
library(robotstxt)
paths_allowed("https://www.consultancy.uk") 
```


``` r
base_url <- "https://www.consultancy.uk/jobs/page/1"
listings_html <- base_url %>%
  read_html()
```

Condition: Identify the CSS selectors in order to extract the relevant information from this page, namely
job
firm
functional area
type

``` r
get_listings <- function(page) {
  
  base_url <- "https://www.consultancy.uk/jobs/page/"
  url <- str_c(base_url, page)
  listings_html <- read_html(url)
  
  
  job <- listings_html %>%
    html_nodes(css = "span.title") %>%
    html_text2() 
  
  firm <- listings_html %>%
    html_nodes(css = ".hide-phone .row-link") %>%
    html_text2() 

  link <- listings_html %>%
    html_nodes(css = ".hide-phone .row-link") %>%
    html_attr('href') %>%
    str_c("https://www.consultancy.uk", .)
  
    
  functional_area <- listings_html %>%
    html_elements(css = ".initial") %>%
    html_text2() 
  
  type <- listings_html %>%
    html_nodes(css = ".hide-tablet-landscape .row-link") %>%
    html_text2() 


  jobs_df <- tibble(
    job = job,
    firm     = firm,
    functional_area     = functional_area,
    type    = type,
    link = link
  ) 
  
    return(jobs_df)
}

pages <- 1:8 # apply to the first 8 pages; if more, change to 1:X 
jobs <- map_df(pages, get_listings)

glimpse(jobs)
```

``` r
jobs %>% 
  count(firm, sort=TRUE)
```

``` r
jobs %>% 
  count(functional_area, sort=TRUE) %>%
  filter(functional_area != "Unknown") %>%  
  mutate(perc = n/sum(n))
```

