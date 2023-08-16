rm(list = ls())

library(tidyverse)

homegate_files <- list.files("Web Scraping with Selenium/", pattern = "Homegate_scrape_clean_.*")

homegate_agg <- data.frame()

for (f in homegate_files){
  load(file = paste0("Web Scraping with Selenium/", f))
  homegate_data$date_scraped <- as.Date(str_extract(f, "\\d{4}-\\d{2}-\\d{2}"), format = "%Y-%m-%d")
  homegate_agg <- rbind(homegate_agg, homegate_data)
  print(paste0("File ", match(f, homegate_files), "/", length(homegate_files)))
}

save(homegate_agg, file = "Web Scraping with Selenium/Homegate_data.RData")
