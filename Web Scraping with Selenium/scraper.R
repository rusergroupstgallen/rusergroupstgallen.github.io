rm(list = ls())

library(RSelenium)
library(tidyverse)


## Set up ----
# Setting up the driver
# chromeDr_ver <- data.frame("version" = unname(unlist(binman::list_versions("chromedriver")))) %>% mutate("major" = sapply(version, function(x){as.numeric(unlist(str_split(x, "\\."))[1])})) %>% arrange(desc(major))
# chromeDr_ver <- as.character(chromeDr_ver$version[3])
system("find ~/.local/share/ -name LICENSE.chromedriver -print | xargs -r rm")

chromeDr_ver <- "114.0.5735.90"

chromeDr <- RSelenium::rsDriver(browser = "chrome", port = 4569L, chromever = chromeDr_ver, geckover = NULL, # you will have to adjust this version
                                extraCapabilities = list(chromeOptions = list(args = c('--disable-gpu', '--window-size=1920,1080', '--headless',
                                                                                       '--enable-features=NetworkService,NetworkServiceInProcess',
                                                                                       '--disable-dev-shm-usage', '--no-sandbox', 'enable-automation', 'start-maximized', '--disable-browser-side-navigation'),
                                                                              prefs = list(
                                                                                "profile.default_content_settings.popups" = 0L,
                                                                                "download.prompt_for_download" = FALSE,
                                                                                "directory_upgrade" = TRUE
                                                                              ))))

remDr <- chromeDr[["client"]]



## Functions ----
# Function to find child elements
find_homegate_elements <- function(listing){
  
  # Get the href of the parent <a>
  listing_id <- unlist(strsplit(unlist(listing$getElementAttribute("href")), "/(?!.*/)", perl = TRUE))[2]
  
  # Establish default values
  price <- NA
  sq_m <- NA
  nr_rooms <- NA
  location <- NA
  description <- NA
  
  tryCatch(
    expr = {
      suppressMessages({
        price <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//span[contains(@class, 'HgListingCard_price')]"))
        price <- unlist(price$getElementText())
      })
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        sq_m <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//div[contains(@class, 'HgListingRoomsLivingSpace_rooms')]/span[2]"))
        sq_m <- unlist(sq_m$getElementText())
      })
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        nr_rooms <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//div[contains(@class, 'HgListingRoomsLivingSpace_rooms')]/span[1]"))
        nr_rooms <- unlist(nr_rooms$getElementText())
      })
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        location <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//div[contains(@class, 'HgListingCard_address')]"))
        location <- unlist(location$getElementText())
      })
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        description <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//p[contains(@class, 'HgListingDescription_large') or contains(@class, 'HgListingDescription_medium') or contains(@class, 'HgListingDescription_small')]"))
        description <- unlist(description$getElementText())
      })
    },
    error = function(err){
      return(1)
    })
  
  return(data.frame("price" = price,
                    "sq_m" = sq_m,
                    "nr_rooms" = nr_rooms,
                    "location" = location,
                    "description" = description,
                    "listing_id" = listing_id,
                    "listing_url" = unlist(listing$getElementAttribute("href"))
  )
  )
}

## Implicit wait
implWait <- function(wait_s = 30, driver = remDr){
  counter <- 0
  webElem <- NULL
  while(is.null(webElem) & counter < wait_s){
    webElem <- tryCatch(
      expr = {
        suppressMessages({
          driver$findElement(value = "//*/a[contains(@class, 'HgCardElevated_link')]")
        })
      },
      error = function(err){
        NULL
      })
    Sys.sleep(1)
    counter <- counter + 1
    print(paste0("waiting..", counter))
  }
  if (counter == wait_s){
    print(paste0(Sys.time(), " - timed out"))
    return(1)
  }
}



## Listings for rent ----
# Moving to the target page
remDr$navigate("https://www.homegate.ch/rent/real-estate/switzerland")

Sys.sleep(20)

tryCatch(
  cookie_button <- remDr$findElements(value = "//*/button[contains(@id, 'onetrust-accept-btn-handler')]")
)

Sys.sleep(2)

if (length(cookie_button) > 0){
  cookie_button <- remDr$findElement(value = "//*/button[contains(@id, 'onetrust-accept-btn-handler')]")
  Sys.sleep(2)
  cookie_button$clickElement()
  Sys.sleep(2)
}

# Extract all the canton links
e <- remDr$findElements(value = "//*/div[contains(@class, 'GeoDrillDownSRPLink')]/a")
canton_links <- unlist(lapply(e, function(x){x$getElementAttribute("href")}))[2:27]

# Prepare a table for the output
homegate_data <- data.frame()

# Record keeping
failed_links <- data.frame()

# Go through the listings by canton
for (c_link in canton_links){
  
  # Navigate to the page
  remDr$navigate(c_link)
  
  # Find last page
  last_page <- 1
  
  tryCatch(
    expr = {
      suppressMessages({
        last_page <- as.numeric(str_extract(unlist(remDr$findElement(value = "//*/nav[contains(@class, 'HgPaginationSelector')]/a[2]")$getElementText()), "\\d+"))
      })
    },
    error = function(err){
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        last_page <- as.numeric(str_extract(unlist(remDr$findElement(value = "//*/nav[contains(@class, 'HgPaginationSelector')]/a[3]")$getElementText()), "\\d+"))
      })
    },
    error = function(err){
    })
  
  if (!is.na(last_page)){
    current_pages <- paste(c_link, "?ep=", 1:last_page, sep = "")
    
    for (c_page in current_pages){
      remDr$navigate(c_page)
      
      if(!is.null(implWait())){
        Sys.sleep(sample(1:5, 1))
        remDr$navigate(c_page)
      }
      
      Sys.sleep(sample(1:5, 1))
      
      if(is.null(implWait())){
        # Get all the listing wrappers on the page
        parents <- remDr$findElements(value = "//*/a[contains(@class, 'HgCardElevated_link')]")
        
        # Get the values from the child elements
        page_result <- lapply(parents, find_homegate_elements)
        
        # Coerce the results into a data frame
        page_result <- as.data.frame(do.call(rbind, page_result))
        
        # Add a column for the listing type
        page_result$listing_type <- "rent"
        
        # Add datetime
        page_result$datetime <- Sys.time()
        
        # Add column for the current canton
        page_result$canton <- str_remove(c_link, "/matching-list") %>%
          str_split(., "/(?!.*/)") %>%
          unlist(.) %>% .[2] %>%
          str_split(., "-") %>%
          unlist(.) %>% .[2]
        
        # Join with the rest of the scraped data
        homegate_data <- rbind(homegate_data, page_result)
        save(homegate_data, file = "Web Scraping with Selenium/homegate_data_inter_temp.RData")
        
        # Wait for page to load before continuing
        implWait(30)
        Sys.sleep(sample(1:10, 1))
        
        print(paste0(Sys.time(), " - Canton: ", page_result$canton[1], " (rent), page: ", str_extract(c_page, "\\d+"), "/", last_page))
      } else {
        # Add link to failed
        failed_links <- rbind(failed_links, data.frame("datetime" = Sys.time(), "link" = c_page))
        
        print(paste0(Sys.time(), " - Skipped -> Canton: ", page_result$canton[1], " (rent), page: ", str_extract(c_page, "\\d+"), "/", last_page))
      }
    }
  } else {
    # Add link to failed
    failed_links <- rbind(failed_links, data.frame("datetime" = Sys.time(), "link" = c_link))
  }
  print(paste0(Sys.time(), " - Finished canton page"))
}


## Listings to buy ----
# Moving to the target page
remDr$navigate("https://www.homegate.ch/buy/real-estate/switzerland")

# Extract all the canton links
e <- remDr$findElements(value = "//*/div[contains(@class, 'GeoDrillDownSRPLink')]/a")
canton_links <- unlist(lapply(e, function(x){x$getElementAttribute("href")}))[2:27]

# Go through the listings by canton
for (c_link in canton_links){
  
  # Navigate to the page
  remDr$navigate(c_link)
  
  # Find last page
  last_page <- 1
  
  tryCatch(
    expr = {
      suppressMessages({
        last_page <- as.numeric(str_extract(unlist(remDr$findElement(value = "//*/nav[contains(@class, 'HgPaginationSelector')]/a[2]")$getElementText()), "\\d+"))
      })
    },
    error = function(err){
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        last_page <- as.numeric(str_extract(unlist(remDr$findElement(value = "//*/nav[contains(@class, 'HgPaginationSelector')]/a[3]")$getElementText()), "\\d+"))
      })
    },
    error = function(err){
    })
  
  if (!is.na(last_page)){
    current_pages <- paste(c_link, "?ep=", 1:last_page, sep = "")
    
    for (c_page in current_pages){
      remDr$navigate(c_page)
      
      if(!is.null(implWait())){
        Sys.sleep(sample(1:5, 1))
        remDr$navigate(c_page)
      }
      
      Sys.sleep(sample(1:5, 1))
      
      if(is.null(implWait())){
        # Get all the listing wrappers on the page
        parents <- remDr$findElements(value = "//*/a[contains(@class, 'HgCardElevated_link')]")
        
        # Get the values from the child elements
        page_result <- lapply(parents, find_homegate_elements)
        
        # Coerce the results into a data frame
        page_result <- as.data.frame(do.call(rbind, page_result))
        
        # Add a column for the listing type
        page_result$listing_type <- "buy"
        
        # Add datetime
        page_result$datetime <- Sys.time()
        
        # Add column for the current canton
        page_result$canton <- str_remove(c_link, "/matching-list") %>%
          str_split(., "/(?!.*/)") %>%
          unlist(.) %>% .[2] %>%
          str_split(., "-") %>%
          unlist(.) %>% .[2]
        
        # Join with the rest of the scraped data
        homegate_data <- rbind(homegate_data, page_result)
        save(homegate_data, file = "Web Scraping with Selenium/homegate_data_inter_temp.RData")
        
        # Wait for page to load before continuing
        implWait(30)
        Sys.sleep(sample(1:10, 1))
        
        print(paste0(Sys.time(), " - Canton: ", page_result$canton[1], " (buy), page: ", str_extract(c_page, "\\d+"), "/", last_page))
      } else {
        # Add link to failed
        failed_links <- rbind(failed_links, data.frame("datetime" = Sys.time(), "link" = c_page))
        
        print(paste0(Sys.time(), " - Skipped -> Canton: ", page_result$canton[1], " (buy), page: ", str_extract(c_page, "\\d+"), "/", last_page))
      }
    }
  } else {
    # Add link to failed
    failed_links <- rbind(failed_links, data.frame("datetime" = Sys.time(), "link" = c_link))
  }
  print(paste0(Sys.time(), " - Finished canton page"))
}

# Write our scraped data to a file
homegate_data_2 <- homegate_data
load("Web Scraping with Selenium/homegate_data_inter_temp.RData")
homegate_data <- unique(rbind(homegate_data, homegate_data_2))
save(homegate_data, failed_links, file = paste0("Web Scraping with Selenium/Homegate_scrape_raw_", Sys.Date(), ".RData"))


# ## Finally ----
# # Some clean-up
# homegate_data$listing_id <- sapply(homegate_data$listing_url, function(x){as.numeric(str_extract(x, "\\d+"))})
# homegate_data$price <- sapply(homegate_data$price, function(x){str_remove(x, "CHF ")}) %>%
#   sapply(function(x){str_remove(x, "\\.–")}) %>%
#   sapply(function(x){str_remove(x, "/ month")}) %>% 
#   sapply(function(x){str_remove(x, "/ one time")}) %>% 
#   sapply(function(x){str_remove_all(x, ",")}) %>% 
#   sapply(function(x){trimws(x, "both")}) %>% 
#   unlist() %>% 
#   as.numeric()
# homegate_data$sq_m <- sapply(homegate_data$sq_m, function(x){str_remove(x, "m² living space")}) %>% 
#   sapply(function(x){str_remove_all(x, ",")}) %>% 
#   unlist() %>% 
#   sapply(function(x){trimws(x, "both")}) %>% 
#   as.numeric()
# homegate_data$zip_code <- sapply(homegate_data$location, function(x){str_extract(x, "\\d{4}")}) %>%
#   as.numeric()
# homegate_data$nr_rooms <- sapply(homegate_data$nr_rooms, function(x){str_remove(x, " room")}) %>%
#   sapply(function(x){str_remove_all(x, "s")}) %>%
#   unlist() %>% 
#   sapply(function(x){trimws(x, "both")}) %>% 
#   as.numeric()
# 
# # Write our scraped data to a file
# homegate_data <- unique(homegate_data)
# save(homegate_data, file = paste0("Web Scraping with Selenium/Homegate_scrape_clean_", Sys.Date(), ".RData"))

# Close connection
print(paste0(Sys.time(), " - Scrape complete. Shutdown."))
remDr$close()
chromeDr[["server"]]$stop()

# source("Web Scraping with Selenium/scraper.R")
# while(TRUE){
#   if (weekdays(Sys.Date()) == "Saturday" & lubridate::hour(Sys.time()) == 12){
#     source("Web Scraping with Selenium/scraper.R")
#   } else {
#     print(paste0("It is ", weekdays(Sys.Date()), " at ", lubridate::hour(Sys.time())))
#   }
#   Sys.sleep(3599)
# }

