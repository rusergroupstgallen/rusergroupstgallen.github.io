rm(list = ls())

library(RSelenium)
library(tidyverse)


## Set up ----
# Setting up the driver
chromeDr <- rsDriver(browser = "chrome", port = 4567L, chromever = "106.0.5249.61", # you will have to adjust this version
                     extraCapabilities = list(chromeOptions = list(args = c('--disable-gpu', '--window-size=1920,1080'),
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
        price <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//span[contains(@class, 'ListItemPrice_price')]/span[2]"))
        price <- unlist(price$getElementText())
      })
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        sq_m <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//span[contains(@class, 'ListItemLivingSpace_value')]"))
        sq_m <- unlist(sq_m$getElementText())
      })
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        nr_rooms <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//span[contains(@class, 'ListItemRoomNumber_value')]"))
        nr_rooms <- unlist(nr_rooms$getElementText())
      })
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        location <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//div[contains(@class, 'ListItem') and contains(@class, '_data_')]/p[2]/span"))
        location <- unlist(location$getElementText())
      })
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      suppressMessages({
        description <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//div[contains(@class, 'ListItemDescription_description')]/p"))
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
                    "listing_url" = unlist(listing$getElementAttribute("href"))))
}

## Implicit wait
implWait <- function(wait_s = 30){
  counter <- 0
  webElem <- NULL
  while(is.null(webElem) & counter < wait_s){
    webElem <- tryCatch(
      expr = {
        suppressMessages({
          remDr$findElement(value = "//*/a[contains(@class, 'ListItem')]")
        })
      },
      error = function(err){
        NULL
      })
    Sys.sleep(1)
    counter <- counter + 1
    print("waiting")
  }
}



## Listings for rent ----
# Moving to the target page
remDr$navigate("https://www.homegate.ch/rent/real-estate/switzerland")

# Extract all the canton links
e <- remDr$findElements(value = "//*/div[contains(@class, 'GeoDrillDownSRPLink')]/a")
canton_links <- unlist(lapply(e, function(x){x$getElementAttribute("href")}))[2:27]

# Prepare a table for the output
homegate_data <- data.frame()

# Go through the listings by canton
last_page <- FALSE
for (c_link in canton_links){
  
  # Navigate to the page
  remDr$navigate(c_link)
  
  # Go through all the pages per canton
  while (last_page == FALSE){
    
    # Get all the listing wrappers on the page
    parents <- remDr$findElements(value = "//*/a[contains(@class, 'ListItem')]")
    
    # Get the values from the child elements
    page_result <- lapply(parents, find_homegate_elements)
    
    # Coerce the results into a data frame
    page_result <- as.data.frame(do.call(rbind, page_result))
    
    # Add a column for the listing type
    page_result$listing_type <- "rent"
    
    # Add column for the current canton
    page_result$canton <- str_remove(c_link, "/matching-list") %>%
      str_split(., "/(?!.*/)") %>%
      unlist(.) %>% .[2] %>%
      str_split(., "-") %>%
      unlist(.) %>% .[2]
    
    # Join with the rest of the scraped data
    homegate_data <- rbind(homegate_data, page_result)
    
    # Check if we're on the last page, otherwise go to next page
    tryCatch(
      next_page_button <- remDr$findElements(value = "//*/a[contains(@aria-label, 'Go to next page')]")
    )
    
    if (length(next_page_button) < 1){
      last_page <- TRUE
    } else {
      e <- remDr$findElement(value = "//*/a[contains(@aria-label, 'Go to next page')]")
      e$clickElement()
      
      # Wait for page to load before continuing
      implWait(30)
      
      print("Next page")
    }
  }
  
  # Reset the last page indicator
  last_page <- FALSE
  
  print("Finished canton page")
}

# Write our scraped data to a file
save(homegate_data, file = "Homegate_scrape.RData")


## Listings to buy ----
# Moving to the target page
remDr$navigate("https://www.homegate.ch/buy/real-estate/switzerland")

# Extract all the canton links
e <- remDr$findElements(value = "//*/div[contains(@class, 'GeoDrillDownSRPLink')]/a")
canton_links <- unlist(lapply(e, function(x){x$getElementAttribute("href")}))[2:27]

# Go through the listings by canton
last_page <- FALSE
for (c_link in canton_links){
  
  # Navigate to the page
  remDr$navigate(c_link)
  
  # Go through all the pages per canton
  while (last_page == FALSE){
    
    # Get all the listing wrappers on the page
    parents <- remDr$findElements(value = "//*/a[contains(@class, 'ListItem')]")
    
    # Get the values from the child elements
    page_result <- lapply(parents, find_homegate_elements)
    
    # Coerce the results into a data frame
    page_result <- as.data.frame(do.call(rbind, page_result))
    
    # Add a column for the listing type
    page_result$listing_type <- "buy"
    
    # Add column for the current canton
    page_result$canton <- str_remove(c_link, "/matching-list") %>%
      str_split(., "/(?!.*/)") %>%
      unlist(.) %>% .[2] %>%
      str_split(., "-") %>%
      unlist(.) %>% .[2]
    
    # Join with the rest of the scraped data
    homegate_data <- rbind(homegate_data, page_result)
    
    # Check if we're on the last page, otherwise go to next page
    tryCatch(
      next_page_button <- remDr$findElements(value = "//*/a[contains(@aria-label, 'Go to next page')]")
    )
    
    if (length(next_page_button) < 1){
      last_page <- TRUE
    } else {
      e <- remDr$findElement(value = "//*/a[contains(@aria-label, 'Go to next page')]")
      e$clickElement()
      
      # Wait for page to load before continuing
      implWait(30)
      
      print("Next page")
    }
  }
  
  # Reset the last page indicator
  last_page <- FALSE
  
  print("Finished canton page")
}

# Write our scraped data to a file
save(homegate_data, file = "Homegate_scrape.RData")


## Finally ----
# Some clean-up
homegate_data$listing_id <- sapply(homegate_data$listing_id, as.numeric)
homegate_data$price <- sapply(homegate_data$price, function(x){str_remove(x, "\\.-")}) %>%
  sapply(., function(x){str_remove(x, ",")}) %>%
  as.numeric()
homegate_data$sq_m <- sapply(homegate_data$sq_m, function(x){str_remove(x, "m2")}) %>%
  as.numeric()
homegate_data$zip_code <- sapply(homegate_data$location, function(x){str_extract(x, "\\d{4}")}) %>%
  as.numeric()
homegate_data$nr_rooms <- sapply(homegate_data$nr_rooms, function(x){str_remove(x, "rm")}) %>%
  as.numeric()

# Write our scraped data to a file
save(homegate_data, file = "Homegate_scrape.RData")

# Close connection
remDr$close()
chromeDr[["server"]]$stop()
