rm(list = ls())

library(RSelenium)
library(xml2)


## Set up ----
# Setting up the driver
chromeDr <- rsDriver(browser = "chrome", port = 4566L, chromever = "105.0.5195.52", # you will have to adjust this version
                     extraCapabilities = list(chromeOptions = list(args = c('--disable-gpu', '--window-size=1280,800'),
                                                                   prefs = list(
                                                                     "profile.default_content_settings.popups" = 0L,
                                                                     "download.prompt_for_download" = FALSE,
                                                                     "directory_upgrade" = TRUE
                                                                   ))))

remDr <- chromeDr[["client"]]



## Listings for rent ----
# Moving to the target page
remDr$navigate("https://www.homegate.ch/rent/real-estate/switzerland")

# Extract all the canton links
e <- remDr$findElements(value = "//*/div[contains(@class, 'GeoDrillDownSRPLink')]/a")
canton_links <- unlist(lapply(e, function(x){x$getElementAttribute("href")}))[2:27]

# Prepare a table for the output
homegate_data <- data.frame()

# Go through the listings by canton
for (c_link in canton_links){
  
  # Go through all the pages per canton
  while (last_page == FALSE){
    
    # Get the price
    e <- remDr$findElements(value = "//*/span[contains(@class, 'ListItemPrice_price')]/span[2]")
    prices <- unlist(lapply(e, function(x){x$getElementText()}))
    
    # Get the square meters
    e <- remDr$findElements(value = "//*/span[contains(@class, 'ListItemLivingSpace_value')]")
    sq_m <- unlist(lapply(e, function(x){x$getElementText()}))
    
    # Get the nr. of rooms
    e <- remDr$findElements(value = "//*/span[contains(@class, 'ListItemRoomNumber_value')]")
    nr_rooms <- unlist(lapply(e, function(x){x$getElementText()}))
    
    # Get the location
    e <- remDr$findElements(value = "//*/div[contains(@class, 'ListItem') and contains(@class, '_data_')]/p[2]/span")
    location <- unlist(lapply(e, function(x){x$getElementText()}))
    
    # Get the description
    e <- remDr$findElements(value = "//*/div[contains(@class, 'ListItemDescription_description')]/p")
    description <- unlist(lapply(e, function(x){x$getElementText()}))
    
    
    # Write the data to the output table
    homegate_data <- rbind(homegate_data,
                           data.frame("canton_link" = c_link,
                                      "type" = "rent",
                                      "price" = prices,
                                      "sq_m" = sq_m,
                                      "nr_rooms" = nr_rooms,
                                      "location" = location,
                                      "description" = description))
    
    # Check if we're on the last page, otherwise go to next page
    tryCatch(next_page_button <- remDr$findElements(value = "//*/svg[contains(@class, 'Icon') and contains(@class, 'disabledButton')]"))
    
    if (length(next_page_button) < 1){
      last_page <- TRUE
    } else {
      e <- remDr$findElement(value = "//*/a[contains(@class, 'router-link-active HgPaginationSelector_nextPreviousArrow') and contains(@aria-label, 'Go to next page')]")
      e$clickElement()
    }
  }
  
  # Reset the last page indicator
  last_page <- FALSE
}



## Listings to buy ----
# Moving to the target page
remDr$navigate("https://www.homegate.ch/buy/real-estate/switzerland")

# Extract all the canton links
e <- remDr$findElements(value = "//*/div[contains(@class, 'GeoDrillDownSRPLink')]/a")
canton_links <- unlist(lapply(e, function(x){x$getElementAttribute("href")}))[2:27]

# Go through the listings by canton
for (c_link in canton_links){
  
  # Go through all the pages per canton
  while (last_page == FALSE){
    
    # Get the price
    e <- remDr$findElements(value = "//*/span[contains(@class, 'ListItemPrice_price')]/span[2]")
    prices <- unlist(lapply(e, function(x){x$getElementText()}))
    
    # Get the square meters
    e <- remDr$findElements(value = "//*/span[contains(@class, 'ListItemLivingSpace_value')]")
    sq_m <- unlist(lapply(e, function(x){x$getElementText()}))
    
    # Get the nr. of rooms
    e <- remDr$findElements(value = "//*/span[contains(@class, 'ListItemRoomNumber_value')]")
    nr_rooms <- unlist(lapply(e, function(x){x$getElementText()}))
    
    # Get the location
    e <- remDr$findElements(value = "//*/div[contains(@class, 'ListItem') and contains(@class, '_data_')]/p[2]/span")
    location <- unlist(lapply(e, function(x){x$getElementText()}))
    
    # Get the description
    e <- remDr$findElements(value = "//*/div[contains(@class, 'ListItemDescription_description')]/p")
    description <- unlist(lapply(e, function(x){x$getElementText()}))
    
    
    # Write the data to the output table
    homegate_data <- rbind(homegate_data,
                           data.frame("canton_link" = c_link,
                                      "type" = "buy",
                                      "price" = prices,
                                      "sq_m" = sq_m,
                                      "nr_rooms" = nr_rooms,
                                      "location" = location,
                                      "description" = description))
    
    # Check if we're on the last page, otherwise go to next page
    tryCatch(next_page_button <- remDr$findElements(value = "//*/svg[contains(@class, 'Icon') and contains(@class, 'disabledButton')]"))
    
    if (length(next_page_button) < 1){
      last_page <- TRUE
    } else {
      e <- remDr$findElement(value = "//*/a[contains(@class, 'router-link-active HgPaginationSelector_nextPreviousArrow') and contains(@aria-label, 'Go to next page')]")
      e$clickElement()
    }
  }
  
  # Reset the last page indicator
  last_page <- FALSE
}



## Finally ----
# Write our scraped data to a file
save(homegate_data, file = "Homegate_scrape.RData")

# Close connection
remDr$close()
chromeDr[["server"]]$stop()
