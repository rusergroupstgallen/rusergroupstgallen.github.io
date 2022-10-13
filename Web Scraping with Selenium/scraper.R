rm(list = ls())

library(RSelenium)


## Set up ----
# Setting up the driver
chromeDr <- rsDriver(browser = "chrome", port = 4567L, chromever = "106.0.5249.61", # you will have to adjust this version
                     extraCapabilities = list(chromeOptions = list(args = c('--disable-gpu', '--window-size=1280,800'),
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
      price <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//span[contains(@class, 'ListItemPrice_price')]/span[2]"))
      price <- unlist(price$getElementText())
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
      nr_rooms <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//span[contains(@class, 'ListItemRoomNumber_value')]"))
      nr_rooms <- unlist(nr_rooms$getElementText())
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      location <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//div[contains(@class, 'ListItem') and contains(@class, '_data_')]/p[2]/span"))
      location <- unlist(location$getElementText())
    },
    error = function(err){
      return(1)
    })
  
  tryCatch(
    expr = {
      description <- listing$findChildElement(value = paste0("//*/a[contains(@href, '", listing_id, "')]//div[contains(@class, 'ListItemDescription_description')]/p"))
      description <- unlist(description$getElementText())
    },
    error = function(err){
      return(1)
    })
  
  return(data.frame("price" = price,
                    "sq_m" = sq_m,
                    "nr_rooms" = nr_rooms,
                    "location" = location,
                    "description" = description,
                    "listing_id" = listing_id))
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
for (c_link in canton_links){
  
  # Navigate to the page
  remDr$navigate(c_link)
  
  # Go through all the pages per canton
  while (last_page == FALSE){
    
    # Get all the listing wrappers on the page
    parents <- remDr$findElements(value = "//*/a[contains(@class, 'ResultList_ListItem')]")
    
    # Get the values from the child elements
    page_result <- lapply(parents, find_homegate_elements)
    
    # Coerce the results into a data frame
    page_result <- as.data.frame(do.call(rbind, page_result))
    
    # Add a column for the listing type
    page_result$listing_type <- "rent"
    
    # Join with the rest of the scraped data
    homegate_data <- rbind(homegate_data, page_result)
    
    # Check if we're on the last page, otherwise go to next page
    tryCatch(
      next_page_button <- remDr$findElements(value = "//*/p[contains(@class, 'HgPaginationSelector_nextPreviousArrow')]")
    )
    
    if (length(next_page_button) < 1){
      last_page <- TRUE
    } else {
      e <- remDr$findElement(value = "//*/a[contains(@aria-label, 'Go to next page')]")
      e$clickElement()
    }
  }
  
  # Reset the last page indicator
  last_page <- FALSE
}



## Listings to buy ----
# TODO



## Finally ----
# Write our scraped data to a file
save(homegate_data, file = "Homegate_scrape.RData")

# Close connection
remDr$close()
chromeDr[["server"]]$stop()
