rm(list = ls())

library(RSelenium)
library(xml2)


# Setting up the driver
chromeDr <- rsDriver(browser = "chrome", port = 4566L, chromever = "105.0.5195.52", # you will have to adjust this version
                     extraCapabilities = list(chromeOptions = list(args = c('--disable-gpu', '--window-size=1280,800'),
                                                                   prefs = list(
                                                                     "profile.default_content_settings.popups" = 0L,
                                                                     "download.prompt_for_download" = FALSE,
                                                                     "directory_upgrade" = TRUE
                                                                   ))))

remDr <- chromeDr[["client"]]

# Moving to the target page
remDr$navigate("https://www.homegate.ch/mieten/immobilien/land-schweiz")

# TODO: get "buy" page as well

## Listings for rent
# Click region
e <- remDr$findElement(value = '//*[@id="app"]/main/div/div/div[2]/div[1]/div/div[2]/div/div[1]/div[2]/a/span')
e$clickElement()

# Click next page button
e <- remDr$findElement(value = '//*[@id="app"]/main/div/div[2]/div/div[4]/div[3]/nav/a[6]')
e$clickElement()











### Close connection
remDr$close()
chromeDr[["server"]]$stop()
