library(rvest)
library(jsonlite)
library(anytime)

# Settings for the filter parameters -----
category <- 273
location <- FALSE
searchTerm <- "playing card"

# Function to build the search url -----
buildURL <- function(cat, loc, search) {
  request <- c(cat, loc, search)
  searchParameter <- c("&category_id=", "&woe_id=", "&term=")
  searchUrl <- "https://www.kickstarter.com/projects/search.json?"
  
  # Seperate searchTerm words with a plus
  if (searchTerm != FALSE) {
    x <-  data.frame(strsplit(searchTerm, " "))
    if (nrow(x) > 1) {
      request[3] = paste(x[[1]] ,sep = "", collapse = "+")
    }
  }

  # Append full queries to the url
  for (i in 1:length(request)) {
    if (request[i] != FALSE) {
      searchUrl = paste(searchUrl, searchParameter[i], request[i], sep="")
    }
  }
  
  return(searchUrl)
}

# Run scraper on the search page to gather first part of data ------

# Create empty data.frame
searchPageDF <- data.frame()

# Build the searchURL and go trough all 200 pages
searchUrl = buildURL(category, location, searchTerm)

tryCatch (
  {
    for (j in 1:200) {
      
      raw.data <- readLines(paste(searchUrl, "&page=", j, sep=""), warn = "F")
      searchJSON <- fromJSON(raw.data)
      
      # Read raw flatted data from JSON 
      rd <- fromJSON(raw.data, flatten = TRUE)
      
      # Insert into data frame
      rd <- data.frame(rd)
      
      # Only keep columns that are wanted
      keeps <- c("projects.id","projects.name" ,"projects.blurb","projects.goal", 
                 "projects.pledged", "projects.state", "projects.country", "projects.currency", 
                 "projects.currency_symbol", "projects.deadline", "projects.launched_at", "projects.staff_pick", "projects.backers_count", "projects.static_usd_rate", "projects.usd_pledged",
                 "projects.creator.name", "projects.location.name", "projects.location.country", "projects.location.state", 
                 "projects.urls.web.project", "projects.category.slug")
      rd <- rd[keeps]
      
      # Bind data to searchPageDF
      searchPageDF <- rbind(searchPageDF, rd)
      
      print(j)
      # Wait 10 seconds before going back to the site
      Sys.sleep(2)
    }
  },
  error=function(cond) {
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  warning=function(cond) {
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  },
  finally={
    betterNames <- c("ID","name" ,"blurb","fundGoal", "fundPledged", "fundState", "country", "fundCurrency", "fundCurrencySymbol", 
                     "fundEnd", "fundStart", "isStaffpick", "fundBakers", "fundUSDRate", "fundUSDPledged", "creator", "city", "country", "state", "url", "categorySlug")
    names(searchPageDF) <- betterNames
    
    # Order columns, to make more sense
    searchPageDF <- searchPageDF[,c(1, 20, 2, 3, 6, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 12)]
  }
)
     
# Run scraper on the project page for second part of data -----

start_time <- Sys.time() # Calculate time

# Create a new data.frame, if there isn't any in existence allready
if (exists("projectPageDF") && is.data.frame(get(projectPageDF))) {
  print("Data frame exists. Appending Data to current data frame")
} else {
  projectPageDF <- data.frame(ID = character(), "fundLength" = character(), "descriptionLength"= character(), "hasVideo"= character(), "amountFAQ"= character(), "amountUpdates"= character(), "amountComments"= character(), "amountImages"= character(),  "amountGifs"= character(), "amountVideos"= character(), "amountRewards"= character(), "lowestReward" = character())
  print("Data frame succesfully created.")
}

# Start scraping the urls

for (i in 1:nrow(searchPageDF)) {
  
  Sys.sleep(2) # Wait two seconds
  
  # Import variables from previous scrape
  kickUrl <- searchPageDF[[i,2]]
  
  # Download the source code of the project page as temporary file
  download.file(as.character(kickUrl), destfile = "scrapedpage.html", quiet=TRUE)
  kickProject <- read_html("scrapedpage.html")
  
  print("----------------------------------------------------------")
  print(paste(i , "th project loaded", sep=""))
  print(paste("ID:", searchPageDF$ID[i]))
  print(paste("Name:", searchPageDF$name[i]))
  print(paste("URL:", searchPageDF$url[i]))
  
  # Scrape first part of data, where the CSS-Selectors are the same for successfull and unsuccessfull projects
  descriptionHtml <- kickProject %>% 
    html_nodes(".description-container")
  
  descriptionText <- kickProject %>% 
    html_nodes(".description-container") %>% 
    html_text()
  
  rewardOptions <- kickProject %>% 
    html_nodes(".pledge__amount .money") %>%
    html_text()
  
  risksAndChallenges <- kickProject %>% 
    html_nodes(".js-risks") %>% 
    html_text()
  
  amountVideos <- descriptionHtml %>% 
    html_nodes(".video-player") %>% 
    html_text()
  
  amountYouTubeEmbed <- kickProject %>% 
    html_nodes("iframe")  %>% 
    html_text()
  
  amountImages <- kickProject %>% 
    html_nodes(".fit") %>%
    html_attr("src")
  
  amountGifs <- kickProject %>% 
    html_nodes(".js-lazy-image") %>% 
    html_text()
  
  amountUpdates <- kickProject %>% 
    html_nodes(".project-nav__link--updates .count") %>% 
    html_text()
  
  amountComments <- kickProject %>% 
    html_nodes(".project-nav__link--comments .count") %>% 
    html_text()
  
  amountFAQ <- kickProject %>% 
    html_nodes(".project-nav__link--faqs .count") %>% 
    html_text()
  
  amountRewards <- kickProject %>% 
    html_nodes(".pledge-selectable-sidebar") %>% 
    html_text()
  
  ### Scrape second part of data
  
  if (searchPageDF$fundState[[i]] == "successful") {
    
    hasVideo <- kickProject %>%
      html_nodes("#video_pitch")
    
    fundLength <- kickProject %>% 
      html_nodes(".f5") %>% 
      html_text()
    
  } else if (searchPageDF$fundState[[i]] == "failed") {

    hasVideo <- kickProject %>%
      html_nodes(".order-2-lg .svg-icon__icon--play")
    
    fundLength <- kickProject %>% 
      html_nodes(".f5") %>% 
      html_text()
    
  } else if (searchPageDF$fundState[[i]] == "live") {
    
    hasVideo <- kickProject %>%
      html_nodes(".z3")
    
    fundLength <- kickProject %>% 
      html_nodes(".f5") %>% 
      html_text()
    
  } else if (searchPageDF$fundState[[i]] == "canceled") {
    
    hasVideo <- kickProject %>%
      html_nodes(".z3")
    
    fundLength <- kickProject %>% 
      html_nodes(".f5") %>% 
      html_text()
  }
  

  ##############################
  # Count and calculate values # 
  ##############################
  
  # Count the number of entries in the vector
  amountRewards <- length(amountRewards)
  amountVideos <- length(amountVideos)
  amountYouTubeEmbed <- length(amountYouTubeEmbed)
  
  if (length(hasVideo) > 0) {
    hasVideo = TRUE
    # amountVideos counts the presentation video and therefore needs to be adjusted
    if (searchPageDF$fundState[[i]] == "successful") { 
      amountVideos = amountVideos - 1
    }
  } else {
    hasVideo = FALSE
  }
  
  # Calculate the number of words in the describition by using a small function
  wordcount <- function(str) {sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1}
  descriptionLength <- wordcount(descriptionText)  
  
  # Get the funding length by using a regex expression on a sentence
  x <- fundLength
  pattern <- "\\((.*)\\d"
  m <- regexpr(pattern, x)
  fundLength <- regmatches(x, m)
  
  # Count number of images and gifs
  imgCount <- 0
  gifCount <- 0
  
  if (length(amountImages)> 0) {
    for (j in 1:length(amountImages)) {
      if (grepl("jpg", amountImages[j])) {
        imgCount = imgCount + 1
      } else if (grepl("png", amountImages[j])) {
        imgCount = imgCount + 1
      } else if (grepl("gif", amountImages[j])) {
        gifCount = gifCount + 1 
      }
    }
  }
  
  #######################
  # Clean-up all values # 
  #######################
  
  # Extract Characters
  descriptionText <- gsub("\n", "", descriptionText)
  risksAndChallenges <- gsub("\n", "", risksAndChallenges)
  rewardOptions <- gsub("\\D", "", rewardOptions)
  amountComments <- gsub("\\D", "", amountComments) 
  fundLength <- gsub("\\D", "", fundLength)
  
  #################
  # Format values # 
  #################
  
  # Create numeric numbers where it is important
  rewardOptions <- as.numeric(rewardOptions)
  amountGifs <- as.numeric(gifCount)
  amountImages <- as.numeric(imgCount) 
  amountVideos <- as.numeric(amountVideos) +  as.numeric(amountYouTubeEmbed) 
  amountUpdates <- as.numeric(amountUpdates)
  amountComments <- as.numeric(amountComments)
  amountRewards <- as.numeric(amountRewards)
  fundLength <- as.numeric(fundLength)
  lowestReward <- as.numeric(rewardOptions[1])
  
  # Workaround resolving the amountFAQ = 0 issue
  if (length(amountFAQ) == 0 ) {
    amountFAQ = 0
  } else {
    amountFAQ <- as.numeric(amountFAQ)
  }
  
  ############################################
  # Load all project factors into data.frame # 
  ############################################
  
  # Function that sets missing values to NA
  checkEmpty <- function(arg1){
    if (length(arg1) == 0) {
      arg1 = NA
    } else {
      return(arg1)
    }
  }
  
  # Create temporary data.frame
  newrow <- data.frame(
    "ID" = searchPageDF$ID[[i]], "fundLength" = checkEmpty(fundLength),
    "descriptionLength"= checkEmpty(descriptionLength), "hasVideo" = checkEmpty(hasVideo), 
    "amountFAQ" = checkEmpty(amountFAQ), "amountUpdates" = checkEmpty(amountUpdates), 
    "amountComments" = checkEmpty(amountComments), "amountImages" = checkEmpty(amountImages),  
    "amountGifs"= checkEmpty(amountGifs), "amountVideos" = checkEmpty(amountVideos), "amountRewards"= checkEmpty(amountRewards), "lowestReward" = checkEmpty(lowestReward)
  )

  
  
  print("Data frame succesfully created.")
  
  # Bind data.frame to to kickPrjectDF
  projectPageDF <- rbind(projectPageDF, newrow)
  print("Data scraped and project entered into the data frame")
  print("----------------------------------------------------------")
  
} 

end_time <- Sys.time()
print("----------------------------------------------------------")
print("SCANNING FINSIHED")
print(paste("Number of projects scanned:", nrow(searchPageDF)))
print(paste("Number of entered projects:", nrow(projectPageDF)))
print(paste("Time elapsed:", round(end_time - start_time, digits = 4), "minutes"))
print("----------------------------------------------------------")


# Merge and clean data
# Delete all variables expect projectPageDF
#rm(list=setdiff(ls(), "searchPageDF", "projectPageDF", "kickProjectDF", "kickProjectDFraw"))

# Merge tables
kickProjectDF <- merge(projectPageDF,searchPageDF, by="ID")
kickProjectDFraw <- kickProjectDF

# Convert time format
kickProjectDF$fundEnd <- anydate(kickProjectDF$fundEnd)
kickProjectDF$fundStart <- anydate(kickProjectDF$fundStart)
kickProjectDF$fundLength <- kickProjectDF$fundEnd - kickProjectDF$fundStart

# Split categorySlug in parent and child
splitList <- matrix(unlist(strsplit(kickProjectDF$categorySlug, '/')), ncol=2, byrow=TRUE)
kickProjectDF$categoryParent <- splitList[,1]
kickProjectDF$categoryChild <- splitList[,2]

# Drop duplicated columns
drops <- c("country.1","fundState.1", "categorySlug")
kickProjectDF <- kickProjectDF[ , !(names(kickProjectDF) %in% drops)]

# Reorder columns
write.csv(kickProjectDF, file = "kickProjectDF.csv")

# Calculate fundUSDGoal into USD
kickProjectDF$fundUSDGoal <- kickProjectDF$fundGoal * kickProjectDF$fundUSDRate

# Format columns
kickProjectDF$fundUSDGoal = as.numeric(kickProjectDF$fundUSDGoal)
kickProjectDF$fundUSDPledged = as.numeric(kickProjectDF$fundUSDPledged)
kickProjectDF$fundLength = as.numeric(kickProjectDF$fundLength)
kickProjectDF$fundStart = as.Date(kickProjectDF$fundStart)
kickProjectDF$fundEnd = as.Date(kickProjectDF$fundEnd)