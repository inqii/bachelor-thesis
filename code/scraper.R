#  Load library
start_time <- Sys.time() # Calculate time
library(rvest)

# For testing purposes
# kickUrl <- "https://www.kickstarter.com/projects/1755686645/vodrock-organic-vodka-est-2013-made-in-germany"
# kickProject <- read_html(as.character(kickUrl))

# Read database of kickURLs into a data.frame
projectLinkDB <- read.csv(file="kickProjectsDB-sample-5000.csv", header=FALSE, sep=",")

# Create a new data.frame, if there isn't any in existence allready
if (exists("kickProjectDF") && is.data.frame(get(kickProjectDF))) {
  print("Data frame exists. Appending Data to current data frame")
} else {
  kickProjectDF <- data.frame(
    "id" = character(), "title" = character(),"kickUrl" = character(), "subtitle" = character(),"creator" = character(),
    "category" =character(), "location" = character(),"start" = character(), 
    "fundEnd" = character(),"fundLength" = character(),"fundGoal" = character(), 
    "fundResult" = character(), "fundCurrency" = character(),"fundUSDRate" = character(),"fundCalcMoney" = character(), "fundSuccess" = character(),"amountBakers" = character(), 
    "descriptionLength" = character(), "hasVideo" = character(), "amountFAQ" = character(),"amountUpdates" = character(),
    "amountComments" = character(), "amountImages" = character(), "amountGifs" = character(),"amountVideos" = character(),
    "amountRewards" =  character(), "lowestReward" = character())
  
  print("Data frame succesfully created.")
}

###########################
# Start scraping the urls #
###########################

for (i in 1:projectLinkDB) {

  Sys.sleep(2) # Wait two seconds
  
  # Import variables from the projectLinkDB
  kickUrl <- projectLinkDB[i,1]
  fundUSDRate <- as.numeric(round(projectLinkDB[i,2], digits = 2))
  fundCalcMoney <- as.numeric(round(projectLinkDB[i,3], digits = 2))
  
  # Check if the URL and therefore the project is allready part of the data.frame
  if (grepl(kickUrl , kickProjectDF[3] == TRUE)) {
    next
    print("Allready found row entry for this project, skipping to the next")
  }
  
  # Download the source code of the project page as temporary file
  download.file(as.character(kickUrl), destfile = "scrapedpage.html", quiet=TRUE)
  kickProject <- read_html("scrapedpage.html")
  
  print("----------------------------------------------------------")
  print(paste(i, "th Project loaded", sep = ""))
  
  # Check if campaign was successful nor not
  checkFundSuccess <- kickProject %>% 
    html_nodes("#main_content") %>% 
    html_attr("class")

  if (checkFundSuccess == "Campaign-state-failed" | length(checkFundSuccess) == 0 ) {
    fundSuccess = FALSE
    print("Campaign has failed. Starting to scrape data!") 
  } else if (checkFundSuccess == "Campaign-state-successful"){
    fundSuccess = TRUE
    print("Campaign was successful. Starting to scrape data!")
  } else {
    print("Campaign active or canceled. Jumping to the next project!")
    print("----------------------------------------------------------")
    next
  }
  
  # Scrape first part of data, where the CSS-Selectors are the same for successfull and unsuccessfull projects
  
  descriptionHtml <- kickProject %>% 
    html_nodes(".description-container")
  
  descriptionText <- kickProject %>% 
    html_nodes(".description-container") %>% 
    html_text()
  
  rewardOptions <- kickProject %>% 
    html_nodes(".pledge__amount .money") %>%
    html_text()
  
  fundStart <- kickProject %>% 
    html_nodes(".f5 .js-adjust-time:nth-child(1)") %>%
    html_text()
  
  fundEnd <- kickProject %>% 
    html_nodes(".js-adjust-time+ .js-adjust-time") %>%
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
  
  fundLength <- kickProject %>% 
    html_nodes(".f5") %>% 
    html_text()

  ### Scrape second part of data
  
  if (fundSuccess) {
    title <- kickProject %>% 
      html_nodes(".relative .hero__link") %>%
      html_text()
    
    subTitle <- kickProject %>% 
      html_nodes(".js-edit-profile-blurb") %>%
      html_text()
    
    creator <- kickProject %>% 
      html_nodes(".mobile-show .js-update-text-color") %>%
      html_text()
    
    category <- kickProject %>% 
      html_nodes(".type-12+ .type-12") %>% 
      html_text()
    
    location <- kickProject %>% 
      html_nodes(".type-12:nth-child(1)") %>%
      html_text()
  
    fundResult <- kickProject %>% 
      html_nodes(".NS_campaigns__spotlight_stats .money") %>%
      html_text()
    
    fundGoal <- kickProject %>% 
      html_nodes(".navy-500 .money") %>%
      html_text()
    
    amountBakers <- kickProject %>% 
      html_nodes(".NS_campaigns__spotlight_stats b") %>%
      html_text()
    
    hasVideo <- kickProject %>%
      html_nodes("#video_pitch")
    
  } else {
    title <- kickProject %>% 
      html_nodes(".hide .medium.mb3") %>%
      html_text()
    
    subTitle <- kickProject %>% 
      html_nodes(".hide .type-18-md") %>%
      html_text()
    
    projectMeta <- kickProject %>%
      html_nodes(".type-12.keyboard-focusable") %>% 
      html_text()
    
    creator <- kickProject %>% 
      html_nodes("#react-project-header .hide .type-14.keyboard-focusable") %>%
      html_text()
    
    # Better way to get category and location
    projectMeta <- kickProject %>%
      html_nodes(".type-12.keyboard-focusable") %>% 
      html_text()
    
    if (length(projectMeta) == 6) {
      category <- projectMeta[2]
      location <- projectMeta[3]
    } else if (length(projectMeta) == 4) {
      category <- projectMeta[1]
      location <- projectMeta[2]
    }
    
    fundResult <- kickProject %>% 
      html_nodes(".hide .type-24-md .soft-black") %>%
      html_text()
    
    fundGoal <- kickProject %>% 
      html_nodes("#react-project-header .hide .money") %>%
      html_text()
    
    amountBakers <- kickProject %>% 
      html_nodes(".hide .soft-black span") %>%
      html_text()

    hasVideo <- kickProject %>%
      html_nodes(".order-2-lg .svg-icon__icon--play")

  }

  print(paste("Title:", title ))
  print(paste("URL:", kickUrl ))
  
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
    if (fundSuccess == 1) { 
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
  category <- gsub("\n", "", category)
  descriptionText <- gsub("\n", "", descriptionText)
  location <- gsub("\n", "", location)
  risksAndChallenges <- gsub("\n", "", risksAndChallenges)
  subTitle <- gsub("\n", "", subTitle)
  rewardOptions <- gsub("\\D", "", rewardOptions)
  amountBakers <- gsub("\\D", "", amountBakers)
  amountComments <- gsub("\\D", "", amountComments) 
  fundGoal <- gsub(",", "", fundGoal)
  fundCurrency <- gsub("[0-9]", "", fundGoal)
  fundGoal <- gsub("\\D", "", fundGoal) 
  fundResult <- gsub("\\D", "", fundResult)
  fundLength <- gsub("\\D", "", fundLength)
    
  
  #################
  # Format values # 
  #################
  
  # Create numeric numbers where it is important
  rewardOptions <- as.numeric(rewardOptions)
  amountBakers <- as.numeric(amountBakers)
  amountGifs <- as.numeric(gifCount)
  amountImages <- as.numeric(imgCount) 
  amountVideos <- as.numeric(amountVideos) +  as.numeric(amountYouTubeEmbed) 
  amountUpdates <- as.numeric(amountUpdates)
  amountComments <- as.numeric(amountComments)
  amountRewards <- as.numeric(amountRewards)
  fundGoal <- as.numeric(fundGoal)
  fundResult <- as.numeric(fundResult)
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
    "id" = id, "title" = checkEmpty(title),"kickUrl" = checkEmpty(kickUrl), "subtitle" = checkEmpty(subTitle),"creator" =checkEmpty(creator),
    "category" = checkEmpty(category), "location" = checkEmpty(location),"start" = checkEmpty(fundStart), 
    "fundEnd" = checkEmpty(fundEnd),"fundLength" = checkEmpty(fundLength),"fundGoal" = checkEmpty(fundGoal), 
    "fundResult" = checkEmpty(fundResult),"fundCurrency" = checkEmpty(fundCurrency), "fundUSDRate" = checkEmpty(fundUSDRate),"fundCalcMoney" = checkEmpty(fundCalcMoney),"fundSuccess" = checkEmpty(fundSuccess),"amountBakers" = checkEmpty(amountBakers), 
    "descriptionLength" =checkEmpty(descriptionLength), "hasVideo" = checkEmpty(hasVideo), "amountFAQ" = checkEmpty(amountFAQ), "amountUpdates" = checkEmpty(amountUpdates),
    "amountComments" = checkEmpty(amountComments), "amountImages" = checkEmpty(amountImages), "amountGifs" =checkEmpty(amountGifs),"amountVideos" = checkEmpty(amountVideos),
    "amountRewards" = checkEmpty(amountRewards), "lowestReward" = checkEmpty(lowestReward)
  )
  
  # Bind data.frame to to kickPrjectDF
  kickProjectDF <- rbind(kickProjectDF, newrow)
  print("Data  scraped and project entered into the data frame")
  print("----------------------------------------------------------")
  
} 


end_time <- Sys.time()
print("----------------------------------------------------------")
print("SCANNING FINSIHED")
print(paste("Number of projects scanned:", nrow(projectLinkDB)))
print(paste("Number of entered projects:", nrow(kickProjectDF)))
print(paste("Time elapsed:", round(end_time - start_time, digits = 4), "minutes"))
print("----------------------------------------------------------")

# Delete all variables expect kickProjectDF
# rm(list=setdiff(ls(), "kickProjectDF"))
