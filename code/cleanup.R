# Reorder data.frame and create new clean data.frame ----
kickProjectDF <- kickProjectDF[,c(1, 13, 14, 15, 16, 17, 18, 19 , 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 2, 3, 4, 5, 6, 7, 8, 9, 10 , 11, 12)]
kickProjectDFC = kickProjectDF

# Remove every row that contains missing data "NA" -----
kickProjectDFC = na.omit(kickProjectDFC)

# Remove all campaings with a fund result under 200 $ ----
kickProjectDFC = subset(kickProjectDFC, kickProjectDFC$fundUSDPledged > 200)

# Remove all not finshed camapigns and set fundState to boolean -----
kickProjectDFC = subset(kickProjectDFC, kickProjectDFC$fundState == "successful" | kickProjectDFC$fundState == "failed")
kickProjectDFC$fundState <- ifelse (kickProjectDFC$fundState=="failed", 0, 1)
kickProjectDFC$fundState = as.logical(kickProjectDFC$fundState)

# Remove outliers from specific factors -----
# Function to SET all data to NA, which is not between the IQR * 1.5 ----
remove_outliers <- function(x, na.rm = TRUE, ...) {

  #find position of 1st and 3rd quantile not including NA's
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)

  H <- 1.5 * IQR(as.matrix(x), na.rm = na.rm)

  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  x<-y

  #get rid of any NA's
  #x[!is.na(x)]
  return(x)
}

outlierCol <- c("lowestReward", "fundUSDGoal", "fundUSDPledged", "descriptionLength", "amountUpdates", "amountComments", "amountImages", "amountRewards")
print("Removing outliers for the following columns:")
print(outlierCol)

for (i in 1:length(outlierCol)) {
  temp_data <-  kickProjectDFC[ , grepl(outlierCol[i], names(kickProjectDFC)) ]
  temp_data_cleaned = remove_outliers(temp_data)
  kickProjectDFC[, as.character(outlierCol[i])] = temp_data_cleaned
  print(i)
}

# Delete Columns with NA
kickModelDF = na.omit(kickProjectDFC)




