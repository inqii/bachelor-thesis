library("car")

library(devtools)
install_github("dgrtwo/broom")
library(broom)
options(scipen=999)

options(scipen=999)
# Model 1 (Complete Database with outliers) -----
model_1 <- lm(fundUSDPledged ~ amountVideos + amountFAQ + descriptionLength + fundUSDGoal + fundLength + fundBakers + amountVideos + amountRewards + hasVideo + isStaffpick, data=kickProjectDF)
summary(model_1)

# Model 2 (Outliers removed from all some factors) ----
model_2 <- lm(fundUSDPledged ~ fundLength + fundUSDGoal + descriptionLength + amountFAQ + amountUpdates + amountRewards + lowestRewards + amountImages + amountGifs + amountVideos + hasVideo + isStaffpick, data=kickModelDF)
summary(model_2)
tidy_model_2 <- tidy(model_2)
write.csv(tidy_model_2, "tidy_model_2.csv")

model_2DF <- data.frame(model_2)

# Model 3 (Logistical Regression Model on fundState as dependend variabel) ----
model_3 = glm(fundState ~ amountVideos + amountFAQ + descriptionLength + fundUSDGoal + fundLength + fundBakers + amountVideos + amountRewards + hasVideo + isStaffpick
              data=kickModelDF,
              family = binomial(link="logit"),
              na.action(na.omit)
)
summary(model_3)

# Model 4 (Outliers removed from all factors) ----
model_4 <- lm(fundUSDPledged ~ isStaffpick, data=kickModelDF)
summary(model_4)

#Testing the models
vif(model_2)
1/vif(model_2)
mean(vif(model_2))