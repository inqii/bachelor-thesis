# Packages -----
library(cowplot)
library(dplyr)
library(anytime)
library(purrr)
library(tidyr)

# Styling for scattterplots ----
options(scipen=999)

fh_colors <- c(
  `mint`        = "#05A798",
  `black`      = "#191D22",
  `green`       = "#04B440",
  `blue`     = "#0251A1",
  `light_blue`     = "#0288AD",
  `light_green` = "#09AD62",
  `dark grey`  = "#8c8c8c",
  `flat_yellow` = "#EFC241",
  `flat_green` = "#42C977",
  `flat_blue` = "#409AD6",
  `flat_lila` = "#995FB2",
  `flat_red` = "#E34E47"

  )

fh_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (fh_colors)
  fh_colors[cols]
}

fh_palettes <- list(
  `main`  = fh_cols("black", "mint"),

  `colorful`  = fh_cols("mint", "light_green", "green", "blue" , "light_blue"),

  `flat_pie`  = fh_cols("flat_lila", "flat_red", "flat_yellow", "flat_green" , "black")

)

fh_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- fh_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

scale_color_fh <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fh_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("fh_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_fh <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fh_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("fh_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

library(cowplot)
options(scipen=999)

# Piecharts ----

pie1 <-ggplot(kickProjectDF, aes(x = "", fill = factor(fundState))) +
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(fill="fundState", x=NULL, y=NULL) +
  coord_polar(theta = "y", start=0) +
  scale_fill_fh("flat_pie")

# Histograms ------
hist1<- ggplot(kickProjectDF, aes(fundStart)) +
  geom_histogram(aes(fill=fundState), bins = 15, alpha = 0.9) +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs( x = "Years", y = "Frequency") +
  scale_fill_fh("flat_pie")

plot_grid(pie1, hist1, labels = c("A", "B"), align = "h")
ggsave("distribution_kickprojects.png", plot = last_plot() , device = png(), scale = 2, width = 16, height = 5,
       dpi = 300, units = "cm")

ggplot(kickProjectDF, aes(fundUSDPledged)) +
  geom_histogram(aes(fill=fundState), bins = 15, alpha = 0.9) +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title="Histogram 1.1 ", subtitle="Frequency of funding", x = "Funding in $", y = "Frequency") +
  scale_fill_fh("flat_pie")

overviewhist <- kickProjectDF
overviewhist$ID <- NULL
overviewhist$fundUSDRate <- NULL
overviewhist$fundGoal <- NULL

overviewhist %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

ggsave("numeric_histograms.png", plot = last_plot() , device = png(), scale = 3, width = 14, height = 7,
       dpi = 300, units = "cm")

# Successrate by Staffpick
fundState.pct <- kickProjectDF %>%
  filter(fundState %in% c("successful", "failed")) %>%
  group_by(isStaffpick, fundState) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(fundState), pct)

fundState.pct$isStaffpick <- factor(fundState.pct$isStaffpick,
                                  levels=fundState.pct$isStaffpick[1:(nrow(fundState.pct)/2)])

ggplot(fundState.pct, aes(isStaffpick, pct, fill=fundState)) + geom_bar(stat="identity") +
  ggtitle("Success vs. Failure Rate by staffpick") +
  xlab("Staffpick") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) +
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5),
            colour="white", size=5) + theme_bw() +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"),
        axis.text.x=element_text(size=12), legend.position="bottom",
        legend.title=element_text(size=12, face="bold")) + coord_flip()

ggsave("success_rate_isStaffpick.png", plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
       dpi = 300, units = "cm")

# Successrate by hasVideo
fundState.pct <- kickProjectDF %>%
  filter(fundState %in% c("successful", "failed")) %>%
  group_by(hasVideo, fundState) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(fundState), pct)

fundState.pct$hasVideo <- factor(fundState.pct$hasVideo,
                                    levels=fundState.pct$hasVideo[1:(nrow(fundState.pct)/2)])

ggplot(fundState.pct, aes(hasVideo, pct, fill=fundState)) + geom_bar(stat="identity") +
  ggtitle("Success vs. Failure Rate by hasVideo") +
  xlab("hasVideo") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) +
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5),
            colour="white", size=5) + theme_bw() +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"),
        axis.text.x=element_text(size=12), legend.position="bottom",
        legend.title=element_text(size=12, face="bold")) + coord_flip()

ggsave("success_rate_hasVideo.png", plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
       dpi = 300, units = "cm")

# Automatic scatterplots ----

# Saving all relevant faktors
df_variables <- c(
  `fL`   = "fundLength",
  `fG`   = "fundUSDGoal",
  `fP`   = "fundPledged",
  `fUP`  = "fundUSDPledged",
  `fS`   = "fundState",
  `fB`   = "fundBakers",
  `aFAQ` = "amountFAQ",
  `aU`   = "amountUpdates",
  `aC`   = "amountComments",
  `aI`   = "amountImages",
  `aG`   = "amountGifs",
  `aV`   = "amountVideos",
  `aR`   = "amountRewards",
  `lR`   = "lowestReward",
  `dL`   = "descriptionLength",
  `hV`   = "hasVideo",
  `iSP`  = "isStaffpick")

# Function to return list or single factor
df_var <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (df_variables)
  df_variables[cols]
}

# Factor combinations to be plotted
df_combinations <- data.frame(
  `fL_vs_fUP`  = df_var("fL", "fUP"),
  `fG_vs_fUP`  = df_var("fG", "fUP"),
  `fB_vs_fUP`  = df_var("fB", "fUP"),
  `aC_vs_fUP`  = df_var("aC", "fUP"),
  `aU_vs_fUP`  = df_var("aU", "fUP"),
  `aC_vs_fUP`  = df_var("aC", "fUP"),
  `aI_vs_fUP`  = df_var("aI", "fUP"),
  `aV_vs_fUP`  = df_var("aV", "fUP"),
  `aR_vs_fUP`  = df_var("aR", "fUP"),
  `dL_vs_fUP`  = df_var("dL", "fUP"),
  `hV_vs_fUP`  = df_var("hV", "fUP"),
  `iSp_vs_fUP`  = df_var("iSp", "fUP")
)

#Function to remove outliers
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

# Function to go trough all combinations, remove outlier and create scatterplots
for (i in 1:ncol(df_combinations)) {
  x1 = character(0)
  y1 = character(0)
  x1 = df_combinations[1,i]
  y1 = df_combinations[2,i]

  tempDF = kickModelDF

  for (j in 1:2) {
    temp_data = subset(tempDF, ,c(as.character(df_combinations[j,i])))
    temp_data_cleaned = remove_outliers(temp_data)
    tempDF[, as.character(df_combinations[j,i])] = temp_data_cleaned
  }

  # Delete Columns with NA
  tempDF = na.omit(tempDF)

  print(
    scatter <- ggplot(kickProjectDFC, aes_string(as.character(x1), as.character(y1))) +
      geom_point(aes(colour=fundState, alpha= .4)) +
      geom_smooth(method="loess", se=F, colour="#FF5200") +
      theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
      labs(title= paste(x1, "vs", y1), subtitle="With outlier" x = x1, y = y1, colour="Funding success") +
      scale_color_fh()
  )
  print(paste("Scatterplot", "-", colnames(df_combinations)[i], "printed"))

  ggsave(paste(x1, "_", y1, "_outliers.png",  sep=""), plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
         dpi = 300, units = "cm")

  Sys.sleep(2)

  Sys.sleep(2)
  print(
    scatterClean <- ggplot(tempDF, aes_string(as.character(x1), as.character(y1))) +
      geom_point(aes(colour=fundState, alpha= .4)) +
      geom_smooth(method="loess", se=F, colour="#FF5200") +
      theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
      labs(title= paste(x1, "vs", y1), x = x1, y = y1, colour="Funding success") +
      scale_color_fh()
  )

  ggsave(paste(x1, "_", y1, ".png",  sep=""), plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
         dpi = 300, units = "cm")


  print(paste("Scatterplot", "-", colnames(df_combinations)[i], "(Without outliers) printed"))
}

# Scatterplot for Staffpick, amountGifs, amountFAQ
ggplot(kickModelDF, aes(x = isStaffpick,y = fundUSDPledged)) +
  geom_jitter(aes(color=fundState, alpha= .4)) +
  geom_smooth(method="loess", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title= "isStaffpick vs. fundUSDPledged", x = "isStaffpick", y = "fundUSDPledged", colour="Funding success") +
  scale_color_fh()

ggsave("isStaffpick_fundUSDPledged.png" ,plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
       dpi = 300, units = "cm")

Sys.sleep(2)

ggplot(kickModelDF, aes(x = hasVideo,y = fundUSDPledged)) +
  geom_jitter(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="loess", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title= "hasVideo vs. fundUSDPledged", x = "hasVideo", y = "fundUSDPledged", colour="Funding success") +
  scale_color_fh()

ggsave("hasVideo_fundUSDPledged.png" ,plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
       dpi = 300, units = "cm")
Sys.sleep(2)

ggplot(kickModelDF, aes(x = amountGifs,y = fundUSDPledged)) +
  geom_jitter(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title= "amountGifs vs. fundUSDPledged", subtitle="With outlier", x = "amountGifs", y = "fundUSDPledged", colour="Funding success") +
  scale_color_fh()

ggsave("amountGifs_fundUSDPledged.png" ,plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
       dpi = 300, units = "cm")
Sys.sleep(2)

ggplot(kickModelDF, aes(x = amountFAQ,y = fundUSDPledged)) +
  geom_jitter(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title= "amountFAQ vs. fundUSDPledged", subtitle="With outlier",x = "amountFAQ", y = "fundUSDPledged", colour="Funding success") +
  scale_color_fh()

Sys.sleep(2)

ggsave("amountFAQ_fundUSDPledged.png" ,plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
       dpi = 300, units = "cm")

ggplot(kickModelDF, aes(x = lowestReward,y = fundUSDPledged)) +
  geom_jitter(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="loess", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title= "lowestReward vs. fundUSDPledged", x = "lowestReward", y = "fundUSDPledged", colour="Funding success") +
  scale_color_fh()

Sys.sleep(2)

ggsave("lowestReward_fundUSDPledged.png" ,plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
       dpi = 300, units = "cm")

ggplot(kickModelDF, aes(x = amountVideos,y = fundUSDPledged)) +
  geom_jitter(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title= "amountVideos vs. fundUSDPledged", x = "amountVideos", y = "fundUSDPledged", colour="Funding success") +
  scale_color_fh()

Sys.sleep(2)

ggsave("amountVideos_fundUSDPledged.png" ,plot = last_plot() , device = png(), scale = 1, width = 12, height = 9,
       dpi = 300, units = "cm")

# Scatterplot for Staffpick, amountGifs, amountFAQ
paste(x1, "vs", y1)

# Normal Scatterplots -----------------
# x = descriptionLength, y = fundCalcMoney, kickProjectDFC
ggplot(kickProjectDFC, aes(x = fundUSDPledged, y = fundLength)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Number of words in describition", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh()

# x = descriptionLength, y = fundCalcMoney, kickProjectDFCclean
ggplot(kickProjectDFCclean, aes(x = descriptionLength, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Number of words in describition", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh()

# x = amountImages, y = fundCalcMoney, kickProjectDFC
ggplot(kickProjectDFC, aes(x = amountImages, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Amount of images", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh()

# x = amountImages, y = fundCalcMoney, kickProjectDFCclean
ggplot(kickProjectDFCclean, aes(x = amountImages, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Amount of images", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh()

# x = amountImages, y = fundCalcMoney, kickProjectDFCclean
ggplot(kickProjectDFC, aes(x = amountGifs, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Amount of gifs", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh() +
  ylim(c(0, 500000))

ggplot(kickProjectDFC, aes(x = amountVideos, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Amount of Videos", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh() +
  ylim(c(0, 500000))

ggplot(kickProjectDFC, aes(x = amountUpdates, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Amount of updates", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh()

ggplot(kickProjectDFC, aes(x = amountComments, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Amount of comments", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh()

ggplot(kickProjectDFC, aes(x = amountFAQ, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Amount of FAQs", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh()

ggplot(kickProjectDFC, aes(x = lowestReward, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Lowest reward", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh()

ggplot(kickProjectDFC, aes(x = amountRewards, y = fundCalcMoney)) +
  geom_point(aes(colour=fundState, alpha= .4)) +
  geom_smooth(method="lm", se=F, colour="#FF5200") +
  theme_bw() + theme(text = element_text(family = "Verdana", colour = "#00A299", size = 12)) +
  labs(title = "Scatter 1", x= "Amount of rewards", y="Fundraising result in $", colour="Funding success") +
  scale_color_fh()