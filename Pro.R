lifetime_ = "https://ourworldindata.org/grapher/share-who-report-lifetime-anxiety-or-depression.csv?v=1&csvType=full&useColumnShortNames=true"
medication_ = "https://ourworldindata.org/grapher/dealt-with-anxiety-depression-took-prescribed-medication.csv?v=1&csvType=full&useColumnShortNames=true"
talking_ = "https://ourworldindata.org/grapher/dealt-with-anxiety-depression-friends-family.csv?v=1&csvType=full&useColumnShortNames=true"
policy_ = "https://ourworldindata.org/grapher/stand-alone-policy-or-plan-for-mental-health.csv?v=1&csvType=full&useColumnShortNames=true"

#import data file as csv
dat_l <- read.csv(lifetime_)
dat_m <- read.csv(medication_)
dat_t <- read.csv(talking_)
dat_p <- read.csv(policy_)

#Read R package
#Suppress the startup messages when loading tidyverse
suppressPackageStartupMessages(library(tidyverse))

#Merge data from different files by Entity
dat_lm <- left_join(dat_l, dat_m, by = "Entity")
dat_lmt <- left_join(dat_lm, dat_t, by = "Entity")
dat <-left_join(dat_lmt, dat_p, by = "Entity")

#omit rows with NA in any column of data frame
dat <- na.omit (dat)

#Simplification of columns
dat <- dat %>%
  rename(code = Code.x, year = Year.x, lifetime = share__question_mh7a__have_been_anxious_depressed__answer_yes__gender_all__age_group_all, medication = share__question_mh8d__took_prescribed_medication_when_anxious_depressed__answer_yes__gender_all__age_group_all, talking = share__question_mh8c__talked_to_friends_or_family_when_anxious_depressed__answer_yes__gender_all__age_group_all, policy = stand_alone_policy_or_plan_for_mental_health)

#Removing duplicates
dat <- dat[!duplicated(dat$Entity),]
dat <- dat %>% select(-Code.y, -Year.y, -Code.x.x, -Year.x.x, -Code.y.y, -Year.y.y)


#Calculate correlation coefficients for each pair (medication ~ lifetime and talking ~ lifetime)
correlation1 <- cor(dat$medication, dat$lifetime)
correlation2 <- cor(dat$talking, dat$lifetime)

#Convert data to long format for ggplot
data_long <- dat %>%
  pivot_longer(cols = c(medication, talking), names_to = "x_var", values_to = "x_value")
y <- dat$lifetime

#Plot of linear correlation coefficients
#Create the scatter plot with both medication and talking against lifetime in the same plot
#The blue and red dots and lines represent medication ~ lifetime, talking ~ lifetime
library(ggplot2)
suppressWarnings({
  p <- ggplot(data_long, aes(x = x_value, y = lifetime, color = x_var)) 
  p + geom_point() + 
    geom_smooth(aes(color = x_var), method = "lm", se = FALSE) +  
    ggtitle(paste("Correlation Coefficients:")) +
    scale_color_manual(values = c("medication" = "blue", "talking" = "red")) + 
    theme(panel.background = element_rect(fill = "white"), plot.title = element_text(hjust = 0.5),  panel.grid.minor = element_line(color = "lightgray", size = 0.25))  
  
  ggsave("plot/correlation coefficients.png", plot = p, width = 8, height = 6, dpi = 300)
})

#Saving the plot
ggsave("plot/correlation coefficients.png", plot = p, width = 8, height = 6, dpi = 300)

