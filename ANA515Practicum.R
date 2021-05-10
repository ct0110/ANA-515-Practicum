# ANA 515 Practicum
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

# Exclude the useless variables in GT1 in the first dataframe
View(GT_2018_19_Responses_1)
gt1 <- names(GT_2018_19_Responses_1) %in% c("School Preferences", 
                                            "School Assigned", 
                                            "Will you enroll there?", 
                                            "X13" ,"X14")
newgt1 <- GT_2018_19_Responses_1[!gt1]
View(newgt1)

# Exclude the useless variables GT2 in the second dataframe
View(GT_2018_19_Responses_2)
gt2 <- names(GT_2018_19_Responses_2) %in% c("verbal plus nonverbal", 
                                            "School Preferences", 
                                            "School Assigned", 
                                            "Will you enroll there?", 
                                            "X14", "X15")
newgt2 <- GT_2018_19_Responses_2[!gt2]
View(newgt2)

# Adding Rows: to join two data frames (datasets) vertically, use the rbind 
# function. The two data frames must have the same variables, but they do not 
# have to be in the same order.
gt3 <- rbind(newgt1, newgt2)
View(gt3)

# na.omit() – remove rows with na from a list
# This is the easiest option. 
# The na.omit() function returns a list without any rows that contain na values.
# This is the fastest way to remove na rows in the R programming language.
# remove na in r - remove rows - na.omit function / option
gt4 <- na.omit(gt3)
View(gt4)

# Sub in R – Finding Alternative Matches
# Sometimes what you’re looking for may involve more than one thing. 
# In the example below, we want to adjust a pet specific text (dog, cat, etc.) 
# to refer the companion animal as a more generic “pet”. We use the | operator 
# within a regular expression to set this up.
# sub in r - regular expression for alternatives
# Replace Character or Numeric Values in Data Frame to deal with missing 
# data, outliers and errors by justifying
# data2$x4 <- as.character(data2$x4)
# data2[data2 == "f2"] <- "YYY"

gt4$`Entering Grade Level` <- as.character(gt4$`Entering Grade Level`)
gt4$`Entering Grade Level`[gt4$`Entering Grade Level` == "first"] <- "1"

gt4$`Entering Grade Level` <- as.character(gt4$`Entering Grade Level`)
gt4$`Entering Grade Level`[gt4$`Entering Grade Level` == "Kindergarten"] <- "K"

gt4$`Entering Grade Level` <- as.character(gt4$`Entering Grade Level`)
gt4$`Entering Grade Level`[gt4$`Entering Grade Level` == "Kinder"] <- "K"

gt4$`Entering Grade Level` <- as.character(gt4$`Entering Grade Level`)
gt4$`Entering Grade Level`[gt4$`Entering Grade Level` == "k"] <- "K"

gt4$`Birth Month` <- as.character(gt4$`Birth Month`)
gt4$`Birth Month`[gt4$`Birth Month` == "11"] <- "November"

gt4$`Birth Month` <- as.character(gt4$`Birth Month`)
gt4$`Birth Month`[gt4$`Birth Month` == "12"] <- "December"

gt4$`Birth Month` <- as.character(gt4$`Birth Month`)
gt4$`Birth Month`[gt4$`Birth Month` == "2"] <- "February"

gt4$`Birth Month` <- as.character(gt4$`Birth Month`)
gt4$`Birth Month`[gt4$`Birth Month` == "Febrauary"] <- "February"

gt4$`Birth Month` <- as.character(gt4$`Birth Month`)
gt4$`Birth Month`[gt4$`Birth Month` == "september"] <- "September"

gt4$`OLSAT Verbal Percentile` <- as.character(gt4$`OLSAT Verbal Percentile`)
gt4$`OLSAT Verbal Percentile`[gt4$`OLSAT Verbal Percentile` == "91%"] <- "91"

gt4$`NNAT Non Verbal Percentile` <- as.character(gt4$`NNAT Non Verbal Percentile`)
gt4$`NNAT Non Verbal Percentile`[gt4$`NNAT Non Verbal Percentile` == "71%"] <- "71"

gt4$`NNAT Non Verbal Percentile` <- as.character(gt4$`NNAT Non Verbal Percentile`)
gt4$`NNAT Non Verbal Percentile`[gt4$`NNAT Non Verbal Percentile` == "98%"] <- "98"

gt4$`NNAT Non Verbal Percentile` <- as.character(gt4$`NNAT Non Verbal Percentile`)
gt4$`NNAT Non Verbal Percentile`[gt4$`NNAT Non Verbal Percentile` == "99%"] <- "99"

gt4$`NNAT Non Verbal Percentile` <- as.character(gt4$`NNAT Non Verbal Percentile`)
gt4$`NNAT Non Verbal Percentile`[gt4$`NNAT Non Verbal Percentile` == "0"] <- 
  mean(gt4$`NNAT Non Verbal Percentile`)

gt4$`Overall Score` <- as.character(gt4$`Overall Score`)
gt4$`Overall Score`[gt4$`Overall Score` == "9"] <- mean(gt4$`Overall Score`)

gt4$`NNAT Non Verbal Raw Score` <- as.character(gt4$`NNAT Non Verbal Raw Score`)
gt4$`NNAT Non Verbal Raw Score`[gt4$`NNAT Non Verbal Raw Score` == "40/50"] <- "45"

gt4$`NNAT Non Verbal Raw Score` <- as.character(gt4$`NNAT Non Verbal Raw Score`)
gt4$`NNAT Non Verbal Raw Score`[gt4$`NNAT Non Verbal Raw Score` == "4"] <- 
  mean(gt4$`NNAT Non Verbal Raw Score`)

gt4$`NNAT Non Verbal Raw Score` <- as.character(gt4$`NNAT Non Verbal Raw Score`)
gt4$`NNAT Non Verbal Raw Score`[gt4$`NNAT Non Verbal Raw Score` == "n45"] <- "45"

gt4$`OLSAT Verbal Score` <- as.character(gt4$`OLSAT Verbal Score`)
gt4$`OLSAT Verbal Score`[gt4$`OLSAT Verbal Score` == "25/30"] <- "28"

gt4$`OLSAT Verbal Score` <- as.character(gt4$`OLSAT Verbal Score`)
gt4$`OLSAT Verbal Score`[gt4$`OLSAT Verbal Score` == "83%"] <- mean(gt4$`OLSAT Verbal Score`)

gt4$`OLSAT Verbal Score` <- as.character(gt4$`OLSAT Verbal Score`)
gt4$`OLSAT Verbal Score`[gt4$`OLSAT Verbal Score` == "0.83"] <- mean(gt4$`OLSAT Verbal Score`)

# remove na in r - remove rows - na.omit function / option
gt5 <- na.omit(gt4)
View(gt5)

# use the round() function
round(gt5,digits=0)

# Data Visuals: Histogram

gt5$`OLSAT Verbal Score` <- as.numeric(gt5$`OLSAT Verbal Score`)
hist(gt5$`OLSAT Verbal Score`, col="blue", main="NYC parents report their kids’ scores on 
     the gifted and talented exam", xlab= "OLSAT Verbal Score", ylab ="Frequency", 
     labels = TRUE)

gt5$`NNAT Non Verbal Raw Score`<- as.numeric(gt5$`NNAT Non Verbal Raw Score`)
hist(gt5$`NNAT Non Verbal Raw Score`, col="brown", main="NYC parents report their kids’ scores on 
     the gifted and talented exam", xlab= "NNAT Non Verbal Raw Score", ylab ="Frequency", 
     labels = TRUE)

# Data Visuals: Plot
set.seed(1)
ovs <- gt5$`OLSAT Verbal Score`
nvrs <- gt5$`NNAT Non Verbal Raw Score`
Birth_Month <- gt5$`Birth Month`
District <- gt5$District

ggplot(gt5, aes(x = ovs, y = nvrs)) + 
  geom_point(aes(color = District)) + labs(x = "OLSAT Verbal Score", 
                                    y = "NNAT Non Verbal Raw Score")

ggplot(gt5, aes(x = ovs, y = nvrs)) + 
  geom_point(aes(color = Birth_Month)) + labs(x = "OLSAT Verbal Score", 
                                           y = "NNAT Non Verbal Raw Score")
