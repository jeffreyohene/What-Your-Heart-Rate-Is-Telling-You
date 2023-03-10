#libraries
library(tidyverse)
library(broom)
library(Metrics)

# Read datasets Cleveland_hd.csv into hd_data
hd_data <- read_csv('datasets/Cleveland_hd.csv')

# take a look at the first 5 rows of hd_data
head(hd_data,n=5)
     
# Use the 'mutate' function from dplyr to recode our data
hd_data %>% 
  mutate(hd= ifelse(class > 0, 1, 0))-> hd_data

# recode sex using mutate function and save as hd_data
hd_data %>% 
  mutate(sex= factor(sex, levels= 0:1,  labels= c('Female','Male')))-> hd_data

# Does sex have an effect? Sex is a binary variable in this dataset,
# so the appropriate test is chi-squared test
hd_sex <- chisq.test(hd_data$sex,hd_data$hd)

# Does age have an effect? Age is continuous, so we use a t-test
hd_age <- t.test(hd_data$age ~ hd_data$hd)

# What about thalach? Thalach is continuous, so we use a t-test
hd_heartrate <- t.test(hd_data$thalach ~ hd_data$hd)

# Print the results to see if p<0.05.
print(hd_sex)
print(hd_age)
print(hd_heartrate)

# Recode hd to be labelled
hd_data%>%
  mutate(hd_labelled= ifelse(hd== 0/1, 'No Disease', 'Disease')) -> hd_data

# age vs hd
ggplot(hd_data, 
       aes(x= hd_labelled, y= age)) + 
geom_boxplot()

# sex vs hd
ggplot(hd_data,
      aes(x=hd_labelled, color= sex)) + 
geom_bar(position="fill") + 
ylab("Sex %")

# max heart rate vs hd
ggplot(hd_data,
      aes(x= hd_labelled, y= thalach)) + 
geom_col()


# use glm function from base R and specify the family argument as binomial
model <- glm(data=hd_data, hd~ age + sex + thalach, family= "binomial")

# extract the model summary
summary(model)


# tidy up the coefficient table
tidy_m <- tidy(model)
tidy_m

# calculate OR
tidy_m$OR <- exp(tidy_m$estimate)

# calculate 95% CI and save as lower CI and upper CI
tidy_m$lower_CI <- exp(tidy_m$estimate - 1.96 * tidy_m$std.error)
tidy_m$upper_CI <- exp(tidy_m$estimate + 1.96 * tidy_m$std.error)

# display the updated coefficient table
tidy_m

# get the predicted probability in our dataset using the predict() function
pred_prob <- predict(model,hd_data, type = "response")

# create a decision rule using probability 0.5 as cutoff and save the predicted decision into the main data frame
hd_data$pred_hd <- ifelse(pred_prob >= 0.5,1,0)

# create a newdata data frame to save a new case information
newdata <- data.frame(age = 45, sex = "Female", thalach = 150)

# predict probability for this new case and print out the predicted value
p_new <- predict(model,newdata, type = "response")
p_new


# calculate auc, accuracy, clasification error
auc <- auc(hd_data$hd, hd_data$pred_hd)
accuracy <- accuracy(hd_data$hd, hd_data$pred_hd)
classification_error <- ce(hd_data$hd, hd_data$pred_hd)

# print out the metrics on to screen
print(paste("AUC=", auc))
print(paste("Accuracy=", accuracy))
print(paste("Classification Error=", classification_error))

# confusion matrix
table(hd_data$hd,hd_data$pred_hd, dnn=c("True Status","Predicted Status")) # confusion matrix

















