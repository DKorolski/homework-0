#Loading libraries
if(!require(tidyverse)) install.packages ("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages ("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages ("data.table", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org") #Loading dataset

#Loading dataset
library(rvest)
url <- "http://tunedit.org/repo/DASL/CancerSurvival.arff"
web <- read_html(url)
t <- html_nodes(web,"textarea")
desc <- html_text(t)
desc1 <- str_sub(desc,-852,-4)
desc5 <- gsub(",","\t",desc1)
desc6 <- str_replace_all(desc5,"'","")
desc7 <- read_tsv(desc6,col_names = c("Survival", "Organ"))
z <- data.frame(desc7)
str(z)

# alternative loading from file (github repository link is provided) if url is broken z <- read_excel (path = 'cancer_1.xlsx', sheet = 'cancer')

library (lubridate)
#Preparing dataset
#testing for any N/A
colSums(is.na(z))
#exploring dataset
str(z)
#converting char to factor
z$Organ <- factor(z$Organ)
table(z$Organ)
#H0_hypothesis - survival time is equally dependent on organ
#H1_hypothesis - survival time is not equally dependent on organ
#checking for amount of data in the set
table(z)
#dataset is small. Data is set of independent medical cases
#Plotting dataset distibution
boxgraph <- ggplot(data = z, aes(x = Organ, y = Survival)) +
geom_boxplot() + 
labs(x = "Diagnosis", y = "Survival time in days") +
theme(axis.text.x = element_text(angle = 15, vjust = 0.9, hjust = 0.5))
#The plotted boxplot shows the presence of extremely high survival rates in each group. 
#Since these outliers are uniformly distributed across all groups, it is more likely to conclude that these are not sample artifacts, but a strong sign of an asymmetric distribution. 
#Their exclusion will lead to a distortion of the initial nature of the distribution, so it was decided to leave all the data in the array for further analysis.

qq.raw <- ggplot(aes(sample = Survival), data = z) +
geom_qq() + geom_qq_line() +
scale_x_continuous(labels = NULL, name = "Theoretic quantiles") +
scale_y_continuous(labels = NULL, name = "Experimental quantiles") +
facet_wrap(~Organ, ncol = 5)

#Performing t-test comparing selected organ (breast) with others
Stomach <- (z$Survival[z$Organ == "Stomach"])
Breast <- (z$Survival[z$Organ == "Breast"])
Bronchus <- (z$Survival[z$Organ == "Bronchus"])
Colon <- (z$Survival[z$Organ == "Colon"])
Ovary <- (z$Survival[z$Organ == "Ovary"])
t_St <- t.test(Breast, Stomach)
t_Br <- t.test(Breast, Bronchus)
t_C <- t.test(Breast, Colon)
t_O <- t.test(Breast, Ovary)
p_vals <- c(t_St$p.value, t_Br$p.value, t_C$p.value, t_O$p.value)
#Performing Holm adjustment 
p_holm <- p.adjust(p_vals, method = 'holm')
sum(p_holm <= 0.05)
p_holm
#H1_hypothesis is correct for Breast-Stomach, Breast-Bronchus pairs
#plotting results
ggplot(z, aes(x = Organ, y = Survival, colour = Organ)) + geom_point(size = 2) + scale_colour_brewer( palette = 'Set1') + facet_wrap(~Organ, nrow = 1)+theme(axis.text.x = element_blank())
#Making prediction system
#preparing train and test datasets for small dataset with normal distribution
z$Organ <- factor(z$Organ)
str(z)
# Set seed
set.seed (42)
#making permutations 
n_obs <- nrow(z)
permuted_rows<-sample(n_obs)

z_shuffled <- z[permuted_rows, ]

# Identify row to split on: split
split <- round(n_obs * 0.6)

# Create train
train <- z_shuffled[1:split, ]
str(train)
# Create test
test <- z_shuffled[(split+1):n_obs, ]
str(test)
#using linear regression model
# Fit lm model on train: model
model <- lm (Survival ~ Organ , train)
head (model)
# Predict on test: p
p <- predict(model,test)
length(p)
# Compute errors: error
error <- p - test[["Survival"]]
length(p)
nrow(test)
length(error)
# Calculate RMSE
rmse1<-sqrt(mean(error^2))
rmse_result <- data_frame(method = "Linear regression model 1", RMSE = rmse1)
rmse_result %>% knitr::kable()

#CROSS-VALIDATION.Fit Predictive Models Over Different Tuning Parameters
# Fit lm model using 10-fold CV: model
model <- train(
  Survival~Organ , 
  z,
  method = "lm",
  trControl = trainControl(            #train-control func
    method = "cv", 
    number = 10, #10-fold cross validation
    verboseIter = TRUE
  )
)
rmse2<-model$results$RMSE
rmse_result <- data_frame(method = "Linear regression+ 10 fold cross validation", RMSE = rmse2)
rmse_result %>% knitr::kable()

# Fit lm model using 5 x 5-fold CV: model
model <- train(
  Survival~Organ, 
  z,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", 
    number = 5,
    repeats = 5, 
    verboseIter = TRUE
  )
)
rmse3<-model$results$RMSE
rmse_result <- data_frame(method = "Linear regression+ 5x5 fold repeated cross validation", RMSE = rmse3)
rmse_result %>% knitr::kable()

# Show the coefficients
mod <- lm(Survival~Organ, z)
coef(mod)
# Show the full output
summary(mod)
# View summary of model
summary(mod)
# Compute the mean of the residuals
mean(residuals(mod))
# Compute RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))
z$pred <- predict(model)
# Make a plot to compare predictions to actual (prediction on x axis). 
ggplot(z, aes(x = pred, y = Survival)) + 
  geom_point() +
  geom_abline(color = "blue")
#training average+organ effect system
mu <- mean(train$Survival)
survival_avgs <- train %>%
  group_by(Organ) %>%
  summarize(b_i = mean(Survival - mu))
# predicted ratings
predicted_ratings_bi <- mu + test %>%
  left_join(survival_avgs, by = "Organ") %>%
  .$b_i
rmse_4 <- RMSE(test$Survival, predicted_ratings_bi)
rmse_result <- data_frame(method = "Linear regression+ regular model", RMSE = rmse_4)
rmse_result %>% knitr::kable()
#Checking for any flaws.Significant flaws solved with lintr.

# Results

rmse_result <- data_frame(method = "Linear regression model 1", RMSE = rmse1)

rmse_results <- bind_rows(rmse_result,
                          data_frame(method = "Linear regression+ 10 fold cross validation", RMSE = rmse2))
rmse_results <- bind_rows(rmse_results,
                           data_frame(method = "Linear regression+ 5x5 fold repeated cross validation", RMSE = rmse3))
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Linear regression+ regular model", RMSE = rmse_4))
rmse_results %>% knitr::kable()


#Conclusion.This IDV project was examined  to observe data, check hypothesis and to predict survival time. The model evaluation performance through the RMSE ( root mean squared error) showed that the Linear regression models are useful to predict survival time on the test set. 

print("Operating System:")
version
