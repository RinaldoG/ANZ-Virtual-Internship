library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(corrplot)
library(lsr)
library(ggpubr)
library(tidyverse)
library(rpart)
library(car)
library(DAAG)
library(Metrics)

#Data Import
anz <- read_xlsx("ANZ.xlsx")
anz <- anz[,c(-3:-6,-8,-9,-19:-23)]

#Data Subsetting
salary <- anz %>% 
  filter(txn_description=="PAY/SALARY") %>%
  group_by(first_name) %>%
  summarise(age = max(age),
            gender = max(gender),
            "salary" = (sum(amount)*4))

salary2 <- anz %>%
  filter(!txn_description=="PAY/SALARY") %>%
  group_by(first_name) %>%
  summarise(most_suberb = head(names(sort(table(merchant_suburb),decreasing=TRUE)),1),
            most_state = head(names(sort(table(merchant_state),decreasing=TRUE)),1),
            most_txn = head(names(sort(table(txn_description),decreasing=TRUE)),1))

salary3 <- anz %>%
  group_by(first_name)%>%
  arrange(extraction) %>%
  summarise("initial balance" = head(balance,1),
            "final balance" = tail(balance,1))
salary3 <- salary3 %>% mutate("overall balance change" = `final balance`-`initial balance`)

ann_sal <- inner_join(salary3,salary2, by = 'first_name')
ann_sal <- inner_join(ann_sal,salary, by = 'first_name')
ann_sal <- ann_sal[,-1]

##Model matrix
suberb <- data.frame(model.matrix(~most_suberb+0, data = ann_sal))
state <- data.frame(model.matrix(~most_state+0, data = ann_sal))
txn <- data.frame(model.matrix(~most_txn+0, data = ann_sal))

total_sal_anz <- ann_sal[,c(-4:-6)]
total_sal_anz$gender <- ifelse(total_sal_anz$gender=="M",1,0)
total_sal_anz <- cbind(total_sal_anz,suberb,state,txn)

#Scatter Plots
ggscatter(total_sal_anz, x = "age", y = "salary", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Customer Age", ylab = "Annual Salary")

ggscatter(total_sal_anz, x = "initial balance", y = "salary", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Customer Initial Balance", ylab = "Annual Salary")

ggscatter(total_sal_anz, x = "final balance", y = "salary", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Customer Final Balance", ylab = "Annual Salary")

ggscatter(total_sal_anz, x = "overall balance change", y = "salary", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Overall Change in Balance", ylab = "Annual Salary")

#Variable Correlation Identifier
p <- data.frame(cor(total_sal_anz))
p <- p %>% filter(salary > .15 | salary < -0.15)
p <- p[-2,]
rownames(p)

##Linear Regression Model on Train and Test data
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(total_sal_anz), 0.8*nrow(total_sal_anz))  # row indices for training data
train_x <- total_sal_anz[trainingRowIndex, ]  # model training data
test_x  <- total_sal_anz[-trainingRowIndex, ]   # test data

lrmodel <- lm(salary ~. , data = train_x)# build the model
salPred <- predict(lrmodel, test_x)  # predict salary

paste0("Model Accuracy on test data ",round(sigma(lrmodel)/mean(total_sal_anz$salary)*100,2),"%")

#prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=test_x$salary, predicteds=salPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 18.25%
paste0("Prediction Accuracy ",round(correlation_accuracy[1,2]*100,2),"%")
paste("Not very good at all but still happy for my first attempt :)")

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
paste0("Min Max Accuracy ",round(min_max_accuracy*100,2),"%")

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
paste0("Min Max Accuracy ",round(mape*100,2),"%")
# => 57.93%, mean absolute percentage deviation





#Decision TREE

anztree <- rpart(salary ~. ,data=train_x,method="anova")

plot(anztree,margin=0.1)
text(anztree,cex=.8)

plotcp(anztree)
summary(anztree)





































