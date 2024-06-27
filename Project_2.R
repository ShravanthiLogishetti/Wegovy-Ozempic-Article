library(readxl)
dataset <- read_excel("C:/Users/Shai(daa)/Desktop/Data Mining/project2/dataset2.xlsx")
View(dataset)
head(dataset)
library(forecast)
library(tsibble)
library(fpp3)
library(tidyverse)
library(ggplot2)

dataset2 = dataset %>%
  mutate(year = yearmonth(Date))%>%
  as_tsibble(
    index = year,
    key = c(Open,High,Low, `Adj Close`, Volume)
  )
head(dataset2)
dataset2_tsibble <- tsibble(Year = dataset2$year, Volume = dataset2$Volume)

sales_ts <- ts(dataset2_tsibble$Volume, frequency = 12)
train_size <- floor(0.8 * length(sales_ts))
train_data <- window(sales_ts, end = train_size)

# Extract the remaining data as the test set
test_data <- sales_ts[(train_size + 1):length(sales_ts)]



train_size <- floor(0.8 * length(sales_ts))
train_data <- window(sales_ts, end = train_size)
test_data <- window(sales_ts, start = train_size + 1)
arima_model <- auto.arima(train_data)

forecast_values <- forecast(arima_model, h = length(test_data))


accuracy(forecast_values, test_data)

plot(forecast_values, main = "Sales Forecast", xlab = "Time", ylab = "Sales", xlim = c(min(time(dataset2_tsibble$Year)), max(time(dataset2_tsibble$Year))))
lines(test_data, col = "red")
##################################

















autoplot(dataset2_tsibble) +
  labs(title = "Time Series Plot",
       x = "Year",
       y = "Volume")
library(readr)
menu <- read_csv("C:/Users/Shai(daa)/Desktop/Data Mining/project2/FastFoodNutritionMenuV2.csv")
View(menu)
head(menu)

menu2 = subset(menu, menu$`Cholesterol
(mg)` >= 200)
table(menu2$Item, menu2$`Cholesterol
(mg)`)
menu2 %>%
  group_by(Item)%>%
  ggplot(aes(x = menu2$Item, y = menu2$`Cholesterol
(mg)`)) +
  geom_point()
#################################Prediction

#prediction of sales using random forest


library(tidyverse)
#special libraries
library(mdsr)
library(rpart)
library(partykit)  
library(caret)     
library(randomForest)
library(readr)


library(readxl)
obesity <- read_excel("C:/Users/Shai(daa)/Desktop/Data Mining/project2/Obesity.xlsx")
View(obesity)
obesity$Label = factor(obesity$Label, 
                       levels = c("Normal Weight", "Underweight", "Overweight", "Obese"),
                       labels = 1,2,3,4)
obesity$Gender = factor(obesity$Gender, 
                       levels = c("Male", "Female"),
                       labels = 0,1)
#Splitting the dataset
library(caTools)
library(ggplot2)

library(pROC)
library(MLmetrics)
split = sample.split(dataset2$Volume, SplitRatio = 0.75 )
training_set = subset(dataset2, split == TRUE)
test_set = subset(dataset2, split == FALSE)
names(train)
dataset2 = dataset[-6]
form = as.formula(Volume ~.)



topleft(obesity)
obesity %<>% dplyr::select(-ID)
table(obesity$BMI)
obesity %<>% dplyr::rename(Class = BMI)



wine_ss <- dataset %>% add_missinglabels_mar(Volume~.,prob=0.95) 


#colSums(is.na(wine_ss))
#table(wine_ss$Class)
library(scales)
library(tidyverse)
library(RSSL)
library(caret) 
library(plyr)
library(lattice) 
library(magrittr) 
library(useful)
library(MASS) 
library(ssc) 
library(GGally)



rf_model = randomForest(form, data = training_set, 
                        ntree = 500, 
                        mtry = 7,
                        nodesize = 5, 
                        proximity= T, 
                        importance = TRUE)

rf_model
varImpPlot(rf_model)
rf_model_predict = predict(rf_model, newdata = test_set)
accuracy(rf_model_predict, test$Label)
rf_acc = accuracy(rf_model_predict, test$BMI)[2]
plot(rf_model)


