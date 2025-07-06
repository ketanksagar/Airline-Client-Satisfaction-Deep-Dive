Includes the following models and analysis:
Logistic Regression
Logistic Regression with interactive terms
Backwards Selection
GAM - Generalized Additive Model
Decision Tree
Random Forest


---
title: "ML Project: Airline Passenger Satisfaction"
author: "Liv Marcinkus, Giulia Neves Monteiro, and Ruthie Montella"
date: "2024-11-12"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(ggplot2)
```

Read in our dataset: 

```{r}
test_data <- read.csv("C:\\Users\\Graduate\\Downloads\\test.csv")
train_data <- read.csv("C:\\Users\\Graduate\\Downloads\\train.csv")
#irline_data_start <- cbind(test_data, train_data)
airline_data_merge <- merge(test_data, train_data, by = c("X","Gender","id", "Customer.Type","Age","Type.of.Travel","Class", "Flight.Distance","Inflight.wifi.service","Departure.Arrival.time.convenient","Ease.of.Online.booking", "Gate.location","Food.and.drink","Online.boarding","Seat.comfort","Inflight.entertainment", "On.board.service","Leg.room.service","Baggage.handling", "Checkin.service", "Inflight.service", "Cleanliness","Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes","satisfaction" ),all = TRUE)     
duplicates <- airline_data_merge[duplicated(airline_data_merge$id),]
airline_data <- airline_data_merge[!duplicated(airline_data_merge$id),]

colnames(airline_data)
nrow(airline_data)
```
We merged our train and test data sets in order to complete ealy analysis with full data set. This led to duplicate rows under the "X" column, however the "id" column still represents our unique identifiers of each respondent. 

## Inital Exploration & Summary Stats

First looking at the dataset as a whole: 

```{r}
str(airline_data)
head(airline_data)
tail(airline_data)

dim(airline_data)
```

To check the quality of the dataset we checked how many NA values are present:

```{r}
colSums(is.na(airline_data)) # 83 NA's in entire dataset 
```

To replace our NA values with 0's (since all are located in the Arrival.Delay.in.Minutes column): 

```{r}
airline_data[is.na(airline_data)] <- 0

# Also replace the NA values to 0 for the train and test datasets that will be used:

train_data[is.na(train_data)] <- 0
test_data[is.na(test_data)] <- 0

# COnfirm that there are no NA values:
colSums(is.na(train_data)) 
colSums(is.na(test_data)) 
```



### Examination of individual variables:

- Looking at variables we hypothesize will be strongly related to customer satisfaction: 

#### Age 

```{r}
summary(airline_data$Age)
hist(airline_data$Age)
```
The plot reveals that there are no significantly over represented groups within the `Age` variable, with the highest number of responses coming from people within the range of roughly 35-55 years old. 


#### Gender

```{r}
table(airline_data$Gender)
```

#### Customer Type - loyal or disloyal 

```{r}
table(airline_data$Customer.Type)
# many more loyal customers than non loyal 
```


#### Type of Travel and Class Traveled 

```{r}
table(airline_data$Type.of.Travel) # way more travel for business than personal reasons 
```
```{r}
table(airline_data$Class) 
```




#### Cleanliness

```{r}
table(airline_data$Cleanliness)
hist(airline_data$Cleanliness)
```

While not used in the most conventional way, this visual allows us to visually examine the distribution of responant ratings within the `Cleanliness` variable. It enables us to easily identify that most people rated airplane cleanliness in the 3-5 range. 


#### Flight Distance

```{r}
summary(airline_data$Flight.Distance)
```


#### Inflight Entertainment and Wifi Service Rating Distributions 

```{r}
hist(airline_data$Inflight.entertainment)
```

The `Inflight.entertainment` histogram reveals that in general most people are fairly satisfied with their in flight entertainment options, with 4 as the mode of the ratings. 


```{r}
hist(airline_data$Inflight.wifi.service)
```

The `Inflight.wifi.service` visual reveals that in flight WiFi may be an area for improvement for airlines as most respondents rated the service around 2 or 3 out of a possible 5. 


#### CheckIn Service

```{r}
# summary(airline_data$Checkin.service)
table(airline_data$Checkin.service)
```


#### Arrival Delay

```{r}
summary(airline_data$Arrival.Delay.in.Minutes)
```

The median arrival delay being 0 indicates that the vast majority of flights had no delay at all.


#### Departure Delay

```{r}
summary(airline_data$Departure.Delay.in.Minutes)
```

The median departure delay being 0 indicates that the vast majority of flights had no delay at all.


## Data Visualizations 


#### Relationship between Cleanliness and Class flown:

```{r}
boxplot(airline_data$Cleanliness ~ airline_data$Class)
```

This boxplot reveals that the distribution of customers' `Cleanliness` rating somewhat differs when examined by `Class` flown. It is clear that the median cleanliness rating for business class flyers is higher than ratings of those who flew in economy plus or economy. Interestingly, there is not a similar difference reflected in the cleanliness ratings between the economy and economy plus categories. This indicates that the divide to focus on is that between the business class and the rest of the classes. 


#### Flight Distance by Inflight Service Ratings 

```{r}
boxplot(airline_data$Flight.Distance ~ airline_data$Inflight.service)
```

We also wanted to take a look at how `Inflight.service` ratings are distributed by `Flight.Distance`. Notably, all of the ratings of 0 for in flight service correlated with longer median flight distances. This is really surprising as one would reasonably assume that longer flights would be accompanied by better in flight service, so this observation is definitely something we will further examine later in our analysis. With the exception of the 0 category, however, the remaining in flight service ratings are all relatively evenly distributed between 1 and 5, with the flight distances for each rating also being generally very similar. 


#### Distribution of Departure Delays (in minutes)


```{r}
# hist(airline_data$Departure.Delay.in.Minutes)
subset_most_vals <- airline_data$Departure.Delay.in.Minutes[airline_data$Departure.Delay.in.Minutes < 400]
breaks <- seq(0, 400, by = 10)  # Create bins every 10 minutes
hist(subset_most_vals, 
     breaks = breaks,  # Use the custom breaks
     main = "Histogram of Departure Delays", 
     xlab = "Departure Delay (Minutes)", 
     ylab = "Frequency")
```

In order to gain insights from this histogram of `Departure.Delay.in.Minutes` I had to perform a little bit of data manipulation. I first tried to remove only a couple of outliers but still did not have a good view of the distribution, as the x axis extended all the way to 1200 minutes, making the data not readable. My next step was to filter the data so only departure delays less than 500 minutes were visible. When this still did not produce a readable visual I filtered the results all the way down departure delays under 400 minutes. While some values were excluded from this visual, the majority of the values are still included and it allows us to visualize the general distribution of departure delays. This being said it is clear that the vast majority of flights experience no delay, and the number of flights decreases significantly as the departure delays become longer and longer. 


## Logsitic Regression

Preliminary Steps - Analyzing the response Variable
```{r}
summary(train_data$satisfaction)
```

```{r}
g_1 <- ggplot(train_data, aes(x = satisfaction, y = X)) + # Set X-axis as insurance charges
  geom_col(fill = "blue") + # Use geom_density to get density plot
  theme_bw() + # Set theme for plot
  theme(panel.grid.major = element_blank(), # Turn of the background grid
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Satisfactions", y = "Number of Respondents", # Set plot labels
       title = "Overview of Satisfaction")

g_1 # Generate plot
```

Since the response variable, satisfaction, is categorical we are using logistic regression. 

We run a logistic regression using the glm() function, as logistic regression is a generalized linear model. To select linear regression we set our link function by setting family = binomial(link = logit) Lets try single variable logistic regressions first:

```{r}
# Converting the chr data type for satisfaction to numeric, having 'satisfied' by 1 and 'neutral or dissatisfied' by 0
use_data <- train_data[,-c(1:2)] # Dropped the first two columns because they were identification columns we don't need in our model.
use_data$satisfaction <- as.numeric(use_data$satisfaction == "satisfied")


fit_1 <- glm(satisfaction ~ ., # Set formula
             family=binomial(link='logit'), # Set logistic regression
             data= use_data) # Set dataset
t1 <- summary(fit_1) # Summarize model
t1
# We want the lowest AIC value since the model with a lower value is considered the best fit, as it balances model complexity with explanatory power.
#We can test removing insignificant variables and seeing if it helps lower the AIC value
```
Lets look at the predictors that aren't significant to remove them on by one and see how that impacts the AIC value:
```{r}
# All the insignificant variables that have a significance level above a 0.01 
t1$coefficients[which(t1$coefficients[,4] >= 0.01),]
```

```{r}
# Removing Flight Distance predictor
fit_2 <- glm(satisfaction ~ Gender+Customer.Type+Age+Type.of.Travel+Class+Inflight.wifi.service+Departure.Arrival.time.convenient+Ease.of.Online.booking+Gate.location+Food.and.drink+Online.boarding+Seat.comfort+Inflight.entertainment+On.board.service+Leg.room.service+Baggage.handling+Checkin.service+Inflight.service+Cleanliness+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, # Set formula
             family=binomial(link='logit'), # Set logistic regression
             data= use_data) # Set dataset
t2 <- summary(fit_2) # Summarize model
t2

t2$coefficients[which(t2$coefficients[,4] >= 0.01),]

# No difference in AIC, lets move forward to removing Gender
```
```{r}
# Removing Gender predictor
fit_3 <- glm(satisfaction ~ Customer.Type+Age+Type.of.Travel+Class+Inflight.wifi.service+Departure.Arrival.time.convenient+Ease.of.Online.booking+Gate.location+Food.and.drink+Online.boarding+Seat.comfort+Inflight.entertainment+On.board.service+Leg.room.service+Baggage.handling+Checkin.service+Inflight.service+Cleanliness+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, # Set formula
             family=binomial(link='logit'), # Set logistic regression
             data= use_data) # Set data set
t3 <- summary(fit_3) # Summarize model
t3

# Increased AIC by 2. We should keep Gender. Removing these insignificant predictors isn't helping our AIC, lets instead try adding an interactive term
```

Since we have reached a point where the AIC value isn't improving, lets try to improve the model by adding interactive terms.

To determine which variables to use for an interaction term in a statistical model, we identify  two independent variables where we suspect the effect of one variable significantly changes depending on the level of the other variable.

```{r}
# included the interactive term of Class and Inflight entertainment
int1 <- glm(satisfaction ~ . + Class * Inflight.entertainment, # Set formula with interaction terms
             family=binomial(link='logit'), # Set logistic regression
             data= use_data) # Set dataset
t5 <- summary(int1) 
t5
```
```{r}
# included the interactive term of customer type and chec in service
int2 <- glm(satisfaction ~ . + (Class * Inflight.entertainment) + (Customer.Type * Checkin.service), # Set formula with interaction terms
             family=binomial(link='logit'), # Set logistic regression
             data= use_data) # Set dataset
t6 <- summary(int2) 
t6

# We reduced the AIC value even further! Lets add one more interactive term
```
```{r}
# included the interactive term of type of travel and departure delay
int3 <- glm(satisfaction ~ . + (Class * Inflight.entertainment) + (Customer.Type * Checkin.service) + (Type.of.Travel*Departure.Delay.in.Minutes), # Set formula with interaction terms
             family=binomial(link='logit'), # Set logistic regression
             data= use_data) # Set data set
t7 <- summary(int3) 
t7

# We reduced the AIC even further showing we had a good selection for the interactive terms, helping improve the model's performance
```


## Backward Selection

Being used with the initial model with all predictors and it will remove the predictors that are statistically insignificant one by one to lower the AIC value.

```{r}
lm_bwd <- step(fit_1, direction='backward', k=log(nrow(use_data)))
```



## GAM - Generalized Additive Model

Estimate a GAM model with all predictors to capture potential nonlinear relationship. We can use plot() function to visualize the estimated coefficients and splines for each predictor. Note that we can still interpret the estimated model gam1 due to the additivity of GAM.

```{r}
library(gam)
# put all numerical predictors with a spline of degree of freedom 4

gam1 <-gam(satisfaction~Gender+Customer.Type+s(Age)+Type.of.Travel+Class+s(Inflight.wifi.service)+s(Departure.Arrival.time.convenient)+s(Ease.of.Online.booking)+s(Gate.location)+s(Food.and.drink)+s(Online.boarding)+s(Seat.comfort)+s(Inflight.entertainment)+s(On.board.service)+s(Leg.room.service)+s(Baggage.handling)+s(Checkin.service)+s(Inflight.service)+s(Cleanliness)+s(Departure.Delay.in.Minutes)+s(Arrival.Delay.in.Minutes)+s(Flight.Distance), family='binomial', data=use_data)

summary(gam1)

#plot(gam1, col='blue')
```


#### Model Evaluation

We are going to adjust the satisfaction column in the test data so that it is also numeric like the training data we have been using
```{r}
test_use_data <- test_data[,-c(1:2)] # Dropped the first two columns because they were identification columns we don't need in our model.
test_use_data$satisfaction <- as.numeric(test_use_data$satisfaction == "satisfied")
```

Doing a model evaluation (out of sample) for the models we have done so far

```{r}
library(caret)

fit_1_pred <- predict(fit_1, newdata=test_use_data, type='response')
int3_pred <- predict(int3, newdata=test_use_data, type='response')
lm_bwd_pred <- predict(lm_bwd, newdata=test_use_data, type='response')
gam1_pred <- predict(gam1, newdata=test_use_data, type='response')
```

Adjusting the predictors as factors for each model type to then generate their confusion matrix.

For the first logistic regression model:
```{r}
# Ensure predictions are factors with appropriate levels
pred_factor <- factor(ifelse(fit_1_pred > 0.5, '1', '0'), levels = c('0', '1'))
actual_factor <- factor(test_use_data$satisfaction, levels = c('0', '1'))
# Print levels for debugging
print(levels(pred_factor))
print(levels(actual_factor))
# Calculate the confusion matrix
fit_1_acc <- confusionMatrix(pred_factor, actual_factor, positive = '1')
print(fit_1_acc)
```
For the logistic regression model with all interaction terms:
```{r}
# Ensure predictions are factors with appropriate levels
pred_factor5 <- factor(ifelse(int3_pred > 0.5, '1', '0'), levels = c('0', '1'))
actual_factor5 <- factor(test_use_data$satisfaction, levels = c('0', '1'))
# Print levels for debugging
print(levels(pred_factor5))
print(levels(actual_factor5))
# Calculate the confusion matrix
int3_acc <- confusionMatrix(pred_factor5, actual_factor5, positive = '1')
print(int3_acc)
```

For the backward selection model
```{r}
# Ensure predictions are factors with appropriate levels
pred_factor2 <- factor(ifelse(lm_bwd_pred > 0.5, '1', '0'), levels = c('0', '1'))
actual_factor2 <- factor(test_use_data$satisfaction, levels = c('0', '1'))
# Print levels for debugging
print(levels(pred_factor2))
print(levels(actual_factor2))
# Calculate the confusion matrix
lm_bwd_acc <- confusionMatrix(pred_factor2, actual_factor2, positive = '1')
print(lm_bwd_acc)
```
For the GAM model:
```{r}
# Ensure predictions are factors with appropriate levels
pred_factor3 <- factor(ifelse(gam1_pred > 0.5, '1', '0'), levels = c('0', '1'))
actual_factor3 <- factor(test_use_data$satisfaction, levels = c('0', '1'))
# Print levels for debugging
print(levels(pred_factor3))
print(levels(actual_factor3))
# Calculate the confusion matrix
gam1_acc <- confusionMatrix(pred_factor3, actual_factor3, positive = '1')
print(gam1_acc)
```

#### Lift Chart

Let’s now generate a lift chart to compare the prediction performance of the models
```{r}
library(lattice)
library(caret)

# Converting our dependent variable as a factor for the lift function to run:
test_use_data$satisfaction <- factor(test_use_data$satisfaction, levels = c(0, 1))
data <- cbind.data.frame(test_use_data$satisfaction, fit_1_pred, int3_pred, lm_bwd_pred, gam1_pred)

lift_chart_u <- caret::lift(test_use_data$satisfaction ~ fit_1_pred + int3_pred + lm_bwd_pred + gam1_pred, class=1, cuts = 200, data = data) # doing this so that it pulls the right lift function from caret90
lift_chart_u <- caret::lift(test_use_data$satisfaction ~ fit_1_pred + int3_pred + lm_bwd_pred + gam1_pred, class=1, cuts = 200, data = data) # doing this so that it pulls the right lift function from caret90

xyplot(lift_chart_u,auto.key=list(columns=4, main='Lift Chart'))
```

## Decision Tree 
```{r}
library(ggplot2)
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)	
library(reshape2) # Load reshape 2 for melting
library(DMwR) # Load data mining with R for SMOTE
library(splitstackshape) # Used for stratified sampling
```

## Visualize before model: 
This is a example of a visualization, we only used online boarding because it was the most influential variable, but other could be plugged in here as well.
```{r}
library(ggplot2)

airline_data2 <- airline_data
# Ensure 'Online.boarding' is treated as a factor for categorical visualization
airline_data2$Online.boarding <- factor(airline_data2$Online.boarding, 
                                       levels = c(1, 2, 3, 4, 5), 
                                       labels = c("Very Unsatisfied", "Unsatisfied", 
                                                  "Neutral", "Satisfied", 
                                                  "Very Satisfied"))

# Create the density plot for Online.boarding satisfaction levels
g1 <- ggplot(airline_data2, aes(x = Online.boarding, fill = Online.boarding)) +
  geom_bar(alpha = 0.7) +  # Bar plot as density doesn't work well for categorical values
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.background = element_blank()) +
  labs(x = "Online Boarding Satisfaction Levels",
       title = "Distribution of Online Boarding Satisfaction",
       fill = "Satisfaction Level") +
  scale_fill_manual(values = c("Very Unsatisfied" = "red", 
                               "Unsatisfied" = "orange", 
                               "Neutral" = "yellow", 
                               "Satisfied" = "green", 
                               "Very Satisfied" = "blue"))

# Display the plot
g1
```


```{r}
tree_1 <- rpart(satisfaction ~., # Set tree formula
data = airline_data2[, c(2, 4:25)], 
control = rpart.control(cp = 0.001)) # Set dataset
par(xpd = NA) # Set this avoid cut-off text
plot(tree_1)  # Plot tree
text(tree_1, digits = 3) # Add text
fancyRpartPlot(tree_1, cex = 0.2)
tree_1
```

#### Evaluate model 
```{r}
# Step 1: Load necessary libraries
library(rpart)
library(rattle)  # For fancyRpartPlot
library(caret)   # For confusionMatrix function

# Step 2: Split the dataset into training and testing sets
set.seed(123)  # Set seed for reproducibility
split_index <- createDataPartition(airline_data2$satisfaction, p = 0.7, list = FALSE)
train_data1 <- airline_data2[split_index, ]
test_data1 <- airline_data2[-split_index, ]

# Step 3: Train the decision tree
tree_1 <- rpart(satisfaction ~ ., 
                data = train_data1[, c(2, 4:25)], 
                control = rpart.control(cp = 0.001))

# Step 2: Ensure test data matches training data (factor levels)
test_data1$Online.boarding <- as.factor(test_data1$Online.boarding)  # Ensure factor consistency

# Step 3: Make predictions on the test set
predictions <- predict(tree_1, test_data1[, c(2, 4:25)], type = "class")

# Step 4: Generate a confusion matrix
conf_matrix <- confusionMatrix(predictions, as.factor(test_data1$satisfaction))

# Print the confusion matrix and accuracy
print(conf_matrix)
cat("Accuracy of the model:", round(conf_matrix$overall['Accuracy'] * 100, 2), "%\n")
```

To get important variables from model
```{r}
#importance from the decision tree of variables 
importance <- data.frame(Variable = names(tree_1$variable.importance),
                         Importance = tree_1$variable.importance)
importance <- importance[order(-importance$Importance), ]
print(importance)
```

Important variables to a graph
```{r}
ggplot(importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variables", y = "Importance") +
  theme_minimal()
```

#### Random Forest
```{r}
library(randomForest)
library(rpart) # Load rpart for decision trees
library(caret) # Used for analysing results
```

Replace NA's: 
```{r}
colSums(is.na(train_data))
train_data[is.na(train_data)] <- 0
# so randomforest can automatically treat it as a classification target 
train_data$satisfaction <- factor(train_data$satisfaction, levels = c("neutral or dissatisfied", "satisfied"))
```


Single classification tree: 
```{r}
tree_model <- rpart(satisfaction ~., # Set tree formula
                data = train_data)

# Predict with classification tree
tree_preds <- predict(tree_model, test_data, type = "class")
```

To analyze results from the model --> confusion matrix 

```{r}
# Confusion matrix for single tree
t <- table(tree_preds,test_data$satisfaction) # Create table
confusionMatrix(t, positive = "satisfied")
```
Accuracy measure of 0.8838 for this inital classification tree - not bad but we will try to improve. 


Trying bagging: set mtry to the number of variables in the dataset 

```{r}
set.seed(258506) # Set random number generator seed for reproducability


# Use random forest to do bagging
bag_mod <- randomForest(satisfaction ~., # Set tree formula
                data = train_data, # Set dataset
                mtry = 24, # Set mtry to number of variables 
                ntree = 200, do.trace = TRUE) # Set number of trees to use
bag_mod
```

```{r}
bag_preds <- predict(bag_mod, test_data) # Create predictions for bagging model

t <- table(bag_preds,test_data$satisfaction) # Create table
confusionMatrix(t,  positive = "satisfied")
```
This increased our accuracy to: 0.9628 

By using bagging we achieved a test set error rate of 0.9628 compared to 0.8838  for our classification tree. The difference is substaintial compared to the single tree model. 


Can check our out-of-bag error for the trees as each one is built and then plot them to analyze if the model has already converged to our optimal solution or if it is still converging. 

```{r}
oob_error <- bag_mod$err.rate[,1] # Extract oob error
plot_dat <- cbind.data.frame(rep(1:length(oob_error)), oob_error) # Create plot data
names(plot_dat) <- c("trees", "oob_error")


# Plot oob error
g_1 <- ggplot(plot_dat, aes(x = trees, y = oob_error)) + # Set x as trees and y as error
  geom_point(alpha = 0.5, color = "blue") + # Select geom point
  geom_smooth() + # Add smoothing line
  theme_bw() + # Set theme
  theme(panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  labs(x = "Number of Trees", title = "Error Rate v Number of Trees",
       y = "Error Rate")  # Set labels
g_1 # Print plot
```
From this plot we can see that as the number of trees increases the error rate continually falls. However once about 150 trees were created it appears that the error rate essentially plateaued even as the number of trees iterated up to 200. Therefore we can start looking at parameter tuning of this model (we can tune the number of trees and node size).

Takes a long time to run but would tune the parameters: 
```{r}
trees <- c(10, 25, 50, 100, 200, 500, 1000) # Create vector of possible tree sizes
nodesize <- c(1, 10, 25, 50, 100, 200, 500, 1000) # Create vector of possible node sizes

params <- expand.grid(trees, nodesize) # Expand grid to get data frame of parameter combinations
names(params) <- c("trees", "nodesize") # Name parameter data frame
res_vec <- rep(NA, nrow(params)) # Create vector to store accuracy results

for(i in 1:nrow(params)){ # For each set of parameters
  set.seed(987654) # Set seed for reproducability
  mod <- randomForest(satisfaction ~. , # Set formula
                      data=train_data,# Set data
                      mtry = 24, # Set number of variables
                      importance = FALSE,  # 
                      ntree = params$trees[i], # Set number of trees
                      nodesize = params$nodesize[i]) # Set node size
  res_vec[i] <- 1 - mod$err.rate[nrow(mod$err.rate),1] }
```

Now we put the results of that for loop into a dataframe that we can later use for a visualization analysis:

```{r}
res_db <- cbind.data.frame(params, res_vec) # Join parameters and accuracy results
names(res_db)[3] <- "oob_accuracy" # Name accuracy results column
res_db # gives us accuracy of results which can also be visualized in the heatmap below
```


Visualization of the results from above: 

```{r}
res_db$trees <- as.factor(res_db$trees) # Convert tree number to factor for plotting
res_db$nodesize <- as.factor(res_db$nodesize) # Convert node size to factor for plotting
g_2 <- ggplot(res_db, aes(y = trees, x = nodesize, fill = oob_accuracy)) + # set aesthetics
  geom_tile() + # Use geom_tile for heatmap
  theme_bw() + # Set theme
  scale_fill_gradient2(low = "blue", # Choose low color
    mid = "white", # Choose mid color
    high = "red", # Choose high color
    midpoint =mean(res_db$oob_accuracy), # Choose mid point
    space = "Lab", 
    na.value ="grey", # Choose NA value
    guide = "colourbar", # Set color bar
    aesthetics = "fill") + # Select aesthetics to apply
  labs(x = "Node Size", y = "Number of Trees", fill = "OOB Accuracy") # Set labels
g_2
```


View the best test results:

```{r}
res_db[which.max(res_db$oob_accuracy),]
```
Based on the visualization results we can see that the recommended number of trees in 1000 with node size of 1. This combination has a predicted accuracy of 0.96343, which is comparable to our previous 0.9628  accuracy score. However since we took the time to produce these parameters let's run a model using them: 

```{r}
set.seed(258506)
bag_mod1 <- randomForest(satisfaction ~., # Set tree formula
                data = train_data, # Set dataset
                mtry = 24, # Set mtry to number of variables 
                ntree = 1000, do.trace = TRUE,
                nodesize= 1) # Set number of trees to use
bag_mod1
```

```{r}
bag_preds <- predict(bag_mod1, test_data) # Create predictions for bagging model

t <- table(bag_preds,test_data$satisfaction) # Create table
confusionMatrix(t,  positive = "satisfied")
```

```{r}
# Extract Importance
importance_matrix <- randomForest::importance(bag_mod1)
# Print importance matrix
importance_matrix
```

```{r}
varImpPlot(bag_mod1, type =2, n.var = 10) # Plot importance
```

```{r}
partialPlot(bag_mod1 , train_data, x.var = "Online.boarding", which.class = "satisfied") # Generate partial dependency plot for difference in significant strikes
```

```{r}
partialPlot(bag_mod1 , train_data, x.var = "Inflight.wifi.service", which.class = "satisfied")
```

We can see that the `Inflight.wifi.service` variable is either reported as 0 or 1, or 4 or 5, with very few ratings in between. Considering wifi is an amenity that either works well or not at all, this polarization in ratings makes perfect sense. 

```{r}
partialPlot(bag_mod1 , train_data, x.var = "Seat.comfort", which.class = "satisfied")
```
