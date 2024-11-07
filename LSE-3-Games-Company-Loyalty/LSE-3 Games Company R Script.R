##############################################################################
###                                                                        ###
###         LSE Data Analytics Career Accelerator Course 3 Assignment      ###        ###
###                     Predicting Future Outcomes                         ###
###                                                                        ###
##############################################################################

# SET UP WORKING DIRECTORY ------------------------------------------------- #
# Check working directory
getwd()

# If incorrect: un-comment, change the path and execute the following code.
setwd('/Users/genaferguson/LSE/LSE Course 3/Assignment')
getwd()


# INSTALL ADDITIONAL PACKAGES ---------------------------------------------- #
# Install GGally for additional plotting features
#install.packages('GGally')

# IMPORT LIBRARIES --------------------------------------------------------- #
# Import required libraries.
library(tidyverse)
library(ggplot2)
library(dplyr)
library(GGally)

# Import library to manage package conflicts and set preferences.
library(conflicted)
conflicts_prefer(dplyr::filter())
conflicts_prefer(dplyr::lag())

# Set my preferred colors.
my_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#999999")

my_dblue <- '#0072B2'
my_dorange <- '#D55E00'
my_lblue <- '#56B4E9'
my_lorange <- '#E69F00'
my_green <- '#009E73'
my_yellow <- '#F0E442'
my_pink <- '#CC79A7'


# IMPORT DATA -------------------------------------------------------------- #
# Import v2 of the turtle reviews data.
trv2 <- read.csv('turtle_reviews_v2.csv', header = TRUE)

# View the data.
str(trv2)

as_tibble(trv2)

summary(trv2)

# Check for missing data.
sum(is.na(trv2))



# -------------- EXPLORATORY DATA ANALYSIS: Tables & Summaries ------------- #

# View the numeric variables.
summary(select(trv2, loyalty_points, remuneration, spending_score,
               age, product))

# View counts of potential categorical independent variables.
table(trv2$gender)
table(trv2$education)
table(trv2$age_group)

# View first five rows of text fields.
head(trv2$summary)
head(trv2$review)


# ----------------- EXPLORATORY DATA ANALYSIS: Visualisations -------------- #

# NUMERIC COLUMNS ---------------------------------------------------------- #
# Plot a pair plot of all numeric variables
# Select and plot the data.
ggpairs(data  = select(trv2, loyalty_points, remuneration, spending_score,
                       age),
        title = 'Loyalty Points vs Potential Independent Variables',
        lower = list(continuous = wrap("points", color = my_dblue)),
        diag = list(continuous = wrap('densityDiag', color = my_dorange)),
        upper = list(continuous = wrap("cor", color = 'black')),
        columnLabels = c('Loyalty Points', 'Remuneration', 'Spending Score',
                        'Age'))
# COMMENTS: 
# 1) None of the four columns plotted appear to be normally distributed based
#    base on the shape of the probability distributions (diagonal)
# 2) Remuneration and spending score appear strongly correllated with loyalty
#    points; loyalty points increases as remuneration and spending score
#    increase (row 1 and column 1)
# 3) Remuneration vs spending score suggests there may be clusters present
# 4) Age is not correlated with loyalty points


# LOYALTY POINTS ----------------------------------------------------------- #
# Plot a histogram of loyalty points.
ggplot(data = trv2, aes(x = loyalty_points)) +  
  geom_histogram(fill =  my_dblue, color = 'white', bins = 11) +
  labs(title = 'Loyalty Points') +
  theme_classic()

# Plot a boxplot of loyalty points.
ggplot(data = trv2, aes(x = loyalty_points)) +  
  geom_boxplot(fill = my_dblue) +
  labs(title = 'Loyalty Points') +
  theme_classic()

# COMMENTS:
# Loyalty points appears right skewed (histogram) and appears to contain
# outliers (boxplot)


# REMUNERATION ------------------------------------------------------------- #
# Plot a histogram of remuneration.
ggplot(data = trv2, aes(x = remuneration)) +  
  geom_histogram(fill =  my_dblue, color = 'white', bins = 11) +
  labs(title = 'Remuneration') +
  theme_classic()

# Plot a boxplot of remuneration.
ggplot(data = trv2, aes(x = remuneration)) +  
  geom_boxplot(fill = my_dblue) +
  labs(title = 'Remuneration') +
  theme_classic()

# COMMENTS: 
# Remuneration appears to be slightly right skewed (histogram) but does not
# contain outliers (boxplot)


# SPENDING SCORE ----------------------------------------------------------- #
# Plot a histogram of spending score.
ggplot(data = trv2, aes(x = spending_score)) +  
  geom_histogram(fill =  my_dblue, color = 'white', bins = 11) +
  labs(title = 'Spending Score') +
  theme_classic()

# Plot a boxplot of spending score.
ggplot(data = trv2, aes(x = spending_score)) +  
  geom_boxplot(fill = my_dblue) +
  labs(title = 'Loyalty Points') +
  theme_classic()

# COMMENTS: 
# Spending score appears have more than one peak (histogram) but does not
# contain outliers (boxplot)


# AGE ---------------------------------------------------------------------- #
# Plot a histogram of age.
ggplot(data = trv2, aes(x = age)) +  
  geom_histogram(fill =  my_dblue, color = 'white', bins = 11) +
  labs(title = 'Age') +
  theme_classic()

# Plot a boxplot of age.
ggplot(data = trv2, aes(x = age)) +  
  geom_boxplot(fill = my_dblue) +
  labs(title = 'Age') +
  theme_classic()

# COMMENTS: 
# Age appears to be right skewed (histogram) but does not contain outliers
# (boxplot)


# REMUNERATION VS SPENDING SCORE ------------------------------------------- #
ggplot(data = trv2, aes(x = remuneration, y = spending_score)) +  
  geom_point(color = my_dblue, alpha = 0.1, size = 3) +
  labs(title = 'Spending Score vs Remuneration') +
  theme_classic()

# COMMENTS: 
# More detailed plot confirms the appearance of 5 potential clusters when 
# remuneration and spending score are plotted on a scatterplot


# GENDER ------------------------------------------------------------------- #
# Plot a barplot of gender
ggplot(data = trv2, aes(x = gender)) +  
  geom_bar(fill = my_dblue) +
  scale_fill_manual(values = my_colors) +
  labs(title = 'Reviews by Gender') +
  theme_classic()

# View numeric summary of gender.
table(trv2$gender)

# COMMENTS: 
# Females make up ~55% of the data which is slightly higher expected ~50% in
# population.


# EDUCATION ---------------------------------------------------------------- #
# Plot a barplot of education.
ggplot(data = trv2, aes(x = education)) +  
  geom_bar(fill = my_dblue) +
  scale_fill_manual(values = my_colors) +
  labs(title = 'Reviews by Education Level') +
  theme_classic()

# View numeric summary of education
table(trv2$education)

# COMMENTS:
# 88% of reviews are from people with graduate or higher levels of education.


# AGE GROUP ---------------------------------------------------------------- #
# Plot a barplot of AGE_GROUP
ggplot(data = trv2, aes(x = age_group)) +  
  geom_bar(fill = my_dblue) +
  scale_fill_manual(values = my_colors) +
  labs(title = 'Reviews by Age Group') +
  theme_classic()

# View numeric summary of age group
table(trv2$age_group)

# COMMENTS:
# > 60% of reviews are written by people under the age of 40. 


# ------------- CREATE A MULTIPLE LINEAR REGRESSION MODEL ------------------ #
# Look for correlations in the data (repeat pair plot from above)

# CHECK FOR CORRELATIONS --------------------------------------------------- #
# Plot a pair plot of all numeric variables
# Select and plot the data.
ggpairs(data  = select(trv2, loyalty_points, remuneration, spending_score,
                       age),
        title = 'Turtle Games Pair Plot',
        lower = list(continuous = wrap("points", color = my_dblue)),
        diag = list(continuous = wrap('densityDiag', color = my_dorange)),
        upper = list(continuous = wrap("cor", color = 'black')),
        columnLabels = c('Loyalty Points', 'Remuneration', 'Spending Score',
                         'Age'))
# COMMENTS: 
# 1) Remuneration and spending score appear strongly correllated with loyalty
#    points
# 2) Age is not correlated with loyalty points

# CREATE TRAINING AND TESTING DATA SETS ------------------------------------- #
regr_data <- select(trv2, loyalty_points, remuneration, spending_score)
str(regr_data)
# Make this split reproducible
#set.seed(1)

# Split the data 80:20 for training and testing.
sample <- sample(c(TRUE, FALSE), nrow(regr_data), replace=TRUE, prob=c(0.8,0.2))
training_data <- regr_data[sample, ]
testing_data <- regr_data[!sample, ]
#ts <- select(testing_data, remuneration, spending_score)
#str(ts)
# View structure of training set.
str(training_data)

# View structure of testing set.
str(testing_data)


# CREATE REGRESSION MODEL --------------------------------------------------- #
# Train the model with the training data.
mlr_model = lm(loyalty_points~remuneration+spending_score,
               data = training_data)

str(mlr_model)
# View statistical summary of the model.
summary(mlr_model)

# Create a new object and specify the predict function.
loyalty_points_predicted = predict(mlr_model,
                                   newdata = testing_data,
                                   interval = 'confidence')

# View head of predictions and testing_data.
head(loyalty_points_predicted)
head(testing_data)

# RESIDUALS indicate a problem with the predictions.
plot(mlr_model$residuals)

### NOTE: I have chosen to stop regression modelling at this point.
### Predicted vales using testing_data do not align with the regression
### model when the independent variables are used with the model
### coefficients.  I've spent significant time trying to resolve but 
### without success.
### For the assignment I will revert to Python regression modelling which
### I believe to be correct.


### Additional charts created below for assignment -------------------------- #

### trv2 data
# Import v2 of the turtle reviews data.
trv2 <- read.csv('turtle_reviews_v2.csv', header = TRUE)

# View the data.
str(trv2)
as_tibble(trv2)
summary(trv2)

# Check for missing data.
sum(is.na(trv2))

### Use trv2 data ----------------------------------------------------------- #
# Plot a pair plot of all numeric variables
# Select and plot the data.
ggpairs(data  = select(trv2, loyalty_points, remuneration, spending_score,
                       age),
        title = 'Loyalty Points vs Potential Independent Variables',
        lower = list(continuous = wrap("points", color = my_dblue)),
        diag = list(continuous = wrap('densityDiag', color = my_dorange)),
        upper = list(continuous = wrap("cor", color = 'black')),
        columnLabels = c('Loyalty Points', 'Remuneration', 'Spending Score',
                         'Age'))

ggplot(trv2, aes(x = remuneration, y = loyalty_points)) + 
  geom_point(color = my_dblue) + 
  geom_smooth(method = 'lm', color = my_lorange) +
  labs(title = 'Remuneration vs Loyalty Points') +
  theme_classic()

ggplot(trv2, aes(x = spending_score, y = loyalty_points)) + 
  geom_point(color = my_dblue) + 
  geom_smooth(method = 'lm', color = my_lorange) +
  labs(title = 'Spending Score vs Loyalty Points') +
  theme_classic()

### ------------------------------------------------------------------------- #

### trv4 data
# Import v4 of the turtle reviews data.
trv4 <- read.csv('turtle_reviews_v4.csv', header = TRUE)

# View the data.
str(trv4)
as_tibble(trv4)
summary(trv4)

# Check for missing data.
sum(is.na(trv4))

### Use trv4 data ----------------------------------------------------------- #
epsilon_data <- filter(trv4, cluster_name == 'Epsilon')
head(epsilon_data)

# Plot a pair plot of all numeric variables - Epsilon cluster only
# Select and plot the data.
ggpairs(data  = select(epsilon_data, loyalty_points, remuneration, spending_score,
                       age),
        title = 'Epsilon Loyalty Points vs Potential Independent Variables',
        lower = list(continuous = wrap("points", color = my_dblue)),
        diag = list(continuous = wrap('densityDiag', color = my_dorange)),
        upper = list(continuous = wrap("cor", color = 'black')),
        columnLabels = c('Loyalty Points', 'Remuneration', 'Spending Score',
                         'Age'))


