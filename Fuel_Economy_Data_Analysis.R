library(tidyverse)
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue)
library(h2o)
library(faraway)
library(tibble)

setwd("C:/Users/99470/Downloads")
df <- ggplot2::mpg
df <- df %>% select(cty,year,cyl,displ)

# Multicollinearity edende 4cu sualda features kimi verdiyi columnlardan biri gedirdi deye etmedim

# Standardize (Normalize) ----
df %>% glimpse()

df[,-1] <- df[,-1] %>% scale() %>% as.data.frame()


# Modeling ----------------------------------
h2o.init()

h2o_data <- df %>% as.h2o()


# Splitting the data
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'cty'
features <- df %>% select(-cty) %>% names()


# Fitting h2o model ----
model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

#p value-a gore de 4-cu sualda adi kecen features'leri drop etmesin deye cox p value'su olanlari atmadim

model@model$coefficients_table
#results show that 1 unit of increase in displ variable decreases cty target variable by 2.06 unit
#results show that 1 unit of increase in cyl variable decreases cty target variable by 1.57 unit
#results show that 1 unit of increase in year variable increases cty target variable by 0.31 unit
#std error means that -+ difference around the coefficient
#p value give information if our variables are significant of not (under 0.05 is significant)

# Predicting the Test set results
y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict


# Model evaluation-------------------
test_set <- test %>% as.data.frame()
residuals = test_set$cty - y_pred$predict
residuals
range(residuals)
# Calculate RMSE (Root Mean Square Error) ----
RMSE = sqrt(mean(residuals^2))
RMSE
# Calculate Adjusted R2 (R Squared) ----
y_test_mean = mean(test_set$cty)
tss = sum((test_set$cty - y_test_mean)^2) #total sum of squares
rss = sum(residuals^2) #residual sum of squares
R2 = 1 - (rss/tss); R2

n <- test_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))
Adjusted_R2

tibble(RMSE = round(RMSE,1),
       R2, Adjusted_R2)

# Plotting actual & predicted
my_data <- cbind(predicted = y_pred$predict,
                 observed = test_set$cty) %>% 
  as.data.frame()

g <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Output", 
       y="Observed Output",
       title=glue('Test: Adjusted R2 = {round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))
g %>% ggplotly()

# Check overfitting---------------------
y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()
train_set <- train %>% as.data.frame()
residuals = train_set$cty - y_pred_train$predict
residuals
range(residuals)

RMSE_train = sqrt(mean(residuals^2))
RMSE_train

y_train_mean = mean(train_set$cty)
tss = sum((train_set$cty - y_train_mean)^2)
rss = sum(residuals^2)
R2_train = 1 - (rss/tss); R2_train

n <- train_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2_train = 1-(1-R2_train)*((n-1)/(n-k-1))
Adjusted_R2_train

# Plotting actual & predicted
my_data_train <- cbind(predicted = y_pred_train$predict,
                       observed = train_set$cty) %>% 
  as.data.frame()

g_train <- my_data_train %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Train: Adjusted R2 = {round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))
g_train %>% ggplotly()

# Compare ----------------------
library(patchwork)
g_train + g

tibble(RMSE_train = round(RMSE_train,1),
       RMSE_test = round(RMSE,1),
       
       Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2
