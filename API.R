# Loading in plumber package to create API
library(plumber)
# loading in other libraries
library(tidyverse)
library(tidymodels)
library(ranger)
library(parsnip)
library(rpart.plot)

# Reading in Data
dhi_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
dhi_data

# Function to change binary variables into factors
binary_creator <- function(data){
  # Grabbing all binary cols
  binary_cols <- colnames(data)[sapply(data, function(col) all(col %in% c(0, 1)))]
  
  # removing sex from binary_cols
  binary_cols <- setdiff(binary_cols, "Sex")
  
  # Creating Binary cols
  for(var in colnames(data)){
    if(var %in% binary_cols){
      data[[var]] <- factor(data[[var]], levels = c(0,1), labels = c("No", "Yes"))
    }
  }
  return(data)
}
# Running function
dhi_data <- binary_creator(dhi_data)

# Converting other factor variables 
dhi_data <- dhi_data |>
  mutate(
    Sex = factor(Sex, levels = c(0,1), labels = c("female", "male")),
    GenHlth = factor(GenHlth, levels = c(1,2,3,4,5), labels = c("excellent", "very_good", "good", "fair", "poor")),
    Education = factor(Education, levels = c(1,2,3,4,5,6), labels = c("no_schooling", "1_to_8", "9_to_11", "12_or_GED", "some_college", "college_grad")),
    Income = factor(Income, levels = c(1,2,3,4,5,6,7,8), labels = c("10k_less", "15k_less", "20k_less", "25k_less", "35k_less", "50k_less", "75k_less", "75k_more"))
  )

# renaming columns 
dhi_data <- dhi_data |>
  rename(
    "Diabetes" = Diabetes_binary,
    "HeartDisease" = HeartDiseaseorAttack,
    "HvyAlch" = HvyAlcoholConsump,
    "Healthcare" = AnyHealthcare,
    "NoDoc" = NoDocbcCost,
  )

# Creating the model

# Data split and 5 fold CV (Will be using this later to get the best possible model)
dhi_split <- initial_split(model_data, prop = 0.7, strata = Diabetes)
dhi_train <- training(dhi_split)
dhi_test <- testing(dhi_split)
dhi_cv <- vfold_cv(dhi_train, 5, strata = Diabetes)

# Recipe for model
class_recipe <- recipe(Diabetes ~ HighBP + HighChol + BMI + Stroke + PhysActivity + HeartDisease,
                       data = dhi_train) |>
  step_dummy(HighBP, HighChol, Stroke, PhysActivity, HeartDisease)

# creating model 
rf_spec <- rand_forest(trees = 500, mtry = 3, min_n = 10) |>
  set_engine("ranger", importance = "impurity", splitrule = "gini") |>
  set_mode("classification")

# Creating workflow
rf_wkflw <- workflow() |>
  add_recipe(class_recipe) |> 
  add_model(rf_spec)

# fitting data 
rf_fit <- rf_wkflw |>
  fit(dhi_data)

