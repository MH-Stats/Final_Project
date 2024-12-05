#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/

# Loading in plumber package to create API
library(plumber)
# loading in other libraries
library(tidyverse)
library(tidymodels)
library(ranger)
library(parsnip)
library(rpart.plot)

# Reading in Data
dhi_data <- read_csv("../diabetes_binary_health_indicators_BRFSS2015.csv")
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



#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* Diabetes Prediction API
#* Accepts inputs and predicts the likelihood of diabetes
#* @param HighBP Binary variable ("Yes" or "No")
#* @param HighChol Binary variable ("Yes" or "No")
#* @param BMI Numeric variable (e.g., 35.5)
#* @param Stroke Binary variable ("Yes" or "No")
#* @param PhysActivity Binary variable ("Yes" or "No")
#* @param HeartDisease Binary variable ("Yes" or "No")
#* @post /predict
function(HighBP, HighChol, BMI, Stroke, PhysActivity, HeartDisease) {
  # Convert inputs into a data frame
  input <- data.frame(
    HighBP = factor(HighBP, levels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c("No", "Yes")),
    BMI = as.numeric(BMI),
    Stroke = factor(Stroke, levels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c("No", "Yes")),
    HeartDisease = factor(HeartDisease, levels = c("No", "Yes"))
  )
  
  # Validate inputs
  if (any(is.na(input))) {
    return(list(error = "Invalid input. Please ensure all values are provided and correctly formatted."))
  }
  
  # Make prediction
  prediction <- predict(rf_fit, new_data = input)
  
  # Return the prediction
  list(prediction = prediction$.pred_class)
}


#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
