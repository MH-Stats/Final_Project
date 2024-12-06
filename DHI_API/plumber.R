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

#* @apiTitle Diabetes Prediction API
#* @apiDescription This API allows the end user to make predictions of diabetes status through interaction with a random forest model fitted onto the Diabetes Health Indicators Dataset.

#* Diabetes Prediction 
#* Accepts inputs and predicts the status of diabetes as either "Yes" or "No"
#* @param HighBP High Blood Pressure as Binary ("Yes" or "No"). Default: "No"
#* @param HighChol High Cholesterol as Binary ("Yes" or "No"). Default: "No"
#* @param BMI BMI as Numeric (e.g., 30). Default: 28.38
#* @param Stroke Stroke at any point of life as Binary ("Yes" or "No"). Default: "No"
#* @param PhysActivity Physical Activity in Last 30 days as Binary ("Yes" or "No"). Default: "Yes"
#* @param HeartDisease Heart Disease as Binary ("Yes" or "No"). Default: "No"
#* @post /predict
function(HighBP = "No", 
         HighChol = "No", 
         BMI = 28.38, 
         Stroke= "No", 
         PhysActivity = "Yes", 
         HeartDisease = "No"
         ) {
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

#* Example Function Call
#* @param msg Input Examples to see the three example function calls
#* @get /echo
function(msg = "Examples") {
    list(Example_1 = paste0("HighBP = Yes, HighChol = Yes, BMI = 20, Stroke = Yes, PhysicalActivity = Yes, HeartDisease = Yes, Prediction = No"),
         Example_2 = paste0("HighBP = Yes, HighChol = Yes, BMI = 35, Stroke = Yes, PhysicalActivity = Yes, HeartDisease = Yes, Prediction = Yes"),
         Example_3 = paste0("HighBP = No, HighChol = No, BMI = 40, Stroke = No, PhysicalActivity = Yes, HeartDisease = No, Prediction = No")
         )
}

#* Link to Github
#* @get /info
function() {
  list(
    Author = "Mark Heinen",
    Website = "insert URL"
  )
}

#* Plotting the Random Forest Model Confusion matrix
#* @serializer png
#* @get /confusion
function() {
  # Grabbing the predictions from the model tested on the data set
  rf_predictions <- rf_fit |>
    predict(new_data = dhi_data) |>
    bind_cols(dhi_data)
    
  # Creating a confusion matrix 
  con_matrix <- rf_predictions |>
    conf_mat(truth = Diabetes, estimate = .pred_class)
  
  # Plotting the confusion matrix
  con_plot <- autoplot(con_matrix, type = "heatmap") +
    ggtitle("Confusion Matrix")
  print(con_plot)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}

