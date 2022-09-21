############################<TITTLE>##################################
## Descripción:    Diabetes Data: EDA
## Autor: Sharon Trejo
######################################################################.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(discrim)
library(baguette)
library(bonsai)
library(patchwork)


# Loads Data  -------------------------------------------------------------
diabetes <- read_csv("diabetes.csv")

diabetes$Outcome <- as.factor(diabetes$Outcome)

# Explore data 

diabetes %>% glimpse()

diabetes %>% head()

diabetes %>% summary() 


# EDA ---------------------------------------------------------------------

# * Pregnancy -------------------------------------------------------------

diabetes$Pregnancies %>% summary()

hist_pregnancies <- diabetes %>% 
  ggplot(aes(x = Pregnancies)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "Pregnancies Histogram Plot", x = "Pregnancies", y = "Count") +
  theme_minimal()

box_pregnancies <- diabetes %>% 
  ggplot(aes(x = Pregnancies)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "Pregnancies Box Plot", x = "Pregnancies") +
  theme_minimal()

hist_pregnancies + box_pregnancies

# We can see that Pregnancy variable has a distribution skewed to the right with
# a mean value of 3.845 pregnancies per woman.


# * Blood Pressure --------------------------------------------------------

diabetes$BloodPressure %>% summary()

hist_blood <- diabetes %>% 
  ggplot(aes(x = BloodPressure)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "BloodPressure Histogram Plot", x = "BloodPressure", y = "Count") +
  theme_minimal()

box_blood <- diabetes %>% 
  ggplot(aes(x = BloodPressure)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "BloodPressure Box Plot", x = "BloodPressure") +
  theme_minimal()

hist_blood + box_blood

# Blood Pressure is centered around 72 units with few outliers.


#* SkinThickness -----------------------------------------------------------

diabetes$SkinThickness %>% summary()

hist_skin <- diabetes %>% 
  ggplot(aes(x = SkinThickness)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "SkinThickness Histogram Plot", x = "SkinThickness", y = "Count") +
  theme_minimal()

box_skin <- diabetes %>% 
  ggplot(aes(x = SkinThickness)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "SkinThickness Box Plot", x = "SkinThickness") +
  theme_minimal()

hist_skin + box_skin

# SkinThickness has a right skewed distribution with heavy right tail, 
# and a mean of 20.54 units 


# * Insulin ---------------------------------------------------------------

diabetes$Insulin %>% summary()

hist_insulin <- diabetes %>% 
  ggplot(aes(x = Insulin)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "Insulin Histogram Plot", x = "Insulin", y = "Count") +
  theme_minimal()

box_insulin <- diabetes %>% 
  ggplot(aes(x = Insulin)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "Insulin Box Plot", x = "Insulin") +
  theme_minimal()

hist_insulin + box_insulin


# * BMI -------------------------------------------------------------------

diabetes$BMI %>% summary()

hist_bmi <- diabetes %>% 
  ggplot(aes(x = BMI)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "BMI Histogram Plot", x = "BMI", y = "Count") +
  theme_minimal()

box_bmi <- diabetes %>% 
  ggplot(aes(x = BMI)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "BMI Box Plot", x = "BMI") +
  theme_minimal()

hist_bmi + box_bmi


# * DiabetesPedigree ------------------------------------------------------

diabetes$DiabetesPedigreeFunction %>% summary()

hist_pedigree <- diabetes %>% 
  ggplot(aes(x = DiabetesPedigreeFunction)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "DiabetesPedigreeFunction Histogram Plot", 
       x = "DiabetesPedigreeFunction", 
       y = "Count") +
  theme_minimal()

box_pedigree <- diabetes %>% 
  ggplot(aes(x = DiabetesPedigreeFunction)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "DiabetesPedigreeFunction Box Plot", 
       x = "DiabetesPedigreeFunction") +
  theme_minimal()

hist_pedigree + box_pedigree


# * Age -------------------------------------------------------------------

diabetes$Age %>% summary()

hist_age <- diabetes %>% 
  ggplot(aes(x = Age)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "Age Histogram Plot", x = "Age", y = "Count") +
  theme_minimal()

box_age <- diabetes %>% 
  ggplot(aes(x = Age)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "Age Box Plot", x = "Age") +
  theme_minimal()

hist_age + box_age


# *  Outcome ---------------------------------------------------------------

diabetes$Outcome %>% summary()

diabetes %>% 
ggplot(aes(x = Outcome)) +
  stat_count(fill = "steelblue") +
  labs(title = "Outcome Bar Plot", x = "Outcome", y = "Count") +
  theme_minimal()


# Outlier Analysis --------------------------------------------------------

Pregnancies_out <- boxplot(diabetes$Pregnancies, plot = FALSE)
Glucose_out <- boxplot(diabetes$Glucose, plot = FALSE)
BloodPressure_out <- boxplot(diabetes$BloodPressure, plot = FALSE)
SkinThickness_out <- boxplot(diabetes$SkinThickness, plot = FALSE)
Insulin_out <- boxplot(diabetes$Insulin, plot = FALSE)
BMI_out <- boxplot(diabetes$BMI, plot = FALSE)
DiabetesPedigreeFunction_out <- boxplot(diabetes$DiabetesPedigreeFunction, 
                                        plot = FALSE)
Age_out <- boxplot(diabetes$Age, plot = FALSE)

diabetes <- diabetes %>% 
  mutate(Pregnancies = case_when(
    # Modify outliers for Pregnancies
    Pregnancies <= Pregnancies_out$stats[[1]] ~ Pregnancies_out$stats[[1]],
    Pregnancies >= Pregnancies_out$stats[[5]] ~ Pregnancies_out$stats[[5]],
    TRUE ~ Pregnancies),
    # Modify outliers for Glucose
    Glucose = case_when(
      Glucose <= Glucose_out$stats[[1]] ~ Glucose_out$stats[[1]],
      Glucose >= Glucose_out$stats[[5]] ~ Glucose_out$stats[[5]],
      TRUE ~ Glucose),
    # Modify outliers for BloodPressure
    BloodPressure = case_when(
      BloodPressure <= BloodPressure_out$stats[[1]] ~ BloodPressure_out$stats[[1]],
      BloodPressure >= BloodPressure_out$stats[[5]] ~ BloodPressure_out$stats[[5]],
      TRUE ~ BloodPressure ),
    # Modify outliers for SkinThicknes
    SkinThickness = case_when(
      SkinThickness <= SkinThickness_out$stats[[1]] ~ SkinThickness_out$stats[[1]],
      SkinThickness >= SkinThickness_out$stats[[5]] ~ SkinThickness_out$stats[[5]],
      TRUE ~ SkinThickness ),
    # Modify outliers for Insulin
    Insulin = case_when(
      Insulin <= Insulin_out$stats[[1]] ~ Insulin_out$stats[[1]],
      Insulin >= Insulin_out$stats[[5]] ~ Insulin_out$stats[[5]],
      TRUE ~ Insulin),
    # Modify outliers for BMI
    BMI = case_when(
     BMI <= BMI_out$stats[[1]] ~ BMI_out$stats[[1]],
     BMI >= BMI_out$stats[[5]] ~ BMI_out$stats[[5]],
     TRUE ~ BMI),
    # Modify outliers for DiabetesPedigree 
    DiabetesPedigreeFunction = case_when(
      DiabetesPedigreeFunction <= DiabetesPedigreeFunction_out$stats[[1]] ~ DiabetesPedigreeFunction_out$stats[[1]],
      DiabetesPedigreeFunction >= DiabetesPedigreeFunction_out$stats[[5]] ~ DiabetesPedigreeFunction_out$stats[[5]],
      TRUE ~ DiabetesPedigreeFunction),
    # Modify Outliers for Age 
    Age = case_when(
      Age <= Age_out$stats[[1]] ~ Age_out$stats[[1]],
      Age >= Age_out$stats[[5]] ~ Age_out$stats[[5]],
      TRUE ~ Age
    ))


# Training - Testing - Cross Validation -----------------------------------

# Splits in 80-20 proportion

set.seed(123)

diabetes_split <- initial_split(diabetes, prop = 0.80)
diabetes_split

diabetes_train <- training(diabetes_split)
diabetes_test  <- testing(diabetes_split)

diabetes_cv <- vfold_cv(diabetes_train, v = 10)


# Pre-processing ----------------------------------------------------------

model_recipe <- 
  recipe(Outcome ~ ., data = diabetes_train) %>%
  step_mutate(age_group = ifelse(Age %in% 21:35, 0, 1)) %>%
  step_log(Age) %>%
  step_zv(all_predictors())


# Modelling ---------------------------------------------------------------

diabetes_metrics <- metric_set(accuracy, f_meas, precision, recall)
# *  Logistic Regression ---------------------------------------------------

# Model Specification
log_model <- logistic_reg(
  penalty = tune(),
  mixture = tune()
  ) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")
  
# Workflow with Hyperparameter Tunning
set.seed(123)
log_wf <-workflow() %>%
  add_model(log_model) %>% 
  add_recipe(model_recipe)

log_wf

doParallel::registerDoParallel(cores = 3)
log_results <- log_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

log_results %>%
  collect_metrics()

log_best <- log_results %>%
  select_best(metric = "accuracy")
log_best

log_wf <- log_wf %>%
  finalize_workflow(log_best)
log_wf

log_fit <- log_wf %>%
  last_fit(diabetes_split)

test_performance <- log_fit %>% collect_predictions()
test_performance

diabetes_metrics(data = test_performance, truth = Outcome, estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)


# *  Naive Bayes -----------------------------------------------------------
nb_model <- naive_Bayes(
  smoothness = tune(),
  Laplace = tune(),
  ) %>% 
  set_engine("naivebayes") %>% 
  set_mode("classification")

set.seed(123)

nb_wf <- workflow() %>%
  add_model(nb_model) %>% 
  add_recipe(model_recipe)
nb_wf

doParallel::registerDoParallel(cores = 3)
nb_results <-nb_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

nb_results %>%
  collect_metrics()

nb_best <- nb_results %>%
  select_best(metric = "accuracy")
nb_best

nb_wf <- nb_wf %>%
  finalize_workflow(nb_best)
nb_wf

nb_fit <- nb_wf %>%
  last_fit(diabetes_split)

test_performance <- nb_fit %>% collect_predictions()
test_performance

diabetes_metrics <- metric_set(accuracy, f_meas, precision, recall)
diabetes_metrics(data = test_performance, truth = Outcome, estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)


#  * K - Nearest Neighbor -------------------------------------------------

knn_model <- nearest_neighbor(
  neighbors = tune(),
  weight_func = tune(),
  dist_power = tune()
) %>% 
  set_engine("kknn") %>% 
  set_mode("classification") %>% 
  translate()
  

set.seed(123)
knn_wf <- workflow() %>%
  add_model(knn_model) %>% 
  add_recipe(model_recipe)
knn_wf

doParallel::registerDoParallel(cores = 3)
knn_results <- knn_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

knn_results %>%
  collect_metrics()

knn_best <- knn_results %>%
  select_best(metric = "accuracy")
knn_best

knn_wf <- knn_wf %>%
  finalize_workflow(knn_best)
knn_wf

knn_fit <- knn_wf %>%
  last_fit(diabetes_split)

test_performance <- knn_fit %>% collect_predictions()
test_performance

diabetes_metrics(data = test_performance, truth = Outcome, estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)


# * Decision Trees ---------------------------------------------------------

dt_model <- decision_tree(
                cost_complexity = tune(),
                tree_depth = tune(),
                min_n = tune()
                ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

set.seed(123)
dt_wf <-
  workflow() %>%
  add_model(dt_model) %>% 
  add_recipe(model_recipe)
dt_wf

doParallel::registerDoParallel(cores = 3)
dt_results <-
  dt_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

dt_results %>%
  collect_metrics()

dt_best <- dt_results %>%
  select_best(metric = "accuracy")
dt_best

dt_wf <- dt_wf %>%
  finalize_workflow(dt_best)
dt_wf

dt_fit <- dt_wf %>%
  last_fit(diabetes_split)

test_performance <- dt_fit %>% collect_predictions()
test_performance

diabetes_metrics(data = test_performance, truth = Outcome, estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)


# * Random Forest ---------------------------------------------------------

rf_model <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")
  

set.seed(123)
rf_wf <- workflow() %>%
  add_model(rf_model) %>% 
  add_recipe(model_recipe)
rf_wf

doParallel::registerDoParallel(cores = 3) 
rf_results <-rf_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

rf_results %>%
  collect_metrics()

rf_best <- rf_results %>%
  select_best(metric = "accuracy")
rf_best

rf_wf <- rf_wf %>%
  finalize_workflow(rf_best)
rf_wf

rf_fit <- rf_wf %>%
  last_fit(diabetes_split)

test_performance <- rf_fit %>% collect_predictions()
test_performance

diabetes_metrics(data = test_performance, truth = Outcome, estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)


#* Light GBM ---------------------------------------------------------------

lgbm_model <- boost_tree(
              mtry = tune(),
              trees = tune(),
              min_n = tune(),
              tree_depth = tune(),
              learn_rate = tune(),
              loss_reduction = tune()) %>% 
  set_engine("lightgbm") %>% 
  set_mode("classification")

set.seed(123)
lgbm_wf <- workflow() %>%
  add_model(lgbm_model) %>% 
  add_recipe(model_recipe)
lgbm_wf

doParallel::registerDoParallel(cores = 3)
lgbm_results <- lgbm_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

lgbm_results %>%
  collect_metrics()

lgbm_best <- lgbm_results %>%
  select_best(metric = "accuracy")
lgbm_best

lgbm_wf <- lgbm_wf %>%
  finalize_workflow(lgbm_best)
lgbm_wf

lgbm_fit <- lgbm_wf %>%
  last_fit(diabetes_split)

test_performance <- lgbm_fit %>% collect_predictions()
test_performance

diabetes_metrics(data = test_performance, 
                 truth = Outcome, 
                 estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)


# * XGBoost -----------------------------------------------------------------

xgboost_model <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  stop_iter = tune()
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")


set.seed(123)
xgboost_wf <- workflow() %>%
  add_model(xgboost_model) %>% 
  add_recipe(model_recipe)
xgboost_wf

doParallel::registerDoParallel(cores = 3)
xgboost_results <- xgboost_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

xgboost_results %>%
  collect_metrics()

xgb_best <- xgboost_results %>%
  select_best(metric = "accuracy")
xgb_best

xgboost_wf <- xgboost_wf %>%
  finalize_workflow(xgb_best)
xgboost_wf

xgboost_fit <- xgboost_wf %>%
  last_fit(diabetes_split)

test_performance <- xgboost_fit %>% collect_predictions()
test_performance

diabetes_metrics(data = test_performance, truth = Outcome, estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)



