#Questions D 
library(tidyverse)
library(tidymodels)
library(themis) #smote 

# speed up computation with parrallel processing (optional)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

load(file = "df_D.Rdata")
addmargins (table(df_D$Status)*100/nrow(df_D))


# Create train and test sets
df_train <- df_D %>% 
  filter(Age >= 5) %>% 
  select(-c(`Last year`,`Incorporation year`,Age))

addmargins (table(df_train$Status)*100/nrow(df_train))

df_test <- df_D %>% 
  filter(Age < 5) %>% 
  select(-c(`Last year`,`Incorporation year`,Age))

addmargins (table(df_test$Status)*100/nrow(df_test))

set.seed(234)
val_set <- validation_split(df_train, strata = Status, prop = 0.80)
set.seed(234)
df_folds <- vfold_cv(df_train, strata = Status, v=4)
# Pre-processing data 
bnk_rec <- recipe(Status~., data = df_train) %>% 
  #step_normalize(all_numeric()) %>% 
  #step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_smote(Status,over_ratio = 0.75) %>% 
  prep()

#See new balance of bankruptcy class
bnk_rec %>% bake(new_data = NULL) %>% count(Status)
# Active 57% Failed 43%

#Models
metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)
cores <- parallel::detectCores()

#Creation of the models
LogReg_model <- logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")

RandomForest_model <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>%
  set_mode("classification")

XGBoost_model <- boost_tree(
  trees = 500, 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                     
  sample_size = tune(),
  mtry = tune(),         
  learn_rate = tune(),                         
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")


#Workflow models

rf_workflow <- 
  workflow() %>% 
  add_model(RandomForest_model) %>% 
  add_recipe(bnk_rec)

lg_workflow <- 
  workflow() %>% 
  add_model(LogReg_model) %>% 
  add_recipe(bnk_rec)

xgboost_workflow <- 
  workflow() %>% 
  add_model(XGBoost_model) %>% 
  add_recipe(bnk_rec)

# Create tune grids
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

rf_grid <- grid_regular(parameters(RandomForest_model), levels = 4)
xgboost_grid <- grid_regular(parameters(XGBoost_model), levels = 4)
logistic_grid <- grid_regular(parameters(LogReg_model), levels = 4)


#Tune Models
set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 10,
            control = control_grid(verbose = TRUE, save_pred = TRUE),
            metrics = metrics)

set.seed(345)
xg_res <- 
  tune_grid(
    xgboost_workflow,
    resamples = df_folds,
    grid = 10,
    metrics = metrics,
    #control = control_race(verbose_elim = TRUE)
    control = control_grid(verbose = TRUE, save_pred = TRUE)
  ) 

set.seed(345)
glm_rs <- 
  lg_workflow %>%
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(verbose = TRUE, save_pred = TRUE),
            metrics = metrics
    
  )

set.seed(345)
xgboost_res_2 <- 
  tune_grid(
    xgboost_workflow,
    resamples = val_set,
    grid = 10,
    metrics = metrics,
    #control = control_race(verbose_elim = TRUE)
    control = control_grid(verbose = TRUE, save_pred = TRUE)
  ) 

#Evaluation Models
all_models <- xgboost_res_2 %>% collect_metrics(summarize = FALSE) %>% mutate(model = "xgboost") %>% 
  bind_rows(glm_rs %>% collect_metrics(summarize = FALSE) %>% mutate(model = "logistic")) %>% 
  bind_rows(rf_res %>% collect_metrics(summarize = FALSE) %>% mutate(model = "random forest"))

all_models %>% 
  ggplot(aes(x = model, y = .estimate)) + 
  geom_boxplot() + 
  facet_wrap(~.metric, scales = "free")

all_models %>% 
  group_by(model,.metric) %>% 
  summarise(mean = mean(.estimate)) %>%
  pivot_wider(names_from = "model", values_from = "mean") %>% 
  View()


#Fit resamples
logistic_res <- fit_resamples(LogReg_model, bnk_rec, df_folds)
randomForest_res <- fit_resamples(randomForest_model, bnk_rec, df_folds)
XGBoost_res <- fit_resamples(XGBoost_model, bnk_rec, df_folds)


