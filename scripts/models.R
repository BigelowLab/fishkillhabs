# Initial SDM method test

source("setup.R")

species = "Noctiluca scintillans"

cfg = read_configuration(scientificname = species, version = "v1")
model_input = read_model_input(scientificname = species, 
                               version = "v1",
                               log_me = c("depth")) |>
  dplyr::mutate(month = month_as_number(.data$month)) |>
  select(all_of(c("class", cfg$keep))) |>
  drop_na()


model_input_split = spatial_initial_split(model_input, 
                                          prop = 1 / 5,     
                                          strategy = spatial_block_cv)
model_input_split

autoplot(model_input_split)

tr_data = training(model_input_split)
cv_tr_data <- spatial_block_cv(tr_data,
                               v = 5,     
                               cellsize = grid_cellsize(model_input),
                               offset = grid_offset(model_input) + 0.00001
)
autoplot(cv_tr_data)

one_row_of_training_data = dplyr::slice(tr_data,1)
rec = recipe(one_row_of_training_data, formula = class ~ .)
rec

summary(rec)

wflow = workflow_set(
  
  preproc = list(default = rec), # not much happening in our preprocessor
  
  models = list(                 # but we have 4 models to add
    
    # very simple - nothing to tune
    glm = logistic_reg(
      mode = "classification") |>
      set_engine("glm"),
    
    # two knobs to tune
    rf = rand_forest(
      mtry = tune(),
      trees = tune(),
      mode = "classification") |>
      set_engine("ranger", 
                 importance = "impurity"),
    
    # so many things to tune!
    btree = boost_tree(
      mtry = tune(), 
      trees = tune(), 
      tree_depth = tune(), 
      learn_rate = tune(), 
      loss_reduction = tune(), 
      stop_iter = tune(),
      mode = "classification") |>
      set_engine("xgboost"),
    
    # just two again
    maxent = maxent(
      feature_classes = tune(),
      regularization_multiplier = tune(),
      mode = "classification") |>
      set_engine("maxnet")
  )
)
wflow

metrics = sdm_metric_set(yardstick::accuracy)
metrics

wflow <- wflow |>
  workflow_map("tune_grid",
               resamples = cv_tr_data, 
               grid = 3,
               metrics = metrics, 
               verbose = TRUE)

## i	No tuning parameters. `fit_resamples()` will be attempted
## i 1 of 4 resampling: default_glm
## ✔ 1 of 4 resampling: default_glm (2.6s)
## i 2 of 4 tuning:     default_rf
## i Creating pre-processing data to finalize unknown parameter: mtry
## ✔ 2 of 4 tuning:     default_rf (24m 21.4s)
## i 3 of 4 tuning:     default_btree
## i Creating pre-processing data to finalize unknown parameter: mtry
## → A | warning: Passed invalid argument 'info' - entries on it should be passed as direct arguments. This warning will become an error in a future version., Passed invalid function arguments: nthread. 
## These should be passed as a list to argument 'params'. Conversion from argument to 'params' entry will be done automatically, but this behavior will become an error in a future version., 
## Parameter 'watchlist' has been renamed to 'evals'. This warning will become an error in a future version., Argument 'objective' is only for custom objectives. For built-in objectives, 
## pass the objective under 'params'. This warning will become an error in a future version.
## There were issues with some computations   A: x15
## ✔ 3 of 4 tuning:     default_btree (2m 31.2s)
## i 4 of 4 tuning:     default_maxent
## ✔ 4 of 4 tuning:     default_maxent (1m 7.7s)

autoplot(wflow)

model_fits = workflowset_selectomatic(wflow, model_input_split,
                                      filename = "Noctiluca-scintillans-v1-model_fits", # make this dynamic
                                      path = data_path("models"))
model_fits

model_fit_metrics(model_fits)

model_fit_confmat(model_fits)

model_fit_roc_auc(model_fits)

model_fit_varimp_plot(model_fits)

rf = model_fits |>
  filter(wflow_id == "default_rf")
rf

autoplot(rf$splits[[1]])

model_fit_pdp(model_fits, wid = "default_btree", title = "Boosted Tree")
