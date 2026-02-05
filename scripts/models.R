# Initial SDM method test


month_as_number = function(x) {
  lut = seq_along(month.abb) |>
    as.numeric() |>
    rlang::set_names(month.abb)  
  
  lut[x]
}

model_input = model_input |>
  mutate(month = month_as_number(month),
         depth = log(depth)) |>
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

autoplot(wflow)

