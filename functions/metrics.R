plot_roc = function(x, truth, pred, title = "ROC"){
  
  #' Plot an annotated ROC with AUC
  #' 
  #' @param x table of predictive outcomes - see `predict_model`
  #' @param truth the truth column, usually `class`
  #' @param pred the prediction column, usually .pred_presense
  #' @param title chr the optional title
  #' @return ggplot2 object suitable for printing
  
  auc = yardstick::roc_auc(x, {{truth}}, {{pred}}) |>
    dplyr::pull(.estimate)
  roc_curve(x, {{truth}}, {{pred}}) |>
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(lty = 3) +
    coord_equal() +
    theme_bw() + 
    labs(x = "False Positive Rate (Specificity)",
         y = "True Positive Rate (Sensitivity)",
         title = title) + 
    ggplot2::annotate("text", x = 0.8, y= 0.05, 
                      label = sprintf("AUC: %0.3f", auc)) 
}



#' Function that takes a workflow set and returns a metric table,
#' possibly selecting hyperparameter set with the best average metric.
#' 
#' @param wf_set a workflow set
#' @param wflow_id str, one or more workflow ids, by deafult all are computed
#' @param what str, currently accepts only "best" to chose the best set, otherwise
#'   it returns all of the hyperparameters unranked.
#' @return a table of models metrics, possible ranked by the average mean of metrics
metrics_table = function(wf_set, 
                         wflow_id = dplyr::pull(wf_set, dplyr::all_of("wflow_id")),
                         what = "best"){
  
  # call recursively one id at a time
  if (length(wflow_id) > 1){
    rr = sapply(wflow_id,
                function(wid){
                  metrics_table(wf_set, wid, what = what) |>
                    dplyr::mutate(wflow_id = wid, .before = 1)
                }, simplify = FALSE) |>
      dplyr::bind_rows()
    return(rr)
  }
  
  metrics = wf_set$result[[1]]$.metrics[[1]] |> 
    dplyr::pull(dplyr::all_of(".metric"))
  
  keep_me = c("wflow_id", ".config", metrics)
  m =  wf_set |>
    workflowsets::extract_workflow_set_result(wflow_id) |>
    tune::collect_metrics(summarize = TRUE, type = "wide") |>
    #dplyr::select(dplyr::any_of(keep_me))|>
    dplyr::mutate(wflow_id = wflow_id, .before = 1) |>
    dplyr::rowwise() |>
    dplyr::mutate(mean = mean(dplyr::c_across(dplyr::any_of(metrics) ))) |>
    dplyr::arrange(dplyr::desc(mean))
  
  if (tolower(what[1]) == "best"){
    m = dplyr::group_by(m, wflow_id) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()
  }
  m
}


#' function to extract the hyperparameters from the highest ranking model
#' 
#' @param ranked_tbl a ranked table of models ranked based on the average mean of metrics
#' @return tibble of model metrics to be used to finalize the final workflow
best_hyperparams = function(ranked_tbl){
  non_params = c(".metric", ".estimator", "n", ".config", "avg_score")
  
  hyperparams = ranked_tbl |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(non_params)) |>
    dplyr::slice_head(n=1)
  
  return(hyperparams)
}
