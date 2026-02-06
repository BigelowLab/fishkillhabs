partial_dependence_plot = function(x, 
                                   share_y = "all", 
                                   filename = NULL, 
                                   v = NULL,
                                   data = NULL,
                                   outcome = "class",
                                   ...){
  
  #' Given a workflow or fitted model generate a partial dependence plot
  #' 
  #' @param x workflow with a fitted model
  #' @param share_y chr, by default "all" but see `?plot.EffectsData`
  #' @param v chr, variable names `?plot.EffectsData`. Not needed for workflow.
  #' @param data table with outcomes and predictors. Not needed for workflow.
  #' @param outcome chr, the name of the column (variable) that defines the outcome
  #' @param ... other arguments for `?plot.EffectsData`
  #' @return plot object as ggplot or plotly see `?plot.EffectsData`
  
  if (inherits(x, "workflow")){
    stopifnot(is_trained_workflow(x))
    # extract the mold
    mold = workflows::extract_mold(x)
    # build training data
    data = dplyr::bind_cols(mold$outcomes, mold$predictors)
    # extract the fitted model engine
    x = workflows::extract_fit_engine(x)
    v = colnames(mold$predictors)
  } else {
    if (is.null(data)) stop("if not proving a workflow, please provide data and v")
    nm = colnames(data)
    v = nm[!(nm %in% outcome)]
    x = x$fit
  }
  # compute the partial effects
  ix = which(colnames(data) == outcome)
  pd = effectplots::partial_dependence(x, 
                                       v = v, 
                                       data = data,
                                       which_pred = ix) 
  # plot
  p = plot(pd, share_y = share_y, ...)
  # save if requested
  if (!is.null(filename)){
    ggplot2::ggsave(filename, plot = p)
  }
  
  return(p)
}
