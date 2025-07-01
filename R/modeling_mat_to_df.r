#' Modeling matrix to dataframe
#' 
#' Reproduces a human readable dataframe from an aglm model object.
#'
#' @param x Aglm model object.  Either cv.aglm or aglm.
#'
#' @return Dataframe containing the original
#' @export
#' @examples
#' df = data.frame(earned_premium = rexp(1000, 0.01), credit_score = sample(c(rep(997,50),sample(300:850, size = 950, replace = TRUE, prob = 850:300))))
#' df %>% mutate(credit_score_ntile = weighted_ntile(credit_score, 5, weight = earned_premium, solo_levels = c(997))) %>% group_by(credit_score_ntile) %>% summarize(across(earned_premium,sum))
convert_modeling_matrix = function(aglm_model) {
    # Extract relevant pieces of aglm model and backend glmnet model
    if(!is.null(aglm_model@backend_models$cv.glmnet)){
        backend_object = aglm_model@backend_models$cv.glmnet
    } else{backend_object = aglm_model@backend_models$glmnet}
    modeling_matrix = backend_object$call$x
    vars_info = aglm_model@vars_info
    y = backend_object$call$y
    weight = backend_object$call$weights
    additional_data <- aglm_model@backend_models$diagnostic_data$additional_data # Extract additional data
    var_names <- aglm_model@backend_models$diagnostic_data$var_names


    # Extract column names from the modeling matrix
    col_names = colnames(modeling_matrix)
    
    # Initialize an empty dataframe to store the human-readable format
    readable_df = data.frame(matrix(nrow = nrow(modeling_matrix), ncol = length(vars_info)))
    colnames(readable_df) = sapply(vars_info, function(var) var$name) # Set human-readable column names
    
    # Iterate over vars_info to map column names back to variables and levels
    for (j in seq_along(vars_info)) {
        var_name = vars_info[[j]]$name
		quan = (vars_info[[j]]$type == 'quan')
        if(quan){
            readable_df[[var_name]] = modeling_matrix[,col_names == var_name]
            next
        }		
        
        levels = vars_info[[j]]$UD_info$levels
        ordered = vars_info[[j]]$use_OD || vars_info[[j]]$use_LV

	   
        
        # Find columns corresponding to this variable
        matching_cols = grep(paste0("^", var_name, "_UD_"), col_names)
        
        # Map one-hot encoded columns back to levels
        if (length(matching_cols) > 0) {
            print(var_name)
            
            # Use matrix multiplication to find the active level index for each row	
            level_indices = max.col(modeling_matrix[, matching_cols, drop = FALSE], ties.method = "first")
            
            # Map indices to levels, handling cases where no level is active
            readable_df[[var_name]] = ifelse(level_indices > 0, levels[level_indices], NA)
        }

        # Convert to factor or ordered factor
        if (!is.null(levels)) {
            readable_df[[var_name]] <- if (ordered) {
                factor(readable_df[[var_name]], levels = levels, ordered = TRUE)
            } else {
                factor(readable_df[[var_name]], levels = levels)
            }
        }
    }    
    # Combine the human-readable dataframe with additional variables
    final_df = cbind(readable_df, y = y)
    if(!is.null(weight)){final_df = cbind(final_df, weight = weight)}

    # Add additional data to final_df
    if (!is.null(additional_data)) {
        for (name in names(additional_data)) {
            final_df[[name]] <- additional_data[[name]] # Add each named vector as a new column
        }
    }

    if(!is.null(var_names$weight_vars)){colnames(final_df)[colnames(final_df) == "weight"] <- var_names$weight_vars}
    if(!is.null(var_names$y_var)){colnames(final_df)[colnames(final_df) == "y"] <- var_names$y_var}
    
    return(final_df)
}
