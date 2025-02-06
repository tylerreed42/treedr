
aglm_data_summary = function(model, model_data, summary_fields = NULL){
	data_summary = data.frame()
	for(j in 1:length(model@vars_info)){
		var_name = model@vars_info[[j]]$name
		is_O = model@vars_info[[j]]$use_OD
		is_U = model@vars_info[[j]]$use_UD
		is_L = model@vars_info[[j]]$use_LV
		if(is_U){
			temp = model_data %>%
				group_by(.data[[var_name]],.drop = FALSE) %>%
				summarize(across(all_of(summary_fields),sum)) %>%
				mutate(variable = var_name) %>%
				select(variable,'level' = var_name,all_of(summary_fields))
		} else if(is_O | is_L){
			if(is_O){break_points = model@vars_info[[j]]$OD_info$breaks
				} else if(is_L){break_points = model@vars_info[[j]]$LV_info$breaks}
			num_breaks = length(break_points)
			break_points[1] = floor(break_points[1])
			break_points[num_breaks] = ceiling(break_points[num_breaks])
			temp = model_data %>% 
				mutate(var = cut(.data[[var_name]],breaks = break_points,right = FALSE,include.lowest = TRUE)) %>%
				group_by(var,.drop = FALSE) %>%
				summarize(across(all_of(summary_fields),sum)) %>%
				mutate(variable = model@vars_info[[j]]$name) %>%
				select(variable,'level' = var,all_of(summary_fields))
		}
		data_summary = rbind(data_summary,temp)
	}
	return(data_summary)
}

aglm_coef_summary = function(model, penalties = c(model@lambda.1se, model@lambda.min, 0)){
	coef_frame = data.frame()
	for(j in 1:length(model@vars_info)){
		var_name = model@vars_info[[j]]$name
		is_L = model@vars_info[[j]]$use_LV
		is_O = model@vars_info[[j]]$use_OD
		is_U = model@vars_info[[j]]$use_UD
		if(length(model@lambda.1se) == 0){penalty_id = paste0('s_',sprintf('%.3f', penalties))
		} else{
			penalty_id = case_when(
				penalties == model@lambda.1se ~ 's_1se',
				penalties == model@lambda.min ~ 's_min',
				TRUE ~ paste0('s_',sprintf('%.3f', penalties)))}	
		var_levels = if(is_U & is_O){
			model@vars_info[[j]]$UD_info$levels %>% ordered(levels = model@vars_info[[j]]$OD_info$breaks %>% levels())
			} else if(is_U){model@vars_info[[j]]$UD_info$levels
			} else if(is_O){model@vars_info[[j]]$OD_info$breaks[-length(model@vars_info[[j]]$OD_info$breaks)] + diff(model@vars_info[[j]]$OD_info$breaks)/2
			} else if(is_L){model@vars_info[[j]]$LV_info$breaks[-length(model@vars_info[[j]]$LV_info$breaks)] + diff(model@vars_info[[j]]$LV_info$breaks)/2}
		x.mat = aglm:::getMatrixRepresentationByVector(var_levels,var_info = model@vars_info[[j]])
		coef_temp = data.frame(variable = var_name,level = var_levels)
		for(penalty in penalties){
			coefs = coef(model, s = penalty) %>% .[rownames(.) %in% rownames(t(x.mat)),]
			penalties_vec = drop(x.mat %*% coefs)
			coef_temp = cbind(coef_temp,penalties_vec)
		}
		names(coef_temp) = c('variable','level',penalty_id)
		coef_frame = rbind(coef_frame,coef_temp)
	}
	#bandaid to handle when lambda.min is equal to lambda.1se
	if(model@lambda.1se == model@lambda.min){
		replace_index = which(names(coef_frame) == 's_1se')[2]
		names(coef_frame)[replace_index] = 's_min'
	}
	return(coef_frame)
}

#' Produce data summary of aglm model
#'
#' Produce a dataframe containing summary of selected fields given an aglm model, the associated dataframe that produced it, and a character list of columns to summarize.
#' Applies the model's binning processes to the source data to facilitate one to one comparisons of model coefficients and raw data.
#' 
#' @param model aglm or cv.aglm object
#' @param model_data dataframe that produced the aglm object
#' @param summary_fields Character vector of field names to sum
#' @param penalties List of penalty levels for which to produce coefficients.  Equivalent to s in \link[aglm]{predict}.  Defaults to 
#' showing coefficients for \code{lambda.1se}, \code{lambda.min}, and 0 penalty.
#' @return Dataframe containing all modeled variables and levels from the aglm object and the associated summarized variables from the model dataset.
#' @export
#' @examples
#' data('Boston', package = 'MASS')
#' model = cv.aglm(y = Boston$medv, x = Boston %>% select(-medv))
#' data_summary = aglm_output_summary(model, Boston, summary_fields = c('crim','age'))


aglm_output_summary = function(model, model_data, summary_fields, penalties = c(model@lambda.1se, model@lambda.min, 0)){
	out = cbind(
			aglm_data_summary(model, model_data, summary_fields),
			aglm_coef_summary(model, penalties) %>% select(-variable) %>% rename(midpoint = level)) %>%
		select(variable,level,midpoint,everything())
	return(out)
}