#' Weighted ntile
#' 
#' An ntile function that respects weighting of observations.  Useful when creating binned variables for predictive models that will use a vector of weights.  This ensures an equal number of observations will be in each bucket with respect to how the model is created.
#'
#' @param x Numeric vector
#' @param n Number of ntiles
#' @param weight Numeric vector containing weights of each record.  Default 1.
#' @param solo_levels Character vector of individual levels of x which should be binned individually.  These are "special levels" within x such as 0, or levels that otherwise do not make sense to bin with other levels.
#'
#' @return Ordered factor representing x binned into ntiles.  The number of ntiles is n plus the number of solo levels specified.  Levels are formatted as whole numbers.
#' @export
#' @examples
#' df = data.frame(earned_premium = rexp(1000, 0.01), credit_score = sample(c(rep(997,50),sample(300:850, size = 950, replace = TRUE, prob = 850:300))))
#' df %>% mutate(credit_score_ntile = weighted_ntile(credit_score, 5, weight = earned_premium, solo_levels = c(997))) %>% group_by(credit_score_ntile) %>% summarize(across(earned_premium,sum))
weighted_ntile = function(x, n, weight = 1, solo_levels = NULL){
	temp = data.frame(x, weight) %>%
		filter(!(x %in% solo_levels) & !is.na(x)) %>%
		arrange(x) %>%
		group_by(x) %>%
		summarize(weight = sum(weight)) %>%
		mutate(weightsum = cumsum(weight))
	weight_total = sum(temp$weight)
	weight_cutoffs = (1:n)*(weight_total / n)
	value_cutoffs = c(min(x, na.rm = TRUE),sapply(weight_cutoffs, function(x) temp[temp$weightsum<=x,'x'] %>% max()))
	max_value = nchar(max(x))
	labels = sprintf("%s , %s",
		paste0('(',formatC(head(value_cutoffs, -1), format = "f", big.mark = ",", digits = 0)),
		paste0(formatC(tail(value_cutoffs, -1), format = "f", big.mark = ",", digits = 0),']'))
	labels[1] = gsub('\\(','[',labels[1])
	temp2 = cut(x, breaks = value_cutoffs, labels = labels, include.lowest = T, right = T, ordered_result = T)
	solo_ind = x %in% solo_levels
	out = if_else(solo_ind, 
			formatC(x, format = "f", big.mark = ",", digits = 0), 
			as.character(temp2)) %>%
		factor(levels = c(levels(temp2), formatC(solo_levels, format = "f", big.mark = ",", digits = 0)))
	return(out)
}