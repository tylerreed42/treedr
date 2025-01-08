#' Re-order levels
#'
#' Logically order levels of mixed text/numeric strings based on the numbers in the string
#' Output is only the raw unique levels ordered in this fashion.  Factors will need to be created manually.
#' Levels without any numeric part are ordered randomly.
#'
#' @param x Character vector
#' @return Character vector of unique levels, ordered by the numeric portion of the string detected
#' @examples
#' sample_vector = sapply(0:10, function(x) paste(c(sample(LETTERS,5,replace=TRUE), sample(0:9,4,replace=TRUE)), collapse = ''))
#' order_levels(sample_vector)
#' @export
order_levels = function(x){
	uniques = x %>% unique() %>% .[!is.na(.)] %>% as.character()
	numerics_id = which(!(stri_extract_first_regex(uniques,'^[:digit:]+') %>% as.numeric() %>% is.na(.)))
	numerics = uniques[numerics_id]
	others = setdiff(uniques,numerics)
	numerics_index = stri_extract_first_regex(numerics,'^[:digit:]+') %>% as.numeric() %>% order()
	others_index = others %>% order() %>% add(length(numerics_index))
	indices = c(numerics_index,others_index)
	return(x %>% unique() %>% .[indices])
}