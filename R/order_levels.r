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
	indices = x %>% unique() %>% stri_extract_first_regex(.,'[0-9]+') %>% as.numeric() %>% order()
	x %>% unique() %>% .[indices]
}