#' Join-by append
#'
#' Function to append new conditions to a dplyr "join_by" specification.  Useful when multiple related join specifications are used on the same dataframe to avoid writing each specification out multiple times.
#' @param jb1 Dplyr join_by specification
#' @param jb2 Dplyr join_by specification
#'
#' @return Dplyr join_by specification combining all specifications of jb1 and jb2.
#' @export
#' @examples
#' join_cond_1 = join_by(effective_date <= evaluation_period_end)
#' join_cond_2 = join_by(effective_date >= evaluation_period_start)
#' join_cond_combined = join_by_append(join_cond_1, join_cond_2)
join_by_append = function(jb1,jb2){
	new_join_length = length(jb2$x)
	old_join_length = length(jb1$x)
	jb_out = jb1
	for(i in 1:new_join_length){
		append_point = old_join_length + i
		jb_out$x[append_point] = jb2$x[i]
		jb_out$y[append_point] = jb2$y[i]
		jb_out$condition[append_point] = jb2$condition[i]
		jb_out$filter[append_point] = jb2$filter[i]
		jb_out$exprs[append_point] = jb2$exprs[i]
	}
	return(jb_out)
}