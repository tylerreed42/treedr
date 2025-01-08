#' Remove zeros
#' @param x Numeric vector
#' @return Vector with 0s removed
#' @keywords internal

remove_zeros = function(x){
	return(x[which(x != 0)])
}

#' Connect neighborhoods
#'
#' This function "minimally" connects disconnected neighborhood objects produced by \link[spdep]{poly2nb} and the like.
#' The process finds the smallest connected component and searches for the shortest distance to
#' any other component and then adds a neighboring relationship.  It iterates this until there is
#' only a single connected component.
#'
#' This is motivated by cases when geographies have disconnections such that ordinary neighboring heuristics such as queen neighbor / rook neighbor don't recognize spatial units as neighbors that we would ordinarily consider neighbors.
#' For example Monroe County, Florida (the Keys) is not recognized as a neighbor of Miami-Dade County under either the queen or rook criterion because of the ocean disconnecting the two areas.
#'
#' Other ways to solve this issue include using a distance-based neighborhood criteria, adding buffers to polygons, or using a more novel transportation-based neighborhood criteria along the lines of two territories being neighbors if a segment of a road exists connecting the two without intersecting any other polygon.
#' These are better for more comprehensive datasets but this function serves fine when there are only a few obvious disconnections to be resolved.
#'
#' @param geo Spatial object consisting of points, polygons or multi-polygons
#' @param nb Neighbors list based on geo as constructed by spdep::poly2nb or similar
#' @return Neighbors list consisting of a single connected component which respects the originally provided neighboring
#' @export
#' @examples
#' geo = st_sfc(
#' poly_a = st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
#' poly_b = st_polygon(list(rbind(c(2, 0), c(4, 0), c(4, 2), c(2, 2), c(2, 0)))),
#' poly_c = st_polygon(list(rbind(c(5, 5), c(7, 5), c(7, 7), c(5, 7), c(5, 5)))),
#' poly_d = st_polygon(list(rbind(c(7, 5), c(9, 5), c(9, 7), c(7, 7), c(7, 5))))
#' )
#' nb = spdep::poly2nb(geo)
#' new_nb = connect_nb(geo, nb)
#' par(mfrow = c(1,2))
#' plot(geo, main = 'Original neighborhood')
#' plot.nb(nb, coords = geo, col = 'blue',add = TRUE)
#' plot(geo, main = 'New neighborhood')
#' plot.nb(new_nb, coords = geo, col = 'red', add = TRUE)


connect_nb = function(geo, nb){
	component_count = n.comp.nb(nb)$nc
	nb_out = nb
	while(component_count > 1){
		# find smallest component
		geo_working = st_as_sf(geo) %>% mutate(id = 1:length(geo), comp.id = n.comp.nb(nb_out)$comp.id)
		geo_comp = geo_working %>% st_drop_geometry()
		comp_to_check = geo_comp %>%
			group_by(comp.id) %>%
			summarize(count = n()) %>%
			mutate(rank = row_number(desc(count))) %>% # find component with fewest features (likely singletons).  do not allow ties
			filter(rank == 2) %>%
			slice(1) %>%
			pull(comp.id)
		# find nearest component to the smallest component
		target_component = geo_working %>% filter(comp.id == comp_to_check)
		other_component = geo_working %>% filter(comp.id != comp_to_check)
		nearest = st_nearest_feature(target_component, other_component)
		# find nearest element in the nearest component
		distances = st_distance(target_component, other_component[nearest,], by_element = TRUE)
		index_min = which.min(distances)
		id_target = target_component[index_min,] %>% pull(id)
		id_other = other_component[nearest[index_min],] %>% pull(id)
		nb_out[[id_target]] = remove_zeros(c(id_other, nb_out[[id_target]]))
		nb_out[[id_other]] = remove_zeros(c(id_target, nb_out[[id_other]]))
		component_count = n.comp.nb(nb_out)$nc
	}
	return(nb_out)
}
