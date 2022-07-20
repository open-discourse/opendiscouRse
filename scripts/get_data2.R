devtools::load_all()

# initialize OpenDiscourse Object
od_obj <- OpenDiscourse$new()

# get data objects

speeches <- od_obj$get_data("speeches")$data
factions <- od_obj$get_data("factions")$data
politicians <- od_obj$get_data("politicians")$data
contributions_simplified <- od_obj$get_data("contributions_simplified")$data
contributions_extended <- od_obj$get_data("contributions_extended")$data
electoral_terms <- od_obj$get_data("electoral_terms")$data
