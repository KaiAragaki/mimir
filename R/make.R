#' Create a start_condition entry
#'
#' @param seeding_density Numeric. Number of cells/mL.
#' @param growth_time Postgres interval. Time cells spent in the plate.
#' @param plate_form_factor Character. Type of plate.
#' @param cell_id Numeric. ID of cell plated.
#' @return A `data.frame`
make_sc <- function(seeding_density,
                    growth_time,
                    plate_form_factor,
                    cell_id) {
  data.frame(
    seeding_density,
    growth_time,
    plate_form_factor,
    cell_id
  )
}

#' Create a cell entry
#'
#' @param cell_name Character.
#' The 'common' identifier for a cell line, like umuc6_004.
#' @param parent_name Character. The 'common' identifier of the base cell
#' line used to create the cell line. If none, should be the same as `cell_name`
#' @param modifier Character. Free form text describing
#' how the cell line was created.
#' @return A `data.frame`
make_cell <- function(cell_name,
                      parent_name,
                      modifier) {
  data.frame(
    cell_name,
    modifier,
    parent_name
  )
}
#' Create an agent entry
#'
#' @param agent_name Character. The name of the agent.
#' @param concentration Numeric.
#' Final concentration of agent in the specified unit.
#' @param concentration_units Character. Unit of concentration.
#' @param agent_duration Postgres interval. Length agent was exposed to cells.
#' @param agent_start_since_plate Postgres interval.
#' Time since initial plating that agent exposure began.
#' @return A `data.frame`
make_agent <- function(agent_name,
                       concentration,
                       concentration_units,
                       agent_duration,
                       agent_start_since_plate) {
  data.frame(
    agent_name,
    concentration,
    concentration_units,
    agent_duration,
    agent_start_since_plate
  )
}


#' Add a condition to the database
#'
#' @param agent_name
#' @param agent_conc
#' @param agent_conc_units
#' @param agent_duration
#' @param agent_start_since_plate
#' @param sc_seeding_density
#' @param sc_growth_time
#' @param sc_plate_form_factor
#' @param sc_cell_id
#' @param start_condition_id Optional. If specified,
#' will ignore all sc_* arguments.
#' @param agent_id Optional. If specified, will ignore all agent_* arguments.
#' @param condition_set_id Optional. If unspecified, one will be assigned.
#' If specified, must be bundled with an existing condition_set_id.

make_cond <- function(ag_name, ag_conc, ag_conc_units,
                      ag_duration, ag_start_since_plate,
                      seeding_density, growth_time, plate_form_factor, cell,
                      condition_set_id,
                      start_condition_id = NULL,
                      agent_id = NULL,
                      con = xdb_con()) {

  # Might be worth making sure that these condition set IDs exist in the
  # validation step

  if (is.null(agent_id)) {
    agent_df <- validate_agent(
      ag_name, ag_conc, ag_conc_units, ag_duration, ag_start_since_plate, con
    )
    add_if_not_added(agent_df, "agent", con = con)
    agent_id <- find_df(agent_df, con)$agent_id
  }

  if (is.null(start_condition_id)) {
    sc_df <- validate_sc(
      seeding_density, growth_time, plate_form_factor, cell, con
    )
    add_if_not_added(sc_df, "starting_condition", con = con)
    start_condition_id <- find_df(sc_df, con)$start_condition_id
  }

  # Will need to figure out how to make the correct condition set id
  # Can't autoincrement the traditional way (I don't think) since
  # it's not a primary key.
  # Finding the max and then adding 1 probably is a bit slow.
  # Maybe there's a way to look at condition_set and pull a key from there?
  # I think that's the best bet
  data.frame(
    agent_id,
    start_condition_id,
    condition_set_id
  )
}
