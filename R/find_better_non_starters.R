#' Find Better Players
#'
#' @description
#' For each team starter, find another player of the same position or slot, with a higher value.
#'
#' @param data
#' @param cur_team
#' @param non_starter_only
#'
#' @return
#' @export
#'
find_better_players <-
  function(data, cur_team = "Black Wilson", non_starter_only = F) {
    # for each pos, find a non-starter who's better
    assert_cols(data, franchise_name, id, val, starter, pos, player_name)

    team_data <- filter(data,
                        franchise_name == cur_team)
    other_team_data <- filter(data, franchise_name != cur_team)

    if (non_starter_only) {
      team_data <- filter(team_data, starter)
      other_team_data <- filter(other_team_data, !starter)
    }

    # join to other non starters
    team_data <- inner_join(
      x = team_data,
      y = {
        dt <-
          select(other_team_data, franchise_name, id, val, pos, player_name)
        dt <- rename_with(dt, .fn = ~ glue("other_{.x}"))
        dt
      },
      by = join_by(val <= other_val, pos == other_pos)
    )


    return(team_data)
  }




