# ! this script should be commented out


# library(ffoptim)


# all_league_assets <-
#   select(ffdata$all_rostered_assets, all_assets) %>%
#   tidyr::unnest(all_assets)
#
# # join present values to assets
# pres_vals <- ffoptim::get_player_present_values()
#
# joined_vals <- all_league_assets %>%
#   left_join(pres_vals)
#
# desire_fn <-
#   build_desirability_fn(
#     present_values = joined_vals$present_val,
#     future_values = joined_vals$value_2qb,
#     present_scale = 0
#   )
#
# mutate(
#   joined_vals,
#   new_vals = desire_fn(
#     present_value = .data$present_val,
#     future_value = .data$value_2qb
#   ) %>% round(digits = 4)
# ) %>%
#   tibble::view()


# Trade Strategy Overview #
# each trade based on 1:1 basis:
#   - one asset for another asset
# values are defined as:
#   - self value ~ valuation system of owning team
#   - alt value ~ valuation system of opposing team
# trade is evaluated if:
#   - owned asset x more alt value than opposing asset y
#   - opposing asset y has more self value than owned asset x
#   - thus, creating a mutually beneficial asset swap
# total value is calculated as:
#   - self value gained for self
#   - alt value gained for opp
# degree of fleece is set as max allowable alt value
#   - if alt value increase is only 0, the opp value post transaction can only increase by 0
# additional assets:
#   - if alt value is too high, a new asset is introduced
#   - traverse their asset list and sub-assets
#     - select max allowable alt value



















