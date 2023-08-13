get_ginis <- function(dt_etrs, group_by = c("ETR_Type")) {
  dt_ginis <- dt_etrs[
    , .(
      Income_Gini = TAWAwealth::weighted_gini(Eq_ETR_Income, PeopleWeight),
      Net_Income_Gini = TAWAwealth::weighted_gini(Eq_ETR_Net_Income, PeopleWeight)
    ),
    keyby = group_by
    ]
  return(dt_ginis)
}
