

make_fleet <- function(env__, catch_comp, catch_index, fleet_selectivity, area1, fishing_mortality) {
  fleet <- new(env__$Fleet)
  fleet$AddAgeCompData(catch_comp$id, "undifferentiated")
  fleet$AddIndexData(catch_index$id, "undifferentiated")
  fleet$SetAgeCompNllComponent(fleet_age_comp_nll$id)
  fleet$SetIndexNllComponent(fleet_index_comp_nll$id)
  fleet$AddSelectivity(fleet_selectivity$id, 1, area1$id)
  fleet$AddFishingMortality(fishing_mortality$id, 1, area1$id)

  return(fleet)
}
