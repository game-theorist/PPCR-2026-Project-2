#Physical Activity

#Loading raw data

pa_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/PAQ_L.xpt")

#Moderate Activity

#Removing observations with "refused", "dont know" or missing values

refused_or_dunno <- function(x) {
  x %in% c(7777, 9999)
}

pa_moderate <- pa_raw |> 
  select(SEQN:PAD800) |> 
  drop_na() |> 
  filter_out(
    if_any(starts_with("PAD"), refused_or_dunno)
    )

#Transforming observation data to minutes_per_week of moderate activity

pa_moderate_weekly <- pa_moderate |>
  group_by(SEQN) |> 
  summarize(moderate_weekly_minutes = case_when(
    PAD790U == "D" ~ PAD800 * 7,
    PAD790U == "M" ~ PAD800 / 4,
    PAD790U == "Y" ~ PAD800 / 52,
    PAD790U == "W" ~ PAD800)
    )

#calculating weekly MET-minutes

pa_moderate_met <- pa_moderate_weekly |> 
  mutate(moderate_met = moderate_weekly_minutes * 5)

#Vigorous Activity

#Removing observations with "refused", "dont know" or missing values

refused_or_dunno <- function(x) {
  x %in% c(7777, 9999)
}

pa_vigorous <- pa_raw |> 
  select(SEQN,PAD810Q, PAD810U, PAD820) |> 
  drop_na() |> 
  filter_out(
    if_any(starts_with("PAD"), refused_or_dunno)
  )

#Transforming observation data to minutes_per_week of vigorous activity

pa_vigorous_weekly <- pa_vigorous |> 
  group_by(SEQN) |> 
  summarize(vigorous_weekly_minutes = case_when(
    PAD810U == "D" ~ PAD820 * 7,
    PAD810U == "M" ~ PAD820 / 4,
    PAD810U == "Y" ~ PAD820 / 52,
    PAD810U == "W" ~ PAD820)
    )
#Calculating weekly MET-minutes

pa_vigorous_met <- pa_vigorous_weekly |> 
  mutate(vigorous_met = vigorous_weekly_minutes * 7)

#Joining moderate and vigorous weekly minutes

pa_moderate_vigorous_weekly <- full_join(pa_moderate_met, pa_vigorous_met, by = "SEQN")

#Summing MET

pa_moderate_vigorous_met <- pa_moderate_vigorous_weekly |> 
  mutate(weekly_MET = rowSums(pick(moderate_met, vigorous_met), na.rm = TRUE))


#Summing moderate (* 1) and vigorous (* 2) for WHO guideline equivalency

pa_moderate_vigorous_met_who <-pa_moderate_vigorous_met |> 
  group_by(SEQN) |> 
  mutate(who_guideline_total = coalesce(moderate_weekly_minutes, 0) + coalesce(vigorous_weekly_minutes, 0) * 2)

#WHO guideline (150 moderate minutes or 75 vigorous minutes or equivalent; yes or no

pa_moderate_vigorous_met_who_cat <- pa_moderate_vigorous_met_who |> 
  mutate(who_guideline = if_else(who_guideline_total >= 150, 1, 0))

view(pa_moderate_vigorous_met_who_cat)
  

