#Diabetes

#Loading raw data

diabetes_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DIQ_L.xpt")

#selecting "Doctor told you have diabetes", "Ever told you have prediabetes", "Taking insulin now", "Take diabetic pills to lower blood sugar"

diabetes_selected <- diabetes_raw |> 
  select(SEQN, DIQ010, DIQ160, DIQ050, DIQ070)

#Setting "refused" and "dont know" to NA

diabetes_refused_or_dunno <- c(7, 9)

diabetes_not_missing <- diabetes_selected |> 
  mutate(across(everything(), ~ replace_values(.x, diabetes_refused_or_dunno ~ NA)))

#Removing rows with all NA

diabetes_no_all_na <- diabetes_not_missing |> 
  filter_out(if_all(everything(), ~. == NA))


#Changing "No" answer from "2" to "0"

diabetes_clean <- diabetes_not_missing |> 
  mutate(across(everything(), ~ replace_values(.x, 2 ~ 0)))

#Categorizing diabetics (yes = 1, no = 0) based on if they answered YES to any of the questions

diabetes_cat <- diabetes_clean |> 
  mutate(Diabetic = if_else(
    if_any(everything(), ~ . == 1), 1, 0, missing = 0)) |> 
  group_by(SEQN) |> 
  summarize(Diabetic)

#Proportion of diabetics among the whole NHANES 2021-2023 sample

prop_diabetic <- diabetes_cat |> 
  summarize(mean(Diabetic))

view(diabetes_cat)
