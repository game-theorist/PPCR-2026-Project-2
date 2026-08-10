#Demographics

#Loading raw data

demo_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DEMO_L.xpt")

demo_selected <- demo_raw |> 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, WTINT2YR, INDFMPIR)

#Renaming | Male = 1, Female = 0

demo_names <- demo_selected |>
  group_by(SEQN) |> 
  summarize(Gender = if_else(RIAGENDR == 1, 1, 0),
         Age = RIDAGEYR,
         Race = RIDRETH3,
         Interview_Weight = WTINT2YR,
         Ratio_income_poverty = INDFMPIR 
         )
         


View(demo_clean)