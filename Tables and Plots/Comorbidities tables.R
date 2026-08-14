#Comorbidities

# diabetes, hypertension, dyslipidemia, asthma, arthritis, chf, chd, angina, heart_attack, stroke, thyroid, copd, liver_disease, cancer


nhanes_project_2 |> 
  summarize(diabetes = sum(diabetes == 1, na.rm = TRUE),
            hypertension = sum(hypertension == 1, na.rm = TRUE),
            dyslipidemia = sum(dyslipidemia == 1, na.rm = TRUE),
            asthma = sum(asthma == 1, na.rm = TRUE),
            arthritis = sum(arthritis == 1, na.rm = TRUE),
            chf = sum(chf == 1, na.rm = TRUE),
            chd = sum(chd == 1, na.rm = TRUE),
            angina = sum(angina == 1, na.rm = TRUE),
            heart_attack = sum(heart_attack == 1, na.rm = TRUE),
            stroke = sum(stroke == 1, na.rm = TRUE),
            thyroid = sum(thyroid == 1, na.rm = TRUE),
            copd = sum(copd == 1, na.rm = TRUE),
            liver_disease = sum(liver_disease == 1, na.rm = TRUE),
            cancer = sum(cancer == 1, na.rm = TRUE),
  ) |> 
  gt() |> 
  tab_header(
    title = md("Comorbidities")
  ) |> 
  cols_label(
    diabetes = md("Diabetes"),
    hypertension = md("Hypertension"),
    dyslipidemia = md("Dyslipidemia"),
    asthma = md("Asthma"),
    arthritis = md("Arthritis"),
    chf = md("Congestive <br>Heart <br>Failure"),
    chd = md("Coronary <br>Heart <br>Disease"),
    angina = md("Angina"),
    heart_attack = md("Heart <br>Attack"),
    stroke = md("Stroke"),
    thyroid = md("Thyroid <br>Disease"),
    copd = md("COPD"),
    liver_disease = md("Liver <br>Disease"),
    cancer = md("Cancer"),
  )