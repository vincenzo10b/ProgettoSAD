library(dplyr)

#Trasformo i kilogrammi in tonnellate e modifico il nome nella tabella
df_mutate <- df %>%
  mutate(Value = ifelse(Unit == 'Kilograms', Value / 1000, Value))

df_mutate <- df_mutate %>%
  mutate(Unit = ifelse(Unit == 'Kilograms', "Tonnes", Unit))

View(df_mutate)