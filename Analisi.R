# Librerie
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(psych)

# Caricamento dataset
dataset <- read.csv("Migraine_onevsrest_3.csv")

# Prime righe
head(dataset)

# Dimensioni dataset
dim(dataset)

# Nomi colonne
colnames(dataset)

# Struttura dataset
str(dataset)

# Statistiche descrittive
summary(dataset)

# Controllo valori mancanti
colSums(is.na(dataset))

# Distribuzione target
table(dataset$target)

# Percentuali target
prop.table(table(dataset$target))
# Grafico distribuzione target
ggplot(dataset, aes(x = as.factor(target))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribuzione della variabile target",
    x = "Classe",
    y = "Frequenza"
  ) +
  theme_minimal()
# Matrice di correlazione
cor_matrix <- cor(dataset)

# Visualizzazione grafica heatmap (matrice di correlazione)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.7,
         number.cex = 0.5)
# Numero valori unici per feature (ho verificato se Ataxia avesse un unico valore)
sapply(dataset, function(x) length(unique(x)))
# Rimozione feature costante
dataset <- dataset %>%
  select(-Ataxia)
# Nuova matrice di correlazione senza la feature Ataxia
cor_matrix <- cor(dataset)

# Nuova heatmap (matrice di correlazione)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.7)
# Boxplot delle feature
boxplot(dataset[,1:(ncol(dataset)-1)],
        las = 2,
        main = "Boxplot delle feature",
        cex.axis = 0.7)