# Librerie
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)

#caricamento dataset reale
real_data <- read.csv("Migraine_onevsrest_3.csv")

dim(real_data)
colnames(real_data)
summary(real_data)

# Distribuzione target reale
table(real_data$target)
prop.table(table(real_data$target))

#schema dataset reale
schema_real <- data.frame(
  Variabile = colnames(real_data),
  Tipo = sapply(real_data, class),
  Minimo = sapply(real_data, min),
  Massimo = sapply(real_data, max),
  Valori_unici = sapply(real_data, function(x) length(unique(x)))
)

schema_real

write.csv(schema_real,
          "schema_real_migraine.csv",
          row.names = FALSE)

#campione reale per prompt llm
set.seed(123)

sample_real <- real_data %>%
  sample_n(20)

write.csv(sample_real,
          "sample_real_migraine.csv",
          row.names = FALSE)

#caricamento dataset con dati sintetici
synthetic_data <- read.csv("synthetic_migraine.csv")

# Controllo dimensioni nuovo dataset
dim(synthetic_data)

# Controllo colonne nuovo dataset
colnames(synthetic_data)

# Statistiche descrittive nuovo dataset
summary(synthetic_data)
# =========================
# 6. CONFRONTO STATISTICHE DESCRITTIVE
# =========================

comparison_stats <- data.frame(
  Variabile = colnames(real_data),
  Media_Reale = sapply(real_data, mean),
  Media_Sintetica = sapply(synthetic_data, mean),
  SD_Reale = sapply(real_data, sd),
  SD_Sintetica = sapply(synthetic_data, sd),
  Min_Reale = sapply(real_data, min),
  Min_Sintetico = sapply(synthetic_data, min),
  Max_Reale = sapply(real_data, max),
  Max_Sintetico = sapply(synthetic_data, max)
)

comparison_stats
# =========================
# 7. CONFRONTO VALORI UNICI
# =========================

unique_comparison <- data.frame(
  Variabile = colnames(real_data),
  Valori_Unici_Reale = sapply(real_data, function(x) length(unique(x))),
  Valori_Unici_Sintetico = sapply(synthetic_data, function(x) length(unique(x)))
)

unique_comparison
# =========================
# 8. CONFRONTO DISTRIBUZIONE AGE
# =========================

real_age <- real_data %>%
  select(Age) %>%
  mutate(type = "Reale")

synthetic_age <- synthetic_data %>%
  select(Age) %>%
  mutate(type = "Sintetico")

combined_age <- rbind(real_age, synthetic_age)

ggplot(combined_age,
       aes(x = Age,
           fill = type)) +
  geom_histogram(alpha = 0.6,
                 bins = 15,
                 position = "identity") +
  labs(
    title = "Confronto distribuzione Age",
    x = "Age",
    y = "Frequenza"
  ) +
  theme_minimal()
#matrice di correlazione dataset sintetico
cor_matrix <- cor(synthetic_data)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.7,
         number.cex = 0.5)
# =========================
# DISTRIBUZIONE NORMALE - AGE
# =========================

ggplot(real_data, aes(x = Age)) +
  geom_histogram(aes(y = ..density..),
                 bins = 15,
                 fill = "skyblue",
                 color = "black") +
  stat_function(fun = dnorm,
                args = list(mean = mean(real_data$Age),
                            sd = sd(real_data$Age)),
                color = "red",
                linewidth = 1) +
  labs(
    title = "Distribuzione Age con curva Normale",
    x = "Age",
    y = "Densità"
  ) +
  theme_minimal()
# =========================
# DISTRIBUZIONE POISSON - FREQUENCY
# =========================

lambda <- mean(real_data$Frequency)

ggplot(real_data,
       aes(x = Frequency)) +
  geom_bar(fill = "lightgreen",
           color = "black") +
  stat_function(
    fun = function(x) dpois(x, lambda = lambda) * nrow(real_data),
    color = "red"
  ) +
  labs(
    title = "Distribuzione Frequency e Poisson",
    x = "Frequency",
    y = "Frequenza"
  ) +
  theme_minimal()
# =========================
# DISTRIBUZIONE BERNOULLI - NAUSEA
# =========================

table(real_data$Nausea)

prop.table(table(real_data$Nausea))
# =========================
# 11. REGRESSIONE LOGISTICA
# SU DATASET SINTETICO
# =========================

library(caret)

# Conversione target in factor
synthetic_data$target <- as.factor(synthetic_data$target)

# Train/Test split
set.seed(123)

trainIndex <- createDataPartition(
  synthetic_data$target,
  p = 0.8,
  list = FALSE
)

train_data <- synthetic_data[trainIndex, ]
test_data  <- synthetic_data[-trainIndex, ]

# Separazione feature e target
train_x <- train_data[, colnames(train_data) != "target"]
test_x  <- test_data[, colnames(test_data) != "target"]

train_y <- train_data$target
test_y  <- test_data$target

# Scaling
preProcValues <- preProcess(
  train_x,
  method = c("center", "scale")
)

train_x_scaled <- predict(preProcValues, train_x)
test_x_scaled  <- predict(preProcValues, test_x)

# Ricostruzione dataset train e test
train_scaled <- cbind(train_x_scaled,
                      target = train_y)

test_scaled <- cbind(test_x_scaled,
                     target = test_y)

# =========================
# MODELLO LOGISTICO
# =========================

log_model <- glm(
  target ~ .,
  data = train_scaled,
  family = binomial
)

summary(log_model)

# =========================
# PREDIZIONI
# =========================

pred_probs <- predict(
  log_model,
  test_scaled,
  type = "response"
)

pred_class <- ifelse(pred_probs > 0.5,
                     1,
                     0)

pred_class <- as.factor(pred_class)

# =========================
# CONFUSION MATRIX
# =========================

conf_matrix <- confusionMatrix(
  pred_class,
  test_scaled$target,
  positive = "1"
)

conf_matrix
# =========================
# METRICHE
# =========================

accuracy <- conf_matrix$overall["Accuracy"]

precision <- conf_matrix$byClass["Pos Pred Value"]

recall <- conf_matrix$byClass["Sensitivity"]

f1_score <- 2 * (
  (precision * recall) /
    (precision + recall)
)

accuracy
precision
recall
f1_score