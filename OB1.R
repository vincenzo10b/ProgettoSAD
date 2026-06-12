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

# Visualizzazione grafica matrice di correlazione
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.7,
         number.cex = 0.5)
# Numero valori unici per feature (ho verificato se Ataxia avesse un unico valore)
sapply(dataset, function(x) length(unique(x)))
# Rimozione feature costante Ataxia
dataset <- dataset %>%
  select(-Ataxia)
# Nuova matrice di correlazione senza la feature Ataxia
cor_matrix <- cor(dataset)

#Visualizzazione grafica nuova matrice di correlazione
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.7)
# Boxplot delle feature
boxplot(dataset[,1:(ncol(dataset)-1)],
        las = 2,
        main = "Boxplot delle feature",
        cex.axis = 0.7)
# Istogramma della feature Age
ggplot(dataset, aes(x = Age)) +
  geom_histogram(bins = 15,
                 fill = "yellow",
                 color = "black") +
  labs(
    title = "Distribuzione della feature Age",
    x = "Age",
    y = "Frequenza"
  ) +
  theme_minimal()

# Barplot feature Nausea
ggplot(dataset, aes(x = as.factor(Nausea))) +
  geom_bar(fill = "darkgreen") +
  labs(
    title = "Distribuzione della feature Nausea",
    x = "Nausea",
    y = "Frequenza"
  ) +
  theme_minimal()
# Boxplot Age vs Target
ggplot(dataset, aes(x = as.factor(target), y = Age)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Distribuzione dell'età rispetto alla target",
    x = "Target",
    y = "Age"
  ) +
  theme_minimal()
# PREPROCESSING PER IL MODELLO
# Il metodo factor converte la variabile target da numerica a categoriale
dataset$target <- as.factor(dataset$target)
# Train/Test split
set.seed(123)

trainIndex <- createDataPartition(dataset$target,
                                  p = 0.8,
                                  list = FALSE)

train_data <- dataset[trainIndex, ]
test_data  <- dataset[-trainIndex, ]

# Controllo dimensioni
dim(train_data)
dim(test_data)

# Controllo distribuzione target nei due set
table(train_data$target)
table(test_data$target)

# Controllo variabilità Diplopia
table(train_data$Diplopia)
table(test_data$Diplopia)

# Rimozione Diplopia da train e test perché nel training set è costante
train_data <- train_data %>%
  select(-Diplopia)

test_data <- test_data %>%
  select(-Diplopia)

# Separazione feature e target
train_x <- train_data[, colnames(train_data) != "target"]
test_x  <- test_data[, colnames(test_data) != "target"]

train_y <- train_data$target
test_y  <- test_data$target

# Scaling delle feature
preProcValues <- preProcess(train_x,
                            method = c("center", "scale"))

train_x_scaled <- predict(preProcValues, train_x)
test_x_scaled  <- predict(preProcValues, test_x)
# Dataset finali scalati
train_scaled <- cbind(train_x_scaled,
                      target = train_y)

test_scaled <- cbind(test_x_scaled,
                     target = test_y)
# Modello logistico
log_model <- glm(target ~ .,
                 data = train_scaled,
                 family = binomial)
summary(log_model)

dim(train_scaled)

table(train_scaled$target)

summary(train_scaled$target)

str(train_scaled)
# Probabilità predette
pred_probs <- predict(log_model,
                      test_scaled,
                      type = "response")

# Conversione classi
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

pred_class <- as.factor(pred_class)

# Confusion matrix
confusionMatrix(pred_class,
                test_scaled$target)
# Confusion matrix salvata
cm <- confusionMatrix(
  pred_class,
  test_scaled$target,
  positive = "1"
)

cm

# Metriche principali
accuracy <- cm$overall["Accuracy"]
precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]
f1_score <- 2 * ((precision * recall) / (precision + recall))

accuracy
precision
recall
f1_score

