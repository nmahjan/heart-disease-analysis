# DAT 301 Project 1: Heart Disease Prediction and Risk Factor Analysis
# Author: Neil Mahajan

#Required Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(caret)
library(pROC)
library(corrplot)
library(RColorBrewer)
library(scales)

# Import Data
# Download dataset from: https://archive.ics.uci.edu/ml/datasets/heart+disease
# Or use: https://www.kaggle.com/datasets/johnsmith88/heart-disease-dataset

heart_data <- read.csv("heart.csv")

# View structure
str(heart_data)
head(heart_data)
summary(heart_data)

# Data Dimensions
cat("Dataset dimensions:", dim(heart_data)[1], "rows and", 
    dim(heart_data)[2], "columns\n")

# Check for Missing Values
missing_counts <- colSums(is.na(heart_data))
cat("Missing values per column:\n")
print(missing_counts)

missing_pct <- (colSums(is.na(heart_data)) / nrow(heart_data)) * 100
cat("\nPercentage of missing values:\n")
print(round(missing_pct, 2))

# Data Cleaning and Transformation
heart_clean <- heart_data

# Convert categorical variables to factors with meaningful labels
heart_clean$sex <- factor(heart_clean$sex, 
                          levels = c(0, 1), 
                          labels = c("Female", "Male"))

heart_clean$cp <- factor(heart_clean$cp,
                         levels = c(0, 1, 2, 3),
                         labels = c("Typical Angina", "Atypical Angina", 
                                    "Non-anginal", "Asymptomatic"))

heart_clean$fbs <- factor(heart_clean$fbs,
                          levels = c(0, 1),
                          labels = c("<=120 mg/dl", ">120 mg/dl"))

heart_clean$restecg <- factor(heart_clean$restecg,
                              levels = c(0, 1, 2),
                              labels = c("Normal", "ST-T Abnormality", "LV Hypertrophy"))

heart_clean$exang <- factor(heart_clean$exang,
                            levels = c(0, 1),
                            labels = c("No", "Yes"))

heart_clean$slope <- factor(heart_clean$slope,
                            levels = c(0, 1, 2),
                            labels = c("Upsloping", "Flat", "Downsloping"))

heart_clean$thal <- factor(heart_clean$thal,
                           levels = c(1, 2, 3),
                           labels = c("Normal", "Fixed Defect", "Reversible Defect"))

heart_clean$target <- factor(heart_clean$target,
                             levels = c(0, 1),
                             labels = c("No Disease", "Disease"))

# Feature Engineering
# Create age groups
heart_clean$age_group <- cut(heart_clean$age,
                             breaks = c(0, 40, 50, 60, 70, 100),
                             labels = c("<40", "40-49", "50-59", "60-69", "70+"),
                             right = FALSE)

# Create cholesterol categories
heart_clean$chol_category <- cut(heart_clean$chol,
                                 breaks = c(0, 200, 240, Inf),
                                 labels = c("Desirable", "Borderline High", "High"),
                                 right = FALSE)

# Create blood pressure categories
heart_clean$bp_category <- cut(heart_clean$trestbps,
                               breaks = c(0, 120, 140, Inf),
                               labels = c("Normal", "Elevated", "High"),
                               right = FALSE)

# Exploratory Data Analysis

# 1. Disease Prevalence
disease_counts <- table(heart_clean$target)
disease_prop <- prop.table(disease_counts) * 100

ggplot(heart_clean, aes(x = target, fill = target)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(title = "Distribution of Heart Disease Cases",
       x = "Diagnosis",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

cat("\nDisease Prevalence:\n")
print(disease_prop)

# 2. Age Analysis
p1 <- ggplot(heart_clean, aes(x = age, fill = target)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(title = "Age Distribution by Disease Status",
       x = "Age (years)",
       y = "Count",
       fill = "Diagnosis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(heart_clean, aes(x = target, y = age, fill = target)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(title = "Age by Disease Status",
       x = "Diagnosis",
       y = "Age (years)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

grid.arrange(p1, p2, ncol = 2)

# Statistical test for age difference
age_test <- t.test(age ~ target, data = heart_clean)
cat("\nT-test for age difference:\n")
cat("p-value:", age_test$p.value, "\n")

# 3. Gender Analysis
gender_disease <- table(heart_clean$sex, heart_clean$target)
gender_prop <- prop.table(gender_disease, margin = 1) * 100

ggplot(heart_clean, aes(x = sex, fill = target)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Heart Disease Prevalence by Gender",
       x = "Gender",
       y = "Percentage",
       fill = "Diagnosis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

cat("\nDisease prevalence by gender (%):\n")
print(round(gender_prop, 1))

gender_chisq <- chisq.test(gender_disease)
cat("\nChi-square test p-value:", gender_chisq$p.value, "\n")

# 4. Chest Pain Type Analysis
cp_table <- table(heart_clean$cp, heart_clean$target)

ggplot(heart_clean, aes(x = cp, fill = target)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(title = "Chest Pain Type and Heart Disease",
       x = "Chest Pain Type",
       y = "Count",
       fill = "Diagnosis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

cat("\nCross-tabulation of Chest Pain Type and Disease:\n")
print(cp_table)

# 5. Cardiovascular Risk Factors
p1 <- ggplot(heart_clean, aes(x = target, y = chol, fill = target)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(title = "Cholesterol Levels",
       x = "Diagnosis",
       y = "Cholesterol (mg/dl)") +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(heart_clean, aes(x = target, y = trestbps, fill = target)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(title = "Resting Blood Pressure",
       x = "Diagnosis",
       y = "Blood Pressure (mm Hg)") +
  theme_minimal() +
  theme(legend.position = "none")

p3 <- ggplot(heart_clean, aes(x = target, y = thalach, fill = target)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(title = "Maximum Heart Rate",
       x = "Diagnosis",
       y = "Heart Rate (bpm)") +
  theme_minimal() +
  theme(legend.position = "none")

p4 <- ggplot(heart_clean, aes(x = target, y = oldpeak, fill = target)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(title = "ST Depression",
       x = "Diagnosis",
       y = "Oldpeak") +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(p1, p2, p3, p4, ncol = 2)

# 6. Correlation Analysis
numeric_vars <- heart_data %>%
  select(age, trestbps, chol, thalach, oldpeak, ca) %>%
  na.omit()

cor_matrix <- cor(numeric_vars)

corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Numeric Variables",
         mar = c(0, 0, 2, 0))

# Statistical Modeling 

# Prepare data for modeling
model_data <- heart_data
model_data$target <- as.factor(model_data$target)

# Split data (80/20)
set.seed(123)
train_index <- createDataPartition(model_data$target, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("\nTraining set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")

# Build Logistic Regression Model
logit_model <- glm(target ~ age + sex + cp + trestbps + chol + fbs + 
                     restecg + thalach + exang + oldpeak + slope + ca + thal,
                   data = train_data,
                   family = binomial(link = "logit"))

# Model summary
summary(logit_model)

# Extract significant predictors
coef_summary <- summary(logit_model)$coefficients
significant_vars <- coef_summary[coef_summary[, 4] < 0.05, ]

cat("\nStatistically Significant Predictors (p < 0.05):\n")
print(significant_vars)

# Odds ratios
odds_ratios <- exp(coef(logit_model))
cat("\nOdds Ratios:\n")
print(round(odds_ratios, 3))

# Model Evaluation

# Predictions
predictions <- predict(logit_model, test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- confusionMatrix(as.factor(predicted_class), 
                               test_data$target,
                               positive = "1")

cat("\nConfusion Matrix:\n")
print(conf_matrix$table)

cat("\nModel Performance Metrics:\n")
print(conf_matrix$overall)
print(conf_matrix$byClass)

# ROC Curve
roc_obj <- roc(test_data$target, predictions)
plot(roc_obj, main = "ROC Curve - Logistic Regression Model",
     col = "#e74c3c", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 3)),
       col = "#e74c3c", lwd = 2)

cat("\nAUC (Area Under the Curve):", auc(roc_obj), "\n")

# Feature Importance
varImp_values <- varImp(logit_model)
varImp_df <- data.frame(
  Variable = rownames(varImp_values),
  Importance = varImp_values$Overall
)
varImp_df <- varImp_df[order(-varImp_df$Importance), ]

# Plot top 10 features
top_features <- head(varImp_df, 10)

ggplot(top_features, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#3498db") +
  coord_flip() +
  labs(title = "Top 10 Most Important Predictors",
       x = "Variable",
       y = "Importance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# End of Analysis
cat("\n=== Analysis Complete ===\n")