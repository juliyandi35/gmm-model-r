install.packages("readxl")
install.packages("DescTools")
install.packages("RODBC")
install.packages("DBI")
install.packages("writexl")
install.packages("car")     # For heteroskedasticity test (Breusch-Pagan)
install.packages("vars")    # For cross-sectional dependence
install.packages("plm")
install.packages("gmm")
install.packages("zoo")

library(DescTools)
library(RODBC)
library(DBI)
library(readxl)
library(zoo)
library(car)

rm(list = ls())  # Menghapus semua variabel dalam environment

Data_Thesis_1 <- read_excel("Data Thesis age statis.xlsx", sheet = "Sheet1", na = c("", "NA")) 

head(Data_Thesis_1)
summary(Data_Thesis_1)
sapply(Data_Thesis_1[, sapply(Data_Thesis_1, is.numeric)], sd, na.rm = TRUE) #Std. Deviation
View(Data_Thesis_1)
Data_Thesis_1$Quarter <- as.Date(Data_Thesis_1$Quarter,format="%d/%m/%Y")

# Load the Excel file
data <- Data_Thesis_1
View(data)

data <- data[order(data$Ticker),] 

library(writexl)  # Load the package

# Custom Winsorization function
winsorize_custom <- function(x, lower_prob = 0.01, upper_prob = 0.99) {
  lower_bound <- quantile(x, probs = lower_prob, na.rm = TRUE)
  upper_bound <- quantile(x, probs = upper_prob, na.rm = TRUE)
  
  # Replace values below the lower bound and above the upper bound
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  
  return(x)
}

# Apply winsorization only to numeric columns
winsorized_data <- data.frame(lapply(data, function(x) {
  if (is.numeric(x)) {
    return(winsorize_custom(x))  # Apply custom winsorization
  } else {
    return(x)  # Non-numeric columns stay the same
  }
}))


# Save the winsorized data to an Excel file
write_xlsx(winsorized_data, "winsorized_data.xlsx")

#View(winsorized_data)

cat("File Excel telah berhasil ditulis di direktori kerja saat ini.\n")
getwd()

################Descriptive Statistics
# Step 1: Identify numeric columns in your dataset
numeric_columns <- sapply(winsorized_data, is.numeric)

# Step 2: Apply statistical functions only to numeric columns
means <- sapply(winsorized_data[, numeric_columns], mean, na.rm = TRUE)
sds <- sapply(winsorized_data[, numeric_columns], sd, na.rm = TRUE)
medians <- sapply(winsorized_data[, numeric_columns], median, na.rm = TRUE)
mins <- sapply(winsorized_data[, numeric_columns], min, na.rm = TRUE)
maxs <- sapply(winsorized_data[, numeric_columns], max, na.rm = TRUE)
counts <- sapply(winsorized_data[, numeric_columns], function(x) sum(!is.na(x)))

# Step 3: Display the results
descriptive_stats <- data.frame(Means = means, SDs = sds, Medians = medians, Mins = mins, Maxs = maxs,Counts = counts)
print(descriptive_stats)

# Step by step Panel Regression Setup
# Disable scientific notation for numbers
options(scipen = 999)

# Load necessary libraries
library(readxl)
library(plm)
library(lmtest)
library(dplyr)

# Load your dataset (assuming it's already winsorized and named winsorized_data)
dataset_panel <- winsorized_data
View(dataset_panel)
# Convert to a pdata.frame using 'Ticker' as the cross-sectional identifier and 'TimeIndex' as the time variable
data.panel <- pdata.frame(dataset_panel, index = c("Ticker", "Quarter"))

# View the data panel structure (optional, for checking)
View(data.panel)

#------------Panel Selection -------
#ROA
# Uji Chow untuk membandingkan CEM dan FEM
library(plm)
plm_model_femROA <- plm(ROA ~ AGE + CR + LAB + CI + HHI + IR + GGDP, data = data.panel, model = "within")
plm_model_cemROA <- plm(ROA ~ AGE + CR + LAB + CI + HHI + IR + GGDP, data = data.panel, model = "pooling")
pFtest(plm_model_femROA, plm_model_cemROA)

# Uji Hausman untuk membandingkan FEM dan REM
plm_model_remROA <- plm(ROA ~ AGE + CR + LAB + CI + HHI + IR + GGDP, data = data.panel, model = "random")
phtest(plm_model_femROA, plm_model_remROA)

# Uji Breusch-Pagan untuk memilih antara CEM dan REM
plmtest(plm_model_cemROA, type = "bp")

#ROE
# Uji Chow untuk membandingkan CEM dan FEM
library(plm)
plm_model_femROE <- plm(ROE ~ AGE + CR + LAB + CI + HHI + IR + GGDP, data = data.panel, model = "within")
plm_model_cemROE <- plm(ROE ~ AGE + CR + LAB + CI + HHI + IR + GGDP, data = data.panel, model = "pooling")
pFtest(plm_model_femROE, plm_model_cemROE)

# Uji Hausman untuk membandingkan FEM dan REM
plm_model_remROE <- plm(ROE ~ AGE + CR + LAB + CI + HHI + IR + GGDP, data = data.panel, model = "random")
phtest(plm_model_femROE, plm_model_remROE)

# Uji Breusch-Pagan untuk memilih antara CEM dan REM
plmtest(plm_model_cemROE, type = "bp")

#####---------------------Data Cleansing----------------

# Menghapus missing values jika ada
data.panel <- na.omit(data.panel)
summary(data.panel)
View(data.panel)
table(data.panel$Ticker)

data.panel <- data.panel %>%
  group_by(Ticker) %>%
  filter(n() >= 5) %>%
  ungroup()

table(data.panel$Ticker)

View(data.panel)

# Step 1: Identify numeric columns
numeric_columns <- sapply(data.panel, is.numeric)
numeric_columns_names <- names(data.panel)[numeric_columns]
var(data.panel$ROE/10)
var(data.panel$ROA)
var(data.panel$AGE/10)
var(data.panel$CR)
var(data.panel$LAB)
var(data.panel$CI/10)
var(data.panel$HHI*10)
var(data.panel$IR)
var(data.panel$GGDP)

data.panel$ROE <- data.panel$ROE/10
data.panel$AGE <- data.panel$AGE/10
data.panel$CI <- data.panel$CI/10
data.panel$HHI <- data.panel$HHI*10

# Step 2: Apply statistical functions only to numeric columns
means2 <- sapply(data.panel[, numeric_columns_names], mean, na.rm = TRUE)
sds2 <- sapply(data.panel[, numeric_columns_names], sd, na.rm = TRUE)
medians2 <- sapply(data.panel[, numeric_columns_names], median, na.rm = TRUE)
mins2 <- sapply(data.panel[, numeric_columns_names], min, na.rm = TRUE)
maxs2 <- sapply(data.panel[, numeric_columns_names], max, na.rm = TRUE)
count2 <- sapply(data.panel[, numeric_columns_names], function(x) sum(!is.na(x)))

# Step 3: Display the results
descriptive_stats2 <- data.frame(
  Variable = numeric_columns_names,
  Means = means2,
  SDs = sds2,
  Medians = medians2,
  Mins = mins2,
  Maxs = maxs2,
  Count = count2
)

# Print the descriptive statistics
print(descriptive_stats2)

# Menghitung VIF untuk memeriksa multikolinearitas
cor(data.panel[, c("ROA", "AGE", "CR", "LAB","CI", "HHI", "IR", "GGDP" )], use = "complete.obs")

library(car)
vif_data <- lm(ROA ~ AGE + CR + LAB + CI + HHI + IR + GGDP, data = data.panel)
vif(vif_data)

dwtest(vif_data)

#-------------------------------------ROA--------------------------------------
#------SYSTEM GMM----------
#Uji autocorrelation?

gmm_model_sysROA <- pgmm(formula = ROA ~ lag(ROA) + CR + LAB + CI + HHI + IR + GGDP | lag(ROA,2:15),  
                         data = data.panel,
                         effect = "individual",  # Efek individual
                         model = "twostep",      # Menggunakan two-step GMM sesuai rekomendasi
                         transformation = "ld"    # system Difference GMM
)
fitted_sys <- t(gmm_model_sysROA$fitted.values)
fitted_sys <- data.frame(fitted_sys)
colnames(fitted_sys)
fitted_sys$ID <- rownames(fitted_sys)

# Reshape the data into the desired format
library(tidyr)
library(dplyr)
panel_fitted_sys <- fitted_sys %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Tanggal",
               values_to = "ROA_Fitted") %>%
  mutate(Tanggal = as.character(gsub("X", "",Tanggal)))
panel_fitted_sys
panel_true_sys <- data.frame(Ticker = data.panel$Ticker,
                             Tanggal = data.panel$Quarter,
                             ROA = data.panel$ROA)
nrow(panel_true_sys)
nrow(panel_fitted_sys)

var_resid_sys = var(panel_fitted_sys$ROA_Fitted)
var_y_sys = var(panel_true_sys$ROA)

R_Squared_sys = 1 - (var_resid_sys/var_y_sys)
R_Squared_sys*100

# Tampilkan ringkasan hasil estimasi
summary(gmm_model_sysROA)

summary(gmm_model_sysROA)$sargan
summary(gmm_model_sysROA)$hansen

#------------------Difference GMM---------------------
gmm_model_diffROA <- pgmm(formula = ROA ~ lag(ROA) + CR + LAB + CI + HHI + IR + GGDP | lag(ROA,2:15),  
                          data = data.panel,
                          effect = "individual",  # Efek individual
                          model = "twostep",      # Menggunakan two-step GMM sesuai rekomendasi
                          transformation = "d"    # Difference GMM
)
View(gmm_model_diffROA$residuals)

fitted_diff <- t(gmm_model_diffROA$fitted.values)
fitted_diff <- data.frame(fitted_diff)
colnames(fitted_diff)
fitted_diff$ID <- rownames(fitted_diff)

panel_fitted_diff <- fitted_diff %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Tanggal",
               values_to = "ROA_Fitted") %>%
  mutate(Tanggal = as.character(gsub("X", "",Tanggal)))

panel_true_diff <- data.frame(Ticker = data.panel$Ticker,
                             Tanggal = data.panel$Quarter,
                             ROA = data.panel$ROA)

var_resid_diff = var(panel_fitted_diff$ROA_Fitted)
var_y_diff = var(panel_true_diff$ROA)

R_Squared_diff = 1 - (var_resid_diff/var_y_diff)
R_Squared_diff*100

# Tampilkan ringkasan hasil estimasi
summary(gmm_model_diffROA)


#------------ UJI BIAS ------------
# Model Common Effect (CEM)
cem_modelROA <- plm(ROA ~ lag(ROA, 1) + CR + HHI + CI + IR + GGDP + LAB, data = data.panel, model = "pooling")
cem_summaryROA <- summary(cem_modelROA)
print(cem_summaryROA)

# Model Fixed Effect (FEM)
fem_modelROA <- plm(ROA ~ lag(ROA, 1) + CR + HHI + CI + IR + GGDP + LAB, data = data.panel, model = "within")
fem_summaryROA <- summary(fem_modelROA)
print(fem_summaryROA)




#-------------------------------------ROE--------------------------------------

#------SYSTEM GMM----------
gmm_model_sysROE <- pgmm(formula = ROE ~ lag(ROE) + CR + LAB + CI + HHI + IR + GGDP| lag(ROE,2:15),
                         data = data.panel,
                         effect = "individual",  # Efek individual
                         model = "twostep",      # Menggunakan two-step GMM sesuai rekomendasi
                         transformation = "ld"    # system Difference GMM
)
# Tampilkan ringkasan hasil estimasi
summary(gmm_model_sysROE)

#------------------Difference GMM---------------------
gmm_model_diffROE <- pgmm(formula = ROE ~ lag(ROE, 1) + AGE + CR + LAB + CI + HHI + IR + GGDP | lag(ROE,2:15), 
                          data = data.panel,
                          effect = "individual",  # Efek individual
                          model = "twostep",      # Menggunakan two-step GMM sesuai rekomendasi
                          transformation = "d"    # Difference GMM
)

# Tampilkan ringkasan hasil estimasi
summary(gmm_model_diffROE)

#---------- Model CEM FEM ----------
# Model Common Effect (CEM)
cem_modelROE <- plm(ROE ~ lag(ROE, 1) + AGE + CR + HHI + CI + IR + GGDP + LAB, data = data.panel, model = "pooling")
cem_summaryROE <- summary(cem_modelROE)
print(cem_summaryROE)

# Model Fixed Effect (FEM)
fem_modelROE <- plm(ROE ~ lag(ROE, 1) + AGE + CR + HHI + CI + IR + GGDP + LAB, data = data.panel, model = "within")
fem_summaryROE <- summary(fem_modelROE)
print(fem_summaryROE)


#----RANDOM FOREST
# Load necessary libraries
library(readxl)
library(caret)
library(randomForest)
library(dplyr)

# Load the dataset
Data_Thesis_1 <- read_excel("Data/Data Thesis age statis.xlsx", sheet = "Sheet1", na = c("", "NA"))

# Winsorize the data (assumes custom winsorize function is already defined)
winsorized_data <- data.frame(lapply(Data_Thesis_1, function(x) {
  if (is.numeric(x)) {
    return(winsorize_custom(x))  # Apply custom winsorization
  } else {
    return(x)  # Non-numeric columns stay the same
  }
}))

# Convert data to panel format
data.panel <- pdata.frame(winsorized_data, index = c("Ticker", "Quarter"))

# Create lagged variables using dplyr::lag()
data.panel <- data.panel %>%
  group_by(Ticker) %>%
  mutate(
    lag_ROA_1 = lag(ROA, 1),
    lag_ROE_1 = lag(ROE, 1)
  ) %>%
  ungroup()

# Remove rows with NA values generated from lagging
data.panel <- na.omit(data.panel)

# Split the data into training and testing sets (80-20 split)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data.panel$ROA, p = 0.8, list = FALSE, times = 1)
trainData <- data.panel[trainIndex, ]
testData <- data.panel[-trainIndex, ]

#--------------------RANDOM FOREST ROA--------------------------
# Load required library
library(randomForest)

# Fit the Random Forest model
rf_model_ROA <- randomForest(ROA ~ lag_ROA_1 + AGE + CR + LAB + CI + HHI + IR + GGDP,
                             data = trainData,
                             ntree = 500,  # Number of trees
                             mtry = 3,     # Number of variables randomly sampled as candidates at each split
                             importance = TRUE)

# Print summary of the model
print(rf_model_ROA)

# Plot variable importance
varImpPlot(rf_model_ROA)

# Get variable importance as a data frame
var_importance <- as.data.frame(importance(rf_model_ROA))

# Rename columns for better clarity
colnames(var_importance) <- c("%IncMSE", "IncNodePurity")

# Print the variable importance table
print(var_importance)

# Test model performance on test set
predictions_ROA <- predict(rf_model_ROA, newdata = testData)
postResample(predictions_ROA, testData$ROA)


#--------------------RANDOM FOREST ROE--------------------------
# Fit the Random Forest model for ROE
rf_model_ROE <- randomForest(ROE ~ lag_ROE_1 + AGE + CR + LAB + CI + HHI + IR + GGDP,
                             data = trainData,
                             ntree = 500,  # Number of trees
                             mtry = 3,     # Number of variables randomly sampled as candidates at each split
                             importance = TRUE)

# Print summary of the model
print(rf_model_ROE)

# Plot variable importance
varImpPlot(rf_model_ROE)

# Get variable importance as a data frame
var_importance_ROE <- as.data.frame(importance(rf_model_ROE))

# Rename columns for better clarity
colnames(var_importance_ROE) <- c("%IncMSE", "IncNodePurity")

# Print the variable importance table
print(var_importance_ROE)

# Test model performance on test set for ROE
predictions_ROE <- predict(rf_model_ROE, newdata = testData)
postResample(predictions_ROE, testData$ROE)


#------------XGBOOST-------
# Load necessary libraries
library(xgboost)
library(Matrix)
library(caret)

# Split the data into training and testing sets (80-20 split)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data.panel$ROA, p = 0.8, list = FALSE, times = 1)
trainData <- data.panel[trainIndex, ]
testData <- data.panel[-trainIndex, ]

#---------------_XGBOOST ROA---------------------

# Create matrix format for XGBoost
train_matrix_ROA <- xgb.DMatrix(data = as.matrix(trainData[, c("lag_ROA_1", "AGE", "CR", "LAB", "CI","HHI", "IR", "GGDP")]),
                                label = trainData$ROA)
test_matrix_ROA <- xgb.DMatrix(data = as.matrix(testData[, c("lag_ROA_1", "AGE", "CR", "LAB", "CI","HHI", "IR", "GGDP")]),
                               label = testData$ROA)

# Set parameters for XGBoost model
params <- list(
  objective = "reg:squarederror",  # For regression task
  eval_metric = "rmse",            # Root Mean Squared Error as evaluation metric
  eta = 0.1,                       # Learning rate
  max_depth = 6,                   # Maximum depth of a tree
  subsample = 0.8,                 # Subsample ratio of the training data
  colsample_bytree = 0.8           # Subsample ratio of columns when constructing each tree
)

# Train the XGBoost model for ROA
xgb_model_ROA <- xgb.train(
  params = params,
  data = train_matrix_ROA,
  nrounds = 500,                   # Number of boosting rounds
  watchlist = list(train = train_matrix_ROA, test = test_matrix_ROA),
  print_every_n = 50,              # Print progress every 50 rounds
  early_stopping_rounds = 10       # Stop if no improvement after 10 rounds
)

# Variable Importance Plot
importance_matrix_ROA <- xgb.importance(feature_names = colnames(train_matrix_ROA), model = xgb_model_ROA)
xgb.plot.importance(importance_matrix_ROA)

# Test model performance on test set for ROA
predictions_ROA <- predict(xgb_model_ROA, newdata = test_matrix_ROA)
postResample(predictions_ROA, testData$ROA)


#----------------XGBOOST ROE----------------
# Repeat the process for ROE
train_matrix_ROE <- xgb.DMatrix(data = as.matrix(trainData[, c("lag_ROE_1", "AGE", "CR", "LAB", "CI","HHI", "IR", "GGDP")]),
                                label = trainData$ROE)
test_matrix_ROE <- xgb.DMatrix(data = as.matrix(testData[, c("lag_ROE_1", "AGE", "CR", "LAB", "CI","HHI", "IR", "GGDP")]),
                               label = testData$ROE)

# Train the XGBoost model for ROE
xgb_model_ROE <- xgb.train(
  params = params,
  data = train_matrix_ROE,
  nrounds = 500,
  watchlist = list(train = train_matrix_ROE, test = test_matrix_ROE),
  print_every_n = 50,
  early_stopping_rounds = 10
)

# Variable Importance Plot for ROE
importance_matrix_ROE <- xgb.importance(feature_names = colnames(train_matrix_ROE), model = xgb_model_ROE)
xgb.plot.importance(importance_matrix_ROE)

# Test model performance on test set for ROE
predictions_ROE <- predict(xgb_model_ROE, newdata = test_matrix_ROE)
postResample(predictions_ROE, testData$ROE)

