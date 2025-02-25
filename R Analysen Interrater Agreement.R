# Load necessary libraries
library(readxl)
library(irr)
library(dplyr)


# Define the file path and file name
file_path <- "/Users/ninahaffer/Downloads/R Analysen"
file_name <- "AMDP SNOMED CT.xlsx"
full_path <- file.path(file_path, file_name)

# Read the Excel file
data_SCT <- read_excel(full_path)
data_clean_SCT <- na.omit(data_SCT)

# Extract the relevant columns for interrater agreement
rater1_SCT <- data_clean_SCT[["Code(s)_M1_1"]]
rater2_SCT <- data_clean_SCT[["Code(s)_M2_1"]]

# Step 1: Prepare the relevant columns
data_relevant_SCT <- data_clean_SCT[, c("Code(s)_M1_1", "Code(s)_M2_1")]

# Step 2: Convert both columns to factors, ensuring both raters have the same set of levels
# First, create a combined list of all codes across both raters
all_codes <- unique(c(as.character(data_relevant_SCT$`Code(s)_M1_1`), 
                      as.character(data_relevant_SCT$`Code(s)_M2_1`)))

# Assign the same factor levels to both columns
data_relevant_SCT$`Code(s)_M1_1` <- factor(data_relevant_SCT$`Code(s)_M1_1`, levels = all_codes)
data_relevant_SCT$`Code(s)_M2_1` <- factor(data_relevant_SCT$`Code(s)_M2_1`, levels = all_codes)

# Step 3: If necessary, replace "none found" with a valid factor level "None Found" for consistency
data_relevant_SCT$`Code(s)_M1_1` <- factor(as.character(data_relevant_SCT$`Code(s)_M1_1`), 
                                           levels = c(all_codes, "None Found"))
data_relevant_SCT$`Code(s)_M2_1` <- factor(as.character(data_relevant_SCT$`Code(s)_M2_1`), 
                                           levels = c(all_codes, "None Found"))

# Step 4: Convert the factors to numeric values (Krippendorff's Alpha works with numeric values)
data_numeric_SCT <- data.frame(lapply(data_relevant_SCT, function(x) as.numeric(x)))

# Step 5: Transpose the data so that rows represent raters and columns represent subjects
data_kripp_SCT <- t(as.matrix(data_numeric_SCT))

# Step 6: Calculate Krippendorff's Alpha for nominal data
kripp_alpha_result_SCT <- kripp.alpha(data_kripp_SCT, method = "nominal")

# Step 7: Print the result
print(kripp_alpha_result_SCT)

# Calculate the proportion of agreement
prop_agreement <- sum(rater1_SCT == rater2_SCT) / length(rater1_SCT)

# Print the result
print(prop_agreement)


# Define the file path and file name
file_path <- "/Users/ninahaffer/Downloads/R Analysen"
file_name <- "AMDP ICF.xlsx"
full_path <- file.path(file_path, file_name)

# Read the Excel file
data_ICF <- read_excel(full_path)
data_clean_ICF <- na.omit(data_ICF)

# Extract the relevant columns for interrater agreement
rater1_ICF <- data_clean_ICF[["Code(s)_M1_1"]]
rater2_ICF <- data_clean_ICF[["Code(s)_M2_1"]]

# Step 1: Prepare the relevant columns
data_relevant_ICF <- data_clean_ICF[, c("Code(s)_M1_1", "Code(s)_M2_1")]

# Step 2: Convert both columns to factors, ensuring both raters have the same set of levels
# First, create a combined list of all codes across both raters
all_codes <- unique(c(as.character(data_relevant_ICF$`Code(s)_M1_1`), 
                      as.character(data_relevant_ICF$`Code(s)_M2_1`)))

# Assign the same factor levels to both columns
data_relevant_ICF$`Code(s)_M1_1` <- factor(data_relevant_ICF$`Code(s)_M1_1`, levels = all_codes)
data_relevant_ICF$`Code(s)_M2_1` <- factor(data_relevant_ICF$`Code(s)_M2_1`, levels = all_codes)

# Step 3: If necessary, replace "none found" with a valid factor level "None Found" for consistency
data_relevant_ICF$`Code(s)_M1_1` <- factor(as.character(data_relevant_ICF$`Code(s)_M1_1`), 
                                           levels = c(all_codes, "None Found"))
data_relevant_ICF$`Code(s)_M2_1` <- factor(as.character(data_relevant_ICF$`Code(s)_M2_1`), 
                                           levels = c(all_codes, "None Found"))

# Step 4: Convert the factors to numeric values (Krippendorff's Alpha works with numeric values)
data_numeric_ICF <- data.frame(lapply(data_relevant_ICF, function(x) as.numeric(x)))

# Step 5: Transpose the data so that rows represent raters and columns represent subjects
data_kripp_ICF <- t(as.matrix(data_numeric_ICF))

# Step 6: Calculate Krippendorff's Alpha for nominal data
kripp_alpha_result_ICF <- kripp.alpha(data_kripp_ICF, method = "nominal")

# Step 7: Print the result
print(kripp_alpha_result_ICF)

# Calculate the proportion of agreement
prop_agreement <- sum(rater1_ICF == rater2_ICF) / length(rater1_ICF)

# Print the result
print(prop_agreement)


# Define the file path and file name
file_path <- "/Users/ninahaffer/Downloads/R Analysen"
file_name <- "AMDP ICD-10.xlsx"
full_path <- file.path(file_path, file_name)

# Read the Excel file
data_ICD10 <- read_excel(full_path)
data_clean_ICD10 <- na.omit(data_ICD10)

# Extract the relevant columns for interrater agreement
rater1_ICD10 <- data_clean_ICD10[["Code(s)_M1_1"]]
rater2_ICD10 <- data_clean_ICD10[["Code(s)_M2_1"]]

# Step 1: Prepare the relevant columns
data_relevant_ICD10 <- data_clean_ICD10[, c("Code(s)_M1_1", "Code(s)_M2_1")]

# Step 2: Convert both columns to factors, ensuring both raters have the same set of levels
# First, create a combined list of all codes across both raters
all_codes <- unique(c(as.character(data_relevant_ICD10$`Code(s)_M1_1`), 
                      as.character(data_relevant_ICD10$`Code(s)_M2_1`)))

# Assign the same factor levels to both columns
data_relevant_ICD10$`Code(s)_M1_1` <- factor(data_relevant_ICD10$`Code(s)_M1_1`, levels = all_codes)
data_relevant_ICD10$`Code(s)_M2_1` <- factor(data_relevant_ICD10$`Code(s)_M2_1`, levels = all_codes)

# Step 3: If necessary, replace "none found" with a valid factor level "None Found" for consistency
data_relevant_ICD10$`Code(s)_M1_1` <- factor(as.character(data_relevant_ICD10$`Code(s)_M1_1`), 
                                           levels = c(all_codes, "None Found"))
data_relevant_ICD10$`Code(s)_M2_1` <- factor(as.character(data_relevant_ICD10$`Code(s)_M2_1`), 
                                           levels = c(all_codes, "None Found"))

# Step 4: Convert the factors to numeric values (Krippendorff's Alpha works with numeric values)
data_numeric_ICD10 <- data.frame(lapply(data_relevant_ICD10, function(x) as.numeric(x)))

# Step 5: Transpose the data so that rows represent raters and columns represent subjects
data_kripp_ICD10 <- t(as.matrix(data_numeric_ICD10))

# Step 6: Calculate Krippendorff's Alpha for nominal data
kripp_alpha_result_ICD10 <- kripp.alpha(data_kripp_ICD10, method = "nominal")

# Step 7: Print the result
print(kripp_alpha_result_ICD10)

# Calculate the proportion of agreement
prop_agreement <- sum(rater1_ICD10 == rater2_ICD10) / length(rater1_ICD10)

# Print the result
print(prop_agreement)



# Define the file path and file name
file_path <- "/Users/ninahaffer/Downloads/R Analysen"
file_name <- "AMDP ICD-11.xlsx"
full_path <- file.path(file_path, file_name)

# Read the Excel file
data_ICD11 <- read_excel(full_path)
data_clean_ICD11 <- na.omit(data_ICD11)

# Extract the relevant columns for interrater agreement
rater1_ICD11 <- data_clean_ICD11[["Code(s)_M1_1"]]
rater2_ICD11 <- data_clean_ICD11[["Code(s)_M2_1"]]

# Step 1: Prepare the relevant columns
data_relevant_ICD11 <- data_clean_ICD11[, c("Code(s)_M1_1", "Code(s)_M2_1")]

# Step 2: Convert both columns to factors, ensuring both raters have the same set of levels
# First, create a combined list of all codes across both raters
all_codes <- unique(c(as.character(data_relevant_ICD11$`Code(s)_M1_1`), 
                      as.character(data_relevant_ICD11$`Code(s)_M2_1`)))

# Assign the same factor levels to both columns
data_relevant_ICD11$`Code(s)_M1_1` <- factor(data_relevant_ICD11$`Code(s)_M1_1`, levels = all_codes)
data_relevant_ICD11$`Code(s)_M2_1` <- factor(data_relevant_ICD11$`Code(s)_M2_1`, levels = all_codes)

# Step 3: If necessary, replace "none found" with a valid factor level "None Found" for consistency
data_relevant_ICD11$`Code(s)_M1_1` <- factor(as.character(data_relevant_ICD11$`Code(s)_M1_1`), 
                                             levels = c(all_codes, "None Found"))
data_relevant_ICD11$`Code(s)_M2_1` <- factor(as.character(data_relevant_ICD11$`Code(s)_M2_1`), 
                                             levels = c(all_codes, "None Found"))

# Step 4: Convert the factors to numeric values (Krippendorff's Alpha works with numeric values)
data_numeric_ICD11 <- data.frame(lapply(data_relevant_ICD11, function(x) as.numeric(x)))

# Step 5: Transpose the data so that rows represent raters and columns represent subjects
data_kripp_ICD11 <- t(as.matrix(data_numeric_ICD11))

# Step 6: Calculate Krippendorff's Alpha for nominal data
kripp_alpha_result_ICD11 <- kripp.alpha(data_kripp_ICD11, method = "nominal")

# Step 7: Print the result
print(kripp_alpha_result_ICD11)

# Calculate the proportion of agreement
prop_agreement <- sum(rater1_ICD11 == rater2_ICD11) / length(rater1_ICD11)

# Print the result
print(prop_agreement)

