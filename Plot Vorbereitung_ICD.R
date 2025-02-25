# Load necessary libraries
library(readxl)  # Zum Lesen der Excel-Datei
library(dplyr)   # Für die Datenverarbeitung
library(ggplot2) # Für die Visualisierung
library(tidyr)   # Zum Transformieren der Daten

# Define the file path and file name
file_path <- "/Users/ninahaffer/Downloads/R ISO"
file_name <- "ISO-ICD.xlsx"
full_path <- file.path(file_path, file_name)

# 1. Read the Excel file
data <- read_excel(full_path)

# Spalten bereinigen und nur Datenelemente behalten, die nicht leer sind
cleaned_column1 <- data[[1]][!is.na(data[[1]]) & data[[1]] >= 0 & data[[1]] <= 4] #CASCAP-2 ICD-10
cleaned_column2 <- data[[2]][!is.na(data[[2]]) & data[[2]] >= 0 & data[[2]] <= 4] #CASCAP-2 ICD-11
cleaned_column3 <- data[[3]][!is.na(data[[3]]) & data[[3]] >= 0 & data[[3]] <= 4] #AMDP ICD-10
cleaned_column4 <- data[[4]][!is.na(data[[4]]) & data[[4]] >= 0 & data[[4]] <= 4] #AMDP ICD-11

# Function to calculate mean for a column
calculate_mean <- function(column) {
        numeric_column <- as.numeric(column) # Ensure column is numeric
        return(mean(numeric_column, na.rm = TRUE)) # Compute mean while ignoring NAs
}

# Apply the function to each cleaned column and store the results
columns <- list(cleaned_column1, cleaned_column2, cleaned_column3, cleaned_column4)

mean_values <- sapply(columns, calculate_mean)

# Naming the results for clarity
names(mean_values) <- c("CASCAP-2 ICD-10", "AMDP ICD-10", "CASCAP-2 ICD-11", "AMDP ICD-11")

# Print the means for each column
print(mean_values)

# Replace values like 0.5, 1.5, 2.5, 3.5 with the next highest numeric value
cleaned_column1 <- replace(cleaned_column1, cleaned_column1 == 0.5, 1)
cleaned_column1 <- replace(cleaned_column1, cleaned_column1 == 1.5, 2)
cleaned_column1 <- replace(cleaned_column1, cleaned_column1 == 2.5, 3)
cleaned_column1 <- replace(cleaned_column1, cleaned_column1 == 3.5, 4)

cleaned_column2 <- replace(cleaned_column2, cleaned_column2 == 0.5, 1)
cleaned_column2 <- replace(cleaned_column2, cleaned_column2 == 1.5, 2)
cleaned_column2 <- replace(cleaned_column2, cleaned_column2 == 2.5, 3)
cleaned_column2 <- replace(cleaned_column2, cleaned_column2 == 3.5, 4)

cleaned_column3 <- replace(cleaned_column3, cleaned_column3 == 0.5, 1)
cleaned_column3 <- replace(cleaned_column3, cleaned_column3 == 1.5, 2)
cleaned_column3 <- replace(cleaned_column3, cleaned_column3 == 2.5, 3)
cleaned_column3 <- replace(cleaned_column3, cleaned_column3 == 3.5, 4)

cleaned_column4 <- replace(cleaned_column4, cleaned_column4 == 0.5, 1)
cleaned_column4 <- replace(cleaned_column4, cleaned_column4 == 1.5, 2)
cleaned_column4 <- replace(cleaned_column4, cleaned_column4 == 2.5, 3)
cleaned_column4 <- replace(cleaned_column4, cleaned_column4 == 3.5, 4)

# Function to calculate the percentage of each value (0, 1, 2, 3, 4)
calculate_percentages <- function(column) {
        total <- length(column) # Total number of elements
        percentages <- table(column) / total * 100 # Calculate percentage
        return(percentages)
}

# Apply the function to each cleaned column and print results
columns <- list(cleaned_column1, cleaned_column2, cleaned_column3, cleaned_column4
                )

results <- lapply(columns, calculate_percentages)

# Naming the results for clarity
names(results) <- c("CASCAP-2 ICD-10","CASCAP-2 ICD-11", "AMDP ICD-10", "AMDP ICD-11")

# Print percentages for each column
print(results)

# Pakete laden
library(ggplot2)
library(dplyr)
library(tidyr)

# Beispiel: Angenommen, wir haben die 4 bereinigten Spalten als Listen
cleaned_columns <- list(cleaned_column1, cleaned_column2, cleaned_column3, 
                        cleaned_column4)

# Daten für den Plot vorbereiten
plot_data <- lapply(seq_along(cleaned_columns), function(i) {
        col_data <- cleaned_columns[[i]]
        
        # Zähle die Häufigkeiten der Werte zwischen 0 und 4
        value_counts <- table(factor(col_data, levels = 0:4))
        
        # Berechne Prozentsätze
        percentages <- prop.table(value_counts) * 100
        
        # Erstelle ein DataFrame mit Spaltenindex und Werten
        data.frame(Source = paste0("Column", i),  # Column1, Column2, ...
                   Value = as.numeric(names(percentages)),
                   Proportion = as.numeric(percentages))
})

# Kombiniere die einzelnen DataFrames in ein Gesamtdatenframe
plot_data <- do.call(rbind, plot_data)

# Farben manuell definieren (0 = Blau, 4 = Lila)
custom_colors <- c("0" = "#e0f3f8",  # Helles Türkis
                   "1" = "#abd9e9",  # Türkis
                   "2" = "#74add1",  # Hellblau 
                   "3" = "#4575b4",  # Blau 
                   "4" = "#d73027")  # Lila

# Neue Achsenbeschriftungen
axis_labels <- c(
        "Column1" = "CASCAP-2 ICD-10",
        "Column2" = "CASCAP-2 ICD-11",
        "Column3" = "AMDP ICD-10",
        "Column4" = "AMDP ICD-11")

# Umkehren der Reihenfolge der Levels auf der Y-Achse
plot_data$Source <- factor(plot_data$Source, 
                           levels = rev(paste0("Column", 1:4)),  # Reihenfolge umkehren
                           labels = rev(axis_labels))  # Labels umkehren


# Plot erstellen
p <- ggplot(plot_data, aes(y = Source, x = Proportion, fill = as.factor(Value))) +
        geom_bar(stat = "identity") +
        
        # Customize axes and labels
        ylab("") +
        xlab("%") +
        labs(
                fill = "ISO Value",
                title = "ISO Value Distribution in each of the ICD Mappings"
        ) +
        
        # Manuelle Farbzuordnung
        scale_fill_manual(values = custom_colors) +
        
        # No need for reverse transformation here
        scale_x_continuous() +
        
        # Enhance plot aesthetics
        theme_minimal() +
        theme(
                plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                axis.text.y = element_text(size = 10),
                legend.title = element_text(size = 12),
                legend.text = element_text(size = 10),
                panel.spacing = unit(1, "lines")  # Add spacing between facets
        )

# Plot anzeigen
print(p)
