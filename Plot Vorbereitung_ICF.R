# Load necessary libraries
library(readxl)  # Zum Lesen der Excel-Datei
library(dplyr)   # Für die Datenverarbeitung
library(ggplot2) # Für die Visualisierung
library(tidyr)   # Zum Transformieren der Daten

# Define the file path and file name
file_path <- "/Users/ninahaffer/Downloads/R ISO"
file_name <- "ISO-ICF.xlsx"
full_path <- file.path(file_path, file_name)

# 1. Read the Excel file
data <- read_excel(full_path)

# Spalten bereinigen und nur Datenelemente behalten, die nicht leer sind
cleaned_column1 <- data[[1]][!is.na(data[[1]]) & data[[1]] >= 0 & data[[1]] <= 4] #HAMD-7
cleaned_column2 <- data[[2]][!is.na(data[[2]]) & data[[2]] >= 0 & data[[2]] <= 4] #KINDL
cleaned_column3 <- data[[3]][!is.na(data[[3]]) & data[[3]] >= 0 & data[[3]] <= 4] #BDI-II
cleaned_column4 <- data[[4]][!is.na(data[[4]]) & data[[4]] >= 0 & data[[4]] <= 4] #CTQ
cleaned_column5 <- data[[5]][!is.na(data[[5]]) & data[[5]] >= 0 & data[[5]] <= 4] #MADRS
cleaned_column6 <- data[[6]][!is.na(data[[6]]) & data[[6]] >= 0 & data[[6]] <= 4] #YMRS
cleaned_column7 <- data[[7]][!is.na(data[[7]]) & data[[7]] >= 0 & data[[7]] <= 4] #CDRS-R
cleaned_column8 <- data[[8]][!is.na(data[[8]]) & data[[8]] >= 0 & data[[8]] <= 4] #Kidscreen-27
cleaned_column9 <- data[[9]][!is.na(data[[9]]) & data[[9]] >= 0 & data[[9]] <= 4] #CASCAP-2
cleaned_column10 <- data[[10]][!is.na(data[[10]]) & data[[10]] >= 0 & data[[10]] <= 4] #AMDP

# Function to calculate mean for a column
calculate_mean <- function(column) {
        numeric_column <- as.numeric(column) # Ensure column is numeric
        return(mean(numeric_column, na.rm = TRUE)) # Compute mean while ignoring NAs
}

# Apply the function to each cleaned column and store the results
columns <- list(cleaned_column1, cleaned_column2, cleaned_column3, cleaned_column4,
                cleaned_column5, cleaned_column6, cleaned_column7, cleaned_column8,
                cleaned_column9, cleaned_column10)

mean_values <- sapply(columns, calculate_mean)

# Naming the results for clarity
names(mean_values) <- c("HAMD-7", "KINDL", "BDI-II", "CTQ", "MADRS", "YMRS", 
                        "CDRS-R", "Kidscreen-27", "CASCAP-2", "AMDP")

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

cleaned_column5 <- replace(cleaned_column5, cleaned_column5 == 0.5, 1)
cleaned_column5 <- replace(cleaned_column5, cleaned_column5 == 1.5, 2)
cleaned_column5 <- replace(cleaned_column5, cleaned_column5 == 2.5, 3)
cleaned_column5 <- replace(cleaned_column5, cleaned_column5 == 3.5, 4)

cleaned_column6 <- replace(cleaned_column6, cleaned_column6 == 0.5, 1)
cleaned_column6 <- replace(cleaned_column6, cleaned_column6 == 1.5, 2)
cleaned_column6 <- replace(cleaned_column6, cleaned_column6 == 2.5, 3)
cleaned_column6 <- replace(cleaned_column6, cleaned_column6 == 3.5, 4)

cleaned_column7 <- replace(cleaned_column7, cleaned_column7 == 0.5, 1)
cleaned_column7 <- replace(cleaned_column7, cleaned_column7 == 1.5, 2)
cleaned_column7 <- replace(cleaned_column7, cleaned_column7 == 2.5, 3)
cleaned_column7 <- replace(cleaned_column7, cleaned_column7 == 3.5, 4)

cleaned_column8 <- replace(cleaned_column8, cleaned_column8 == 0.5, 1)
cleaned_column8 <- replace(cleaned_column8, cleaned_column8 == 1.5, 2)
cleaned_column8 <- replace(cleaned_column8, cleaned_column8 == 2.5, 3)
cleaned_column8 <- replace(cleaned_column8, cleaned_column8 == 3.5, 4)

cleaned_column9 <- replace(cleaned_column9, cleaned_column9 == 0.5, 1)
cleaned_column9 <- replace(cleaned_column9, cleaned_column9 == 1.5, 2)
cleaned_column9 <- replace(cleaned_column9, cleaned_column9 == 2.5, 3)
cleaned_column9 <- replace(cleaned_column9, cleaned_column9 == 3.5, 4)

cleaned_column10 <- replace(cleaned_column10, cleaned_column10 == 0.5, 1)
cleaned_column10 <- replace(cleaned_column10, cleaned_column10 == 1.5, 2)
cleaned_column10 <- replace(cleaned_column10, cleaned_column10 == 2.5, 3)
cleaned_column10 <- replace(cleaned_column10, cleaned_column10 == 3.5, 4)

# Function to calculate the percentage of each value (0, 1, 2, 3, 4)
calculate_percentages <- function(column) {
        total <- length(column) # Total number of elements
        percentages <- table(column) / total * 100 # Calculate percentage
        return(percentages)
}

# Apply the function to each cleaned column and print results
columns <- list(cleaned_column1, cleaned_column2, cleaned_column3, cleaned_column4,
                cleaned_column5, cleaned_column6, cleaned_column7, cleaned_column8,
                cleaned_column9, cleaned_column10)

results <- lapply(columns, calculate_percentages)

# Naming the results for clarity
names(results) <- c("HAMD-7", "KINDL", "BDI-II", "CTQ", "MADRS", "YMRS", 
                    "CDRS-R", "Kidscreen-27", "CASCAP-2", "AMDP")

# Print percentages for each column
print(results)

# Pakete laden
library(ggplot2)
library(dplyr)
library(tidyr)

# Beispiel: Angenommen, wir haben die 10 bereinigten Spalten als Listen
cleaned_columns <- list(cleaned_column1, cleaned_column2, cleaned_column3, 
                        cleaned_column4, cleaned_column5, cleaned_column6, 
                        cleaned_column7, cleaned_column8, cleaned_column9, 
                        cleaned_column10)

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
        "Column1" = "HAMD-7",
        "Column2" = "KINDL",
        "Column3" = "BDI-II",
        "Column4" = "CTQ",
        "Column5" = "MADRS",
        "Column6" = "YMRS",
        "Column7" = "CDRS-R",
        "Column8" = "Kidscreen-27",
        "Column9" = "CASCAP-2",
        "Column10" = "AMDP"
)

# Umkehren der Reihenfolge der Levels auf der Y-Achse
plot_data$Source <- factor(plot_data$Source, 
                           levels = rev(paste0("Column", 1:10)),  # Reihenfolge umkehren
                           labels = rev(axis_labels))  # Labels umkehren


# Plot erstellen
p <- ggplot(plot_data, aes(y = Source, x = Proportion, fill = as.factor(Value))) +
        geom_bar(stat = "identity") +
        
        # Customize axes and labels
        ylab("") +
        xlab("%") +
        labs(
                fill = "ISO Value",
                title = "ISO Value Distribution in each of the ICF Mappings"
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