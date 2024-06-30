# Load required libraries
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Load the data from Excel
data <- read_excel("ROtoNLfinal.xlsx")

# Combine first and last names for authors and translators
data$author_full_name <- paste(data$auth_first, data$auth_last)
data$translator_full_name <- paste(data$trnsl_first, data$trnsl_last)

# Check if second_translator column exists and handle missing values
if ("second_translator" %in% names(data)) {
  data$second_translator[is.na(data$second_translator)] <- ""  # Replace NA with empty string for consistency
}

# Create a unique set of individuals and entities
unique_people <- unique(c(data$author_full_name, data$translator_full_name, data$second_translator[data$second_translator != ""]))
unique_books <- unique(data$title)
unique_pub_transl <- unique(data$publisher_trnsl)
unique_pub_orig <- unique(data$publisher_original)

# Create mappings for attributes
book_attributes <- data.frame(
  title = data$title,
  trnsl_year = data$trnsl_year,
  original_publ_year = data$original_publ_year,
  genre = data$genre,
  funding = data$funding,
  stringsAsFactors = FALSE
)
translator_attributes <- data.frame(
  full_name = c(data$translator_full_name, data$second_translator[data$second_translator != ""]),
  gender = c(data$trnsl_gender, rep(NA, sum(data$second_translator != ""))),  # Assumes missing gender data for second translators
  stringsAsFactors = FALSE
)
author_attributes <- data.frame(
  full_name = data$author_full_name,
  gender = data$auth_gender,
  stringsAsFactors = FALSE
)

# Prepare node data
node_data <- data.frame(
  id = c(data$author_full_name, data$translator_full_name, data$second_translator[data$second_translator != ""], unique_books, unique_pub_transl, unique_pub_orig),
  label = c(data$author_full_name, data$translator_full_name, data$second_translator[data$second_translator != ""], unique_books, unique_pub_transl, unique_pub_orig),
  type = c(rep("Author", length(data$author_full_name)), rep("Translator", length(data$translator_full_name) + sum(data$second_translator != "")), rep("Book", length(unique_books)), rep("Publisher Transl", length(unique_pub_transl)), rep("Publisher Orig", length(unique_pub_orig))),
  stringsAsFactors = FALSE
)

# Merge book attributes
node_data <- merge(node_data, book_attributes, by.x = "label", by.y = "title", all.x = TRUE)

# Merge translator attributes and resolve potential gender column conflicts
node_data <- merge(node_data, translator_attributes, by.x = "label", by.y = "full_name", all.x = TRUE, suffixes = c("", ".trnsl"))
# If gender.trnsl exists, coalesce it into gender
if ("gender.trnsl" %in% names(node_data)) {
  node_data$gender <- ifelse(is.na(node_data$gender), node_data$gender.trnsl, node_data$gender)
  node_data$gender.trnsl <- NULL  # Remove the temporary column
}

# Merge author attributes, similarly handling gender conflicts
node_data <- merge(node_data, author_attributes, by.x = "label", by.y = "full_name", all.x = TRUE, suffixes = c("", ".auth"))
# If gender.auth exists, coalesce it into gender
if ("gender.auth" %in% names(node_data)) {
  node_data$gender <- ifelse(is.na(node_data$gender), node_data$gender.auth, node_data$gender)
  node_data$gender.auth <- NULL  # Remove the temporary column
}

# Ensure there is only one 'gender' column in the final output
node_data$gender <- ifelse(is.na(node_data$gender), NA, node_data$gender)

# Aggregate the first translation year for all labels
all_labels <- unique(c(data$author_full_name, data$translator_full_name, data$second_translator, data$title, data$publisher_trnsl, data$publisher_original))
aggregated_years <- data.frame(label = all_labels, year = NA_real_)  # Initialize the dataframe

for (label in all_labels) {
  relevant_rows <- data[data$author_full_name == label | data$translator_full_name == label | 
                          data$second_translator == label | data$title == label | 
                          data$publisher_trnsl == label | data$publisher_original == label, ]
  if (nrow(relevant_rows) > 0) {
    aggregated_years$year[aggregated_years$label == label] <- min(relevant_rows$trnsl_year, na.rm = TRUE)
  }
}

# Merge the earliest translation year into node_data
node_data <- merge(node_data, aggregated_years, by = "label", all.x = TRUE)

# Output the prepared data to verify
print(head(node_data))

# Prepare edge data
edge_data <- data.frame(
  Source = rep(data$title, 4),
  Target = c(data$author_full_name, data$translator_full_name, data$publisher_trnsl, data$publisher_original),
  Relation = c(rep("Authored", nrow(data)), rep("Translated", nrow(data)), rep("Published Translation", nrow(data)), rep("Published Original", nrow(data))),
  Language = c(rep("Romanian", nrow(data)), rep("Dutch", nrow(data)), rep("Dutch", nrow(data)), rep("Romanian", nrow(data))),
  stringsAsFactors = FALSE
)

# Add edges for second translators linking them directly to the books
if (any(data$second_translator != "")) {
  second_translator_edges <- data.frame(
    Source = data$second_translator[data$second_translator != ""],
    Target = data$title[data$second_translator != ""],
    Relation = rep("Second Translation", sum(data$second_translator != "")),
    Language = rep("Dutch", sum(data$second_translator != ""))
  )
  edge_data <- rbind(edge_data, second_translator_edges)
}

# Write nodes and edges to CSV files with correct formatting
write.csv(node_data, "ROtoNLnodes.csv", row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
write.csv(edge_data, "ROtoNLedges.csv", row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
