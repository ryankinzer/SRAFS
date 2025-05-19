# Load rmarkdown package
library(rmarkdown)

# Define today's date
today <- format(Sys.Date(), "%Y-%m-%d")

# Define archive filename
archive_dir <- "archive"
output_name <- paste0("SRAFS_summary_", today, ".html")
output_path <- file.path(archive_dir, output_name)

# Create archive folder if it doesn't exist
if (!dir.exists(archive_dir)) dir.create(archive_dir)

# Render the Rmd to HTML
render(here::here("docs","index.Rmd"), output_file = here::here("docs","index.html"))

# Now copy the freshly rendered HTML to the archive
file.copy(here::here("docs","index.html"), output_path, overwrite = TRUE)

cat("Archived copy saved to:", output_path, "\n")
