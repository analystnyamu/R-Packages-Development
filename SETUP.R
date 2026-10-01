# Open RStudio
# File -> Open Project -> enhancedTtest.Rproj

# Then in the R console:

# 1. Set working directory (should be automatic in RStudio)
# The working directory should be the package root
# Use forward slashes (works on all platforms)
setwd("D:/Martin Doc/R PACKAGES/R-Packages-Development/enhancedTtest")
getwd()  # Should show ".../enhancedTtest"

# Should show: D:/Martin Doc/R PACKAGES/R-Packages-Development/enhancedTtest

# Check for package files
file.exists("DESCRIPTION")
# Should be TRUE

file.exists("R")
# Should be TRUE

# List all files
list.files()
# Should show: DESCRIPTION, NAMESPACE, R/, tests/, etc.
# 2. Load devtools

# Check for .Rproj file
list.files(pattern = "\\.Rproj$")
# Should show: "enhancedTtest.Rproj"

# If not, create it:
if (!any(grepl("\\.Rproj$", list.files()))) {
  usethis::use_rstudio()
  cat("✓ Created enhancedTtest.Rproj\n")
}

# Check what R files you have
list.files("R", pattern = "\\.R$")
# Should show your R source files

library(devtools)

# 1. Delete NAMESPACE
#file.remove("NAMESPACE")

# 2. Document
devtools::document()

# Remove old documentation
#unlink("man/auto_ttest.Rd")
  
# 1. Load the package
devtools::load_all()

# 2. Run tests (if you have any)
devtools::test()

# 3. Check the package thoroughly
devtools::check()

# 4. Build the package
devtools::build()

# 5. Install locally
devtools::install()



# # 1. Delete the old Rd file
# unlink("man/enhanced_ttest.Rd")
# 
# # 2. Check if there's a corresponding R file
# if (file.exists("R/enhanced_ttest.R")) {
#   # Delete or rename it
#   file.remove("R/enhanced_ttest.R")
# }
# 
# # Remove it completely
# if (file.exists("R/enhanced_ttest.R")) {
#   file.remove("R/enhanced_ttest.R")
# }
# unlink("man/enhanced_ttest.Rd")

# Remove from NAMESPACE
if (file.exists("NAMESPACE")) {
  namespace_lines <- readLines("NAMESPACE")
  namespace_lines <- namespace_lines[!grepl("enhanced_ttest", namespace_lines)]
  writeLines(namespace_lines, "NAMESPACE")
}

# Regenerate
devtools::document()


# 5. Check
devtools::check()


## Installing the Packages

# Install from the built tarball
install.packages(
  "D:/Martin Doc/R PACKAGES/R-Packages-Development/enhancedTtest_0.1.0.tar.gz",
  repos = NULL,
  type = "source"
)


# Load the package
library(enhancedTtest)

# Check it loaded correctly
packageVersion("enhancedTtest")
# Should show: ‘0.1.0’

# Test with real data
result <- complete_ttest(
  iris[iris$Species %in% c("setosa", "versicolor"), ],
  Sepal.Length ~ Species
)
print(result)
summary(result)

# Test export
result <- complete_ttest(
  iris[iris$Species %in% c("setosa", "versicolor"), ],
  Sepal.Length ~ Species,
  export_word = TRUE,
  filename = "D:/Martin Doc/test_output.docx"
)

## Diagnosis;

# Load the package
devtools::load_all()

# Run the test
result <- complete_ttest(
  iris[iris$Species %in% c("setosa", "versicolor"), ],
  Sepal.Length ~ Species
)

# 1. Check descriptive stats
cat("=== DESCRIPTIVE STATS ===\n")
print(result$descriptive_stats)
str(result$descriptive_stats)

# 2. Check APA table structure
cat("\n=== APA TABLE STRUCTURE ===\n")
if (!is.null(result$apa_table)) {
  cat("Class:", class(result$apa_table), "\n")
  cat("Type:", typeof(result$apa_table), "\n")
  
  # If it's a flextable, extract the data
  if (inherits(result$apa_table, "flextable")) {
    df <- result$apa_table$body$dataset
    cat("\nUnderlying data frame:\n")
    print(df)
    str(df)
  }
}

## Testing the APA formats

result1 <- complete_ttest(
  iris[iris$Species %in% c("setosa", "versicolor"), ],
  Sepal.Length ~ Species
)

print(result1$apa_table$body$dataset)

print(result1)

## Test 3: One-Sample Test

result2 <- complete_ttest(mtcars, mpg ~ 1, mu = 20)
print(result2)
print(result2$apa_table$body$dataset)

## Test 4: Paired Test

set.seed(123)
paired_data <- data.frame(
  value = c(rnorm(30, 10, 2), rnorm(30, 11, 2)),
  time = factor(rep(c("before", "after"), each = 30))
)
result3 <- complete_ttest(paired_data, value ~ time, paired = TRUE)
print(result3)
print(result3$apa_table$body$dataset)

## Test 5: Export to Word

export_apa_to_word(result1$apa_table, "two_sample.docx")
export_apa_to_word(result2$apa_table, "one_sample.docx")
export_apa_to_word(result3$apa_table, "paired.docx")

cat("Files created:\n")
cat("  two_sample.docx:", file.exists("two_sample.docx"), "\n")
cat("  one_sample.docx:", file.exists("one_sample.docx"), "\n")
cat("  paired.docx:     ", file.exists("paired.docx"), "\n")

## Open the .docx files in Word — they should show the APA tables with Times New Roman, proper borders, and all values.

## Delete these files
# Check if they exist
file.exists(c("one_sample.docx", "paired.docx", "two_sample.docx"))

# Delete them
file.remove("one_sample.docx", "paired.docx", "two_sample.docx")

## Tell R what to ignore

usethis::use_build_ignore("README.Rmd")
usethis::use_build_ignore("data-raw")
usethis::use_build_ignore("one_sample.docx")
usethis::use_build_ignore("paired.docx")
usethis::use_build_ignore("two_sample.docx")

## Verify
cat(readLines(".Rbuildignore"), sep = "\n")

# Find non-ASCII characters in each file
tools::showNonASCIIfile("R/export_functions_complete.R")
tools::showNonASCIIfile("R/helpers_complete.R")
tools::showNonASCIIfile("R/print_complete.R")


# =============================================================================
# FIX NON-ASCII CHARACTERS
# =============================================================================

# Mapping of the 4 unique characters found
replacements <- c(
  "\u2713" = "\\u2713",  # ✓ checkmark -> \u2713
  "\u2717" = "\\u2717",  # ✗ x mark -> \u2717
  "\u2022" = "\\u2022",  # • bullet -> \u2022
  "\u2020" = "\\u2020",  # † dagger -> \u2020
  "\u2705" = ""          # ✅ (in comment) -> remove entirely
)

files_to_fix <- c(
  "R/export_functions_complete.R",
  "R/helpers_complete.R",
  "R/print_complete.R"
)

for (f in files_to_fix) {
  if (!file.exists(f)) {
    cat("⚠ File not found:", f, "\n")
    next
  }
  
  content <- readLines(f, warn = FALSE, encoding = "UTF-8")
  original_content <- content
  changes_made <- 0
  
  for (i in seq_along(content)) {
    for (pattern in names(replacements)) {
      if (grepl(pattern, content[i], fixed = TRUE)) {
        content[i] <- gsub(pattern, replacements[[pattern]], content[i], fixed = TRUE)
        changes_made <- changes_made + 1
      }
    }
  }
  
  if (!identical(content, original_content)) {
    writeLines(content, f, useBytes = TRUE)
    cat("✓ Fixed:", f, "(", changes_made, "replacements )\n")
  } else {
    cat("○ No changes needed:", f, "\n")
  }
}

cat("\n✅ Non-ASCII fix complete!\n")
