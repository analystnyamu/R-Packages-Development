# In RStudio or R console
# Install devtools if you haven't
install.packages(c("devtools", "usethis", "roxygen2"))

# Create package structure
library(devtools)
library(usethis)

# Set your working directory to where you want the package
setwd("~D:\\Martin Doc\\R PACKAGES")

# Create the package
create_package("enhancedTtest")