# loading necessary packages
packages <- c("tidyr",
              "dplyr",
              "ggplot2",
              "patchwork",
              "RColorBrewer",
              "jsonlite",
              "rnaturalearth",
              "sf",
              "rnaturalearthdata",
              "rstudioapi") ## add more packages if needed

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Setting working directory
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  setwd(".")
}