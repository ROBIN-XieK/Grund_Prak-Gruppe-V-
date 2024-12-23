# loading necessary packages
packages <- c("tidyr",
              "dplyr",
              "ggplot2",
              "patchwork",
              "RColorBrewer") ## add more packages if needed

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}