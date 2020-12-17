
# List of required packages (packages that are not installed will be in automatically installed
list.of.packages <- c("tidyverse", "grid", "gridExtra", "cowplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
