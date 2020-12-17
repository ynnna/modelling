# ReadME

## Contact details
+ email: iliescuaf@cardiff.ac.uk
+ Institutional profile: [Dr Adela Iliescu](https://www.cardiff.ac.uk/people/view/2420714-).

## Software 
The software needed to reproduce these simulation is R and RStudio. 

First download and install the latest R version for your operating system [here](https://www.r-project.org). Then install RStudio Desktop following the instructions provided [here](https://rstudio.com/products/rstudio/).
Before running the modelling standalone files, open and run  ``load_packages.R``.

## Modelling
Standalone simulations for each HeiDI simulated phenomena. For more info about each phenomena visit the 
[HeiDI app](https://ynnna.shinyapps.io/HeiDI_model/).

Each folder contains the following files:
 
+ `load_packages.R`: file used to check and install necessarry packages.
+ A standalone modelling file  (`~_standalone.R`): contains the used code for creating a dataframe, apply the model function and create plots. It sources the `load_packages.R` `model.R` and `my_theme.R` files.
+ `model.R`: file containinng the modelling function.  It is sourced by the standalone file.
+ `my_theme.R`: controls the appeareance of the plots through a theme function.  It is sourced by the standalone file.

