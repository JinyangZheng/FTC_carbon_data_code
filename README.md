# FTC_carbon_data_code

R plotting scripts, accompanying figure data, and model code for a study of freeze-thaw cycles and soil carbon dynamics.

## Contents

- `Fig_2` through the grouped figure directories contain 24 plotting scripts and 24 matching `.rds` data files for Figures 2–5 and Supplementary Figures S4–S23.
- `FTC_model/Full_model.R` contains the freeze-thaw driver and carbon model functions.
- Each `*_plot.R` script reads the matching `*_data.rds` file from the same directory and writes generated files to an `output` subdirectory.

## Running a figure

Install R and the packages used by the scripts. Package dependencies include `data.table`, `digest`, `dplyr`, `ggforce`, `gghalves`, `ggplot2`, `gtable`, `patchwork`, `ragg`, `scales`, `svglite`, and `systemfonts`. Several figures use Times New Roman, which should be installed for consistent typography.

Download the repository and keep its directory structure intact. From the repository root, run a figure in a fresh R session, for example

```sh
Rscript Fig_2/Fig_2_plot.R
```

Alternatively, in R

```r
source("Fig_2/Fig_2_plot.R")
```

To inspect the bundled data in R

```r
figure_data <- readRDS("Fig_2/Fig_2_data.rds")
names(figure_data)
```

## Model code

```r
source("FTC_model/Full_model.R")
```

This loads model functions. Running a simulation requires a parameter vector, initial conditions, and aligned hourly drivers supplied by the caller, as documented in the source file. The model file is not a complete calibration workflow.

## Scope

The data files contain the inputs bundled for the figure scripts.
