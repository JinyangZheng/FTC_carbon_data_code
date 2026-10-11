# FTC_carbon_data_code

R plotting scripts, accompanying figure data, statistical analysis, and model code for a study of freeze-thaw cycles and soil carbon dynamics.

## Contents

The archive contains 12 folders and 29 source and data files.

- `Fig_2`, `Fig_3_S4_S5`, `Fig_4_S8`, and `Fig_S3`, `Fig_S6`, `Fig_S7`, `Fig_S9`, `Fig_S10`, `Fig_S11` contain 12 plotting scripts and 12 matching `.rds` files for Figures 2–4 and Supplementary Figures S3–S11.
- `Table_1` contains `Table_1_analysis.R` and `Table_1_data.rds`.
- `Table_S2` contains `Table_S2_analysis.R` and `Table_S2_data.rds`.
- `FTC_model/Full_model.R` contains the freeze-thaw driver and carbon model functions.

## Running figures and tables

Install R and the packages used by the scripts. Plotting dependencies are `ggplot2`, `patchwork`, `ragg`, and `data.table`. Table 1 uses `nlme`, `lmerTest`, and `car`. Table S2 uses `nlme` and `car`.

Download the repository and keep its directory structure intact. From the repository root, run scripts with an explicit output directory, for example

```sh
Rscript Fig_2/Fig_2_plot.R output/Fig_2
Rscript Fig_3_S4_S5/Fig_3_plot.R output/Fig_3
Rscript Table_1/Table_1_analysis.R output/Table_1
Rscript Table_S2/Table_S2_analysis.R output/Table_S2
```

## Model code

```r
source("FTC_model/Full_model.R")
```

This loads the model functions. Running a simulation requires a parameter vector, initial conditions, and aligned hourly drivers supplied by the caller, as documented in the source file. The model file is not a complete calibration workflow.

## Scope

The data files contain the inputs bundled for the figure and statistical analysis scripts.

## License and attribution

Copyright © 2026 Jinyang Zheng.

- All R source files (`*.R`) and the accompanying documentation are licensed under the [MIT License](LICENSE).
- All bundled data files (`*.rds`) are licensed under [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/). The full legal text is provided in [LICENSE-DATA](LICENSE-DATA). These data files are excluded from the MIT license.

When sharing or adapting the data, credit Jinyang Zheng and this repository, link to CC BY 4.0, and indicate whether changes were made. Retain any supplied copyright, license, and disclaimer notices as required by the license.

Suggested data attribution

> Jinyang Zheng (2026). FTC_carbon_data_code. https://github.com/JinyangZheng/FTC_carbon_data_code. Licensed under CC BY 4.0. Indicate any changes when reusing the data.

When redistributing the code, retain the MIT copyright and permission notice. These licenses do not cover third-party software dependencies, which retain their own licenses.
