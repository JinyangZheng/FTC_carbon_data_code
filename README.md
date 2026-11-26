# FTC_carbon_data_code

R plotting scripts, accompanying figure data, statistical analysis, and model code for a study of freeze-thaw cycles and soil carbon dynamics.

## Contents

The 11 folders contain 27 R scripts and data files.

- Figures 2–4 and Supplementary Figures S4–S12 each have a plotting script and a matching `.rds` file. These 12 pairs are in `Fig_2`, `Fig_3_S5_S6`, `Fig_4_S9`, `Fig_S4`, `Fig_S7`, `Fig_S8`, `Fig_S10`, `Fig_S11`, and `Fig_S12`.
- `Table_1` contains `Table_1_analysis.R` and `Table_1_data.rds`.
- `FTC_model/Full_model.R` defines `Full_kernel()`, which simulates soil carbon pools and fluxes during freeze–thaw cycles.

## License and attribution

Copyright © 2026 Jinyang Zheng.

- All R source files (`*.R`) and the accompanying documentation are licensed under the [MIT License](LICENSE).
- All bundled data files (`*.rds`) are licensed under [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/). The full legal text is provided in [LICENSE-DATA](LICENSE-DATA). These data files are excluded from the MIT license.

When sharing or adapting the data, credit Jinyang Zheng and this repository, link to CC BY 4.0, and indicate whether changes were made. Retain any supplied copyright, license, and disclaimer notices as required by the license.

Suggested data attribution

> Jinyang Zheng (2026). FTC_carbon_data_code. https://github.com/JinyangZheng/FTC_carbon_data_code. Licensed under CC BY 4.0. Indicate any changes when reusing the data.

When redistributing the code, retain the MIT copyright and permission notice. These licenses do not cover third-party software dependencies, which retain their own licenses.
