# VNG and audiometry in cochlear implant candidates: analysis code (R)

R code that produces the **tables and figures** of a master's thesis on vestibular function (videonystagmography, VNG) and pre-operative audiometry in cochlear implant candidates.

One script reads an Excel workbook and writes a Word report, PNG figures and CSV tables. The repository does **not** contain patient data; a small synthetic example is included so the script can be run end to end.

## What the script produces

|Output|Content|
|-|-|
|Table 1|Pre-operative VNG, vestibular weakness vs normal: sex, age, implanted ear, electrode type, comorbidities|
|Table 2|Pre- vs post-implantation caloric parameters (paired): labyrinthine predominance, directional preponderance, total SPV, caloric weakness|
|Table 3|Etiology of hearing loss|
|Table 4|Vestibular function (normal vs weakness and subtypes: bilateral, ipsilateral, contralateral)|
|Table 5|Pre-operative audiologic findings by vestibular group (250-4000 Hz, LPTA, PTA, maximum discrimination, STR)|
|Table 6|Screening accuracy of low-frequency thresholds (sensitivity, specificity, PPV, NPV with exact 95% CI)|
|Table 7|Surgical factors (electrode, approach, brand) and vestibular deterioration|
|Table 8|Patient-level data set (landscape page)|
|Figure 1|Distribution of hearing loss at the chosen frequency by vestibular group (sensitivity and specificity by cut-off)|
|Figure 2|Sensitivity vs specificity across thresholds|
|Figure 3|Caloric response before vs after implantation, implanted and non-implanted ear|
|Figure 4|Caloric response of the implanted ear before vs after implantation, by surgical factor|

## 

## Quick start

1. Install R (4.3 or later) and, optionally, RStudio.
2. Open the repository folder as the working directory.
3. Run:

```r
source("R/analysis\\\_tables\\\_figures.R")
```

Missing packages are installed automatically (`readxl`, `dplyr`, `stringr`, `stringi`, `tibble`, `flextable`, `officer`, `ggplot2`, `patchwork`, `ragg`, `systemfonts`).

Results are written to `outputs/` (ignored by Git): `Report\\\_VNG\\\_audiology.docx`, `Figure\\\*.png` and `Table\\\*.csv`.

To use your own data, change `file\\\_path` at the top of the script. See [`data/README.md`](data/README.md) for the expected sheets and columns.

## 

## Main settings (top of the script)

|Setting|Default|Meaning|
|-|-|-|
|`pl\\\_cutoff`|19|Labyrinthine predominance above this value (%) = unilateral weakness|
|`dp\\\_cutoff`|17|Directional preponderance cut-off (%), recorded separately|
|`hypo\\\_unilateral\\\_cutoff`|5|Hyporeflexia: SPV sum (cold + warm) of one ear below this value (deg/s)|
|`hypo\\\_bilateral\\\_cutoff`|12|Hyporeflexia: SPV sum of the four tests below this value (deg/s)|
|`hyper\\\_unilateral\\\_cutoff`, `hyper\\\_bilateral\\\_cutoff`|62, 122|Hyperreflexia (deg/s)|
|`weakness\\\_criterion`|`"classification"`|`"classification"` = PL or hyporeflexia; `"threshold"` = PL only|
|`figure\\\_hz`|250|Frequency used in Figures 1-2 and Table 6|
|`cutoff\\\_db`, `cutoff\\\_db\\\_2`, `lpta\\\_cutoff\\\_db`|45, 60, 50|Screening cut-offs (dB HL)|
|`surgical\\\_factors`|electrode, approach, brand|Factors analysed in Table 7 and Figure 4|
|`complete\\\_case\\\_only`|`TRUE`|Table 5 excludes patients with missing audiometry|



## Statistical methods

* Comparisons of proportions: Fisher's exact test.
* Continuous variables between two independent groups: Welch's t-test (Table 1) or Student's/Welch's t-test chosen by an F-test for equal variances (Table 5), together with the Mann-Whitney U test for the median.
* Pre vs post (paired): Wilcoxon signed-rank test; exact McNemar test for caloric weakness; Hodges-Lehmann median difference with 95% CI.
* Screening accuracy: sensitivity, specificity, PPV and NPV with exact (Clopper-Pearson) 95% CI; best cut-off by Youden index.
* Surgical factors: Fisher's exact test, Mann-Whitney U test (2 categories) or Kruskal-Wallis test (more than 2).
* Two-sided tests, alpha = 0.05, no correction for multiple comparisons; complete-case analysis within each test.

## Example output (synthetic data)

The images below come from `data/example\\\_synthetic\\\_data.xlsx`, which is **randomly generated**. The numbers have no clinical meaning.

!\[Figure 1](examples/figures/Figure1\_hearing\_loss\_distribution.png)
!\[Figure 2](examples/figures/Figure2\_sensitivity\_specificity.png)
!\[Figure 3](examples/figures/Figure3\_pre\_post\_caloric\_response\_by\_ear.png)
!\[Figure 4](examples/figures/Figure4\_implanted\_ear\_by\_surgical\_factor.png)

The folder [`examples/`](examples) also has the example Word report and the CSV tables.

## Environment

Developed on Windows and tested with R 4.3.3, readxl 1.4.3, dplyr 1.1.4, flextable 0.9.4, officer 0.6.4, ggplot2 3.4.4 and patchwork 1.2.0. Tables and the Word report use the font `NewsGotT` (`table\\\_font`); if it is not installed, change `table\\\_font` or the default font is used.

## Disclaimer

Research code written for a thesis. It is not a medical device and must not be used for clinical decisions.

## License

MIT, see [`LICENSE`](LICENSE).

## Citation

If you use this code, please cite the repository: \[Isabella França]. VNG and audiometry in cochlear implant candidates: analysis code. GitHub, 2026. \\\[repository URL]`

