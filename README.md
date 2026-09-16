# MSdemo

Demo LC-MS data for [MSdev](https://github.com/DrRuiLi/MSdev): anonymized files, positive and negative, three replicates per group (`QC`, `Blank`, `Sample_GroupA`, `Sample_GroupC`, `Sample_GroupD`).

Raw `.wiff` files are **not** in git. After installing the package, download them from Zenodo (~several GB) into the installed package `extdata` folder:

```r
library(MSdemo)
MSdemo_zenodo()                 # DOI and record URL
MSdemo_download_dataset()       # writes into the installed package by default
demo_raw_dir()
demo_sample_info()
# make_demo()                   # requires MSdev + MSconvertR
```

Zenodo record:

- <https://zenodo.org/records/22673685>
- DOI: `10.5281/zenodo.22673685`

Override the download folder with `MSdemo_download_dataset(dest = ...)` or `options(MSdemo.raw_dir = ...)`. Processed objects are rebuilt into `D:/MSdemo/project`.
