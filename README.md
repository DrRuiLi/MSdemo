# MSdemo

Demo LC-MS data for [MSdev](https://github.com/DrRuiLi/MSdev). Datasets are registered in `inst/extdata/catalog.json`; each has its own Zenodo record and local folder.

Raw vendor files are **not** in git. After installing the package:

```r
library(MSdemo)
MSdemo_datasets()                 # catalog of Zenodo records
MSdemo_zenodo()                   # default dataset metadata
MSdemo_download_dataset()         # writes to <extdata>/<dataset>
demo_raw_dir()
demo_sample_info()
make_demo()                       # MSdev pipeline + companion demo objects
load_demo("MSdev")
load_demo("xcms")                 # positive XcmsExperiment
load_demo("XCMSnExp")
load_demo("data.se")              # SummarizedExperiment
load_demo("sp")                   # ~1000 Spectra from the full set
```

Current default dataset `lcms_wiff`:

- <https://zenodo.org/records/22673685>
- DOI: `10.5281/zenodo.22673685`

Add another dataset by inserting an object under `datasets` in `catalog.json` and creating `inst/extdata/<dataset_id>/`. Override the download folder with `MSdemo_download_dataset(dest = ...)`. `make_demo()` writes `MSdev`, `XcmsExperiment`, `XCMSnExp`, `SummarizedExperiment`, and `Spectra` objects into that same folder.
