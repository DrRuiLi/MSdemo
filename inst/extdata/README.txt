MSdemo dataset catalog.

Registered datasets live in catalog.json. Add another dataset by:
  1. inserting an object under "datasets" with a unique id
  2. creating inst/extdata/<dataset_id>/ (FILELIST.txt, README.txt)
  3. pointing id / doi / url at that dataset's Zenodo record

keep         regex of Zenodo files to download
raw_pattern  regex used to detect that the dataset is present locally

Default dataset: lcms_wiff
  DOI: 10.5281/zenodo.22673685

  library(MSdemo)
  MSdemo_datasets()
  MSdemo_download_dataset()
  demo_raw_dir()
