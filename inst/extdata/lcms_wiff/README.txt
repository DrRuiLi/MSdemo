lcms_wiff: anonymized LC-MS files for the MSdev R package.

30 paired .wiff / .wiff.scan files (~3.6 GB):
  QC, Blank, Sample_GroupA, Sample_GroupC, Sample_GroupD
  positive and negative polarity, 3 replicates each

Example names:
  QC_pos_01.wiff
  Blank_neg_02.wiff
  Sample_GroupA_pos_01.wiff

After installing MSdemo:

  library(MSdemo)
  MSdemo_datasets()
  MSdemo_download_dataset("lcms_wiff")
  demo_raw_dir("lcms_wiff")

DOI: 10.5281/zenodo.22673685
License: CC-BY-4.0
