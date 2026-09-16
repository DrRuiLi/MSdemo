.msdemo_default_project_dir <- function() {
  "D:/MSdemo/project"
}

.msdemo_has_raw <- function(path, pattern) {
  is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path) &&
    dir.exists(path) &&
    length(list.files(path, pattern = pattern, ignore.case = TRUE)) > 0L
}

.msdemo_raw_candidates <- function(dataset) {
  named <- .msdemo_named_raw_dir(dataset)
  pkg_ext <- .msdemo_pkg_extdata()
  candidates <- c(
    named,
    if (!is.na(pkg_ext) && nzchar(pkg_ext)) file.path(pkg_ext, dataset)
  )
  unique(candidates[!is.na(candidates) & nzchar(candidates)])
}

#' @title Directory of demo LC-MS raw files
#' @description Return the folder that holds one catalog dataset. Search
#'   order: a `dest` recorded by [MSdemo_download_dataset()], then
#'   `file.path(find.package("MSdemo"), "extdata", dataset)`.
#'
#' @param dataset Dataset id from [MSdemo_datasets()]. Default: option
#'   `MSdemo.dataset`, otherwise the catalog `default`.
#'
#' @return Character path (forward slashes).
#' @export
demo_raw_dir <- function(dataset = NULL) {
  dataset <- .msdemo_dataset_id(dataset)
  pattern <- .msdemo_raw_pattern(.msdemo_dataset_entry(dataset))
  for (path in .msdemo_raw_candidates(dataset)) {
    if (.msdemo_has_raw(path, pattern)) {
      return(normalizePath(path, winslash = "/", mustWork = TRUE))
    }
  }
  stop(
    "No files for dataset '", dataset, "' found. Run MSdemo_download_dataset(\"",
    dataset, "\").",
    call. = FALSE
  )
}

#' @title Directory for rebuilt demo project objects
#' @description Folder used by [make_demo()] for `MSdev` project output
#'   (`.Rdata`, reports). Root override: `options(MSdemo.project_dir = ...)` or
#'   `MSDEMO_PROJECT_DIR`. Each dataset is written under `<root>/<dataset>`.
#'
#' @param dataset Dataset id from [MSdemo_datasets()]. Default: option
#'   `MSdemo.dataset`, otherwise the catalog `default`.
#'
#' @return Character path (forward slashes). Created if missing.
#' @export
demo_project_dir <- function(dataset = NULL) {
  dataset <- .msdemo_dataset_id(dataset)
  path <- getOption("MSdemo.project_dir")
  if (is.null(path) || !nzchar(path)) {
    path <- Sys.getenv("MSDEMO_PROJECT_DIR", unset = "")
  }
  if (!nzchar(path)) {
    path <- .msdemo_default_project_dir()
  }
  path <- file.path(path, dataset)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' @title List demo raw files
#' @param full.names Logical. Return full paths (default `TRUE`).
#' @param dataset Dataset id from [MSdemo_datasets()].
#' @return Character vector of raw file paths or names.
#' @export
list_demo_raw <- function(full.names = TRUE, dataset = NULL) {
  dataset <- .msdemo_dataset_id(dataset)
  raw_dir <- demo_raw_dir(dataset)
  pattern <- .msdemo_raw_pattern(.msdemo_dataset_entry(dataset))
  files <- list.files(
    raw_dir,
    pattern = pattern,
    ignore.case = TRUE,
    full.names = full.names
  )
  sort(files)
}

#' @title Sample table parsed from demo file names
#' @description Build a compact sample information table from anonymized names
#'   such as `QC_pos_01`, `Blank_neg_02`, `Sample_GroupA_pos_01`.
#'
#' @param dataset Dataset id from [MSdemo_datasets()].
#'
#' @return `data.frame`
#' @export
demo_sample_info <- function(dataset = NULL) {
  files <- list_demo_raw(dataset = dataset, full.names = TRUE)
  base <- tools::file_path_sans_ext(basename(files))
  polarity <- ifelse(
    grepl("_pos_", base, ignore.case = TRUE),
    "1",
    ifelse(grepl("_neg_", base, ignore.case = TRUE), "0", "-1")
  )
  sample_type <- ifelse(
    grepl("^QC_", base, ignore.case = TRUE),
    "QC",
    ifelse(grepl("^Blank_", base, ignore.case = TRUE), "Blank", "Sample")
  )
  group <- sub("_(pos|neg)_[0-9]+$", "", base, ignore.case = TRUE)
  group <- sub("^Sample_", "", group)
  replicate <- as.integer(sub(".*_", "", base))
  data.frame(
    file = files,
    name = base,
    polarity = polarity,
    sample_type = sample_type,
    group = group,
    replicate = replicate,
    stringsAsFactors = FALSE
  )
}

.msdemo_latest_file <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) {
    return(NA_character_)
  }
  files[which.max(file.info(files)$mtime)]
}

.msdemo_default_cpdb <- function() {
  path <- "c:/Users/91879/OneDrive/Code/R/data/MSDB/CompoundDB/CFM_predicted_kegg.compdb"
  if (file.exists(path)) {
    return(path)
  }
  NA_character_
}

#' @title Load a processed demo object
#' @description Load artefacts written by [make_demo()] under [demo_project_dir()],
#'   or a named object still sitting in an older demo folder.
#'
#' @param demo Character. `"MSdev"` (default) loads the latest `MSdev_*.Rdata`
#'   via `MSdev::MSdev_load()`. Other values look for RDS/RDA files matching
#'   `XcmsExperiment`, `XCMSnExp`, `SummarizedExperiment`, or `Spectra`.
#' @param dataset Dataset id from [MSdemo_datasets()].
#'
#' @return The loaded object.
#' @export
load_demo <- function(demo = c("MSdev",
                               "XcmsExperiment", "xcms",
                               "XCMSnExp",
                               "SummarizedExperiment", "data.se",
                               "Spectra", "sp"),
                      dataset = NULL) {
  demo <- match.arg(demo)
  project_dir <- tryCatch(demo_project_dir(dataset), error = function(e) NA_character_)
  file_path <- switch(
    demo,
    "MSdev" = .msdemo_latest_file(project_dir, "^MSdev_.*\\.Rdata$"),
    "XcmsExperiment" = .msdemo_latest_file(project_dir, "XcmsExperiment.*\\.(rda|rds)$"),
    "xcms" = .msdemo_latest_file(project_dir, "XcmsExperiment.*\\.(rda|rds)$"),
    "XCMSnExp" = .msdemo_latest_file(project_dir, "XCMSnExp.*\\.(rda|rds)$"),
    "SummarizedExperiment" = .msdemo_latest_file(
      project_dir, "SummarizedExperiment.*\\.(rda|rds)$"
    ),
    "data.se" = .msdemo_latest_file(project_dir, "SummarizedExperiment.*\\.(rda|rds)$"),
    "Spectra" = .msdemo_latest_file(project_dir, "Spectra.*\\.(rda|rds)$"),
    "sp" = .msdemo_latest_file(project_dir, "Spectra.*\\.(rda|rds)$")
  )
  if (is.na(file_path) || !file.exists(file_path)) {
    stop(
      "No processed '", demo, "' demo found under ", project_dir,
      ". Run MSdemo::make_demo() first.",
      call. = FALSE
    )
  }
  if (demo == "MSdev") {
    if (!requireNamespace("MSdev", quietly = TRUE)) {
      stop("Package 'MSdev' is required to load the MSdev demo.", call. = FALSE)
    }
    return(MSdev::MSdev_load(file_path))
  }
  readRDS(file_path)
}

#' @title Rebuild the MSdev demo project from bundled raw files
#' @description Run the MSdev pipeline on [demo_raw_dir()] and save into
#'   [demo_project_dir()]. Requires **MSdev** (and **MSconvertR** for `.wiff`).
#'
#' @param dataset Dataset id from [MSdemo_datasets()].
#' @param rawDataDir Raw file directory. Default [demo_raw_dir()].
#' @param projectDir Output directory. Default [demo_project_dir()].
#' @param cpdb_path Optional CompoundDb path for annotation. If `NULL` and a
#'   local default file exists, that path is used; otherwise annotation is
#'   skipped.
#' @param convert,xcms,annotate Logical flags for pipeline steps.
#'
#' @return The processed `MSdev` object (invisibly).
#' @export
make_demo <- function(dataset = NULL,
                      rawDataDir = NULL,
                      projectDir = NULL,
                      cpdb_path = NULL,
                      convert = TRUE,
                      xcms = TRUE,
                      annotate = TRUE) {
  dataset <- .msdemo_dataset_id(dataset)
  if (is.null(rawDataDir)) {
    rawDataDir <- demo_raw_dir(dataset)
  }
  if (is.null(projectDir)) {
    projectDir <- demo_project_dir(dataset)
  }
  if (!requireNamespace("MSdev", quietly = TRUE)) {
    stop("Package 'MSdev' is required to rebuild the demo.", call. = FALSE)
  }
  if (is.null(cpdb_path)) {
    cpdb_path <- .msdemo_default_cpdb()
  }
  if (isTRUE(annotate) && (is.na(cpdb_path) || !file.exists(cpdb_path))) {
    warning("No CompoundDb found; skipping annotation.", call. = FALSE)
    annotate <- FALSE
  }
  object <- MSdev::MSdev(rawDataDir = rawDataDir, projectDir = projectDir)
  if (isTRUE(convert)) {
    object <- MSdev::MSdev_msConvert(object)
  }
  if (isTRUE(xcms)) {
    object <- MSdev::MSdev_xcmsProcessing(object)
  }
  if (isTRUE(annotate)) {
    object <- MSdev::MSdev_annotation(object, cpdb_path = cpdb_path)
  }
  MSdev::MSdev_save(object)
  invisible(object)
}
