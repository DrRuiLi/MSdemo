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
#' @description Same folder as [demo_raw_dir()]. [make_demo()] writes the
#'   `MSdev_*.Rdata` object next to the downloaded raw files.
#'
#' @param dataset Dataset id from [MSdemo_datasets()]. Default: option
#'   `MSdemo.dataset`, otherwise the catalog `default`.
#'
#' @return Character path (forward slashes).
#' @export
demo_project_dir <- function(dataset = NULL) {
  demo_raw_dir(dataset)
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

.msdemo_demo_kinds <- function() {
  c("MSdev",
    "XcmsExperiment", "xcms",
    "XCMSnExp",
    "SummarizedExperiment", "data.se",
    "Spectra", "sp")
}

.msdemo_demo_rds_name <- function(demo) {
  switch(
    demo,
    "MSdev" = NA_character_,
    "XcmsExperiment" = "XcmsExperiment.rds",
    "xcms" = "XcmsExperiment.rds",
    "XCMSnExp" = "XCMSnExp.rds",
    "SummarizedExperiment" = "SummarizedExperiment.rds",
    "data.se" = "SummarizedExperiment.rds",
    "Spectra" = "Spectra.rds",
    "sp" = "Spectra.rds",
    NA_character_
  )
}

.msdemo_demo_glob <- function(demo) {
  switch(
    demo,
    "MSdev" = "^MSdev_.*\\.Rdata$",
    "XcmsExperiment" = "XcmsExperiment.*\\.(rda|rds)$",
    "xcms" = "XcmsExperiment.*\\.(rda|rds)$",
    "XCMSnExp" = "XCMSnExp.*\\.(rda|rds)$",
    "SummarizedExperiment" = "SummarizedExperiment.*\\.(rda|rds)$",
    "data.se" = "SummarizedExperiment.*\\.(rda|rds)$",
    "Spectra" = "Spectra.*\\.(rda|rds)$",
    "sp" = "Spectra.*\\.(rda|rds)$",
    NA_character_
  )
}

.msdemo_find_demo_file <- function(dir, demo) {
  rds <- .msdemo_demo_rds_name(demo)
  if (!is.na(rds)) {
    path <- file.path(dir, rds)
    if (file.exists(path)) {
      return(path)
    }
  }
  .msdemo_latest_file(dir, .msdemo_demo_glob(demo))
}

.msdemo_nonempty <- function(x) {
  !is.null(x) && !identical(x, NA) && !(isS4(x) && methods::is(x, "Spectra") && length(x) == 0L)
}

.msdemo_xcms_from_msdev <- function(object) {
  if (!is.list(object@xcmsData) || !length(object@xcmsData)) {
    return(NULL)
  }
  pos <- object@xcmsData$PositiveMS1
  if (is.null(pos) || identical(pos, NA)) {
    pos <- object@xcmsData$Positive
  }
  if (!.msdemo_nonempty(pos)) {
    return(NULL)
  }
  pos
}

.msdemo_as_xcmsnexp <- function(x) {
  if (inherits(x, "XCMSnExp") && !inherits(x, "XcmsExperiment")) {
    return(x)
  }
  out <- tryCatch(methods::as(x, "XCMSnExp"), error = function(e) NULL)
  if (inherits(out, "XCMSnExp")) {
    return(out)
  }
  tryCatch({
    peaks <- as.matrix(xcms::chromPeaks(x))
    fdef <- S4Vectors::DataFrame(as.data.frame(xcms::featureDefinitions(x)))
    mfd <- methods::new(
      "MsFeatureData",
      chromPeaks = peaks,
      featureDefinitions = fdef
    )
    xe <- methods::new("XCMSnExp")
    xe@msFeatureData <- mfd
    xe
  }, error = function(e) NULL)
}

.msdemo_xcmsnexp_from_xcms <- function(xcms_obj) {
  if (is.null(xcms_obj) || (is.list(xcms_obj) && !isS4(xcms_obj))) {
    return(NULL)
  }
  .msdemo_as_xcmsnexp(xcms_obj)
}

.msdemo_save_rds <- function(object, path) {
  saveRDS(object, file = path)
  message("Saved: ", normalizePath(path, winslash = "/", mustWork = FALSE))
}

.msdemo_even_idx <- function(n_sp, n_keep) {
  n_keep <- min(as.integer(n_keep), as.integer(n_sp))
  if (n_keep <= 0L) {
    return(integer(0))
  }
  if (n_keep >= n_sp) {
    return(seq_len(n_sp))
  }
  unique(as.integer(round(seq(1, n_sp, length.out = n_keep))))
}

.msdemo_subset_spectra <- function(sp, n = 1000L) {
  n_sp <- length(sp)
  if (!n_sp) {
    return(sp)
  }
  n_keep <- min(as.integer(n), n_sp)
  if (n_keep < n_sp) {
    ms <- tryCatch(as.integer(Spectra::msLevel(sp)), error = function(e) NULL)
    if (!is.null(ms) && length(unique(ms)) > 1L) {
      idx <- integer(0)
      groups <- split(seq_len(n_sp), ms)
      n_left <- n_keep
      n_g <- length(groups)
      for (i in seq_len(n_g)) {
        gi <- groups[[i]]
        take <- if (i == n_g) {
          n_left
        } else {
          max(1L, as.integer(round(n_keep * length(gi) / n_sp)))
        }
        take <- min(take, length(gi), n_left)
        idx <- c(idx, gi[.msdemo_even_idx(length(gi), take)])
        n_left <- n_left - take
      }
      idx <- unique(idx)
      if (length(idx) < n_keep) {
        extra <- setdiff(seq_len(n_sp), idx)
        need <- min(n_keep - length(idx), length(extra))
        if (need > 0L) {
          idx <- c(idx, extra[.msdemo_even_idx(length(extra), need)])
        }
      }
      sp <- sp[sort(unique(idx))]
    } else {
      sp <- sp[.msdemo_even_idx(n_sp, n_keep)]
    }
  }
  sp <- tryCatch(
    Spectra::setBackend(sp, Spectra::MsBackendMemory()),
    error = function(e) sp
  )
  message("Spectra demo: kept ", length(sp), " / ", n_sp, " spectra")
  sp
}

.msdemo_write_demo_objects <- function(object, dest) {
  xcms_obj <- .msdemo_xcms_from_msdev(object)
  if (!is.null(xcms_obj)) {
    .msdemo_save_rds(xcms_obj, file.path(dest, "XcmsExperiment.rds"))
    xcmsnexp <- .msdemo_xcmsnexp_from_xcms(xcms_obj)
    if (!is.null(xcmsnexp)) {
      .msdemo_save_rds(xcmsnexp, file.path(dest, "XCMSnExp.rds"))
    } else {
      message("Skipping XCMSnExp demo: could not coerce xcms result.")
    }
  } else {
    message("Skipping XcmsExperiment / XCMSnExp demos: no xcmsData.")
  }

  se <- object@advancedAna$feature.se
  if (.msdemo_nonempty(se) && inherits(se, "SummarizedExperiment")) {
    .msdemo_save_rds(se, file.path(dest, "SummarizedExperiment.rds"))
  } else {
    message("Skipping SummarizedExperiment demo: feature.se is empty.")
  }

  sp <- tryCatch(
    MSdev::get_MSdev_Spectra(object),
    error = function(e) NULL
  )
  if (.msdemo_nonempty(sp) && methods::is(sp, "Spectra")) {
    sp <- .msdemo_subset_spectra(sp, n = 1000L)
    .msdemo_save_rds(sp, file.path(dest, "Spectra.rds"))
  } else {
    message("Skipping Spectra demo: no stored MS1/MS2 spectra.")
  }
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
#' @description Load artefacts written by [make_demo()] next to the downloaded
#'   raw files ([demo_raw_dir()]). Same keys as `MSdev::load_demo()`:
#'   `"MSdev"`, `"XcmsExperiment"` / `"xcms"`, `"XCMSnExp"`,
#'   `"SummarizedExperiment"` / `"data.se"`, `"Spectra"` / `"sp"`.
#'
#' @param demo Character. Object kind to load. Default `"MSdev"`.
#' @param dataset Dataset id from [MSdemo_datasets()].
#'
#' @return The loaded object (`MSdev`, positive `XcmsExperiment`, `XCMSnExp`,
#'   `SummarizedExperiment`, or `Spectra`).
#' @export
#'
#' @examples
#' \dontrun{
#' make_demo()
#' load_demo("MSdev")
#' load_demo("xcms")
#' load_demo("XcmsExperiment")
#' load_demo("XCMSnExp")
#' load_demo("data.se")
#' load_demo("sp")
#' }
load_demo <- function(demo = c("MSdev",
                               "XcmsExperiment", "xcms",
                               "XCMSnExp",
                               "SummarizedExperiment", "data.se",
                               "Spectra", "sp"),
                      dataset = NULL) {
  demo <- match.arg(demo)
  raw_dir <- demo_raw_dir(dataset)
  file_path <- .msdemo_find_demo_file(raw_dir, demo)
  if (is.na(file_path) || !file.exists(file_path)) {
    stop(
      "No processed '", demo, "' demo found under ", raw_dir,
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

#' @title Run the MSdev pipeline and write demo objects
#' @description Create an `MSdev` object from [demo_raw_dir()], run convert /
#'   xcms / annotation, then save these artefacts in that same folder:
#'   `MSdev_*.Rdata`, positive `XcmsExperiment.rds`, `XCMSnExp.rds`,
#'   `SummarizedExperiment.rds`, and `Spectra.rds` (~1000 spectra from the
#'   full MS1+MS2 object). Requires **MSdev** (and **MSconvertR** for `.wiff`).
#'   [load_demo()] reads them back.
#'
#' @param dataset Dataset id from [MSdemo_datasets()].
#' @param rawDataDir Raw file directory. Default [demo_raw_dir()]. Also used
#'   as the project directory (object, `msData/`, reports).
#' @param cpdb_path Optional CompoundDb path for annotation. If `NULL` and a
#'   local default file exists, that path is used; otherwise annotation is
#'   skipped.
#' @param convert,xcms,annotate Logical flags for pipeline steps.
#'
#' @return The processed `MSdev` object (invisibly).
#' @export
make_demo <- function(dataset = NULL,
                      rawDataDir = NULL,
                      cpdb_path = NULL,
                      convert = TRUE,
                      xcms = TRUE,
                      annotate = TRUE) {
  dataset <- .msdemo_dataset_id(dataset)
  if (is.null(rawDataDir)) {
    rawDataDir <- demo_raw_dir(dataset)
  }
  .msdemo_remember_raw_dir(dataset, rawDataDir)
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
  object <- MSdev::MSdev(rawDataDir = rawDataDir, projectDir = rawDataDir)
  if (isTRUE(convert)) {
    if (!requireNamespace("MSconvertR", quietly = TRUE)) {
      stop("Package 'MSconvertR' is required to convert vendor files.", call. = FALSE)
    }
    # MSconvertR uses %>% via Depends: tidyverse; :: does not attach it.
    if (!requireNamespace("magrittr", quietly = TRUE) ||
        !suppressPackageStartupMessages(require("magrittr", quietly = TRUE, character.only = TRUE))) {
      stop("Package 'magrittr' is required to convert vendor files.", call. = FALSE)
    }
    object <- MSdev::MSdev_msConvert(object)
  }
  if (isTRUE(xcms)) {
    object <- MSdev::MSdev_xcmsProcessing(object)
    object <- MSdev::MSdev_get_Se(object)
  }
  if (isTRUE(annotate)) {
    object <- MSdev::MSdev_annotation(object, cpdb_path = cpdb_path)
  }
  MSdev::MSdev_save(object)
  .msdemo_write_demo_objects(object, rawDataDir)
  invisible(object)
}
