.msdemo_catalog_path <- function() {
  path <- system.file("extdata", "catalog.json", package = "MSdemo", mustWork = FALSE)
  if (!nzchar(path) || !file.exists(path)) {
    stop("catalog.json is missing from the MSdemo package.", call. = FALSE)
  }
  path
}

.msdemo_catalog <- function() {
  jsonlite::fromJSON(.msdemo_catalog_path(), simplifyVector = FALSE)
}

.msdemo_dataset_ids <- function(cat = .msdemo_catalog()) {
  ids <- names(cat$datasets)
  if (is.null(ids) || !length(ids)) {
    stop("catalog.json has no datasets.", call. = FALSE)
  }
  ids
}

.msdemo_dataset_id <- function(dataset = NULL) {
  cat <- .msdemo_catalog()
  ids <- .msdemo_dataset_ids(cat)
  if (is.null(dataset) || (is.character(dataset) && !nzchar(dataset[[1]]))) {
    opt <- getOption("MSdemo.dataset")
    if (!is.null(opt) && nzchar(opt)) {
      dataset <- opt
    } else {
      dataset <- cat$default
    }
  }
  dataset <- as.character(dataset)[[1]]
  if (!dataset %in% ids) {
    stop(
      "Unknown MSdemo dataset '", dataset, "'. Available: ",
      paste(ids, collapse = ", "), ".",
      call. = FALSE
    )
  }
  dataset
}

.msdemo_dataset_entry <- function(dataset = NULL) {
  dataset <- .msdemo_dataset_id(dataset)
  entry <- .msdemo_catalog()$datasets[[dataset]]
  entry$dataset <- dataset
  entry
}

.msdemo_chr <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  as.character(x[[1]])
}

.msdemo_keep_pattern <- function(entry) {
  pat <- .msdemo_chr(entry$keep, "")
  if (!nzchar(pat)) {
    "\\.(wiff|wiff\\.scan|txt)$"
  } else {
    pat
  }
}

.msdemo_raw_pattern <- function(entry) {
  pat <- .msdemo_chr(entry$raw_pattern, "")
  if (!nzchar(pat)) {
    "\\.wiff$"
  } else {
    pat
  }
}

.msdemo_dir_writable <- function(path) {
  if (!dir.exists(path)) {
    return(isTRUE(dir.create(path, recursive = TRUE, showWarnings = FALSE)))
  }
  tf <- tempfile("msdemo_", tmpdir = path)
  tryCatch({
    con <- file(tf, open = "wb")
    close(con)
    unlink(tf)
    TRUE
  }, error = function(e) FALSE)
}

.msdemo_pkg_extdata <- function() {
  pkg <- tryCatch(find.package("MSdemo", quiet = TRUE), error = function(e) "")
  if (!length(pkg) || !nzchar(pkg[[1]])) {
    return(NA_character_)
  }
  file.path(pkg[[1]], "extdata")
}

.msdemo_data_root <- function() {
  pkg_ext <- .msdemo_pkg_extdata()
  if (is.na(pkg_ext) || !.msdemo_dir_writable(pkg_ext)) {
    stop(
      "Package extdata is not writable. Pass dest = \"...\" to MSdemo_download_dataset().",
      call. = FALSE
    )
  }
  pkg_ext
}

.msdemo_download_dest <- function(dest = NULL, dataset) {
  if (!is.null(dest) && nzchar(dest[[1]])) {
    return(as.character(dest)[[1]])
  }
  file.path(.msdemo_data_root(), dataset)
}

.msdemo_remember_raw_dir <- function(dataset, dest) {
  dirs <- getOption("MSdemo.raw_dirs")
  if (is.null(dirs)) {
    dirs <- list()
  }
  dirs <- as.list(dirs)
  dirs[[dataset]] <- dest
  options(MSdemo.raw_dirs = dirs, MSdemo.dataset = dataset)
}

.msdemo_named_raw_dir <- function(dataset) {
  dirs <- getOption("MSdemo.raw_dirs")
  if (is.null(dirs)) {
    return(NULL)
  }
  dirs <- as.list(dirs)
  path <- dirs[[dataset]]
  if (is.null(path) || !nzchar(path[[1]])) {
    return(NULL)
  }
  as.character(path)[[1]]
}

#' @title Registered MSdemo datasets
#' @description Catalog stored in `inst/extdata/catalog.json`. Each row is one
#'   Zenodo record. Add another dataset by inserting an object under
#'   `datasets` and creating `inst/extdata/<dataset_id>/`.
#'
#' @return A `data.frame` with columns `dataset`, `title`, `doi`, `url`,
#'   `published`, `default`.
#' @export
#'
#' @examples
#' MSdemo_datasets()
MSdemo_datasets <- function() {
  cat <- .msdemo_catalog()
  ids <- .msdemo_dataset_ids(cat)
  rows <- lapply(ids, function(id) {
    x <- cat$datasets[[id]]
    data.frame(
      dataset = id,
      title = .msdemo_chr(x$title),
      doi = .msdemo_chr(x$doi),
      url = .msdemo_chr(x$url),
      published = isTRUE(x$published),
      default = identical(id, cat$default),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @title Zenodo record for an MSdemo dataset
#' @description Metadata from `inst/extdata/catalog.json` (DOI, record URL,
#'   API endpoint, keep/raw patterns). Use [MSdemo_download_dataset()] to
#'   fetch the files.
#'
#' @param dataset Dataset id from [MSdemo_datasets()]. Default: option
#'   `MSdemo.dataset`, otherwise the catalog `default`.
#'
#' @return A named list, including `dataset`.
#' @export
#'
#' @examples
#' MSdemo_zenodo()
#' MSdemo_zenodo("lcms_wiff")
MSdemo_zenodo <- function(dataset = NULL) {
  .msdemo_dataset_entry(dataset)
}
