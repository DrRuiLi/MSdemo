.msdemo_or <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (is.character(x) && !nzchar(x[[1]]))) {
    y
  } else {
    x
  }
}

.msdemo_curl_json <- function(url) {
  h <- curl::new_handle(timeout = 120, followlocation = TRUE)
  res <- curl::curl_fetch_memory(url, handle = h)
  txt <- rawToChar(res$content)
  body <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
  list(status = res$status_code, body = body)
}

.msdemo_file_row <- function(x) {
  nm <- .msdemo_or(x$key, .msdemo_or(x$filename, x$name))
  if (is.list(nm)) nm <- unlist(nm)[[1]]
  sz <- .msdemo_or(x$size, x$filesize)
  if (is.list(sz)) sz <- unlist(sz)[[1]]
  links <- x$links
  url <- NA_character_
  if (!is.null(links)) {
    url <- .msdemo_or(
      links$content,
      .msdemo_or(links$download, links$self)
    )
    if (is.list(url)) url <- unlist(url)[[1]]
  }
  data.frame(
    name = as.character(nm),
    size = as.numeric(sz),
    url = as.character(url),
    stringsAsFactors = FALSE
  )
}

.msdemo_files_from_record <- function(rec) {
  if (is.null(rec)) {
    return(NULL)
  }
  files <- rec$files
  if (is.null(files)) {
    return(NULL)
  }
  if (!is.null(files$entries)) {
    files <- files$entries
  }
  if (!length(files)) {
    return(NULL)
  }
  if (!is.null(files$key) || !is.null(files$filename)) {
    files <- list(files)
  }
  rows <- lapply(files, .msdemo_file_row)
  do.call(rbind, rows)
}

.msdemo_zenodo_file_table <- function(meta) {
  rec_id <- meta$id
  published <- .msdemo_curl_json(
    sprintf("https://zenodo.org/api/records/%s", rec_id)
  )
  if (published$status < 400) {
    tab <- .msdemo_files_from_record(published$body)
    if (!is.null(tab) && nrow(tab)) {
      return(tab)
    }
  }
  stop(
    "Could not list files for Zenodo record ", rec_id, " ",
    "(HTTP ", published$status, "). ",
    "Check the DOI and network, then retry. ",
    "Record: ", .msdemo_chr(meta$url, ""),
    call. = FALSE
  )
}

.msdemo_keep_zenodo_file <- function(name, pattern) {
  grepl(pattern, name, ignore.case = TRUE)
}

.msdemo_download_one <- function(url, destfile, quiet = FALSE) {
  h <- curl::new_handle(
    timeout = 0,
    connecttimeout = 60,
    followlocation = TRUE,
    noprogress = isTRUE(quiet)
  )
  curl::curl_download(url, destfile, handle = h, quiet = quiet)
}

#' @title Download an MSdemo dataset from Zenodo
#' @description Fetch files for one catalog entry (see [MSdemo_datasets()]).
#'   After a normal install the default destination is
#'   `<package>/extdata/<dataset>`. Existing files of the same size are skipped.
#'
#' @param dataset Dataset id from [MSdemo_datasets()]. Default: option
#'   `MSdemo.dataset`, otherwise the catalog `default`.
#' @param dest Directory to write this dataset into. If unset, files go to
#'   `file.path(find.package("MSdemo"), "extdata", dataset)` when that
#'   folder is writable.
#' @param overwrite Logical. Re-download files that already exist.
#' @param quiet Logical. Suppress curl progress.
#'
#' @return The destination path (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' MSdemo_datasets()
#' MSdemo_download_dataset()
#' MSdemo_download_dataset("lcms_wiff")
#' demo_raw_dir("lcms_wiff")
#' }
MSdemo_download_dataset <- function(dataset = NULL,
                                    dest = NULL,
                                    overwrite = FALSE,
                                    quiet = FALSE) {
  meta <- .msdemo_dataset_entry(dataset)
  dataset <- meta$dataset
  dest <- .msdemo_download_dest(dest, dataset)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  if (!.msdemo_dir_writable(dest)) {
    stop("Cannot write to ", dest, ". Pass dest = \"...\".", call. = FALSE)
  }
  dest <- normalizePath(dest, winslash = "/", mustWork = TRUE)

  files <- .msdemo_zenodo_file_table(meta)
  keep <- .msdemo_keep_pattern(meta)
  files <- files[.msdemo_keep_zenodo_file(files$name, keep), , drop = FALSE]
  files <- files[order(files$name), , drop = FALSE]
  if (!nrow(files)) {
    stop(
      "Zenodo record ", meta$id, " has no files matching ", keep, ".",
      call. = FALSE
    )
  }

  message(
    "Downloading ", nrow(files), " files (~",
    sprintf("%.1f", sum(files$size, na.rm = TRUE) / 1e9),
    " GB) for dataset '", dataset, "' from Zenodo ", meta$doi,
    "\n  -> ", dest
  )

  ok <- 0L
  skipped <- 0L
  for (i in seq_len(nrow(files))) {
    nm <- files$name[[i]]
    url <- files$url[[i]]
    sz <- files$size[[i]]
    path <- file.path(dest, nm)
    if (!overwrite && file.exists(path)) {
      local_sz <- file.size(path)
      if (!is.na(sz) && isTRUE(all.equal(as.numeric(local_sz), as.numeric(sz)))) {
        skipped <- skipped + 1L
        next
      }
    }
    if (is.na(url) || !nzchar(url)) {
      stop("No download URL for ", nm, " on Zenodo record ", meta$id, ".", call. = FALSE)
    }
    if (!quiet) {
      message(sprintf("[%d/%d] %s (%.1f MB)", i, nrow(files), nm, sz / 1e6))
    }
    tmp <- paste0(path, ".part")
    if (file.exists(tmp)) {
      unlink(tmp)
    }
    .msdemo_download_one(url, tmp, quiet = quiet)
    if (!file.exists(tmp) || isTRUE(file.size(tmp) == 0)) {
      stop("Download produced an empty file: ", nm, call. = FALSE)
    }
    if (file.exists(path)) {
      unlink(path)
    }
    ok_rename <- file.rename(tmp, path)
    if (!isTRUE(ok_rename)) {
      file.copy(tmp, path, overwrite = TRUE)
      unlink(tmp)
    }
    ok <- ok + 1L
  }

  message("Done: downloaded ", ok, ", skipped ", skipped, " in ", dest)
  .msdemo_remember_raw_dir(dataset, dest)
  invisible(dest)
}
