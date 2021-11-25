#' Tidy codes
#'
#' Tidy a character vector of codes and code ranges by cleaning the input,
#' resolving code ranges, collapsing the codes into an equivalent minimal
#' set of codes that is sorted, and where there is only one code in each
#' element of the vector.
#'
#' It is a time-consuming and error-prone process to manually make
#' code definitions based on a list of codes in a protocol, eg ICD-10 codes
#' used to define diseases in registry data. This utility function can help
#' facilitate the process. All codes in a codebook definition can simply be
#' copy+pasted into a string, and the function will clean everything up.
#'
#' The function only accepts code ranges where the start and end of the range
#' is specified at the same level, eg "AB-AD" is ok, but "20-400" is not. This
#' is because this code ranges is ambiguous: sometimes this actually means
#' 200-400.
#'
#' @param x character. Vector of codes and code ranges.
#' @param range_symbols string. `grepl` pattern used to identify range symbols.
#' @param code_sep_symbols string. `grepl` pattern used to identify code
#'  separator symbols.
#'
#' @return character
#' @export
#'
#' @examples
#' # Simple neat input
#' tidy_codes("A12-A19, B10")
#' # Messy complex input
#' tidy_codes(c("A2;B12-B14 123-125", "G98-H01", "C09-C41"))
tidy_codes <- function(
    x,
    range_symbols = "[:-]",
    code_sep_symbols = "[,\\|;\\s]") {

  # Clean codes
  x <- .clean_codes(x,
                    range_symbols = range_symbols,
                    sep_symbols = code_sep_symbols)

  # Expand code ranges
  codes <- character(0)
  for (i in seq_along(x)) {
    if (grepl("-", x[i])) {
      codes <- c(codes, .expand_code_range(x[i]))
    } else {
      codes <- c(codes, x[i])
    }
  }

  # Collapse codes
  .collapse_codes(codes)
}


#### Helper functions ####

#' Validate code range
#'
#' Validates a code range. Returns the code range, or throws an error if the
#' input is not a proper code range.
#'
#' A proper code range is on the form "from-to", where from and to is the same
#' length, and from is "lower than or equal to" to, eg "A0-A4", "1Z-2B",
#' "ABC-DAG" are valid ranges, but "1-10" and "B1-A9" are not.
#'
#' @param x character. Vector of length 1
#' @param range_sep string. Range separator symbol. Must be length 1.
#'
#' @return character
#' @keywords internal
#'
#' @examples
#' \dontrun{.validate_code_range("A-C")}
.validate_code_range <- function(x, range_sep = "-") {
  if (!(is.character(x) & length(x) == 1L)) {
    stop("'x' must be a character vector of length 1", call. = FALSE)
  }
  if (!(is.character(range_sep) & length(range_sep == 1L) &
        nchar(range_sep) == 1L)) {
    stop("'range_sep' must be a single character symbol", call. = FALSE)
  }

  # Parse code-range into a "from" and "to" part
  from <- unlist(base::strsplit(x, range_sep, fixed = TRUE))[1]
  to <- unlist(base::strsplit(x, range_sep, fixed = TRUE))[2]

  # Check that reassembling the from and to part gives the same code range.
  # This will catch invalid code ranges of the kind "A-" "A-B-D" etc.
  if (x != paste0(from, range_sep, to)) {
    stop(paste0("'", x, "' is not a valid code range"), call. = FALSE)
  }

  # Check that from and to have the same size
  if (nchar(from) != nchar(to)) {
    stop(paste0(
          "'",
          x,
          "' is not a valid code range: From and to have different sizes"
        ),
      call. = FALSE
    )
  }

  # Parse from and to codes into a vector with one character/numeric symbol in
  # each element
  from_parsed <- unlist(strsplit(from, ""))
  to_parsed <- unlist(strsplit(to, ""))

  # Determine if each level of from and to is numeric
  from_parsed_num <- logical(length(from_parsed))
  to_parsed_num <- logical(length(to_parsed))
  for (i in seq_along(from_parsed)) {
    from_parsed_num[i] <- !is.na(suppressWarnings(as.numeric(from_parsed[i])))
    to_parsed_num[i] <- !is.na(suppressWarnings(as.numeric(to_parsed[i])))
  }

  # Check that each level of from and to are both character/numeric
  if (any(from_parsed_num != to_parsed_num)) {
    stop(paste0("'",
                x,
                "' is not a valid code range: ",
                "from and to codes do not have the same structure"),
      call. = FALSE)
  }

  # Check that "from" is lower than or equal to "to" by
  # comparing each element of the parsed codes.
  from_parsed_rev <- rev(from_parsed)
  to_parsed_rev <- rev(to_parsed)
  error_msg <- paste0("'", x, "' is not a valid code range: from > to")
  n_length <- length(from_parsed_rev)
  for (i in seq_along(from_parsed_rev)) {
    # If a level of from is higher than the corresponding level in to,
    # there needs to be a higher level where from is lower than to
    # if from < to.
    if (from_parsed_rev[i] > to_parsed_rev[i]) {
      if (i == n_length) {
        stop(error_msg, call. = FALSE)
      } else {
        tmp <- TRUE
        for (j in seq_along(from_parsed_rev[(i + 1):n_length])) {
          if (from_parsed[j] < to_parsed[j]) tmp <- FALSE
        }
        if (tmp) stop(error_msg, call. = FALSE)
      }
    }
  }

  x
}


#' Collapse codes
#'
#' Collapse vector of codes, if possible.
#'
#' Attempts to collapse a vector of codes into a smaller set of codes that is
#' equivalent. For example, if the input is the codes:
#'  A0, A012, A1, A2, A3, A4, A5, A6, A7, A8, A9, B, B1
#' then the function would detect that A012 is a subcode of A0 and B1 of B, so
#' they can be removed. Furthermore
#'   A0, A1, A2, A3, A4, A5, A6, A7, A8, A9
#' is the set of all subcode chapters in A, so we can remove all these codes
#' and simply include A instead. The output of the function in this example
#' would simply be the codes: A, B.
#'
#' Note that the function is case-insensitive and will return character codes
#' in uppercase.
#'
#' @param x character.
#' @param sort_codes boolean. Sort vector of collapsed codes?
#'
#' @return character
#' @keywords internal
#'
#' @examples
#' \dontrun{.collapse_codes(c(paste0("A", 0:9), "B", "B1"))}
.collapse_codes <- function(x, sort_codes = TRUE) {
  stop <- FALSE
  idx <- 1L
  x <- unique(toupper(x))

  # Iteratively replace subchapter codes with overall chapter code if all
  # subchapters are included in 'x'.
  while (isFALSE(stop)) {
    i_code <- x[idx]

    i_chapter <- substr(i_code, 1, nchar(i_code) - 1)
    i_subcode <- substr(i_code, nchar(i_code), nchar(i_code))

    # Determine if subcode is numeric or character
    subcode_num <- TRUE
    if (is.na(suppressWarnings(as.numeric(i_subcode)))) {
      subcode_num <- FALSE
    }

    # Replace subcodes with chapter codes if all subcodes in chapter
    # is included in 'x'
    if (subcode_num) {
      if (i_chapter != "" & all(paste0(i_chapter, 0:9) %in% x)) {
        x <- x[!x %in% paste0(i_chapter, 0:9)]
        x <- c(x, i_chapter)
        # return to start of vector
        idx <- 1L
        next()
      }
    }
    if (!subcode_num) {
      if (i_chapter != "" & all(paste0(i_chapter, LETTERS) %in% x)) {
        x <- x[!x %in% paste0(i_chapter, LETTERS)]
        x <- c(x, i_chapter)
        # return to start of vector
        idx <- 1L
        next()
      }
    }

    idx <- idx + 1L
    # If we checked all codes in the vector, exit the loop
    if (idx > length(x)) stop <- TRUE
  }

  # Remove subcode if any chapter code in which the subscode belongs,
  # is also included
  codes <- character(0)
  for (i in seq_along(x)) {
    chapter <- x[i]
    keep_code <- TRUE
    while (nchar(chapter) > 1) {
      chapter <- substr(chapter, 1, nchar(chapter) - 1)
      if (chapter %in% x) {
        keep_code <- FALSE
        break()
      }
    }
    if (keep_code) {
      codes <- c(codes, x[i])
    }
  }

  if (sort_codes) codes <- sort(codes)

  codes
}

#' Expand code range
#'
#' Expand a code range into a a character vector with the codes the range
#' represents.
#'
#' @param x Character. Vector of length 1. Code range to be expanded
#' @param range_sep string. Range separator symbol. Must be length 1.
#'
#' @return character
#' @keywords internal
#'
#' @examples
#' \dontrun{.expand_code_range("A-C")}
.expand_code_range <- function(x, range_sep = "-") {

  x <- .validate_code_range(x)

  from <-  unlist(base::strsplit(x, range_sep, fixed = TRUE))[1]
  to <- unlist(base::strsplit(x, range_sep, fixed = TRUE))[2]

  codes <- character(0)
  current_code <- from
  # Expand code range by "increasing" the code one step at a time.
  while (TRUE) {
    # Add code to vector
    codes <- c(codes, current_code)

    # End loop if end of code range has been reached, or increase the code
    if (current_code == to) {
      break()
    } else {
      current_code = .next_code(current_code)
    }
  }

  codes
}


#' Find next code
#'
#' Find the "next" code.
#'
#' @param x character
#'
#' @return character
#' @keywords internal
#'
#' @examples
#' \dontrun{.next_code("AB2")}
.next_code <- function(x) {
  # Parse code
  x_parsed <- unlist(strsplit(x, split = ""))
  x_length <- nchar(x)

  # Determine if each level of the code is numeric
  is_num <- logical(x_length)
  for (i in seq_along(x_parsed)) {
    if (!is.na(suppressWarnings(as.numeric(x_parsed[i])))) {
      is_num[i] <- TRUE
    }
  }

  # Determine if each level of the code can be increased or if the
  # level needs to be reset and the chapter increased when increasing
  # the code
  is_max <- logical(x_length)
  for (i in seq_along(x_parsed)) {
    i_code <- x_parsed[i]
    if (is_num[i]) {
      if (i_code == "9") is_max[i] <- TRUE
    } else {
      if (i_code == "Z") is_max[i] <- TRUE
    }
  }

  # If all levels of the codes are maxed out, we can't increase the code
  # in a well-defined way
  if (all(is_max)) {
    stop(paste0("Code '", x, "' can't be increased"), call. = FALSE)
  }

  # Find the next value for all levels of the code
  all_numbers <- as.character(0:9)
  next_value <- character(x_length)
  for (i in seq_along(x_parsed)) {
    i_value <- x_parsed[i]
    if (is_num[i]) {
      if (i_value == "9") {
        next_value[i] <- "0"
      } else {
        next_value[i] <- all_numbers[which(i_value == all_numbers) + 1]
      }
    } else {
      if (i_value == "Z") {
        next_value[i] <- "A"
      } else {
        next_value[i] <- LETTERS[which(i_value == LETTERS) + 1]
      }
    }
  }

  # Determine next code
  next_code_rev <- rev(x_parsed)
  next_value_rev <- rev(next_value)
  for (i in seq_along(next_code_rev)) {
    # Alway increase the last level
    if (i == 1) {
      next_code_rev[i] <- next_value_rev[i]
    } else {
      # If all previous levels have reset increase the level
      if (all(rev(is_max)[1:(i-1)])) {
        next_code_rev[i] <- next_value_rev[i]
      }
    }
  }

  paste(rev(next_code_rev), collapse = "")
}


#' Clean codes
#'
#' Cleans a vector of codes and/or code ranges by streamlining symbols used for
#' code ranges (dashes) and splitting the vector so that each code(range) is in
#' a separate element.
#'
#' By default the function removes any period symbols present in the input,
#' assumed to be part of the codes. The function will also throw an error if
#' the return vector contains unexpected non-alphanumeric values, assumed to
#' mean that the input character vector includes symbols that have not been
#' properly handled.
#'
#' Note that the function is case-insensitive and will convert any lowercase
#' letters to uppercase.
#'
#' @param x character. Vector of codes and code ranges to tidy.
#' @param remove_periods logical. Remove periods, assumed to be part of codes?
#' @param check_if_clean logical. Check if return vector has been properly
#' cleaned, ie contains no non-alphanumeric symbols
#' (excluding dashes and spaces)?
#' @param range_symbols string. `grepl` pattern used to identify range symbols
#' @param sep_symbols string. `grepl` pattern used to identify code separator
#' symbols.
#'
#' @return character
#' @keywords internal
#'
#' @examples
#' \dontrun{.clean_codes(c("A.1", "123", "A123,B43; K45 K67 A1-B2", "B:D"))}
.clean_codes <- function(x,
                        remove_periods = TRUE,
                        check_if_clean = TRUE,
                        range_symbols = "[-:]",
                        sep_symbols = "[,\\|;\\s]") {

  if (!is.character(x)) {
    stop("'x' is not a character vector", call. = FALSE)
  }
  if (!is.logical(remove_periods)) {
    stop("'remove_periods' is not logical", call. = FALSE)
  }
  if (!is.logical(check_if_clean)) {
    stop("'check_if_clean' is not logical", call. = FALSE)
  }
  if (!(is.character(range_symbols) & length(range_symbols) == 1)) {
    stop("'range_symbols' must be a character vector of length 1", call. = FALSE)
  }
  if (!(is.character(sep_symbols) & length(sep_symbols) == 1)) {
    stop("'sep_symbols' must be a character vector of length 1", call. = FALSE)
  }

  x <- toupper(x)

  # Remove period symbols
  if (remove_periods) {
    x <- gsub("\\.", "", x)
  }

  # Replace code range symbols with dashes
  x <- gsub(range_symbols, "-", x)

  # Replace separator symbols with spaces
  x <- gsub(sep_symbols, " ", x)

  # Trim leading and trailing whitespace, and replace one or more spaces in a
  # row with a single space
  x <- gsub("\\s+", " ", trimws(x))

  # Split codes into a vector with a code(range) in each element
  x <- unlist(base::strsplit(x, " "))

  # Throw error if any unexpected non-alphanumeric symbols left
  if (check_if_clean & any(!grepl("^[[:alnum:]-]*$", x))) {
    stop("Unexpected non-alphanumeric symbols in '", x, "'",
        call. = FALSE)
  }

  x
}
