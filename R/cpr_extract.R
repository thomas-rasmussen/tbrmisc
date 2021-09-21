#' Extract birthdate or biological sex encoded in CPR-numbers
#'
#' Extract information about dates of birth or biological sex, from Danish
#' CPR-numbers.
#'
#' More information on Danish CPR-numbers (in Danish) can be found at
#' https://cpr.dk/cpr-systemet/opbygning-af-cpr-nummeret.
#'
#' @param x Character vector. Vector of CPR-numbers
#' @param extract String. "birthdate" or "sex" depending on what is to be
#' extracted. Sex will be returned as a zero/one vector, where one's indicates
#' males.
#' @param test_modulus11 logical. Test if CPR-numbers fulfill the modulus 11 test?
#' If true, will return error if the modulus 11 test fails for one or more
#' CPR-numbers
#'
#' @return vector
#' @export
#'
#' @examples
#' cpr_extract("0101001234")
cpr_extract <- function(
    x,
    extract = c("birthdate", "sex"),
    test_modulus11 = FALSE) {

  #### Input checks ####
  if (!is.character(x)) {
    stop("'x' is not a character vector", call. = FALSE)
  }
  extract <- match.arg(extract)
  if (!is.logical(test_modulus11)) {
    stop("'test_modulus11' must be of type logical ", call. = FALSE)
  }
  # Remove symbols/white-space sometimes present in CPR-numbers
  x <- gsub("[[:punct:][:space:]]", "", x)
  # Check that all CPR-numbers have the correct length
  if (!all(nchar(x) == 10L)) {
    stop(
      "One or more CPR-number in 'x' does not have length 10.",
      call. = FALSE
    )
  }
  # Check that no CPR-number contains letters.
  if (any(grepl("\\D", x))) {
    stop(
      "One or more CPR-numbers contains letters, ie is not a proper CPR number.",
      call. = FALSE)
  }

  #### Extract information from CPR-numbers ####

  # Extract sex encoded in the last digit: Uneven numbers are males
  cpr_male <- as.numeric(substr(x, 10, 10)) %% 2

  # Extract birthdate using a set of convoluted rules. Find more information
  # at https://cpr.dk/cpr-systemet/opbygning-af-cpr-nummeret
  # Day of birth
  cpr_dd <- as.numeric(substr(x, 1, 2))
  # Month of birth
  cpr_mm <- as.numeric(substr(x, 3, 4))
  # Decade and year of birth
  cpr_yy <- as.numeric(substr(x, 5, 6))
  # 7th digit of the cpr-number - used together with cpr_yy to determine
  # century of birth
  cpr_7 <- as.numeric(substr(x, 7, 7))

  cpr_yyyy <- ifelse(
    cpr_7 %in% c(0, 1, 2, 3),
    as.numeric(paste0("19", formatC(cpr_yy, width = 2, flag = "0"))),
    ifelse(cpr_yy > 36 & cpr_7 %in% c(4, 9),
      as.numeric(paste0("19", formatC(cpr_yy, width = 2, flag = "0"))),
      ifelse(
        cpr_yy <= 36 & cpr_7 %in% c(4, 9),
        as.numeric(paste0("20", formatC(cpr_yy, width = 2, flag = "0"))),
        ifelse(
          cpr_yy < 58 & cpr_7 %in% c(5,6,7,8),
          as.numeric(paste0("20", formatC(cpr_yy, width = 2, flag = "0"))),
          ifelse(
            cpr_yy >= 58 & cpr_7 %in% c(5,6,7,8),
            as.numeric(paste0("18", formatC(cpr_yy, width = 2, flag = "0"))),
            NA
          )
        )
      )
    )
  )

  cpr_birthdate <- paste0(cpr_yyyy, "-", cpr_mm, "-", cpr_dd)

  # Determine if valid birthdates in cpr numbers
  if (!all(!is.na(as.Date(cpr_birthdate, format = '%Y-%m-%d')))) {
    stop("'x' contains one or more cases with invalid encoded birthdates.", call. = FALSE)
  }
  cpr_birthdate <- as.Date(cpr_birthdate)


  #### Modulus 11 test ####
  if (test_modulus11) {
    weights <- c(4, 3, 2, 7, 6, 5, 4, 3, 2, 1)
    sum <- rep(0, length(x))
    for (i in 1:10) {
      sum <- sum + weights[i] * as.numeric(substr(x, i, i))
    }
    if (any(sum %% 11 != 0)) {
      stop("One or more CPR-number does not fulfill the modulus 11 test", call. = FALSE)
    }
  }


  #### return ####

  if (extract == "birthdate") {
    return(cpr_birthdate)
  } else if (extract == "sex"){
    return(cpr_male)
  }
}
