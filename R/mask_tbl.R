# Quiets concerns of R CMD check regarding 'no visible binding
# for global variable ...'
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".row_id", ".mask"))
}

#' Mask table
#'
#' Mask person-sensitive data in a table
#'
#' Masks person-sensitive data in a tbl_summary object made using the gtsummary
#' package. Function should work as intended, but is very experimental and
#' insufficiently undocumented and tested.
#'
#' @param x tbl_summary object.
#'
#' @return tbl_summary object
#' @export
#'
#' @examples
#' \dontrun{
#' TODO: add example
#' }

mask_tbl <- function(x) {

  if (class(x)[1] != "tbl_summary") {
    stop("'x' must have class 'tbl_summary'", call. = FALSE)
  }

  #### Determine stat column names ####
  if (is.null(x$df_by)) {
    stat_col <- "stat_0"
    by_n <- x$N
  } else {
    stat_col <- x$df_by$by_col
    by_n <- x$df_by$n
  }
  #### Extract and augment table_body ####
  x_body <- x$table_body
  x_body[[".row_id"]] <- 1:nrow(x_body)
  for (i in seq_along(stat_col)) {
    x_body[[paste0(".", stat_col[i], "_num")]] <-
      as.numeric(sub("(\\s+).*", "\\1", x_body[[stat_col[i]]]))
  }

  #### Mask continuous variables ####

  # Not necessary in ordinary cases, but could be implemented if needed


  #### Mask binary variables ####

  # Binary variables can be masked without considering other
  # parts of the table body
  bin_var <- unique(x_body$variable[x_body$var_type == "dichotomous"])

  for (i in seq_along(bin_var)) {
    for (j in seq_along(stat_col)) {
      tmp <- eval(parse( text = paste0("x_body$.", stat_col[j], "_num")))
      x_body[[stat_col[j]]] <- ifelse(
        x_body$variable == bin_var[i] &
        0 < tmp & tmp < 5,
        "<5",
        x_body[[stat_col[j]]]
      )
    }
  }

  #### Mask categorical variables ####
  cat_var <- unique(x_body$variable[x_body$var_type == "categorical"])

  for (i in seq_along(cat_var)) {
    # extract datalines for categorical variable, excluding  the label
    i_dat <- x_body[x_body$variable == cat_var[i] & x_body$row_type != "label", ]

    # Mask
    for (j in seq_along(stat_col)) {
      i_dat[[".mask"]] <- 0
      j_col <- stat_col[j]
      j_col_num <- paste0(".", j_col, "_num")
      stop <- FALSE
      while(isFALSE(stop)) {
        # Find lowest unmasked number and corresponding row_id
        # If there are multiple rows with the same min, we pick the first row_id
        min_val <- i_dat[i_dat$.mask == 0, ]
        min_val <- suppressWarnings(min(min_val[[j_col_num]]))
        min_val_rowid <- i_dat[i_dat$.mask == 0 & i_dat[[j_col_num]] == min_val, ]
        min_val_rowid <- min_val_rowid$.row_id[1]


        # Find lowest non-zero unmasked number and corresponding row_id.
        # If there are multiple rows wit the same min, we pick the first row_id
        min_nonzero_val <- i_dat[i_dat$.mask == 0 & i_dat[[j_col_num]] != 0, ]
        min_nonzero_val <- suppressWarnings(
          min(min_nonzero_val[[j_col_num]])
        )
        min_nonzero_rowid <- i_dat[i_dat$.mask == 0 & i_dat[[j_col_num]] == min_nonzero_val, ]
        min_nonzero_rowid <- min_nonzero_rowid$.row_id[1]

        # Find number of observations masked so far
        n_masked <- sum(i_dat$.mask)

        # Find average of masked observations
        avg_masked <- i_dat[i_dat$.mask == 1, ]
        avg_masked <- mean(avg_masked[[j_col_num]])

        # If the lowest non-zero unmasked value is between 0 and 5, mask it and
        # go to next iteration
        if (0 < min_nonzero_val & min_nonzero_val < 5) {
          i_dat[[j_col]]<- ifelse(
            i_dat$.row_id ==  min_nonzero_rowid, "<5", i_dat[[j_col]]
          )
          i_dat[[".mask"]] <- ifelse(
            i_dat$.row_id == min_nonzero_rowid, 1, i_dat[[".mask"]]
          )
          next()
        # If no individual numbers needs to be masked, we move on to check that
        # no, or at least two, numbers have been masked. If not, mask the lowest
        # non-masked number
        } else if (n_masked == 1) {
          i_dat[[j_col]]<- ifelse(
            i_dat$.row_id ==  min_val_rowid, "<5", i_dat[[j_col]]
          )
          i_dat[[".mask"]] <- ifelse(
            i_dat$.row_id ==  min_val_rowid, 1, i_dat[[".mask"]]
          )
          next()
        # If also zero, or at least two, numbers have been masked, we check that
        # the average of the masked values are at least 1. If not, mask the
        # lowest non-masked number
        } else if (!is.nan(avg_masked) & avg_masked < 1) {
          i_dat[[j_col]]<- ifelse(
            i_dat$.row_id ==  min_val_rowid, "<5", i_dat[[j_col]]
          )
          i_dat[[".mask"]] <- ifelse(
            i_dat$.row_id ==  min_val_rowid, 1, i_dat[[".mask"]]
          )
          next()
        }
        # If none of the above, the variable has been sufficiently masked
        else {
          stop <- TRUE
        }
      }
    }

    # Update numbers in table_body
    tmp <- x_body[x_body$variable != cat_var[i] | x_body$row_type == "label", ]
    i_dat <- subset(i_dat, select = -.mask)
    x_body <- rbind(tmp, i_dat)
    x_body <- x_body[order(x_body[[".row_id"]]), ]
  }


  #### Replace table_body ####
  x_body <- subset(x_body, select = -.row_id)
  for (i in seq_along(stat_col)) {
    x_body <- subset(
      x_body,
      select = -eval(parse(text = paste0(".", stat_col[i], "_num")))
    )
  }
  x$table_body <- x_body

  x
}
