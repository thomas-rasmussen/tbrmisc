# Function that takes an input data.frame then smooth dataline based
# on dates variables, then output new data.frame. Make options to
# keep other (constant) variables in the data, and make it possible
# to do the smoothing based on an arbitrary number of grouping variables
# specified through the ellipsis argument.
# 1) start function by sorting data base don gorpuing variables
# 2) make new combined group variable
# 3) Do smoothing while keepding / dropping au variables
# 4) return smoothed data
# 5) Make input checks on date variables. Hvae to be compatible wiht dates
# eg poxit thing, date, or integer?
# Use data.table functions and data.table for efficiency? Probably yes. Need to add
# ad dependency in package. Make sure to test that eveything is done correclty.
# Remember to add tests
# Add input check that x is data.frame, start and end is date-compatible variables
# in x, and that variables in keep_vars exixsts in x. Also check any other group
# variables exixsts in x.


library(data.table)
smooth_periods <- function(x,
                           start,
                           end,
                           group = NULL,
                           max_gap = 1L,
                           keep = NULL) {

dat <- as.data.table(x)[][, c(start, end, group, keep), with = FALSE]
old_names <- c(start, end)
new_names <- c("start", "end")
if (!is.null(group)) {
  old_names <- c(group, old_names)
  new_names <- c("group", new_names)
}
if (!is.null(keep)) {
  old_names <- c(keep, old_names)
  new_names <- c("keep", new_names)
}
setnames(dat, old_names, new_names)

order_vars <- c("start", "end")
if (!is.null(group)) {
  order_vars <- c("group", "start", "end")
}
setorderv(dat, order_vars)

dat[, `:=`(
  new_start = (
    is.na(shift(end)) | group != shift(group) | start > (shift(end) + max_gap)
  )
)][,
   new_end := shift(new_start, n = -1L, fill = TRUE)
]

smoothed <- data.table(
  start = dat$start[dat$new_start],
  end = dat$end[dat$new_end]
)

if (!is.null(group)) {
  smoothed[, group := dat$group[dat$new_start]]
}

if (!is.null(keep)) {
  smoothed[, keep := dat$keep[dat$new_start]]
}

smoothed
}


