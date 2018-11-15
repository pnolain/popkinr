#' Increase the number of dependent variable output sampling
#'
#' @param dataset A NONMEM dataset represented by a data frame
#' @param periods An input list defining the increase of the output sampling. Each list item must be a list with two elements:
#' \itemize{ \item \code{times}: numeric sequence of the times of output
#' \item \code{cmt}: integer corresponding to the target compartment }
#' @param columns_to_fill A character vector of the names of the columns whose values will be carried forward in the new rows.
#'
#' @return A data frame
#'
#' @examples
#' \dontrun{
#' # Read some dataset
#' ds <- readr::read_csv("some_dataset.csv", na = ".")
#'
#' # increase dataset sampling at 2 periods
#' # - 0 to 10, by 0.1 in CMT 1
#' # - 100 to 300, by 10 in CMT 1
#' p <- list(first = list(times = seq(0, 10, 0.1), cmt = 1L),
#'           second = list(times = seq(100, 700, 20), cmt = 1L))
#'
#' # all dataset columns but "AMT", "DV", "RATE", "SS", "II", "ADDL"
#' fill_columns <- setdiff(colnames(ds), c("AMT", "DV", "RATE", "SS", "II", "ADDL"))
#'
#' inflated_ds <- enrich_dataset(ds, p, fill_columns)
#' }
enrich_dataset <- function(dataset, periods = NULL, columns_to_fill = NULL){

  if(!is.null(columns_to_fill))
    stopifnot(all(columns_to_fill %in% colnames(dataset)))

  dataset %>%
    split(.$ID) %>% # split by ID
    map_df(function(x){
      # Prepare rows to insert
      new_rows <- map_df(periods, function(p){
        cmt_times <- x %>% filter(CMT == p$cmt) %>% pull(TIME)

        tibble(TIME = rep(setdiff(p$times, cmt_times), length(p$cmt)),
               CMT = rep(p$cmt, each = length(TIME) / length(p$cmt)))
      })

      df <- x %>%
        add_row(TIME = new_rows$TIME, CMT = new_rows$CMT, EVID = 2L, MDV = 1L)  %>%  # add the new rows
        mutate(EVID = factor(EVID, levels = c(3, 1, 4, 0, 2))) %>% # convert EVID column to factor, for sorting
        arrange(TIME, CMT, EVID) # sort

      if(!is.null(columns_to_fill)){
        df <- df %>% fill(one_of(columns_to_fill))
      } else {
        df <- df %>% fill(everything())
      }
    })
}
