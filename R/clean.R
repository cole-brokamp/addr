#' clean address text
#'
#' remove excess whitespace keep only letters, numbers, and `-`
#' @param .x a vector of address character strings
#' @return a vector of cleaned addresses
#' @export
#' @examples
#' addr_clean("224 Woolper Ave Cincinnati OH 45220")
#' addr_clean("22_4 Woolper Ave. Cincinnati OH 45220")
#' addr_clean("22\\4 W\"oolper Ave; Ci!ncinn&*ati OH 45220")
#' addr_clean(c("224 Woolper Ave Cincinnati OH 45220",
#'              "22_4 Woolper Ave. Cincinnati OH 45220",
#'              "3333 Burnet Ave Cincinnati OH 34230"))
addr_clean <- function(.x) {
  .x |>
    stringr::str_replace_all(stringr::fixed("\\"), "") |>
    stringr::str_replace_all(stringr::fixed("\""), "") |>
    stringr::str_replace_all("[^a-zA-Z0-9- ]", "") |>
    stringr::str_squish()
}
