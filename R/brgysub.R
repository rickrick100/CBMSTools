#' brgysub Function
#' This function allows you subtitute barangay names on barangay columns.
#' @param CBMS CORE HPQ VN05201701 main.csv
#' @keywords cbms
#' @export
#' @examples
#' brgysub()
#'
#' brgysub()

brgysub <- function(m,b) {
  m$brgy <- as.numeric(m$brgy)
  b <- merge(b,m, by="brgy")
  b <- b %>%
    select(-brgy) %>%
    arrange(main.id)
}
