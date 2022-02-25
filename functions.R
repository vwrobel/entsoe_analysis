reset_mongo <- function() {
  collections %>%
    lapply(function(con) {
      con$drop()
    })
  return()
}
