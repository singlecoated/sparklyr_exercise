read_sales <- function(shop_ids, years, months) {
  sdf <- NULL
  for (shop_id in shop_ids) {
    for (year in years) {
      for (month in months) {
        new_sdf <- read_sale(shop_id, year, month)
        if (!is.null(sdf)) {
          if (!is.null(new_sdf)) {
            sdf <- union_all(sdf, new_sdf)
          }
        } else {
          sdf <- new_sdf
        }
      }
    }
  }
  sdf
}