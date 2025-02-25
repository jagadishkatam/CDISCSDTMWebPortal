library(tidyverse)
library(DT)
library(httr2)
library(jsonlite)

api_key <- Sys.getenv("API_KEY")
# API URL
url <- "https://api.library.cdisc.org/api/mdr/products"

# Construct the request
req <- request(url) %>%
  req_headers(
    "Cache-Control" = "no-cache",
    "api-key" = api_key,
    "content-type" = "application/json"
  )

# Send the request and fetch response
resp <- req %>% req_perform()

# Parse JSON response
endpoint <- resp %>% resp_body_json()


endpoint_df <- map_dfr(1:length(endpoint$`_links`), \(x) {
  map_dfr(names(endpoint$`_links`[[x]]$`_links`), \(y){
    if (y %in% c("adam", "sdtmig", "packages")) {
      tibble(link = endpoint$`_links`[[x]]$`_links`[[y]])
    }
  })
})

endpoint_df_links <- map_dfr(endpoint_df$link, \(x) {
  if (is.list(x) && "href" %in% names(x)) {
    tibble(
      endpoint = x$href,
      type = x$type,
      title = x$title
    )
  } else {
    tibble(endw = as.character(x))
  }
}) |> filter(!is.na(endpoint))

endpoint_df_links$product <- sapply(strsplit(endpoint_df_links$endpoint, "/"), \(x) x[3])

saveRDS(endpoint_df_links, "./data/endpoint_df_links.rds")
