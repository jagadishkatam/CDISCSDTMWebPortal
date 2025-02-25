library(tidyverse)
library(DT)
library(httr2)
library(jsonlite)


api_key <- Sys.getenv("API_KEY")

adam <- readRDS("./data/endpoint_df_links.rds") |> filter(str_detect(endpoint, "adam") & str_detect(type, "Guide"))


link <- map_dfr(adam$endpoint, \(x){
  # API URL
  url <- paste0("https://api.library.cdisc.org/api", x, "/datastructures")
  # print(url)
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

  return(
    map_dfr(seq_along(endpoint$`_links`$dataStructures), \(j){
      text <- endpoint$`_links`$dataStructures[[j]]$href
      tibble(
        name = endpoint$`_links`$dataStructures[[j]]$href,
        endpoint = str_extract(endpoint$`_links`$dataStructures[[j]]$href, ".*(?=/datastructures)"),
        dataset = str_extract(text, "(?<=/datastructures/).*")
      )
    })
  )
})


adam_links <- adam |>
  left_join(link, join_by(endpoint)) |>
  mutate(
    endpoint = sapply(strsplit(endpoint, "/"), \(x) x[4])
  )

saveRDS(adam_links, "./data/adam_endpoint_df_links.rds")
