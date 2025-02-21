
library(tidyverse)
library(DT)
library(httr2)
library(jsonlite)

adam <- readRDS("./data/endpoint_df_links.rds") |> filter(str_detect(endpoint,'adam') & str_detect(type,'Guide'))

readRDS("./data/endpoint_df.rds")

link <- map_dfr(adam$endpoint, \(x){
# API URL
url <- paste0("https://api.library.cdisc.org/api",x,"/datastructures")
# print(url)
# Construct the request
req <- request(url) %>%
  req_headers(
    "Cache-Control" = "no-cache",
    "api-key" = "ba3d68879a224d8090406948f8155bae",
    "content-type" = "application/json"
  )

# Send the request and fetch response
resp <- req %>% req_perform()

# Parse JSON response
endpoint <- resp %>% resp_body_json()

return(
  map_dfr(seq_along(endpoint$`_links`$dataStructures), \(j){
    text <- endpoint$`_links`$dataStructures[[j]]$href
  tibble(name=endpoint$`_links`$dataStructures[[j]]$href,
         endpoint=str_extract(endpoint$`_links`$dataStructures[[j]]$href,'.*(?=/datastructures)'),
         dataset = str_extract(text,'(?<=/datastructures/).*')
         )
})
  
)

})


adam_links <- adam |> left_join(link, join_by(endpoint)) |> mutate(
  endpoint=sapply(strsplit(endpoint,'/'),\(x) x[4])
)


saveRDS(adam_links,"./data/adam_endpoint_df_links.rds")





  # API URL
  url <- paste0("https://api.library.cdisc.org/api/mdr/adam/adam-adae-1-0/datastructures/ADAE")
  # print(url)
  # Construct the request
  req <- request(url) %>%
    req_headers(
      "Cache-Control" = "no-cache",
      "api-key" = "ba3d68879a224d8090406948f8155bae",
      "content-type" = "application/json"
    )
  
  # Send the request and fetch response
  resp <- req %>% req_perform()
  
  # Parse JSON response
  endpoint <- resp %>% resp_body_json()
  

  adam_df <- map_dfr(seq_along(endpoint$analysisVariableSets), function(x) {
    map_dfr(seq_along(endpoint$analysisVariableSets[[x]]$analysisVariables), function(j) {
      # Extract the codelist value from the current analysis variable

      tibble(
        name                  = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$name,
        label                 = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$label,
        description           = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$description,
        core                  = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$core,
        simpleDatatype        = endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$simpleDatatype,
        ordinal               = endpoint$analysisVariableSets[[x]]$ordinal,
        analysisVariableSets  = endpoint$analysisVariableSets[[x]]$name,
        codelist              = if (length(str_extract(endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$`_links`$codelist[[1]]$href, "C.*")) == 0) NA_character_ 
        else str_extract(endpoint$analysisVariableSets[[x]]$analysisVariables[[j]]$`_links`$codelist[[1]]$href, "C.*")
      )
    })
  })


  ct <- readRDS("./data/endpoint_df.rds") |> filter(str_detect(endpoint,'adam') & str_detect(type,'Guide'))
  

            