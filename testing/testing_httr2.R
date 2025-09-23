

test_embeddings <- function ( port_number = 8081, api_key = "sealsaretasty", input = "test") {
    print("FUNCTION CALL STARTING")

    blackfish_url <- paste("http://localhost:", port_number, "/v1/embeddings", sep = "")
    print(blackfish_url)

    format_request <- function(input, base_url = blackfish_url) {
      print("FORMATTING REQUEST")
      headers <- c(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      )
      body <- list(
        messages = list(
            list(role = "user", content = input) 
        )
    )
      httr2::request(base_url) |>
        # headers
        httr2::req_headers(!!!headers) |>
        # body
        httr2::req_body_json(body)
    }

    req <- format_request("test")
    print("FORMATTING COMPLETE:")
    print(req)

    print("DRY RUN:")
    dry_run <- httr2::req_dry_run(req)
    print(dry_run)

    print("MAKING CALL:")
    resp <- httr2::req_perform(req)
    print(resp)
}

test_embeddings()