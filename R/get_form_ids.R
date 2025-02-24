#' Get Form IDs from Formdesk API
#'
#' This function retrieves form data from the Formdesk API and returns it as a dataframe.
#'
#' @param base_url The base URL of the Formdesk API. If NULL, it will attempt to read from the FORMDESK_BASE_URL environment variable.
#' @param api_key The API key for authentication. If NULL, it will attempt to read from the FORMDESK_API_KEY environment variable.
#'
#' @return A dataframe containing form information with columns:
#'   \itemize{
#'     \item id - The form ID
#'     \item name - The form name
#'     \item user_id - The user ID associated with the form
#'     \item resourceType - The type of resource
#'     \item created - The creation date of the form
#'     \item lastModified - The last modification date of the form
#'     \item location - The API endpoint for the specific form
#'   }
#'   Returns NULL if there was an error fetching or processing the data.
#'
#' @import httr
#' @import purrr
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' # Assuming FORMDESK_API_KEY and FORMDESK_BASE_URL are set as environment variables
#' df <- get_form_ids()
#'
#' # Or with explicit base_url and api_key
#' df <- get_form_ids("https://www.formdesk.com/api/rest/v1/your_account/forms/", "your_api_key")
#' }
#'
#' @export
get_form_ids <- function(base_url = NULL, api_key = NULL) {

  # 1. Handle API Key and Base URL
  if (is.null(api_key)) {
    api_key <- Sys.getenv("FORMDESK_API_KEY")
    if (api_key == "") {
      stop("API key not provided and FORMDESK_API_KEY environment variable not set.")
    }
  }

  if (is.null(base_url)) {
    base_url <- Sys.getenv("FORMDESK_BASE_URL")
    if (base_url == "") {
      stop("Base URL not provided and FORMDESK_BASE_URL environment variable not set.")
    }
  }

  # Ensure the base_url ends with a forward slash
  if (!grepl("/$", base_url)) {
    base_url <- paste0(base_url, "/")
  }

  # 2. Construct the API Endpoint
  forms_endpoint <- base_url

  # 3. Fetch Data from the API
  response <- tryCatch({
    GET(
      forms_endpoint,
      add_headers("Authorization" = paste("Bearer", api_key))
    )
  },
  error = function(e) {
    cat("Error during API request:", e$message, "\n")
    return(NULL)  # Return NULL on request error
  })

  # 4. Process the response
  if (status_code(response) == 200) {
    content <- httr::content(response)

    # Convert the list to a dataframe
    df <- map_df(content, ~ {
      list(
        id = .x$id,
        name = .x$name,
        user_id = .x$user_id,
        resourceType = .x$meta$resourceType,
        created = .x$meta$created,
        lastModified = .x$meta$lastModified,
        location = .x$meta$location
      )
    })

    return(df)
  } else {
    cat("Error fetching data from API. Status code:", status_code(response), "\n")
    return(NULL)
  }
}
