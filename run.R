# utils.R
library(tidyverse)
library(httr)
library(fs)
library(jsonlite)
library(furrr)
library(progressr)

# Configuration
CONFIG <- list(
  base_url = "https://www.waterqualitydata.us/data",
  codes_url = "https://www.waterqualitydata.us/Codes",
  code_endpoints = list(
    country = "countrycode",
    county = "countycode",
    state = "statecode"
  ),
  endpoints = list(
    sites = list(
      path = "Station/search",
      params = list()
    ),
    organizations = list(
      path = "Organization/search",
      params = list()
    ),
    projects = list(
      path = "Project/search",
      params = list()
    ),
    weighting = list(
      path = "ProjectMonitoringLocationWeighting/search",
      params = list()
    ),
    physChem = list(
      path = "Result/search",
      params = list(dataProfile = "resultPhysChem")
    ),
    biological = list(
      path = "Result/search",
      params = list(dataProfile = "biological")
    ),
    narrowResult = list(
      path = "Result/search",
      params = list(dataProfile = "narrowResult")
    ),
    activity = list(
      path = "Activity/search",
      params = list(dataProfile = "activityAll")
    ),
    activityMetric = list(
      path = "ActivityMetric/search",
      params = list()
    ),
    resultDetectionQuantitationLimit = list(
      path = "ResultDetectionQuantitationLimit/search",
      params = list()
    ),
    biologicalMetric = list(
      path = "BiologicalMetric/search",
      params = list()
    )
  ),
  base_dir = "locations",
  location_types = list(
    county_countries = c("US", "FM", "PS", "RM"),
    state_countries = c("CA", "MX")
  )
)

# Helper Functions
clean_name <- function(x) {
  x %>%
    gsub("\\s+(County|Borough|Parish|Census Area|Municipality)\\b", "", .) %>%
    gsub("[^[:alnum:][:space:]]", "", .) %>%
    trimws() %>%
    gsub("\\s+", "_", .)
}

format_code <- function(code) {
  gsub(":", "%3A", code)
}

get_location_type <- function(country_code) {
  if (country_code %in% CONFIG$location_types$county_countries) return("county")
  if (country_code %in% CONFIG$location_types$state_countries) return("state")
  return("country")
}

# Function to get codes from the API
get_codes <- function(code_type) {
  if (!code_type %in% names(CONFIG$code_endpoints)) {
    stop(sprintf("Invalid code type: %s", code_type))
  }
  
  url <- sprintf("%s/%s?mimeType=json", 
                 CONFIG$codes_url,
                 CONFIG$code_endpoints[[code_type]])
  
  fromJSON(url)$codes
}

# Function to create the directory structure
create_directory_structure <- function() {
  # Get both county and country codes
  counties <- get_codes("county")
  countries <- get_codes("country")
  
  # Process county-based locations
  county_locations <- map(seq_len(nrow(counties)), ~{
    row <- counties[.x, ]
    codes <- strsplit(row$value, ":")[[1]]
    if (length(codes) < 2) return(NULL)
    
    country_code <- codes[1]
    location_type <- get_location_type(country_code)
    
    if (location_type == "county") {
      if (length(codes) < 3) return(NULL)
      state_code <- codes[2]
      county_code <- codes[3]
      
      parts <- strsplit(row$desc, ",")[[1]]
      if (length(parts) < 3) return(NULL)
      
      state_name <- clean_name(trimws(parts[2]))
      county_name <- clean_name(trimws(parts[3]))
      
      if (any(c(state_name, county_name) %in% c("", "Unspecified"))) return(NULL)
      
      path <- file.path(
        CONFIG$base_dir,
        country_code,
        paste0(state_code, "_", state_name),
        paste0(county_code, "_", county_name)
      )
      
      dir_create(path)
      return(path)
    } else if (location_type == "state") {
      state_code <- codes[2]
      if (state_code == "00") return(NULL)
      
      parts <- strsplit(row$desc, ",")[[1]]
      if (length(parts) < 2) return(NULL)
      
      state_name <- clean_name(trimws(parts[2]))
      if (state_name %in% c("", "Unspecified")) return(NULL)
      
      path <- file.path(
        CONFIG$base_dir,
        country_code,
        paste0(state_code, "_", state_name)
      )
      
      dir_create(path)
      return(path)
    }
    NULL
  }) %>% compact()
  
  # Process country-level locations
  country_locations <- countries %>%
    filter(!value %in% unique(gsub(":.*", "", counties$value))) %>%
    pull(value) %>%
    map_chr(~{
      path <- file.path(CONFIG$base_dir, .x)
      dir_create(path)
      path
    })
  
  # Return all created paths
  c(county_locations, country_locations)
}

# Function to download data for a single location and endpoint
download_endpoint_data <- function(location, endpoint_name, p = NULL) {
  if (!is.null(p)) p()  # Update progress
  
  endpoint_config <- CONFIG$endpoints[[endpoint_name]]
  
  # Parse location path to get codes
  parts <- str_split(location, "/")[[1]]
  country_code <- parts[2]
  
  # Build URL parameters
  params <- list(
    countrycode = format_code(country_code),
    mimeType = "csv",
    zip = "yes"
  )
  
  # Add state code if present
  if (length(parts) >= 3) {
    state_code <- str_extract(parts[3], "^[^_]+")
    params$statecode <- format_code(paste0(country_code, ":", state_code))
    
    # Add county code if present
    if (length(parts) >= 4) {
      county_code <- str_extract(parts[4], "^[^_]+")
      params$countycode <- format_code(paste0(country_code, ":", state_code, ":", county_code))
    }
  }
  
  # Add endpoint-specific parameters
  params <- c(params, endpoint_config$params)
  
  # Build query string
  query <- paste(
    names(params),
    unlist(params),
    sep = "=",
    collapse = "&"
  )
  
  url <- sprintf("%s/%s?%s", CONFIG$base_url, endpoint_config$path, query)
  
  # Download file
  zip_file <- file.path(location, paste0(endpoint_name, ".zip"))
  
  tryCatch({
    response <- GET(url, write_disk(zip_file, overwrite = TRUE))
    if (status_code(response) == 200) {
      return(list(success = TRUE, path = location))
    }
  }, error = function(e) {
    return(list(success = FALSE, path = location, error = e$message))
  })
  
  list(success = FALSE, path = location)
}

# Main execution function
download_water_quality_data <- function() {
  # First create the directory structure
  cat("Creating directory structure...\n")
  locations <- create_directory_structure()
  cat(sprintf("Created %d location directories\n", length(locations)))
  
  # Initialize parallel processing
  plan(multisession)
  
  # Process each endpoint in parallel
  for (endpoint_name in names(CONFIG$endpoints)) {
    cat(sprintf("\nProcessing endpoint: %s\n", endpoint_name))
    
    # Create progress bar
    handlers(global = TRUE)
    with_progress({
      p <- progressor(steps = length(locations))
      
      # Download data for all locations in parallel
      results <- future_map(
        locations,
        ~download_endpoint_data(.x, endpoint_name, p),
        .progress = FALSE
      )
      
      # Summarize results
      successes <- sum(map_lgl(results, "success"))
      failures <- length(results) - successes
      
      cat(sprintf("Completed %s:\n", endpoint_name))
      cat(sprintf("  Successful: %d\n", successes))
      cat(sprintf("  Failed: %d\n", failures))
      
      # Log failures if any
      if (failures > 0) {
        failed_paths <- map_chr(
          results[!map_lgl(results, "success")],
          "path"
        )
        log_file <- file.path(CONFIG$base_dir, paste0("failed_", endpoint_name, ".txt"))
        writeLines(failed_paths, log_file)
        cat(sprintf("  Failed locations written to: %s\n", log_file))
      }
    })
  }
  
  # Clean up parallel processing
  plan(sequential)
}

# Run the download
download_water_quality_data()