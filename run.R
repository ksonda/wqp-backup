# run.R
library(tidyverse)
library(httr)
library(fs)
library(jsonlite)

# Configuration
CONFIG <- list(
  base_url = "https://www.waterqualitydata.us/data",
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
  # Countries that use county system
  county_countries = c("US", "FM", "PS", "RM")
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

# Core download function with retry logic
download_data <- function(url, output_file, max_retries = 3) {
  retry_count <- 0
  success <- FALSE
  
  while (retry_count < max_retries && !success) {
    tryCatch({
      if (retry_count > 0) {
        cat(sprintf("\nRetry attempt %d\n", retry_count))
        Sys.sleep(2^retry_count)  # Exponential backoff
      }
      
      response <- GET(url, write_disk(output_file, overwrite = TRUE))
      
      if (status_code(response) == 200) {
        success <- TRUE
        return(TRUE)
      }
    }, error = function(e) {
      cat(sprintf("\nError: %s\n", e$message))
    })
    retry_count <- retry_count + 1
  }
  return(FALSE)
}

# Location processing function
process_location <- function(location_data) {
  codes <- strsplit(location_data$value, ":")[[1]]
  if (length(codes) < 2) return(NULL)  # Need at least country and state
  
  country_code <- codes[1]
  state_code <- codes[2]
  county_code <- if(length(codes) >= 3) codes[3] else NULL
  
  # Split description into components
  parts <- strsplit(location_data$desc, ",")[[1]]
  if (length(parts) < 2) return(NULL)  # Need at least country and state
  
  # Clean names
  state_name <- clean_name(trimws(parts[2]))
  county_name <- if(length(parts) >= 3) clean_name(trimws(parts[3])) else NULL
  
  # Skip if state is unspecified
  if (state_name %in% c("", "Unspecified")) return(NULL)
  
  # Handle countries with and without counties differently
  if (country_code %in% CONFIG$county_countries) {
    # For countries with counties, require valid county information
    if (is.null(county_code) || county_code == "000" || 
        is.null(county_name) || county_name %in% c("", "Unspecified")) {
      return(NULL)
    }
    
    # Full path with county
    path <- file.path(
      CONFIG$base_dir,
      country_code,
      paste0(state_code, "_", state_name),
      paste0(county_code, "_", county_name)
    )
  } else {
    # For countries without counties, only use state level
    if (state_code == "00") return(NULL)
    
    # Path without county
    path <- file.path(
      CONFIG$base_dir,
      country_code,
      paste0(state_code, "_", state_name)
    )
  }
  
  list(
    country_code = country_code,
    state_code = state_code,
    county_code = county_code,
    state_name = state_name,
    county_name = county_name,
    path = path,
    has_counties = country_code %in% CONFIG$county_countries
  )
}

# Build URL with parameters
build_url <- function(endpoint_config, location_info) {
  # Start with base parameters for country and state
  params <- list(
    countrycode = format_code(location_info$country_code),
    statecode = format_code(paste0(location_info$country_code, ":", location_info$state_code))
  )
  
  # Add county code only for countries that use counties
  if (location_info$has_counties && !is.null(location_info$county_code)) {
    params$countycode <- format_code(paste0(
      location_info$country_code, ":",
      location_info$state_code, ":",
      location_info$county_code
    ))
  }
  
  # Add common parameters
  params$mimeType <- "csv"
  params$zip <- "yes"
  
  # Add endpoint-specific parameters
  params <- c(params, endpoint_config$params)
  
  # Build query string
  query <- paste(
    names(params),
    unlist(params),
    sep = "=",
    collapse = "&"
  )
  
  sprintf("%s/%s?%s", CONFIG$base_url, endpoint_config$path, query)
}

# Main download handler
download_endpoint_data <- function(endpoint_name, endpoint_config, location_info) {
  # Construct URL with parameters
  url <- build_url(endpoint_config, location_info)
  
  # Ensure directory exists
  dir_create(location_info$path)
  
  # Download file
  zip_file <- file.path(location_info$path, paste0(endpoint_name, ".zip"))
  download_data(url, zip_file)
}

# Main execution function
download_water_quality_data <- function(endpoint_name) {
  # Validate endpoint
  if (!endpoint_name %in% names(CONFIG$endpoints)) {
    stop("Invalid endpoint name")
  }
  
  endpoint_config <- CONFIG$endpoints[[endpoint_name]]
  
  # Get county codes
  counties <- fromJSON("https://www.waterqualitydata.us/Codes/countycode?mimeType=json")$codes
  
  # Create base directory
  dir_create(CONFIG$base_dir)
  
  # Initialize logging
  log_file <- file.path(CONFIG$base_dir, paste0("download_log_", endpoint_name, ".txt"))
  cat(format(Sys.time()), sprintf("Starting %s downloads\n", endpoint_name), file = log_file)
  
  # Process locations
  valid_locations <- map(seq_len(nrow(counties)), ~process_location(counties[.x, ]))
  valid_locations <- compact(valid_locations)
  
  # Setup progress tracking
  pb <- txtProgressBar(min = 0, max = length(valid_locations), style = 3)
  results <- list(success = 0, failed = list())
  
  # Download data for each location
  for (i in seq_along(valid_locations)) {
    location <- valid_locations[[i]]
    success <- download_endpoint_data(endpoint_name, endpoint_config, location)
    
    # Update results
    if (success) {
      results$success <- results$success + 1
    } else {
      results$failed <- c(results$failed, location$path)
    }
    
    # Log progress
    cat(format(Sys.time()), sprintf("%s: %s\n", 
                                    location$path, 
                                    if(success) "SUCCESS" else "FAILED"), 
        file = log_file, append = TRUE)
    
    setTxtProgressBar(pb, i)
    #   Sys.sleep(1)  # Rate limiting
  }
  
  close(pb)
  
  # Write summary to log
  write_summary <- function(file) {
    cat("\nDownload Summary:\n", file = file, append = TRUE)
    cat("Successful downloads:", results$success, "\n", file = file, append = TRUE)
    cat("Failed downloads:", length(results$failed), "\n", file = file, append = TRUE)
    if (length(results$failed) > 0) {
      cat("\nFailed locations:\n", file = file, append = TRUE)
      cat(paste(unlist(results$failed), collapse = "\n"), "\n", file = file, append = TRUE)
    }
  }
  
  # Print and log summary
  write_summary("")  # Console
  write_summary(log_file)  # Log file
}

# Example usage:
# Initialize folder structure
dir_create(CONFIG$base_dir)

# Download data for each endpoint
walk(names(CONFIG$endpoints), download_water_quality_data)

# Or download specific endpoints:
# download_water_quality_data("physChem")
# download_water_quality_data("biological")