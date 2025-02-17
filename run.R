# utils.R
library(tidyverse)
library(httr)
library(fs)
library(jsonlite)

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
  # Location handling configurations
  location_types = list(
    county_countries = c("US", "FM", "PS", "RM"),  # Countries with county system
    state_countries = c("CA", "MX"),               # Countries with state system only
    # All other countries handled at country level only
    ocean_codes = c("EM", "LE", "LH", "OA", "OI", "OP", "QO", "QS", "ZC")  # Ocean/Lake codes
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

# Determine location type
get_location_type <- function(country_code) {
  if (country_code %in% CONFIG$location_types$county_countries) return("county")
  if (country_code %in% CONFIG$location_types$state_countries) return("state")
  if (country_code %in% CONFIG$location_types$ocean_codes) return("ocean")
  return("country")
}

# Location processing function
process_location <- function(location_data) {
  codes <- strsplit(location_data$value, ":")[[1]]
  if (length(codes) < 1) return(NULL)
  
  country_code <- codes[1]
  location_type <- get_location_type(country_code)
  
  # Get basic location info
  parts <- strsplit(location_data$desc, ",")[[1]]
  
  # Process based on location type
  result <- list(
    country_code = country_code,
    location_type = location_type
  )
  
  if (location_type == "county") {
    # Need all three levels for county-based countries
    if (length(codes) < 3 || length(parts) < 3) return(NULL)
    
    state_code <- codes[2]
    county_code <- codes[3]
    
    if (state_code == "00" || county_code == "000") return(NULL)
    
    state_name <- clean_name(trimws(parts[2]))
    county_name <- clean_name(trimws(parts[3]))
    
    if (any(c(state_name, county_name) %in% c("", "Unspecified"))) return(NULL)
    
    result$state_code <- state_code
    result$county_code <- county_code
    result$state_name <- state_name
    result$county_name <- county_name
    result$path <- file.path(
      CONFIG$base_dir,
      country_code,
      paste0(state_code, "_", state_name),
      paste0(county_code, "_", county_name)
    )
    
  } else if (location_type == "state") {
    # Need state level for state-based countries
    if (length(codes) < 2 || length(parts) < 2) return(NULL)
    
    state_code <- codes[2]
    if (state_code == "00") return(NULL)
    
    state_name <- clean_name(trimws(parts[2]))
    if (state_name %in% c("", "Unspecified")) return(NULL)
    
    result$state_code <- state_code
    result$state_name <- state_name
    result$path <- file.path(
      CONFIG$base_dir,
      country_code,
      paste0(state_code, "_", state_name)
    )
    
  } else {
    # Country or ocean level only
    result$path <- file.path(CONFIG$base_dir, country_code)
  }
  
  return(result)
}

# Build URL with parameters
build_url <- function(endpoint_config, location_info) {
  # Start with country code
  params <- list(
    countrycode = format_code(location_info$country_code)
  )
  
  # Add state code if present
  if (!is.null(location_info$state_code)) {
    params$statecode <- format_code(paste0(location_info$country_code, ":", location_info$state_code))
  }
  
  # Add county code if present
  if (!is.null(location_info$county_code)) {
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
  
  # Get both county and country codes
  counties <- get_codes("county")
  countries <- get_codes("country")
  
  # For countries in the county list, we'll process them normally
  # For other countries, we'll create country-level entries
  additional_locations <- countries %>%
    filter(!value %in% unique(gsub(":.*", "", counties$value))) %>%
    mutate(
      value = value,  # Just the country code
      desc = desc     # Country name
    )
  
  # Create base directory
  dir_create(CONFIG$base_dir)
  
  # Initialize logging
  log_file <- file.path(CONFIG$base_dir, paste0("download_log_", endpoint_name, ".txt"))
  cat(format(Sys.time()), sprintf("Starting %s downloads\n", endpoint_name), file = log_file)
  
  # Process locations from county codes
  county_locations <- map(seq_len(nrow(counties)), ~process_location(counties[.x, ]))
  
  # Process additional country-level locations
  country_locations <- map(seq_len(nrow(additional_locations)), ~{
    location_data <- additional_locations[.x, ]
    list(
      country_code = location_data$value,
      location_type = "country",
      path = file.path(CONFIG$base_dir, location_data$value)
    )
  })
  
  # Combine and clean up locations
  valid_locations <- c(county_locations, country_locations)
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
    Sys.sleep(1)  # Rate limiting
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