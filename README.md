# Backup the Water Quality Portal

An R script for archiving  the [Water Quality Portal (WQP)](https://www.waterqualitydata.us/). This script systematically downloads data as zipped csv by the lowest administrative unit possible (typically county, but varies by country) to minimze server timeouts and improve archive indexing,  organizing it into a hierarchical directory structure.

## Features

- Downloads data from each WQP Web Service endpoint:
  - Organization Data
  - Site Data Only
  - Project Data
  - Project Monitoring Location Weighting*
  - Sample Results (Physical/Chemical)
  - Sample Results (Biological)
  - Sample Results (Narrow)
  - Sampling Activity
  - Sampling Activity Metrics
  - Biological Habitat Metrics
  - Result Detection Quantitation Limit Data
- Handles both countries with and without county-level administrative divisions
- Creates organized directory structure based on geographic hierarchy
- Includes retry logic
- Comprehensive logging and progress tracking
- Rate limiting to respect API endpoints

## Directory Structure

The script creates a hierarchical directory structure based on geographic divisions:

For countries with county systems (US, FM, PS, RM):
```
locations/
├── US/
│   ├── 06_California/
│   │   ├── 001_Alameda/
│   │   │   ├── sites.zip
│   │   │   ├── organizations.zip
│   │   │   └── ...
│   │   └── 003_Alpine/
│   └── 36_New_York/
└── FM/
    └── ...
```

For countries without county systems:
```
locations/
├── CA/
│   ├── 01_Alberta/
│   │   ├── sites.zip
│   │   ├── organizations.zip
│   │   └── ...
│   └── 02_British_Columbia/
└── MX/
    └── ...
```

## Requirements

- R >= 4.0.0
- Required R packages:
  - tidyverse
  - httr
  - fs
  - jsonlite
  - furrr
  - progressr
  - parallelly

Install dependencies:
```r
install.packages(c("tidyverse", "httr", "fs", "jsonlite", "furrr", "progressr", "parallelly"))
```

## Usage

1. Clone the repository:
```bash
git clone https://github.com/ksonda/wqp-backup.git
cd wqp-backup
```

2. Run the script:
```r
source("run.R")
```

The script will:
1. Create the complete directory structure
2. Download data for each endpoint in sequence
3. Process locations in parallel within each endpoint

## Configuration

The tool's behavior can be customized by modifying the `CONFIG` list in the script:

```r
CONFIG <- list(
  base_url = "https://www.waterqualitydata.us/data",
  endpoints = list(...),
  base_dir = "locations",
  location_types = list(
    county_countries = c("US", "FM", "PS", "RM"),
    state_countries = c("CA", "MX")
  ),
  parallel = list(
    workers = parallelly::availableCores() - 1,  # Use all cores except one
    chunk_size = 100  # Number of locations to process in each chunk
  )
)
```

- `base_url`: Base URL for the Water Quality Portal API
- `endpoints`: List of endpoints and their configurations
- `base_dir`: Base directory for downloaded data
- `location_types`: Geographic division configurations
- `parallel`: Parallel processing settings
  - `workers`: Number of parallel workers to use
  - `chunk_size`: Number of locations to process in each chunk

## Logging

The tool creates detailed logs for each endpoint:
- Failed downloads logged to `failed_{endpoint}.txt`
- Includes timestamps and failure details
- Provides summary statistics after each endpoint


## Rate Limiting

To respect the API's resources:
- 1-second delay between requests
- Exponential backoff on failures
- Maximum of 3 retry attempts per download

## Contributing

General issue/ PR etiquette

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## References

- Water Quality Portal. Washington (DC): National Water Quality Monitoring Council, United States Geological Survey (USGS), Environmental Protection Agency (EPA); 2021. https://doi.org/10.5066/P9QRKUVJ.

