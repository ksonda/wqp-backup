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
  - Sampling Activity Metrics*
  - Biological Habitat Metrics*
  - Result Detection Quantitation Limit Data*
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

Install dependencies:
```r
install.packages(c("tidyverse", "httr", "fs", "jsonlite"))
```

## Usage

1. Clone the repository:
```bash
git clone https://github.com/yourusername/wqp-backup.git
cd wqp-backup
```

2. Basic usage - download all data types:
```r
source("water_quality_downloader.R")
walk(names(CONFIG$endpoints), download_water_quality_data)
```

3. Download specific data types:
```r
# Download only physical/chemical results
download_water_quality_data("physChem")

# Download only biological results
download_water_quality_data("biological")

# Download only site information
download_water_quality_data("sites")
```

## Configuration

The tool's behavior can be customized by modifying the `CONFIG` list in the script:

```r
CONFIG <- list(
  base_url = "https://www.waterqualitydata.us/data",
  endpoints = list(...),
  base_dir = "locations",
  county_countries = c("US", "FM", "PS", "RM")
)
```

- `base_url`: Base URL for the Water Quality Portal API
- `endpoints`: List of endpoints and their configurations
- `base_dir`: Base directory for downloaded data
- `county_countries`: Countries that use county-level administrative divisions

## Logging

The tool creates detailed logs for each download session:
- Located in the base directory as `download_log_{endpoint}.txt`
- Includes timestamps, success/failure status, and detailed error information
- Provides summary statistics at the end of each run

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

