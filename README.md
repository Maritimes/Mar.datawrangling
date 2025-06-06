# Mar.datawrangling

<!-- badges: start -->
![R Package](https://img.shields.io/badge/R-package-blue)
![GitHub last commit](https://img.shields.io/github/last-commit/Maritimes/Mar.datawrangling)
<!-- badges: end -->

**BREAKING CHANGES in Latest Version**️

**Major changes that will impact all users:**

1. **Oracle credentials removed** - Pass an existing oracle connection (`cxn`) instead of username/password
2. **Data encryption** - Protected B RData files are now encrypted and cannot be easily shared
3. **data.dir parameter removed** - Extractions now go to standardized folders within `C:\DFO-MPO`

**Why these changes?** See [NEWS.md](NEWS.md) for detailed explanation.

---

## Overview

An R package for extracting and working with data from Maritimes fisheries science databases. Enables authorized users to:

- Extract data locally for faster access and offline work
- Filter data using GUI or scripting approaches  
- Combine datasets into analysis-ready formats
- Generate outputs for R, Excel, or GIS applications

**Note**: These functions require appropriate database permissions and will not work without them.

## Installation

```r
# Install from GitHub
library(devtools)
install_github('Maritimes/Mar.datawrangling')
```

## Quick Start

```r
library(Mar.datawrangling)

# 1. Establish Oracle connection first (NEW REQUIREMENT)
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), "<oracle.username>", "<oracle.password>"", "PTRAN")

# 2. Extract data (first time will prompt for extraction)
get_data(db = 'rv', cxn = cxn)

# 3. filter the data
GSSPECIES <- GSSPECIES[GSSPECIES$CODE == 10,]  # Cod only
GSMISSIONS <- GSMISSIONS[GSMISSIONS$YEAR >= 2020,]  # Recent years
self_filter()  # Apply filters to all related tables

# 4. Create analysis-ready dataset
my_data <- summarize_data()
```

## Key Functions

### Data Extraction
- `get_data()` - Extract database tables locally

### Data Filtering  
- `clip_by_poly()` - Filter data to geographic area
- `get_survey()`- Simplifies extraction of particulars survey (from the `rv` databas
- `self_filter()` - Apply filters programmatically

### Data Export
- `summarize_data()` - Combine all tables into single dataframe
- `save_data()` - Export as CSV or shapefile

### Utilities
- `cleanup()` - Clean R environment 
- `qc_findorphans()` - Find orphaned records for QC

## Basic Workflow

```r
# Extract cod data from 2020+ summer surveys
library(Mar.datawrangling)

# Connect to Oracle
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), "<oracle.username>", "<oracle.password>"", "PTRAN")

# Get and filter data
get_data('rv', cxn = cxn)
GSSPECIES <- GSSPECIES[GSSPECIES$CODE == 10,]  # Cod
GSXTYPE <- GSXTYPE[GSXTYPE$XTYPE == 1,]  # Survey type
GSMISSIONS <- GSMISSIONS[GSMISSIONS$YEAR >= 2020 & GSMISSIONS$SEASON == 'SUMMER',]
self_filter()

# Create summary and export
cod_data <- summarize_data()
save_data(cod_data, formats = c('csv','shp'), filename = 'cod_summer_2020plus')
```

## Full Database List

| Parameter | Database | Description |
|-----------|----------|-------------|
| `'rv'` | Groundfish/RV/Ecosystem Surveys | Bottom trawl surveys in shore waters off southwest Nova Scotia |
| `'rvp70'` | Pre-1970 Groundfish Surveys | Bottom trawl surveys conducted prior to 1970 |
| `'isdb'` | Industry Surveys Database | DFO at-sea fish catch observations from commercial vessels |
| `'chid'` | Cape Chidley | Exploratory fishing surveys of benthic fish fauna at 900-1800m |
| `'redfish'` | Redfish | Stratified random design surveys targeting deep sea redfish |
| `'marfis'` | MARFIS | Policy and Economics catch and effort data |
| `'comland67'` | COMLAND (1967-1985) | Commercial landings - like MARFIS, but earlier |
| `'comland86'` | COMLAND (1986-2001) | Commercial landings - separated from 1967-1985 due to code table differences |
| `'stomach'` | Stomach Database | Fish stomach contents database |
| `'asef'` | Atlantic Salmon Enumeration | Tagged salmon records at fishways |
| `'meso'` | Mesopelagic Database | Mesopelagic species data |
| `'meso_gully'` | Mesopelagic Gully | Mesopelagic data specific to Gully area |
| `'juvesh'` | Juvenile Silver Hake | Juvenile silver hake survey database |

## Migration Guide (Breaking Changes)

**Old way:**
```r
get_data('rv', data.dir = "C:/my_project", username = "myuser", password = "mypass")
```

**New way:**
```r
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), "<oracle.username>", "<oracle.password>"", "PTRAN")  # Connect first
get_data('rv', cxn = cxn)  # Data stored in standard location
```

## Contact

- mike.mcmahon@dfo-mpo.gc.ca
- [GitHub Issues](https://github.com/Maritimes/Mar.datawrangling/issues)