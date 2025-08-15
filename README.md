# SIERA-*ex* (Single Island Endemic Representativeness Analysis for ex situ Collections) 
SIERA-*ex* is an interactive Shiny web application built in R for assessing the geographic representativeness of rare, single island endemic *ex situ* plant collections against wild collections on Kaua‘i. 2km buffer ranges are selected by default, in line with International Union for Conservation of Nature standards for *Area of Occupancy*. Smaller buffers are also selectable for higher resolution, and larger buffers are available in the case that the software is adapted to other islands or continental areas.

The program accommodates analyses at the highest taxonomic rank (e.g. genus, when hybridization occurs) while retaining specific and infraspecific metadata (see [documentation](https://github.com/plantrecords/sieraex/blob/main/comprehensive_instructions.md#siera-ex-documentation)). The app uses spatial data files along with user uploaded CSV datasets to perform gap analysis and generate a report on representativeness across geographic units. 

Program components and instructions including vector layers and sample datasets be downloaded and installed from [this repository](https://github.com/plantrecords/sieraex). Optional representativeness scoring and priority values can be manually calculated in concert with the software by following the steps laid out in [Kevin's thesis](https://doi.org/10.13140/RG.2.2.27268.64641) as described in [documentation](https://github.com/plantrecords/sieraex/blob/main/comprehensive_instructions.md#siera-ex-documentation). 

## Quick Start -- for comprehensive instructions, view [documentation](https://github.com/plantrecords/sieraex/blob/main/comprehensive_instructions.md#siera-ex-documentation)

### 1. Install R & RStudio
- **R**: [Download from CRAN](http://cran.r-project.org)  
- **RStudio** (recommended): [Download here](https://posit.co/download/rstudio-desktop)

### 2. Clone or Download this Repository
### 3. Place the basemaps folder and associated subfolders in the same directory as sieraex.R
### 4. Install Required R Packages

In RStudio:

    install.packages(c(
     "shiny",
    "leaflet",
    "DT",
    "dplyr",
    "sf",
    "plotly",
    "shinyBS",
    "raster",
    "leafem",
    "shinycssloaders",
    "smoothr",
    "shinyjs"
    ))

Install additional packages as necessary.

### 5. Run the App

- Open sieraex.R in RStudio
- Click Run App

Use the UI to:

- Upload sample wild and ex situ CSV datasets. To upload custom datasets, see [documentation](https://github.com/plantrecords/sieraex/blob/main/comprehensive_instructions.md#siera-ex-documentation).
- Curate points interactively
- Run gap analysis
- Generate representativeness report

## Deployment Considerations

SIERA ex is open source and designed to run in an RStudio/Leaflet environment on any platform (Mac, PC, etc.). You can run the app locally by executing the code in RStudio, or deploy it to a server (e.g., using shinyapps.io or a Shiny Server) if you wish to share it publicly.

When using or sharing SIERA-ex, please provide attribution to:

- Kevin Houck, NTBG
- Atlanta Botanical Garden (SIERA-ex was built using modified open-source code from ABG's [GAMMA](https://github.com/AtlantaBotanicalGarden/gap-analysis-shiny-app) project)
- Hawai‘i State GIS Program (moku, ahupuaʻa, and watershed shapefiles, and GRA GEOJson)
- Dr. Kawika Winter and Matthew P. Lucas (wao layer)
- Potter, Kevin M. 2023. Ecological regions of Hawai'i. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2023-0018 (ecoregion layer)

## Reference

Houck K. SIERA-*ex* (Single-Island Endemic Representativeness Analysis for *ex situ* Collections): a Kauaʻi-based plant conservation model. [M.S.]. Honolulu, HI: University of Hawai’i at Mānoa; 2025. https://doi.org/10.13140/RG.2.2.27268.64641
