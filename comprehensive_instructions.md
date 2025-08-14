# SIERA ex Documentation
SIERA ex (Single Island Endemic Representativeness Analysis for Ex Situ Collections) is an interactive Shiny web application built in R for assessing the geographic representativeness of rare, single island endemic ex situ plant collections against wild collections on Kaua‘i. 2km buffer ranges are selected by default, in line with IUCN AOO standards, however 1km buffers may be preferable for higher resolution (It may be applicable to other islands and possibly continental areas as well; for this reason, buffer size options >2 km are available.) The program accommodates analyses at the highest taxonomic rank (e.g. genus, when hybridization occurs) while retaining specific and infraspecific metadata (see Data Preparation in VII.1.2). The app uses spatial data files along with user uploaded CSV datasets to perform gap analysis and generate a report on representativeness across geographic units. Program components and instructions including vector layers and sample datasets be downloaded and installed from this repository https://github.com/plantrecords/sieraex. Optional representativeness scoring and priortiy values can be manually calculated in concert with the software by following the steps laid out in Kevin's thesis as described below in "Executing".

The below instructions include directions for proper use and calculation of representativeness metrics. This workflow is also described in the Manual Build section VI.1.3 and VI.2 of Kevin's thesis. Feel free to contact Kevin Houck directly for assistance.

## Running the App

Download sieraex.R, and the basemaps directory including each of its subfolders and associated files into the same directory. Once downloaded, open the `sieraex.R` file in RStudio. Click the **Run App** button in the upper right of the script editor. The app will open in a browser window. You can interact with the map, upload your CSV files (as described below), remove points by clicking on the marker or data table record, run gap analysis, generate representativeness score, and view the representativeness report.

## Data Preparation

Although the app provides tools to filter and remove unwanted data, it is highly recommended that you pre-curate your datasets to include only the desired points. You will need two `.csv` files (one for occurrence data, and one for holdings data).

### Wild CSV
The wild data CSV (which may include several thousand rows) must include the following fields (**column names must match exactly**):

- `Taxon.Name`: The Hawaiian or scientific name of the individual at the desired taxonomic level.
- `fullname`: Include species/subspecies/variety if known.
- `Longitude`: Longitude coordinate (in decimal degrees).
- `Latitude`: Latitude coordinate (in decimal degrees).
- `Locality`: A description of the collection locality.
- `Collection.ID`: A unique identifier for each wild individual (e.g. georeference code or record number).

**Note:** To maximize performance, it is recommended that the number of unique `Taxon.Name` entries be limited to a few hundred.

### Ex Situ CSV
The ex situ data CSV must include the following fields:

- `Taxon.Name`: The Hawaiian or scientific name of the individual at the desired taxonomic level.
- `fullname`: As above.
- `Longitude`: Longitude coordinate (in decimal degrees).
- `Latitude`: Latitude coordinate (in decimal degrees).
- `Locality`: As above.
- `Collection.ID`: Unique identifier for the ex situ accession (e.g. accession number).
- `Current.Germplasm.Type`: E.g., "Plantout," "Propagule," or "Storage."
- `Germplasm.Type.Qty`: Number of ex situ plants/propagules represented by the row.

Ensure that coordinates are in decimal degrees and that all column names match exactly.

If you wish to visualize sympatry or admixture across a geographic range (if applicable):

- Place the highest taxonomic rank (the rank at which hybridization occurs) in the field `Taxon.Name` while retaining the individual name in the field `fullname`.
- Otherwise, duplicate the individual name in both `fullname` and `Taxon.Name`.

## Executing

1. Upload occurrence data into the “wild” tab.  
2. Select a taxon. If you wish to visualize sympatry or admixture across a geographic range, you may wish to select multiple taxa (note that selecting intergeneric taxa will cause a warning pop-up).  
3. Clicking on the dropdown enables curation of the dataset, and points can be searched and removed as needed.  
4. When the wild dataset is ready, upload holdings data into the “ex situ” tab, and likewise select taxa and curate as needed.  
5. When the datasets are ready, select desired buffer size, and **Run Gap Analysis**, followed by **Generate Representativeness Report**. This may take a few seconds depending on the size of the dataset.  
6. (Optional) Manually evaluate the Representativeness Report for each spatial layer to determine place-based representativeness scores as described in III.3.2 of Kevin's thesis. Combine these with KRS and SRS to generate FRS. **Note:** If not including the wao vector layer, you will only divide by 7 to generate FRS.
7. Manually examine the map to identify subpopulations falling outside of the ex situ buffers.
8. (Optional) Assign values based upon Table III.3.2 and Table III.3.3a from Kevin's thesis, and use the equations provided to determine `FCSSIERAex`. View your uploaded data table for candidate subpopulations to determine `GRepresentation` and to calculate `Epriority`.

## Deployment Considerations

SIERA ex is open source and designed to run in an RStudio/Leaflet environment on any platform (Mac, PC, etc.). You can run the app locally by executing the code in RStudio, or deploy it to a server (e.g., using shinyapps.io or a Shiny Server) if you wish to share it publicly.

When using or sharing SIERA ex, please provide attribution to:

- Kevin Houck, NTBG
- Atlanta Botanical Garden
- Hawai‘i State GIS Program (for the moku, ahupuaʻa, and watershed shapefiles, and GRA GEOJson)
- Dr. Kawika Winter and Matthew P. Lucas (if using the wao layer)
- Potter, Kevin M. 2023. Ecological regions of Hawai'i. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2023-0018 (if using the ecoregion layer)

## Detailed instructions

### Build Requirements

- R (version 4.0 or later recommended)
- RStudio (highly recommended for editing and running the code)
- The following R packages must be installed:
  - shiny
  - leaflet
  - DT
  - dplyr
  - sf
  - plotly
  - shinyBS
  - raster
  - leafem
  - shinycssloaders

### Installation

1. **Download and Install R:**  
   Download R from CRAN (http://cran.r-project.org) and follow the installation instructions for your operating system.

2. **Download and Install RStudio (recommended):**  
   RStudio for your operating system can be downloaded from https://posit.co/download/rstudio-desktop.

3. **Create the folder `sieraex`** on your desktop. 

4. **Place the basemaps folder and all subfolders and associated files in same directory** Note that newer versions of some of the layers may be available from Hawaiʻi State GIS Portal (http://geoportal.hawaii.gov), but they may require modifications to the sieraex.R as described below.
   - moku, ahupuaa, and watershed_dar

Ensure the following files and folders are placed in a folder named `basemaps` in the same directory as the main R script (`sieraex.R`) (ensure that spelling and case matches exactly):

1. **`moku_folder`** containing the complete moku shapefile (`moku.shp`, `moku.dbf`, `moku.shx`, `moku.prj`, etc.).  
   - Ensure case for the associated files matches exactly (change the filenames if needed).  
   - SIERA-ex calls for the field `~MOKU`. Later versions of the shapefile may have changed case. In that case you will need to update the SIERA-ex code to call for the updated field name in the shapefile.

2. **`ahupuaa_folder`** containing the complete ahupuaa shapefile and all of its associated files (`ahupuaa.shp`, `ahupuaa.dbf`, `ahupuaa.shx`, `ahupuaa.prj`, etc.).  
   - Ensure case for the associated files matches exactly (change filenames if needed).  
   - SIERA-ex calls for the field `~ahupuaa`. Later versions of the shapefile may have changed case. In that case you will need to update the SIERA-ex code to call for the updated field name in the shapefile.

3. **`watershed_dar_folder`** containing the complete Watersheds (DAR Version) `watershed_dar.shp` shapefile and all of its associated files (`watershed_dar.shp`, `watershed_dar.dbf`, etc.).  
   - Ensure case for the associated files matches exactly (change filenames if needed).  
   - SIERA-ex calls for the field `~NAME`. Later versions of the shapefile may have changed case. In that case you will need to update the SIERA-ex code to call for the updated field name in the shapefile.

5. **Ecoregions_folder** contains `RDS-2023-0018.zip` from the USDA Research Data Archive (https://www.fs.usda.gov/rds/archive/catalog/RDS-2023-0018). If downloading a newer Ecological regions of Hawai'i shapefile package:
   - Unzip the file, change the name of the `Data` folder to `Ecoregions_folder` (ensure case and spelling) and relocate the entire folder and its contents to the previously created `basemaps` folder.  
   - SIERA-ex calls for the field `~NAME`. Later versions of the shapefile may have changed case. In that case you will need to update the SIERA-ex code to call for the updated field name in the shapefile.

6. **GRA Layer:**  
   SIERA-ex loads the GRA layer from:  
   ```
   https://services.arcgis.com/HQ0xoN0EzDPBOEci/arcgis/rest/services/AllPopRefs1/FeatureServer/0/query?where=1=1&outFields=*&f=geojson
   ```  
   It calls for the field `~NAME` from the GRA layer. Changes in the field name or the URL may require updating the SIERA-ex code to call for the updated field name. These will periodically be updated at the repository.

7. **wao_folder:**  
   If you wish to utilize the wao.png in your application and calculations, you must provide attribution as described below. **Note:** If the wao layer is not used, all instances of `wao` in the SIERA-ex code will need to be commented out.

### Install Required Packages

RStudio will prompt to auto-install some of these upon opening `sieraex.R`. You can also install them manually:

```r
install.packages(c("shiny", "leaflet", "DT", "dplyr", "sf", "plotly", "shinyBS", "raster", "leafem", "shinycssloaders"))
```

You may have to run other `install.packages()` commands if other packages are missing, depending on the R version in use.

If you wish to obtain sample datasets please visit this repository or contact Kevin Houck.
