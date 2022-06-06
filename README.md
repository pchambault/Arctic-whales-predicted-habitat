[![DOI](https://zenodo.org/badge/496708138.svg)](https://zenodo.org/badge/latestdoi/496708138)

# Arctic-whales-predicted-habitat
This repository contains code and data necessary to generate the analyses and figures associated with the manuscript entitled "Future seasonal changes in habitat for Arctic whales during predicted ocean warming" (2022, Science Advances). Manuscript authors: Philippine Chambault, Kit M. Kovacs, Christian Lydersen, Olga Shpak, Jonas Teilmann, Christoffer M. Albertsen, Mads Peter Heide-Jørgensen.

The repository is organized as follows:

- Sub-directory "Env_data": sample of the environmental raster layers used for each species and locality in summer. Each raster contains the six environmental variables used in the model training in the following order: SST (in deg C), SSH (in cm), SSS (in spu), MLD (in m), Bathymetry (in m) and distance to shore (in km). These layers were extracted from Copernicus MyOcean (CMEMS) at 0.25° and monthly and from GEBCO (1 km). These raster layers were primarly used for exploration and to mask the land to generate the pseudo-absences. For further information, please see the Methods section.
- Sub-directory "Scripts": all scripts needed to conduct the analysis described in the associated Manuscript.

The filtered tracking dataset needed to run the scripts is available on Dryad (https://doi.org/10.5061/dryad.tqjq2bw2c). It includes the satellite tag-associated data filtered for the three whale species (Belugas, Narwhals and Bowhead whales), both localities (West and East Greenland) and both seasons (summer: Aug-Sep and winter: Dec-Mar); Bel refers to Beluga, Nar to Narwhal and Bw to Bowhead whale.

Both contemporary and climatic environmental data needed to run the habitat modelling scripts can be found on Copernicus-MyOcean (https://resources.marine.copernicus.eu/?option=com_csw&task=results) and CMIP6 (https://esgf-node.llnl.gov/search/cmip6/) portals.

