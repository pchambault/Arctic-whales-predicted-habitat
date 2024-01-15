[![DOI](https://zenodo.org/badge/496708138.svg)](https://zenodo.org/badge/latestdoi/496708138)

# Arctic-whales-predicted-habitat
<<<<<<< HEAD
This repository contains code and data necessary to generate the analyses and figures associated with the manuscript entitled "Future seasonal changes in habitat for Arctic whales during predicted ocean warming" (published in 2022 in Science Advances). Manuscript authors: Philippine Chambault, Kit M. Kovacs, Christian Lydersen, Olga Shpak, Jonas Teilmann, Christoffer M. Albertsen, Mads Peter Heide-Jørgensen.
=======
This repository contains code and data necessary to generate the analyses and figures associated with the manuscript entitled "Future seasonal changes in habitat for Arctic whales during predicted ocean warming" (2022, Science Advances). Manuscript authors: Philippine Chambault, Kit M. Kovacs, Christian Lydersen, Olga Shpak, Jonas Teilmann, Christoffer M. Albertsen, Mads Peter Heide-Jørgensen.
>>>>>>> abae46a86aefa10f7264fb38b0136984f8253d5a

The repository is organised as follows:

- Sub-directory "Env_data": sample of the environmental raster layers used for each species and locality in summer. Each raster contains the six environmental variables used in the model training in the following order: SST (in deg C), SSH (in cm), SSS (in psu), MLD (in m), Bathymetry (in m) and distance to shore (in km). These layers were extracted from Copernicus MyOcean (CMEMS) at 0.25° and monthly and from GEBCO (1 km). These raster layers were primarily used for exploration and to mask the land to generate the pseudo-absences. For further information, please see the Methods section.

<<<<<<< HEAD
=======
- Sub-directory "Env_data": sample of the environmental raster layers used for each species and locality in summer. Each raster contains the six environmental variables used in the model training in the following order: SST (in deg C), SSH (in cm), SSS (in spu), MLD (in m), Bathymetry (in m) and distance to shore (in km). These layers were extracted from Copernicus MyOcean (CMEMS) at 0.25° and monthly and from GEBCO (1 km). These raster layers were primarly used for exploration and to mask the land to generate the pseudo-absences. For further information, please see the Methods section.
>>>>>>> abae46a86aefa10f7264fb38b0136984f8253d5a
- Sub-directory "Scripts": all scripts needed to conduct the analysis described in the associated Manuscript.

The filtered tracking dataset needed to run the scripts is available on Dryad (https://doi.org/10.5061/dryad.tqjq2bw2c). It includes the satellite tag-associated data filtered for the three whale species (Belugas, Narwhals and Bowhead whales), both localities (West and East Greenland) and both seasons (summer: Aug-Sep and winter: Dec-Mar); Bel refers to Beluga, Nar to Narwhal and Bw to Bowhead whale.

Both contemporary and climatic environmental data needed to run the habitat modelling scripts can be found on Copernicus-MyOcean (https://resources.marine.copernicus.eu/?option=com_csw&task=results) and CMIP6 (https://esgf-node.llnl.gov/search/cmip6/) portals.

<<<<<<< HEAD
Funding sources:
PC was supported by the European Union’s Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement No48068 (project WARMM). This study was supported by the Norwegian Research Council (ICE-whales, grant no. 244488/E10), the Norwegian Polar Institute, the Norway-Russia Environment Commission, the Office of Naval Research (ONR, USA, award nos. N00014-13-1-0854, N00014-14-1-0424, and N00014-17-1-2233), the Commission for Scientific Research in Greenland, the Danish Cooperation for the Environment in the Arctic (DANCEA), the Carlsberg Foundation, the Greenland Institute of Natural Resources, and the Environmental Agency for Mineral Resource Activities of the Government of Greenland.
=======
>>>>>>> abae46a86aefa10f7264fb38b0136984f8253d5a
