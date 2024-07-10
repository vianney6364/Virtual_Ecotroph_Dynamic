# Virtual_Ecotroph_Dynamic

R scripts to run the EcoTroph model in a theoretical unexploited ecosystem and analyze climate change effects

WARNING: In this repository, you will find an original implementation of the EcoTroph_Dynamic modelling framework (Gascuel et al. 2008), which is completely independent of the EcoTroph Package (https://cran.r-project.org/web/packages/EcoTroph/index.html). 

EcoTroph-Dyn is a simplified semi-empirical model of the flow of biomass through the food web, from primary producers to top predators (Gascuel 2005, Gascuel et al. 2011). It has been used in particulate to assess the ecosytem impact of fishing.
In this implementation of EcoTroph-Dyn, biomass and production of upper trophic levels (at each trophic level) are calculated based on the trophic transfer efficiency and the flow kinetic at each trophic level, using sea water temperature, net primary production and transfer efficiency of high trophic levels (see du Pontavice et al 2021 https://doi.org/10.1111/gcb.15576) as the model inputs. 

The details and the theoretical basis about the implementation of EcoTroph-Dynamic available in this repository would be presented in Guibourd de luzinais et al. (2024).

How to run those scripts?
  1. Open the R script: EcoTroph_Dynamic_core_function.R. 
  2. Run the first lines and the fonctions called "create_data_envi",and "ecotroph_dynamic_core". 
  3. Try to use ecotroph with the examples at the end of the script (3 examples) 
     or use your own data following the details in the R script.

REFERENCES
Du Pontavice, H., Gascuel, D., Kay, S., & Cheung, W. (2023). Climate-induced changes in ocean productivity and food-web functioning are projected to markedly affect European fisheries catch. Marine Ecology Progress Series, 713, 21–37. https://doi.org/10.3354/meps14328Gascuel, D., Bozec, Y.-M., Chassot, E., Colomb, A., & Laurans, M. (2005). The trophic spectrum: Theory and application as an ecosystem indicator. ICES Journal of Marine Science, 62(3), 443–452. https://doi.org/10.1016/j.icesjms.2004.12.013
Gascuel, D., Guenette, S. & Pauly, D. (2011). The trophic-level-based ecosystem modelling approach: theoretical overview and practical uses. ICES J. Mar. Sci.. DOI: https://doi.org/10.1093/icesjms/fsr062
Guibourd de Luzinais, V., Gascuel, D., Reygondeau, G., D., & Cheung, W. W. L. (2024). Large potential impacts of Marine Heatwaves on ecosystem functioning. manuscript submitted for publication in Global Change Biology. 

