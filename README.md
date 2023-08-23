# Virtual_Ecotroph_Dynamic

R scripts to run the EcoTroph model in a theoretical unexploited ecosystem and analyze climate change effects

WARNING: In this repository, you will find an original implementation of the EcoTroph modelling framework (Gascuel 2005, Gascuel et al. 2011), which is completely independent of the EcoTroph Package (https://cran.r-project.org/web/packages/EcoTroph/index.html). 

EcoTroph is a simplified semi-empirical model of the flow of biomass through the food web, from primary producers to top predators (Gascuel 2005, Gascuel et al. 2011). It has been used in particulat to assess the ecosytem impact of fishing. In this implementation of EcoTroph, biomass and production of upper trophic levels (at each trophic level) are calculated based on the trophic transfer efficiency and the flow kinetic at each trophic level, using sea water temperature, net primary production and transfer efficiency of high trophic levels (see du Pontavice et al 2021 https://doi.org/10.1111/gcb.15576) as the model inputs. 

The details and the theoretical basis about the implementation of EcoTroph _Dynamic available in this repository would be presented in Guibourd de luzinaiset al. (2023).

How to run those scripts?
1. Open the R script: EcoTroph_Dynamic_core_function.R. 
2. Run the first lines and the fonction called "ecotroph_core". 
3. Try to use ecotroph with the examples at the end of the script (4 examples) 
  or use your own data following the details in the R script.

REFERENCES
Guibourd de Luzinais, V., Gascuel, D., Reygondeau, G., Stock, C., D., & Cheung, W. W. L. (2021). Large potential impacts of Marine Heatwave on ecosystem functioning. manuscript submitted for publication in Global Change Biology. 
Gascuel, D., Morissette,L., Palomares, M., Christensen, V. (2008). Trophic flow kinetics in marine ecosystems: Toward a theoretical approach to ecosystem functioning, doi:10.1016/j.ecolmodel.2008.05.012
Gascuel, D., Guenette, S. & Pauly, D. (2011). The trophic-level-based ecosystem modelling approach: theoretical overview and practical uses. ICES J. Mar. Sci.. DOI: https://doi.org/10.1093/icesjms/fsr062