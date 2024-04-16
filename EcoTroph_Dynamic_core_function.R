########################################################################################
#creation EcoTroph_dynamic reference state with no equal length of trophic classes (CT)#
#with a same time step for passing to one CT at next one                               #
########################################################################################
# rm(list=ls()) 
#'* 1. Study framework*
# tl= Trophic Level
# TE: Transfer efficiency (du pontavice et al., 2019 DOI: https://doi.org/10.1111/gcb.14944)
# NPP: Net Primary Production (here fixed to a decided value)
# SST: Sea Surface Temperature
# Kinetic of biomass flows depending on TL an SST. (Gascuel 2008 DOI: https://doi.org/10.1016/j.ecolmodel.2008.05.012)
# i.e K(tl,SST)=20.19*(tl^(-3.26))*exp(.041*sst)
# transfert efficiency depending only to time (SST evolution)(based on du Pontavice et al., 2019 DOI: 10.1111/gcb.14944)
# i.e TE= exp((-2.162) + (-0.025)*sst + b + a*sst)*1.038013 with a and b depending of ecosystem type


#'*Load required library:*
# install.packages("Rcpp")
# install.packages("tidyverse")
# install.packages("data.table")
library(Rcpp)
library(tidyverse)
library(data.table)

setwd(dir = "~/ownCloud/Thèse/CHAP1_ papier EcoTroph virtuelle/Github_Virtual_Ecotroph_Dynamic/")
#'*load several fonctions* 
source("FUNCTIONS/transfer_efficiency.R") # calulculate TE HTL
source("FUNCTIONS/dyn_reference state_add_rect_morta.R") # calculate reference state of EcoTroph Dynamic
sourceCpp("FUNCTIONS/tl_by_dtime_bis2_rect_morta.cpp") #Compute the lower and upper bounds for each trophic level within the food web

sourceCpp("FUNCTIONS/dyn_dispatch_prod_rect_morta.cpp") # production redistribution proportionally to the width of the old classes included into each of the new ones 
sourceCpp("FUNCTIONS/compute_new_time_rcpp_rect_morta_a_fixed.cpp") #compute the new (t+1) state of the ecosystem
sourceCpp("FUNCTIONS/phi_pass_ct1_rcpp_rect_morta.cpp") #make biomass flow to the next trophic class and compute new production value
sourceCpp("FUNCTIONS/add_morta_biologic_ltl.cpp") #Marine heatwave mortality law for trophic level below or equal to 2.5
sourceCpp("FUNCTIONS/add_morta_biologic_htl.cpp") # Marine heatwave mortality law for trophic level above or equal to 2.5

# ---------------------------------------------------------------------
# 3 examples: 3 theoritical grid cells representing polar, temperate, and tropical, respectively, for 14 year (see at the end of the script)
# Each example is based on the average value of SST from 1982 to 2011 : NOAA _ AVHRR data  https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00680
# ---------------------------------------------------------------------

#' #'*to test EcoTroph Dynamic function execute create_data_envi() function*
# ---------------------------------------------------------------------
#'*1. create virtual environment function*
#d=duration of simulated MHW with d as integer value between 1 and 10 (between 1 to 10 time_step i.e between a fortnight and 5 months duration)
#i=intensity of simulated MHW seq(1,16,0.5)) with i positive numeric  value between 1 and 16 (between 1 to 16 degree above normal SST)
#biome=which biome you want to simulate with biome chara ("polar","temperate","tropical")
#h=percentage of biomass not supporting simulated MHW. With h positive numeric  value between 0 and 1 (between 0 and 100% species acclimatisation capacity)

create_data_envi<-function (d,i,biome,h){
  if (biome == "polar"){npp=4.499573e-07;clim=0;surface_area=7035490465} #surface_area: Surface of  random  cell in m² Bering Sea (coordinates: 173.5°W, 55.5°N)
  if (biome == "temperate"){npp=5.898543e-07;clim=12;surface_area=7556565706} #surface_area: Surface of  random  cell in m² West of Ireland (coordinates: 12.5°W, 52.5°N)
  if (biome == "tropical"){npp=1.320424e-06;clim=25;surface_area=12286884909} #surface_area: Surface of  random  cell in m² Southwest of Galápagos (coordinates: 94.5°W, 3.5°S)
  
  data_envi<-data.frame(eco_type=rep(biome,366),
                        time_step=seq(1,366,1),
                        # NPP: Net primary production in t.an-1 (data in mole.m-2.s-1 --> x 12 g.mol-1 (mole of carbon to g of carbon) 
                        #  x 365*24*3600 (day*hour*sec) (second ==> year)
                        # x * 9 (carbon ==> biomass)
                        npp=rep((npp*365/26 * 24 * 3600 * 9 * 12 * surface_area * 10^(-6)),366), #divided by 26 to get NPP per fornight
                        sst=c(rep(clim,5),rep(clim+i,d),rep(clim,360-d+1)),
                        mhw=c(rep(F,5),rep(T,d),rep(F,360-d+1)),
                        ratio=h)
  return(data_envi)
  
  
}

#'* data_envi : data frame with 6 columns:*
# time_step (number of the fortnight from 0 to 364 as simulated 14 year)
# 2.npp (npp in mole of Carbon .m-2.s-1) fix npp over time and estimate within each cell as the average over 1950_2100 with GFDL-ESM2M (Dunne et al., 2012; DOI: https://doi.org/10.1175/JCLI-D-11-00560.1) and the emission scenario : RCP8.5 ("no mitigation policy" scenario)
# 3.sst (in °C)
# 4.mhw (booleen either TRUE or FALSE at each fortnight)
# 5.biome : 3 choices of ecosystem types: "polar","temperate","tropical"# the sensivty of te_htl to temperature is varying among ecosystem types (du pontavice et al., 2019 DOI: https://doi.org/10.1111/gcb.14944)
# 6. ratio: percentage of biomass not supporting simulated MHW. With h positive numeric  value between 0 and 1 (between 0 and 100% species acclimatisation capacity)

#----------------------------------------------------------------------------------

ecotroph_dynamic_core<-function(data_envi,nb_time_step,dtime){ 
  #'*1.Reference state*
  #'*1.1.environnemental condition for reference state*
  npp_ref<-data_envi$npp[1]
  sst_ref<-data_envi$sst[1]
  mhw_ref<-data_envi$mhw[1]
  eco_type=data_envi$eco_type[1]
  ratio=data_envi$ratio[1]
  
  #'* 1.2. Computation of MHW associated mortality if MHW==T*
  if (mhw_ref==F){
    
    morta_ltl_ref<-0  
    morta_htl_ref<-0
  } else {
    morta_ltl_ref<-morta_bio_ltl_rcpp(eco_type=eco_type,sst=sst_ref)*ratio
    morta_htl_ref<-morta_bio_htl_rcpp(eco_type=eco_type,sst=sst_ref)*ratio
    
  }
  #'* 1.3. Compute unequal trophic classes limit of reference state* 
  CT_dataframe_ref_v4_rect_morta<-rcpp_tl_by_dtime2_rect_morta(sst=sst_ref,dtime=dtime,morta_ltl=morta_ltl_ref,morta_htl=morta_htl_ref)
  
  
  #'*1.4. Compute production and biomass for each trophic class*
  ET_ref_rect_morta<-ecotroph_core_dyn_ref_rect_morta(CT_dataframe_ref_v4_rect_morta,eco_type,dtime,npp_ref,sst_ref,mhw_ref,
                                                      morta_ltl =morta_ltl_ref,morta_htl = morta_htl_ref ) %>% mutate(time_step=0) 
  
  #------------------------------------------------------
  #'*STEP T to T+1* 
  #----------------------------------------
  result_rect_morta<-list()   # ceration of a list to store all the fortnight output
  
  result_rect_morta[[paste('time_step',0)]]<-ET_ref_rect_morta #save reference state in the list
  #envi data prepation for all the time step computation remove initial condition [-1]
  sst<-data_envi$sst[-1] 
  npp<-data_envi$npp[-1]
  mhw<-data_envi$mhw[-1]
  
  #'*2. computing next time_step*
  #'* 2.1. for each time_step i,computation of MHW associated mortality if MHW==T*
  for (i in 1:(nb_time_step-1)){
    
    if (mhw[i]==F){
      
      morta_ltli<-0  
      morta_htli<-0  
    } else {
      morta_ltli<-morta_bio_ltl_rcpp(eco_type=eco_type,sst=sst[i])*ratio
      morta_htli<-morta_bio_htl_rcpp(eco_type=eco_type,sst=sst[i])*ratio
      ssts<-sst[i]
      ET_ref_rect_morta<-ET_ref_rect_morta %>% mutate(sst=ssts,  #transition between reference state and new environnemental conditions
                                                      te=te_funct(eco_type,sst),
                                                      mortality = case_when(tl<2.5~morta_ltli,
                                                                            tl>=2.5~morta_htli),
                                                      MHW=mhw[i],
                                                      fishing=0,
                                                      #'*with mortality*
                                                      kinetic=(20.19*(tl^(-3.26))*exp(.041*sst_ref)+fishing),
                                                      heat_tho=(kinetic*mortality),
                                                      kinetic_heat=kinetic+heat_tho)
    }
    #'*2.2. make biomass flow to the next trophic class and compute new production value*
    phi_pass_ct1_v4_rect_morta<-phi_pass_ct1_rcpp_rect_morta(ET=select(ET_ref_rect_morta,-production))

    
    #'* 2.3. Compute limit of Trophic classes time t+1 *
    CT_dataframe_t1_v4_rect_morta<-rcpp_tl_by_dtime2_rect_morta(sst=sst_ref,dtime=dtime,
                                                                morta_ltl = morta_ltli,
                                                                morta_htl = morta_htli)
    #'*2.4. actualisation of phi by CT*
    actualize_t1_rect_morta<-dyn_dispatch_prod_rect_morta_rcpp(ET_T1=phi_pass_ct1_v4_rect_morta,ET_T2=CT_dataframe_t1_v4_rect_morta)

    #'*2.5. compute t+1 te,kinetic,Production and biomass of time t+1: compute the new (t+1) state of the ecosystem*
    test_t1_rect_morta<-compute_new_time_rcpp_rect_morta_a_fixed(actualize_t1=actualize_t1_rect_morta,eco_type=eco_type,sst_ref=sst_ref,
                                                                 nppi=npp[i],ssti=sst[i],time_stepi=i,dtime=dtime,
                                                                 mhwi=mhw[i],morta_htli,morta_ltli)
    
    result_rect_morta[[paste('time_step',i)]]<-test_t1_rect_morta #add each time_temp (fortnigtht) to the list
    
    ET_ref_rect_morta<-test_t1_rect_morta # new time_step becomes reference state for computing the next one
  }
  result_rect_morta<-rbindlist(result_rect_morta,use.names = TRUE) %>% #finally,transform list into dataframe data.table::rbindlist()
    mutate(intensity=data_envi$intensity[1], #adding simulation in information to compare then after
           duration=data_envi$duration[1],
           ratio=data_envi$ratio[1]) 

#'*2.6. this bloc is not mandatory: it just format data by trophic class of 0.1 width from tl =2 to tl=5.5* 
#'*you can put it in comment to see the raw output data*
  # result_rect_morta<-result_rect_morta %>%
  #   mutate(tl=round(tl,1)) %>%
  #   group_by(time_step,tl) %>%
  #       summarise(biomass=sum(biomass),
  #             production=sum(production),
  #             te=mean(te),
  #             kin=mean(kinetic),
  #             phi=mean(flow_m_per_tl),
  #             kin_heat=mean(kinetic_heat),
  #             mortality=mean(mortality),
  #             heat_tho=mean(heat_tho), #with heat_tho=(kinetic*mortality)
  #             mhw=mhw[1])
  return(result_rect_morta) #return the result for one theoritical ocean cell
}


#' *to test function*
# ---------------------------------------------------------------------
# 3 EXAMPLES FOR POLAR, TEMPERATE, TROPICAL  
# 3 example with random parameter that you can choose
#With:
#d=duration of simulated MHW with d as integer value between 1 and 10 (between 1 to 10 time_step i.e between a fortnight and 5 months duration)
#i=intensity of simulated MHW seq(1,16,0.5)) with i positive numeric  value between 1 and 16 (between 1 to 16 degree above normal SST)
#biome=which biome you want to simulate with biome chara ("polar","temperate","tropical")
#h=percentage of species not supporting MHW thermal stress. With h positive numeric  value between 0 and 1 (between 0 and 100% species acclimation capacity)

# POLAR
data_envi<-create_data_envi(d=5,i=2.5,biome = "polar",h=0.5) #choose your parameter here 
output_ecotroph<-ecotroph_dynamic_core(data_envi,nb_time_step = 366,dtime = 1/26) # with nb_time_step no more (can be less) than 366 as it is the size of data envi and dtime=1/26 to work at fortnight basis  
# TEMPERATE
data_envi<-create_data_envi(d=5,i=2.5,biome = "temperate",h=0.5) #choose your parameter here 
output_ecotroph<-ecotroph_dynamic_core(data_envi,nb_time_step = 366,dtime = 1/26) # with nb_time_step no more (can be less) than 366 as it is the size of data envi and dtime=1/26 to work at fortnight basis  
# TROPICAL
data_envi<-create_data_envi(d=5,i=2.5,biome = "tropical",h=0.5) #choose your parameter here 
output_ecotroph<-ecotroph_dynamic_core(data_envi,nb_time_step = 366,dtime = 1/26) # with nb_time_step no more (can be less) than 366 as it is the size of data envi and dtime=1/26 to work at fortnight basis  
# ---------------------------------------------------------------------
head(output_ecotroph)
dim(output_ecotroph)
