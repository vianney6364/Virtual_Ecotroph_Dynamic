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


#Load required library:
# install.packages("Rcpp")
# install.packages("tidyverse")
# install.packages("data.table")
library(Rcpp)
library(tidyverse)
library(data.table)

# load several fonction 
source("FUNCTIONS/production_tl_2to3.R") #process to calculate consumer production between TL=2 and TL=3
source("FUNCTIONS/geom_sequence_prod.R") # geometric sequence to speed up the process to calculate consumer production
# ---------------------------------------------------------------------
source("FUNCTIONS/transfer_efficiency.R") # calulculate TE HTL
source("FUNCTIONS/dyn_reference state_add_rect_morta.R") # calculate reference state of EcoTroph Dynamic
sourceCpp("rcpp function add morta/tl_by_dtime_bis2_rect_morta.cpp") #Compute the lower and upper bounds for each trophic level within the food web

sourceCpp("rcpp function add morta/dyn_dispatch_prod_rect_morta.cpp") # production redistribution proportionally to the width of the old classes included into each of the new ones 
sourceCpp("rcpp function add morta/compute_new_time_rcpp_rect_morta_a_fixed.cpp") #compute the new (t+1) state of the ecosystem
sourceCpp("rcpp function add morta/phi_pass_ct1_rcpp_rect_morta.cpp") #make biomass flow to the next trophic class and compute new production value
sourceCpp("rcpp function add morta/add_morta_biologic.cpp") # Marine heatwave mortality law

# ---------------------------------------------------------------------
# 3 examples: 3 theoritical grid cells representing polar, temperate, and tropical, respectively, for 14 year (see at the end of the script)
# Each example is based on the average value of SST from 1982 to 2011 : NOAA _ AVHRR data  https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00680
load("DATA/example_data.RData")
# ---------------------------------------------------------------------

#' #'*to test EcoTroph Dynamic function execute le 4 next row*
# ---------------------------------------------------------------------
# data_envi : data frame with 6 columns:
# time_step (number of the fortnight from 0 to 364 as simulated 14 year)
# 2.npp (npp in mole of Carbon .m-2.s-1)
# 3.sst (in °C)
# 4.mhw (booleen either TRUE or FALSE at each fortnight)
# 5.eco_type : 3 choices of ecosystem types: "polar","temperate","tropical"# the sensivty of te_htl to temperature is varying among ecosystem types (du pontavice et al., 2019 DOI: https://doi.org/10.1111/gcb.14944)

# nb_time_step=366;dtime=1/24;eco_type="tropical";data_envi=data_envi_tropical;ratio=0.8

#-------------------------------------------
#'*1. create virtual environment data_frame*
#d=duration of simulated MHW
#i=intensity of simulated MHW
#h=percentage of biomass not supporting simulated MHW
create_data_envi<-function (d,biome,h){
  if (biome == "polar"){pp=4.499573e-07;clim=0;surface_area=7035490465} #surface_area: Surface of the the random theoritical cell in m²
  if (biome == "temperate"){pp=5.898543e-07;clim=12;surface_area=7556565706}
  if (biome == "tropical"){pp=1.320424e-06;clim=25;surface_area=12286884909}
  
  return(data.frame(eco_type=rep(biome,366),
                    time_step=seq(1,366,1),
                    npp=rep((pp*365/26 * 24 * 3600 * 9 * 12 * surface_area * 10^(-6)),366), #divided by 26 to get NPP per fornight
                    sst=c(rep(clim,5),rep(clim+i,d),rep(clim,360-d+1)),
                    mhw=c(rep(F,5),rep(T,d),rep(F,360-d+1)),
                    ratio=h))
  
  
}
#----------------------------------------------------------------------------------

EcoTroph_dyn_iterative_MHW<-function(data_envi,nb_time_step,dtime){ 
  #'*1.Reference state*
  npp_ref<-data_envi$npp[1]
  sst_ref<-data_envi$sst[1]
  mhw_ref<-data_envi$mhw[1]
  eco_type=data_envi$eco_type[1]
  ratio=data_envi$ratio[1]
  
  #'* 2. Creation of unequal trophic classes length for same time step to reach the upper trophic classe*
  if (mhw_ref==F){
    
    morta_ref<-0  
  } else {
    morta_ref<-morta_bio_rcpp(eco_type=eco_type,sst=sst_ref)*ratio
    
  }
  #Compute unequal trophic classes division
  CT_dataframe_ref_v4_rect_morta<-rcpp_tl_by_dtime2_rect_morta(sst=sst_ref,dtime=dtime,morta=morta_ref)
  # # CT_dataframe_ref2<-tl_by_dtime2(sst_ref,dtime)
  # CT_dataframe_ref_v4_morta<-CT_dataframe_ref_v4_morta %>% mutate(TLinf=lag(tl),TLsup=tl,time=round(time,3))
  # CT_dataframe_ref_v4_morta$TLinf[1]<-2.000 
  
  #'*3. Compute production and biomass for each trophic class*
  dyn_eco_ref_fishing_v4_rect_morta<-ecotroph_core_dyn_ref_rect_morta(CT_dataframe_ref_v4_rect_morta,eco_type,dtime,npp_ref,sst_ref,mhw_ref,morta=morta_ref)
  
  #------------------------------------------------------
  #'*STEP T to T+1* 
  #----------------------------------------
  ET_ref_rect_morta<-dyn_eco_ref_fishing_v4_rect_morta%>%mutate(time_step=0) 
  
  result_rect_morta<-list()   # ceration of a list to store all the fortnight output
  
  result_rect_morta[[paste('time_step',0)]]<-ET_ref_rect_morta #save reference state in the list
  #envi data prepation for all the time step computation
  sst<-data_envi$sst[-1] 
  npp<-data_envi$npp[-1]
  mhw<-data_envi$mhw[-1]
  
  #'*4. computing next time_step*
  for (i in 1:(nb_time_step-1)){
    
    if (mhw[i]==F){
      
      mortality<-0  
    } else {
      mortality<-morta_bio_rcpp(eco_type=eco_type,sst=sst[i])*ratio
      mortality2<-mortality
      ssts<-sst[i]
      ET_ref_rect_morta<-ET_ref_rect_morta %>% mutate(sst=ssts,
                                                      te=te_funct(eco_type,sst),
                                                      mortality=mortality2,
                                                      MHW=mhw[i],
                                                      fishing=0,
                                                      #'*with mortality*
                                                      kinetic=(20.19*(tl^(-3.26))*exp(.041*sst_ref)+fishing)*dtime,
                                                      heat_tho=(kinetic*mortality),
                                                      kinetic_heat=kinetic+heat_tho*dtime)
    }
    #'*4.1 creating offset of phi and compute new production value*
    phi_pass_ct1_v4_rect_morta<-phi_pass_ct1_rcpp_rect_morta(ET=select(ET_ref_rect_morta,-production))
    # phi_pass_ct1_v4_morta<-ET%>%mutate(flow_actualize=lag(flow_tl_2to7)*exp(-(-log(te)+fishing/kinetic)*delta_tl_dyn),
    #                           flow_m_actualize=flow_actualize*(1-exp(-(-log(te)+fishing/kinetic)*delta_tl_dyn))/
    #                             (delta_tl_dyn*(-log(te)+fishing/kinetic)),
    #                           production=flow_m_actualize*delta_tl_dyn)%>%
    #   as.data.frame()
    # print(head(phi_pass_ct1_v4_rect_morta))
    
    #'* 4.2 Compute limit of Trophic classes time t+1 :need to pick new time sst and npp*
    #Compute unequal trophic classes division at time t+1
    # dtime<-1/26 # possible to change value to see influence on biomass flow through the food web
    CT_dataframe_t1_v4_rect_morta<-rcpp_tl_by_dtime2_rect_morta(sst=sst_ref,dtime=dtime,
                                                                morta=mortality)
    #'*4.3 actualisation of phi by CT*
    actualize_t1_rect_morta<-dyn_dispatch_prod_rect_morta_rcpp(ET_T1=phi_pass_ct1_v4_rect_morta,ET_T2=CT_dataframe_t1_v4_rect_morta)
    # print(head(actualize_t1_rect_morta))
    # dim(actualize_t1)
    
    #'*4.4 compute t+1 te,kinetic,Production and biomass *
    test_t1_rect_morta<-compute_new_time_rcpp_rect_morta_a_fixed(actualize_t1=actualize_t1_rect_morta,eco_type=eco_type,sst_ref=sst_ref,
                                                                 nppi=npp[i],ssti=sst[i],time_stepi=i,dtime=dtime,mhwi=mhw[i],morta=mortality)
    
    result_rect_morta[[paste('time_step',i)]]<-test_t1_rect_morta #add each new time_temp to the list
    
    ET_ref_rect_morta<-test_t1_rect_morta # new time_step becomes reference state for computing the next one
  }
  result_rect_morta<-rbindlist(result_rect_morta,use.names = TRUE) %>% #finally,transform list into dataframe data.table::rbindlist()
    mutate(intensity=data_envi$intensity[1], #adding simulation in information to compare then after
           duration=data_envi$duration[1],
           ratio=data_envi$ratio[1]) 
  return(result_rect_morta) #return the result for one theoritical ocean cell
}


#' *to test function*
#' output_ecotroph_dyn_iterative_temperate2<-EcoTroph_dyn_iterative("entre le num d'iteration que tu veux i.e 1500",1/24,"temperate",data_envi)
#'profvis::profvis(output_ecotroph_dyn_iterative_temperate2<-EcoTroph_dyn_iterative(10000,1/24,"temperate",data_envi))
