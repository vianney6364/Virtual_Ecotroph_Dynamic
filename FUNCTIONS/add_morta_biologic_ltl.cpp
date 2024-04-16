#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
//Model morta biologique et morta50=cat4

double morta_bio_ltl_rcpp(String eco_type,double ssti) {
  double b;
  double d;
  double e;
  double c;
  double clim;
  double mhw_cat;
  double anomaly;
  double affected_value;
  if (eco_type == "polar"){clim=0;anomaly=1.8; mhw_cat=(ssti-clim)/anomaly; b=-1.4511-exp(0.4223*(clim-22.4926));c=0; d=1;e=3.29-0.485*clim+0.0306*pow(clim,2)-0.000608*pow(clim,3);}
  // if (eco_type == "polar"){clim=0;anomaly=1.8; mhw_cat=(ssti-clim)/anomaly; b=-1.4511-exp(0.4223*(clim-22.4926));c=0; d=1;e=1.89-0.0542*clim;} 
  //stable value from ~/ownCloud/Thèse/Script/~/ownCloud/Thèse/Script/SCRIPT EcoTroph/script ecotroph dynamique/Virtual dynamique EcoTroph GOMPERTZ morta/morta uniform food web/data/creation mean_anomaly by clim temperature by biome_RData.R
  // and filter(df,eco_type=="polar")$percentage[1]/100
  if (eco_type == "temperate"){clim=12;anomaly=4.0; mhw_cat=(ssti-clim)/anomaly; b=-1.4511-exp(0.4223*(clim-22.4926));c=0; d=1;e=3.29-0.485*clim+0.0306*pow(clim,2)-0.000608*pow(clim,3);}
  // if (eco_type == "temperate"){clim=12;anomaly=4.0; mhw_cat=(ssti-clim)/anomaly; b=-1.4511-exp(0.4223*(clim-22.4926));c=0; d=1;e=1.89-0.0542*clim;} 
  
  if (eco_type == "tropical"){clim=25;anomaly=2.7; mhw_cat=(ssti-clim)/anomaly; b=-1.4511-exp(0.4223*(clim-22.4926));c=0; d=1;e=3.29-0.485*clim+0.0306*pow(clim,2)-0.000608*pow(clim,3);}
  // if (eco_type == "tropical"){clim=25;anomaly=2.7; mhw_cat=(ssti-clim)/anomaly; b=-1.4511-exp(0.4223*(clim-22.4926));c=0; d=1;e=1.89-0.0542*clim;} 
  
  affected_value= c+(d-c)*exp(-exp(b*(mhw_cat-e)));
  return affected_value;
}

