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

// double morta_bio_rcpp(String eco_type,double ssti) {
//   double b;
//   double d;
//   double e;
//   double c;
//   double morta_value;
//   if (eco_type == "polar"){b=-0.61663; d=1;e=24.88030; c=0;} //stable value from ~/ownCloud/Thèse/Script/script model mortality/model morta biologic/mean_acclimatation temperature by biome.Rmd
//   // and filter(df,eco_type=="polar")$percentage[1]/100
//   if (eco_type == "temperate"){b=-0.26520; d=1;e=19.13062; c=0;}
//   if (eco_type == "tropical"){b=-1.5485837; d=1;e=27.3202157; c=0;}
//   morta_value= c+(d-c)*exp(-exp(b*(ssti-e)));
//   return morta_value;
// }

// model by cat
double morta_bio_rcpp(String eco_type,double ssti) {
  double b;
  double d;
  double e;
  double c;
  double clim;
  double mhw_cat;
  double anomaly;
  double affected_value;
  if (eco_type == "polar"){clim=0;anomaly=1.8; mhw_cat=(ssti-clim)/anomaly; b=-3.06+0.466*clim-0.0221*pow(clim,2);c=0; d=1;e=3.09-0.0938*clim;} //stable value from ~/ownCloud/Thèse/Script/~/ownCloud/Thèse/Script/SCRIPT EcoTroph/script ecotroph dynamique/Virtual dynamique EcoTroph GOMPERTZ morta/morta uniform food web/data/creation mean_anomaly by clim temperature by biome_RData.R
  // and filter(df,eco_type=="polar")$percentage[1]/100
  if (eco_type == "temperate"){clim=12;anomaly=4.0; mhw_cat=(ssti-clim)/anomaly; b=-3.06+0.466*clim-0.0221*pow(clim,2)	;c=0; d=1;e=3.09-0.0938*clim;}
  if (eco_type == "tropical"){clim=25;anomaly=2.7; mhw_cat=(ssti-clim)/anomaly;b=-3.06+0.466*clim-0.0221*pow(clim,2);c=0; d=1;e=3.09-0.0938*clim;}
  affected_value= c+(d-c)*exp(-exp(b*(mhw_cat-e)));
  return affected_value;
}
// morta by cat and biome
// double morta_bio_rcpp(String eco_type,double ssti) {
//   double b;
//   double d;
//   double e;
//   double c;
//   double clim;
//   double mhw_cat;
//   double anomaly;
//   double affected_value;
//   if (eco_type == "polar"){clim=0;anomaly=2.1; mhw_cat=(ssti-clim)/anomaly; b=-1.84+0.0299*clim;c=0; d=1;e=3.1-0.128*clim;} //stable value from ~/ownCloud/Thèse/Script/script model mortality/model morta biologic/mean_anomaly by clim temperature by biome.R
//   // and filter(df,eco_type=="polar")$percentage[1]/100
//   if (eco_type == "temperate"){clim=12;anomaly=3.5; mhw_cat=(ssti-clim)/anomaly; b=0.691+-0.204*clim	;c=0; d=1;e=3.15-0.0921*clim;}
//   if (eco_type == "tropical"){clim=25;anomaly=2.2; mhw_cat=(ssti-clim)/anomaly;b=9.65-0.603*clim;c=0; d=1;e=4.54-0.153*clim;}
//   affected_value= c+(d-c)*exp(-exp(b*(mhw_cat-e)));
//   return affected_value;
// }


// morta by sst
// double morta_bio_rcpp(String eco_type,double ssti) {
//   double b;
//   double d;
//   double e;
//   double c;
//   double clim;
//   double affected_value;
//   if (eco_type == "polar"){clim=0; b=-0.411+0.031*clim-0.00214*pow(clim,2);c=0; d=1;e=11.4+0.665*clim;}
//   if (eco_type == "temperate"){clim=12; b=-0.411+0.031*clim-0.00214*pow(clim,2)	;c=0; d=1;e=11.4+0.665*clim;}
//   if (eco_type == "tropical"){clim=25; b=-0.411+0.031*clim-0.00214*pow(clim,2)	;c=0; d=1;e=11.4+0.665*clim;}
//   affected_value= c+(d-c)*exp(-exp(b*(ssti-e)));
//   return affected_value;
// }



// double morta_bio_rcpp(DataFrame coef,double ssti) {
//   // NumericVector vb = coef["b"];
//   double b = coef["b"].values[0];
//   Rcout << "The value of b : " << b << "\n";
//   // NumericVector vd = coef["d"];
//   double d = coef["d"].values[0];
//   Rcout << "The value of d : " << d << "\n";
//   // NumericVector ve = coef ["e"];
//   double e = coef["e"].values[0];
//   Rcout << "The value of e : " << e << "\n";
//   double morta_value;
//   morta_value= (d/(1+exp(b*(ssti-e))))/100;
//   return morta_value;
// }

