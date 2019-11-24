#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List pdistC(int x, int y, NumericVector x2, NumericVector y2, NumericVector N) {
  int n = x2.size();
  NumericVector out(2);
  double D = 99999;
  
  for(int i = 0; i < n ; ++i) {
    double d = sqrt(pow(x2[i] - x, 2.0) + pow(y2[i] - y, 2.0)) ;
    
    if((d < D) & (d > 0)) {
      D = d;
      out[0] = sqrt(sqrt(d/ 1609.344) ) + 0.1;
      out[1] = N[i];
    }
  }
  List ret;
  ret["dist"] = out[0];
  ret["N"] = out[1];
  return ret;
}

