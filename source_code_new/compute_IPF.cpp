#include <Rcpp.h>
#include <numeric>
using namespace Rcpp;

// Declare nested functions
List getMarginals(DataFrame df, int max_zones);
NumericMatrix getMatrix(DataFrame df, int max_zones);
NumericVector getMatRCSums(NumericMatrix &mat, String type);
List getMatRCSums2(NumericMatrix &mat);
NumericMatrix applyFac(NumericMatrix &mat, NumericVector fac, String type);
NumericVector computeFac(NumericVector target, NumericVector matSum);
bool isConverge(NumericVector colsum2, NumericVector col_target);
DataFrame mat2DF(NumericMatrix &mat);

// [[Rcpp::export]]
List runIPF(DataFrame df, NumericVector gfac, NumericVector tripEnds, int maxTrips, int max_iter) {
  
  // this is only for the developer and is turned on here
  bool debug = 0;
  
  // Initialize data frame vectors
  NumericVector i = df["I"];
  NumericVector j = df["J"];
  NumericVector v = df["Trip"];
  bool converge = 0;

  int iter = 0;
  // int max_iter = 20;
  
  // Declare working vectors and matrices
  NumericMatrix m1;
  List rowColSums_iter;
  NumericVector iter_rowTotal;
  NumericVector iter_colTotal;
  NumericVector rcfac;
  List ret;
    
  // Get maximum row and columns
  int max_zones = max(i);
  Rcout << "The max zones is " << max_zones << "\n";
  
  //----------------------------------------------------------------------------
  // GET SEED
  // Get seed row, col sums and matrix
  List rowColSums = getMarginals(df, max_zones);
  NumericMatrix seedMat = getMatrix(df, max_zones);
  
  // Get seed row/col sums
  NumericVector seed_rowTotal = rowColSums["rowSum"];
  NumericVector seed_colTotal = rowColSums["colSum"];
  
  // DEBUG:: print a few seed values
  if(debug){
    for(int x = 0; x < 10; x++){
      for (int y = 0; y < 2; y++){
        Rcout << "seed mat at Row "<< x << " Col " << y << " = "<< seedMat(x, y) << " \n" ;
      }
    }
    
    ret["row_seed"] = seed_rowTotal;
    ret["col_seed"] = seed_colTotal;
  }
  
  //----------------------------------------------------------------------------
  // COMPUTE TARGETS
  NumericVector row_target(max_zones);
  NumericVector col_target(max_zones);
  
  // Get initial row / column targets   
  for(int x = 0; x < max_zones ; x++ ){
    
    if(gfac[x] > 0){
      row_target[x] =  gfac[x] * seed_rowTotal[x];
      col_target[x] =  gfac[x] * seed_colTotal[x];
    } else if (gfac[x] == -9999){
      // base trips are zero but seed table consists of trips 
      if(seed_rowTotal[x]  > 0)  row_target[x] = tripEnds[x];
      if(seed_rowTotal[x] == 0)  row_target[x] = 0;
      if(seed_colTotal[x]  > 0)  col_target[x] = tripEnds[x];
      if(seed_colTotal[x] == 0)  col_target[x] = 0;
      gfac[x] = 1.0;
    } else{
      Rcout << "Check growth factors and trip ends " << std::endl;
    }

    // Cap them at max and lower the initial growth factors
    if(row_target[x] > maxTrips) {
       row_target[x] = maxTrips;
       gfac[x] = 0.5;
    }
    if(col_target[x] > maxTrips) {
       col_target[x] = maxTrips;
       gfac[x] = 0.5;
     }
  }
  
  // DEBUG
  if(debug){
    ret["row_target"] = row_target;
    ret["col_target"] = col_target;

    // Log data (not really reequired)
    Rcout << "------------------------------------------------------------------" << std::endl;
    Rcout << "Seed and Target Trip Ends " << std::endl;
    Rcout << "TAZ    SEED-ROW     SEED-COL     TARGET-ROW   TARGET-COL  " << std::endl;
    for(int x = 0; x < row_target.size() ; x++){
      Rprintf("%-6i %-12.2f %-12.2f %-12.2f %-12.2f  \n", x + 1 , seed_rowTotal[x], seed_colTotal[x], row_target[x],  col_target[x]);
    }
    Rcout << "------------------------------------------------------------------" << std::endl;
  }
  
  //----------------------------------------------------------------------------
  // FRATAR Loop begin here
  while((iter < max_iter) & (!converge)) {
    
    // APPLY Initial FACTORS
    if(iter == 0){
      // Rcout << "Applying row factors \n";
      m1 = applyFac(seedMat, gfac, "row");
    } else{
      rcfac = computeFac(row_target, iter_rowTotal);
      
      // cap growth factor (to 50% for all maxed out zones)
      for(int x = 0; x < max_zones; x++ ){
        if(iter_rowTotal[x] >= maxTrips) {
          iter_rowTotal[x] = maxTrips;
          // rcfac[x] = 0.45;
        }
      }
      m1 = applyFac(m1, rcfac, "row");
    }
    
    iter += 1;
    
    // compute marginals
    // NumericVector iter_colTotal =  getMatRCSums(m1, "col");
    rowColSums_iter = getMatRCSums2(m1);
    iter_rowTotal = rowColSums_iter["rowSum"];
    iter_colTotal = rowColSums_iter["colSum"];
    
    // DEBUG:: print a few mat values to check factor application
    if(debug){
      for(int x = 0; x < 10; x++){
        for (int y = 0; y < 2; y++){
          Rcout << "fact mat at Row "<< x << " Col " << y << " = "<< m1(x, y) << " \n" ;
        }
      }
      
      ret["rowsum1"] = iter_rowTotal;
      ret["colsum1"] = iter_colTotal;
    }
    
    // Check for convergence
    converge = isConverge(iter_colTotal, col_target);
    
    // Compute factors
    rcfac = computeFac(col_target, iter_colTotal);
    
    // cap growth factors
    for(int x = 0; x < max_zones; x++ ){
      if(iter_colTotal[x] >= maxTrips) {
        iter_colTotal[x] = maxTrips;
        // rcfac[x] = 0.45;
      }
    }  
    
    // Apply col factors
    m1 = applyFac(m1, rcfac, "col");
    
    // compute marginals
    rowColSums_iter = getMatRCSums2(m1);
    iter_rowTotal = rowColSums_iter["rowSum"];
    iter_colTotal = rowColSums_iter["colSum"];
    
    // check for convergence
    converge = isConverge(row_target, iter_rowTotal);
    
    // message at the end
    if((converge == 1) || (iter == max_iter)){
      ret["rowsum"] = iter_rowTotal;
      ret["colsum"] = iter_colTotal;
      if(converge == 1){
        Rcout << " Model converged at iteration : " << iter << std::endl ;
      } else if(iter == max_iter){
        Rcout << " Model NOT converged at iteration : " << iter << std::endl ;
      } 
    }

    
    // DEBUG: Not sure if we need to report every iteration (leave it for debug)
    if(debug){
      
      Rcout << "------------------------------------------------------------------" << std::endl;
      Rcout << "Target & Iterim Trip Ends: Iteration =  " << iter << std::endl;
      Rcout << "TAZ    TARGET-ROW   TARGET-COL  ITER-ROW     ITER-COL    DELTA    " << std::endl;
      for(int x = 0; x < row_target.size() ; x++){
        Rprintf("%-6i %-12.2f %-12.2f %-12.2f %-12.2f %-8.2f \n", x + 1 , row_target[x],  col_target[x], iter_rowTotal[x], iter_colTotal[x], iter_rowTotal[x] - row_target[x]);
      }
      Rcout << "------------------------------------------------------------------" << std::endl;
    }
    
  } // Loop ends
  
  
  // Write final iteration to log
  Rcout << "------------------------------------------------------------------" << std::endl;
  Rcout << "Target & Iterim Trip Ends: Iteration =  " << iter << std::endl;
  Rcout << "TAZ    TARGET-ROW   TARGET-COL  ITER-ROW     ITER-COL    DELTA    " << std::endl;
  for(int x = 0; x < row_target.size() ; x++){
    Rprintf("%-6i %-12.2f %-12.2f %-12.2f %-12.2f %-8.2f \n", x + 1 , row_target[x],  col_target[x], iter_rowTotal[x], iter_colTotal[x], iter_rowTotal[x] - row_target[x]);
  }
  Rcout << "------------------------------------------------------------------" << std::endl;
  
  // Convert matrix into R dataframe
  DataFrame df2 = mat2DF(m1);
  ret["newMat"] = df2;
  return ret;
    
}



// Function to check convergence (in RCPP bool is actually int)
bool isConverge(NumericVector colsum2, NumericVector col_target){
   int size = colsum2.size();
  
   // start as converged
   bool converged = 1;
   
   // check for converence
   for (int x = 0; x < size; x++) {
     
     if((colsum2[x] > 0) & (col_target[x] > 0) & (abs(colsum2[x] - col_target[x]) > 100)) converged = 0;
   }
  
  return converged;
}

// Function to compute seed marginals from data frame
// don't [[Rcpp::export]]
List getMarginals(DataFrame df,  int max_zones) {
  
    NumericVector i = df["I"];
    NumericVector j = df["J"];
    NumericVector v = df["Trip"];

    int size = v.size();
    NumericVector row_total (max_zones);
    NumericVector col_total (max_zones);
    
    // TODO:: use auto iterator instead of value at index (cpp11)
    for (int x = 0; x < size; x++) {
      // CPP index starts with zero but R uses 1
      row_total[i[x] - 1] += v[x];
      col_total[j[x] - 1] += v[x];
    }
    
    List ret;
    ret["rowSum"] = row_total;
    ret["colSum"] = col_total;
    return ret;

}    
    
// Function to convert vector to Matrix    
// [[Rcpp::export]]
NumericMatrix getMatrix(DataFrame df, int max_zones) {
  
    NumericVector i = df["I"];
    NumericVector j = df["J"];
    NumericVector v = df["Trip"];
    NumericMatrix m (max_zones, max_zones);
    
    for (int x = 0; x < v.size(); x++) {
      m(i[x] - 1, j[x] - 1) = v[x];
    }
    
  return m;
}    

// Function to convert matrix back to data frame      
 DataFrame mat2DF(NumericMatrix &mat) {
    
    int max_zones = mat.nrow();
    int cnt = 0;
    NumericVector i(max_zones * max_zones);
    NumericVector j(i.size());
    NumericVector v(i.size());
    
    for(int x = 0; x < max_zones; x++){
      for (int y = 0; y < max_zones; y++){
        i[cnt] = x + 1;
        j[cnt] = y + 1 ;
        v[cnt] =  mat(x,y);
        cnt += 1;
      }
    }

    DataFrame df = DataFrame::create( Named("I") = i,       
                                      Named("J") = j,
                                      Named("Trip") = v);   
  return df;
}      


// Function to convert Matrix row/col sums
// While I like accumulate, something is odd here where last row/col is reported as first
// don't [[Rcpp::export]]
List getMatRCAccum(NumericMatrix &mat ) {
  
    NumericVector row_sum(mat.nrow());
    NumericVector col_sum(mat.ncol());
    
    for (int x = 0; x < row_sum.size(); ++x) {
      row_sum[x] = std::accumulate(mat(x,_).begin(), mat(x,_).end(), 0.0);
    }
  
    for (int x = 0; x < col_sum.size(); x++) {
      col_sum[x] = std::accumulate(mat(_,x).begin(), mat(_,x).end(), 0.0);
    }
    
  List ret;
  ret["rowSum"] = row_sum;
  ret["colSum"] = col_sum;
  return ret;
}  

// Apply row, col factors   
// don't [[Rcpp::export]]
NumericVector computeFac(NumericVector target, NumericVector matSum) {
  
    int nr = matSum.size();
    NumericVector fac(nr);
    
    for (int x = 0; x < nr ; x++) {
       if(matSum[x] > 0) {
         fac[x] = target[x] / matSum[x];
       } else{
         fac[x] = 1.0;
       }
    } 
    
  return fac;
} 
    
// Apply row, col factors
// don't [[Rcpp::export]]
NumericMatrix applyFac(NumericMatrix &mat, NumericVector fac, String type ) {

    if(type == "col"){
      for (int x = 0; x < fac.size() ; ++x) {
        if(fac[x] > 0){
           mat(_,x) = mat(_,x) * fac[x] ;
        }
        // DEBUG:: write out new mat values
        // if(x < 10)  Rcout << "new value after at row 1: " <<  mat(1, x) << "\n" ;
      } 
    } else if (type == "row"){  
      for (int x = 0; x < fac.size() ; ++x) {
        if(fac[x] > 0){
          mat(x,_) = mat(x,_) * fac[x] ;
        }
        // DEBUG:: write out new mat values
        // if(x < 10)  Rcout << "new value after at col 1: " <<  mat(x, 1) << "\n" ;
      } 
    } else{
      Rcout << "ERROR: specify type as 'row' or 'col'" << "\n" ;
    }
  
  return mat;

}  

// Function to do both Row column sums 
// don't [[Rcpp::export]]
List getMatRCSums2(NumericMatrix &mat) {  
  
    int max_zones = mat.nrow();
    NumericVector row_total (max_zones);
    NumericVector col_total (max_zones);
    
    for (int x = 0; x < max_zones; ++x) {
      for (int y = 0; y < max_zones; ++y) {
        row_total[x] += mat(x, y);
        col_total[y] += mat(x, y);
      }
    }
    
    List ret;
    ret["rowSum"] = row_total;
    ret["colSum"] = col_total;
    return ret;
}    

// Function to get row or col sum
NumericVector getMatRCSums(NumericMatrix &mat, String type) {  
  
    int nr = mat.nrow(), nc = mat.ncol();
    NumericVector vec_sum(nr) ; 
    Rcout << "Number of zones: "<<  nr << " \n";
    
    if(type == "row"){
      for (int x = 0; x < nr; ++x) {
        for(int y = 0; y < nc; ++y) {
          if(mat(x , y) > 0){
            vec_sum[x] += mat(x , y);
          }
        }
      } 
    } else if (type == "col"){
      for (int y = 0; y < nc; ++y) {
        NumericVector col = mat( _ , y);
        for(int x = 0; x < col.size() ; ++x) {
          // vec_sum[x] += mat(x , y);
          if(col[x] > 0){
            vec_sum[y] += col[x] ;
          }
        }
      }
    } else{
      Rcout << "ERROR: specify type as 'row' or 'col' \n" ;
    }

    return vec_sum;
}    
    
