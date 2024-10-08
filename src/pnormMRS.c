
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <Rdefines.h>

/* see https://ssrn.com/abstract=2842681 */
const double g2 =  -0.0150234471495426236132;
const double g4 = 0.000666098511701018747289;
const double g6 = 5.07937324518981103694e-06;
const double g8 = -2.92345273673194627762e-06;
const double g10 = 1.34797733516989204361e-07;
const double m2dpi = -2.0 / M_PI; //3.141592653589793115998;

/* nrow of a matrix */

int NROW
(
    SEXP x
) {

    SEXP a;
    a = getAttrib(x, R_DimSymbol);
    if (a == R_NilValue) return(XLENGTH(x));
    if (TYPEOF(a) == REALSXP)
        return(REAL(a)[0]);
    return(INTEGER(a)[0]);
}

/* ncol of a matrix */

int NCOL
(
    SEXP x
) {

    SEXP a;
    a = getAttrib(x, R_DimSymbol);
    if (a == R_NilValue) return(1);
    if (TYPEOF(a) == REALSXP)
        return(REAL(a)[1]);
    return(INTEGER(a)[1]);
}

/*
 Approximation of the standard normal CDF

   I. Matic, R. Radoicic, D. Stefanica (2018),
   A Sharp Polya-Based Approximation to the Normal CDF,
   Applied Mathematics and Computation 322, 111–122
   https://ssrn.com/abstract=2842681

 About 3x speed-up compared to pnorm()
*/

void C_pnormMRS (double *dx, int n, double *da) {

    double tmp;
    double x2, x4, x6, x8, x10, tx;
//    double m2dpi = -2.0 / 3.141592653589793115998;
    
    for (int i = 0; i < n; i++) {
        tx = dx[i];
        if (R_FINITE(tx)) {
            x2 = tx * tx;
            x4 = x2 * x2;
            x6 = x4 * x2;
            x8 = x6 * x2;
            x10 = x8 * x2;
            tmp = 1 + g2 * x2 + g4 * x4 + g6 * x6  + g8 * x8 + g10 * x10;
            tmp = m2dpi * x2 * tmp;
            da[i] = .5 + ((tx > 0) - (tx < 0)) * sqrt(1 - exp(tmp)) / 2.0;
        } else {
            da[i] = (tx > 0 ? 1.0 : 0.0);
        }
    }
}

/* R interface */

SEXP R_pnormMRS (SEXP x) {

    SEXP ans;
    
    int n = LENGTH(x);

    PROTECT(ans = allocVector(REALSXP, n));

    C_pnormMRS(REAL(x), n, REAL(ans));
    
    UNPROTECT(1);
    return(ans);
}

/* compute Phi(upper) - Phi(lower) and multiply probabilties for
   Marsaglia (1963) algorithm */
    
SEXP R_inner (SEXP upper, SEXP lower) {

   int nrow = NROW(lower);
   int ncol = NCOL(lower);
   
   double *tmpl, *tmpu, *dl, *du, *da;
   
   SEXP ans;
   
   PROTECT(ans = allocVector(REALSXP, ncol));
   da = REAL(ans);
   for (int j = 0; j < ncol; j++) da[j] = 1.0;
   du = REAL(upper);
   tmpu = R_Calloc(nrow, double);
   dl = REAL(lower);
   tmpl = R_Calloc(nrow, double);
   
   for (int j = 0; j < ncol; j++) {
       C_pnormMRS(du + j * nrow, nrow, tmpu);
       C_pnormMRS(dl + j * nrow, nrow, tmpl);
       for (int i = 0; i < nrow; i++) {
           /* also possible to use more accurate normal CDFs
           tmpu[i] = pnorm(du[j * nrow + i], 0.0, 1.0, 1.0, 0.0);
           tmpl[i] = pnorm(dl[j * nrow + i], 0.0, 1.0, 1.0, 0.0);
           */
           da[j] = da[j] * (tmpu[i] - tmpl[i]);
       }
   }
   
   R_Free(tmpl); R_Free(tmpu);
   UNPROTECT(1);
   return(ans);
}
