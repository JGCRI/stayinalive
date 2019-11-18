#include <Rcpp.h>

using namespace Rcpp;

//' Apply a drought duration threshold to a time series
//'
//' Eliminate any droughts with a duration less than the specified threshold.
//'
//' Note that this function modifies the tseries argument.  This is done in order to
//' avoid the overhead of copying the vector.  Since our intended use is to \code{apply}
//' this to the rows of a matrix, this is of no consequence, since apply will do its own
//' copying.  However, if the function is used on a vector, users should be prepared for
//' this un-R-like behavior.
//'
//' @param tseries (NumericVector) Time series of drought duration (time steps
//' not in drought have a duration of zero).  CAUTION:  This time series will be
//' modified.  See details.
//' @param thresh (double) Minimum drought duration.  Shorter droughts will
//' be eliminated.
//' @return (NumericVector) Time series with droughts shorter than the threshold removed.
//' @export
// [[Rcpp::export]]
NumericVector apply_duration_thresh(NumericVector tseries, double thresh)
{
    int len = tseries.size();

    // Iterate backwards; the final value tells us the total length of the drought, so
    // we know immediately if we need to drop it.
    int t = len-1;
    while(t > 0) {
        int drlen = tseries[t];
        if(drlen > 0 && drlen < thresh) {
            // Drought is too short.  Eliminate
            for(int i=0; i<drlen; ++i)
                tseries[t-i] = 0;
        }
        // Skip to the beginning of the drought we just processed.  We
        // can actually skip one more because we know that the time
        // slice immediately before the start of the drought had to be
        // no-drought.  As a bonus, since no-drought periods have
        // drlen==0, this same logic allows us to keep scanning when
        // we haven't found a drought.
        t -= drlen+1;
    }

    return tseries;
}
