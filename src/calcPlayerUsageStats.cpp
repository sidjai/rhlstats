#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericVector cppCalcTryHard(
	NumericVector points,
	NumericVector delPoints,
	NumericVector tendtryHard)
{
	NumericVector tryhard = delPoints / (tendtryHard * points);
	return( tryhard );
}

//[[Rcpp::export]]
NumericVector cppCalcLifeTime(
	NumericVector uptimes,
	NumericVector dailyTend,
	NumericVector uptimeTend)
{
	NumericVector lifeTime = (uptimes * dailyTend) / uptimeTend;
	return( lifeTime );
}
