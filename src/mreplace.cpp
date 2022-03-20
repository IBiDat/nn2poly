#include <Rcpp.h>
using namespace Rcpp;

//' Replace multiple characters in a string at the same time
//'
//' @param str original string
//' @param rep vector containing the needed maping to perform the replacement
//'
//' @return str
//'
//'
// [[Rcpp::export]]
std::string mreplace(std::string str, CharacterVector rep) {
  CharacterVector names = rep.names();
  std::unordered_map<char, char> map;

  for (int i = 0; i < rep.size(); i++) {
    map[names[i][0]] = rep[i][0];
  }

  for (char& c : str) {
    auto got = map.find(c);
    if (got != map.end())
      c = got->second;
  }

  return str;
}



/*** R
mreplace("1,1,2", c("1"="2", "2"="4"))
*/
