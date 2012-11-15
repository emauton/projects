#ifndef _UTILITY_H_
#define _UTILITY_H_

#include <string>
#include <vector>
using namespace std;

// Get a (whitespace-separated) list of integers from a string.
// Sample use:
// try {
//   vector<int> v = ParseInts("4 5 6 garbage");
// catch (string e) {
//   cout << "parse error: " << e << endl;
// }
vector<int> ParseInts(const string& buffer);

#endif
