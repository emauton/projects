#include <cstdlib>
#include <string>
#include <vector>
using namespace std;

#include "utility.h"

vector<int> ParseInts(const string& buffer) {
  char *end = (char *) buffer.c_str();
  vector<int> integers;
  while (*end) {
    if (isspace(*end)) {
      end++;
      continue;
    }
    long val = strtol(end, &end, 10);
    if (*end && !isspace(*end)) throw "invalid character: " + string(end);
    integers.push_back(val);
  }
  return integers;
}
