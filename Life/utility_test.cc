#include <string>
using namespace std;

#include "gtest/gtest.h"
#include "utility.h"

TEST(UtilityTest, ValidTokens) {
  string valid("4 5 6");

  ASSERT_NO_THROW({
    vector<int> v = ParseInts(valid);
    EXPECT_EQ(3ul, v.size());
    EXPECT_EQ(4, v[0]);
    EXPECT_EQ(5, v[1]);
    EXPECT_EQ(6, v[2]);
  });
}

TEST(UtilityTest, InvalidTokens) {
  string invalid("4 a 6");

  try {
    ParseInts(invalid);
  } catch (string e) {
    EXPECT_EQ("invalid character: a 6", e);
  }
}
