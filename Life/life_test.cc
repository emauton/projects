#include <string>
using namespace std;

#include "gtest/gtest.h"
#include "life.h"

// Life mostly passes through to tested Cells routines,
// so focus here is on what's new.

class ExpectView : public Life::View {
public:
  ExpectView(Cells& e) : expected(e) { }

  virtual void Show(Cells& c) {
    ok = expected.Equals(c);
  }

  Cells expected;
  bool ok;
};

TEST(LifeTest, BasicGeneration) {
  ASSERT_NO_THROW({
    Life l;
    l.Load("fixtures/valid.cells");
    l.Evolve();

    // After 1 generation, we should have an empty grid.
    Cells empty(2, 3);
    ExpectView expected(empty);
    l.Display(expected);

    EXPECT_TRUE(expected.ok);
    EXPECT_EQ(2, l.Generation());
  });
}

// Support function for testing sequences of Life fixtures.
void Iterate(string sequence[], int length) {
  Life l;
  l.Load(sequence[0]);

  for (int i = 1; i < length; i++) {
    l.Evolve();
    Cells e(sequence[i]);
    ExpectView expected(e);
    l.Display(expected);

    EXPECT_TRUE(expected.ok) << "at sequence[" << i << "]";
    EXPECT_EQ(i + 1, l.Generation()) << "at sequence[" << i << "]";
  }
}

TEST(LifeTest, RowOfThree) {
  string sequence[6] = {"fixtures/rowofthree-1.cells",
                        "fixtures/rowofthree-2.cells",
                        "fixtures/rowofthree-1.cells",
                        "fixtures/rowofthree-2.cells",
                        "fixtures/rowofthree-1.cells",
                        "fixtures/rowofthree-2.cells"};
  Iterate(sequence, 6);
}

TEST(LifeTest, RowOfFive) {
  string sequence[5] = {"fixtures/rowoffive-1.cells",
                        "fixtures/rowoffive-2.cells",
                        "fixtures/rowoffive-3.cells",
                        "fixtures/rowoffive-4.cells",
                        "fixtures/rowoffive-5.cells"};
  Iterate(sequence, 5);
}
