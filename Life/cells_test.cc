#include <cstdio>
#include <iostream>
#include <memory>
#include <string>
using namespace std;

#include "gtest/gtest.h"
#include "cells.h"

TEST(CellsTest, ConstructorAndGetters) {
  Cells c(10, 20);
  EXPECT_EQ(10, c.Rows());
  EXPECT_EQ(20, c.Cols());
}

TEST(CellsTest, DefaultIsDead) {
  Cells c(10, 20);

  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 20; j++) {
      EXPECT_EQ(Cells::DEAD, c(i, j));
    }
  }
}

TEST(CellsTest, SettingCells) {
  Cells c(1, 1);
  c(0, 0) = Cells::ALIVE;
  EXPECT_EQ(Cells::ALIVE, c(0, 0));
  c(0, 0) = Cells::DEAD;
  EXPECT_EQ(Cells::DEAD, c(0, 0));
}

TEST(CellsTest, OutOfBoundsReturnsInvalid) {
  Cells c(1, 1);
  EXPECT_EQ(Cells::INVALID, c(0, 1));
  EXPECT_EQ(Cells::INVALID, c(0, -1));
  EXPECT_EQ(Cells::INVALID, c(1, 0));
  EXPECT_EQ(Cells::INVALID, c(-1, 0));
  c(0, 1) = Cells::DEAD;
  EXPECT_EQ(Cells::INVALID, c(0, 1));
}

TEST(CellsTest, CountingWorks) {
  Cells c(3, 3);
  c(0,0) = Cells::ALIVE;
  c(1,0) = Cells::ALIVE;
  c(2,1) = Cells::ALIVE;
  EXPECT_EQ(1, c.CountLiving(0, 0));
  EXPECT_EQ(2, c.CountLiving(1, 0));
  EXPECT_EQ(3, c.CountLiving(1, 1));
  EXPECT_EQ(1, c.CountLiving(2, 1));
}

TEST(CellsTest, AssignmentWorks) {
  Cells c(3, 3);
  Cells d(2, 3);

  ASSERT_FALSE(c.Equals(d));

  Cells p(2, 2);
  Cells q(2, 2);
  p(0, 0) = Cells::ALIVE;

  ASSERT_FALSE(p.Equals(q));

  Cells s(2, 2);
  Cells t(2, 2);
  s(1, 0) = Cells::ALIVE;
  t(1, 0) = Cells::ALIVE;

  ASSERT_TRUE(s.Equals(t));
}

TEST(CellsTest, CopyConstructorWorks) {
  Cells c(3, 3);
  Cells d(c);
  Cells e = c;

  ASSERT_TRUE(c.Equals(d));
  ASSERT_TRUE(c.Equals(e));
}

TEST(CellsFilesTest, ValidFile) {
  ASSERT_NO_THROW({
    Cells c("fixtures/valid.cells");
    EXPECT_EQ(2, c.Rows());
    EXPECT_EQ(3, c.Cols());
    EXPECT_EQ(Cells::ALIVE, c(0,0));
  });
}

TEST(CellsFilesTest, MissingFile) {
  try {
    Cells c("fixtures/nonexistent.cells");
  } catch(Cells::Error e) {
    EXPECT_EQ("Failed to open: fixtures/nonexistent.cells", e.what());
  }
}

TEST(CellsFilesTest, BadDimensions) {
  try {
    Cells c("fixtures/dimensions.cells");
  } catch(Cells::Error e) {
    EXPECT_EQ("#dimensions != 2 in fixtures/dimensions.cells", e.what());
  }
}

TEST(CellsFilesTest, BadCells) {
  try {
    Cells c("fixtures/bad.cells");
  } catch(Cells::Error e) {
    EXPECT_EQ("Error parsing cells on line 2: invalid character: a", e.what());
  }
}

TEST(CellsFilesTest, BadColumns) {
  try {
    Cells c("fixtures/columns.cells");
  } catch(Cells::Error e) {
    EXPECT_EQ("Wrong number of columns on line 2", e.what());
  }
}

TEST(CellsFilesTest, BadValue) {
  try {
    Cells c("fixtures/value.cells");
  } catch(Cells::Error e) {
    EXPECT_EQ("Invalid value 5 on line 2", e.what());
  }
}

TEST(CellsFilesTest, WriteWorks) {
  ASSERT_NO_THROW({
    Cells c("fixtures/valid.cells"); 
    c.Write("/tmp/write.cells"); 
    Cells d("/tmp/write.cells"); 
    ASSERT_TRUE(c.Equals(d));
    remove("/tmp/write.cells");
  });
}
