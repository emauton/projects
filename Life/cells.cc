#include <iostream>
#include <fstream>
#include <sstream>
using namespace std;

#include "cells.h"
#include "utility.h"

bool Cells::Equals(Cells& c) {
  if (rows != c.rows || cols != c.cols) return false;
  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
      if (At(i, j) != c(i, j)) return false;
    }
  }
  return true;
}

int Cells::CountLiving(int x, int y) {
  // Check the 8 positions around (x, y).
  // This ignores boundary errors because of how At() is implemented.
  int count = 0;
  for (int i = -1; i < 2; i++) {
    for (int j = -1; j < 2; j++) {
      if (i == 0 && j == 0) continue;
      if (At(x + i, y + j) == ALIVE) count++;
    }
  } 
  return count;
}

void Cells::Write(const string& filename) {
  ofstream output(filename); 
  if (!output) throw Error("Failed to open: " + filename);
  output << rows << " " << cols << endl;
  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
      output << At(i, j) << " ";
    }
    output << endl;
  }
  output.close();
}

Cells::Entry& Cells::At(int i, int j) {
  // We'll ignore out-of-range accesses rather than throw an exception.
  if (i < 0 || i >= rows || j < 0 || j >= cols) {
    dummy = INVALID;
    return dummy;
  }
  return grid[cols * i + j];
}

void Cells::Read(const string& filename) {
  ifstream input(filename); 
  if (!input) throw Error("Failed to open: " + filename);
  
  string buffer;
  getline(input, buffer);

  vector<int> integers;
  try {
    integers = ParseInts(buffer);
  } catch(string error) {
    throw Error("Error parsing dimensions: " + error);
  }
  if (integers.size() != 2)
    throw Error("#dimensions != 2 in " + filename);

  rows = integers[0];
  cols = integers[1];
  grid.reset(new Entry[rows * cols]);

  int line = 1;
  while(!input.eof()) {
    getline(input, buffer);
    try {
      integers = ParseInts(buffer);
    } catch(string error) {
      ostringstream e;
      e << "Error parsing cells on line " << line << ": " << error;
      throw Error(e.str());
    }
    if (int(integers.size()) != cols) {
      ostringstream e;
      e << "Wrong number of columns on line " << line;
      throw Error(e.str());
    }

    for (int i = 0; i < cols; i++) {
      Cells::Entry entry;
      switch(integers[i]) {
        case 0:
          entry = DEAD;
          break;
        case 1:
          entry = ALIVE;
          break;
        default:
          ostringstream e;
          e << "Invalid value " << integers[i] << " on line " << line;
          throw Error(e.str());
      }
      At(line - 1, i) = entry;
    }
    line++;
  } 
  input.close();
}

void Cells::Copy(Cells& other) {
  rows = other.rows;
  cols = other.cols;
  grid.reset(new Entry[rows * cols]);
  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
      At(i, j) = other(i, j);
    } 
  } 
}
