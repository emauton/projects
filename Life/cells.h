#ifndef _CELLS_H_
#define _CELLS_H_

#include <string>
#include <memory>
using namespace std;

// A simple matrix of cells (alive or dead) for Conway's game of life.
// Sample use:
//   Cells c(2, 3);
//   c(0, 0) = Cells::ALIVE;
//   cerr << c.Rows() << ", " << c.Cols() << endl;
//   ...
//   try {
//     Cells c("filename");
//     ...
//   } catch (Cells::Error e) {
//     cerr << e.what() << endl;
//   }
//   ...
//   int n = c.CountLiving(1, 2);
//   ...
//   try {
//     c.Write("output");
//   } catch (Cells::Error e) {
//     cerr << e.what() << endl;
//   }

class Cells {
public:
  enum Entry {
    DEAD,
    ALIVE,
    INVALID
  };

  class Error {
  public:
    Error(const string& text) : error(text) { }
    const string& what() { return error; }
  private:
    const string error;
  };

  // Make an nrows * ncols matrix of DEAD cells.
  Cells(int nrows, int ncols)
      : rows(nrows), cols(ncols),
        grid(new Entry[nrows * ncols]), dummy(INVALID) {
    for (int i = 0; i < rows * cols; i++) {
      grid[i] = DEAD;
    }
  }

  // Read matrix contents from a file formatted as follows:
  //   #rows #columns
  //   ... grid of 1 = ALIVE, 0 = DEAD ...
  // For example,
  //   2 3
  //   0 0 1
  //   0 1 0
  Cells(const string& filename) : dummy(INVALID) {
    Read(filename);
  }

  // Copy constructor, assignment, equality.
  Cells(Cells& other) {
    Copy(other);
  }
  Cells& operator=(Cells &other) {
    if (this != &other)
      Copy(other);
    return *this;
  }
  bool Equals(Cells& c); 

  // Accessors.
  int Rows() { return rows; }
  int Cols() { return cols; }
  Entry& operator()(int i, int j) { return At(i, j); }

  // Count how many of the 8 cells around cell (x, y) are ALIVE.
  int CountLiving(int x, int y);

  // Write in the same format as the "filename" constructor.
  void Write(const string& filename);
    
private:
  Cells() {}
  Entry& At(int i, int j);
  void Read(const string& filename);
  void Copy(Cells& other);

  int rows, cols;
  unique_ptr<Entry[]> grid;
  Entry dummy; // So we can return references to an invalid value.
};

#endif
