#ifndef _LIFE_H_
#define _LIFE_H_

#include <string>
using namespace std;

#include "cells.h"

// Conway's game of Life.
// Sample use:
//   Life l;
//   l.Load("filename");
//   l.Evolve();
//   l.Save("filename2");
//   cout << l.Generation() << endl;
//   class TerminalView : public Life::View {
//   public:
//     virtual void Show(Cells& map) {
//       ...
//     }
//   } terminal;
//   l.Display(terminal);
// Implement a Life::View subclass for whatever display you want.

class Life {
public:
  class View {
  public:
    virtual void Show(Cells& c) = 0;
  };

  Life() : map(80, 20), generation(1) { }
  int Rows() { return map.Rows(); }
  int Cols() { return map.Cols(); }

  void Evolve();
  void Display(View& display) { display.Show(map); }
  void Load(const string& filename);
  void Save(const string& filename) { map.Write(filename); }
  int Generation() { return generation; }

private:
  Cells map;
  int generation;
};

#endif
