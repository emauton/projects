#include <cstdlib>
#include <iostream>
using namespace std;

#include <unistd.h>

#include "life.h"

class TerminalView : public Life::View {
public:
  virtual void Show(Cells& map) {
    for (int i = 0; i < map.Rows(); i++) {
      for (int j = 0; j < map.Cols(); j++) {
        if (map(i, j) == Cells::ALIVE)
          cout << 'o';
        else
          cout << '.';
      }
      cout << endl;
    }
  }
};

int main(int argc, char *argv[]) {
  if (argc != 2) {
    cerr << "Usage: life <input file>" << endl;
    exit(1);
  }

  Life life;
  try {
    life.Load(argv[1]);
  } catch (Cells::Error e) {
    cerr << "Loading " << argv[1] << ": " << e.what() << endl;
    exit(1);
  }

  TerminalView view;
  string enter;
  while (1) {
    life.Display(view);
    cout << "Generation: " << life.Generation() << endl;
    life.Evolve();
    sleep(1);
  }

  return 0;
}
