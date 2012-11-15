#include <iostream>
#include <sstream>
using namespace std;

#include <gflags/gflags.h>

#include "life.h"
#include "screen.h"

DEFINE_string(alive, "resources/alive.bmp",
              "10x10 bitmap for living cells."); 
DEFINE_string(dead, "resources/dead.bmp",
              "10x10 bitmap for dead cells."); 
DEFINE_string(font, "/Library/Fonts/Krungthep.ttf",
              "Truetype font for generation display."); 

// A graphical view using the Screen class.
class GraphicsView : public Life::View {
public:
  GraphicsView(Screen& s, string& alive, string& dead)
      : screen(s), alive(alive), dead(dead) { }

  virtual void Show(Cells& map) {
    for (int x = 0; x < map.Cols(); x++) {
      for (int y = 0; y < map.Rows(); y++) {
        // Note transposition: the cell map is in terms of row, col where
        // the graphical output is in terms of x == col, y == row.
        if (map(y, x) == Cells::ALIVE)
          screen.ShowBMP(alive, x * 10, y * 10);
        else
          screen.ShowBMP(dead, x * 10, y * 10);
      }
    }
  }
private:
  Screen& screen;
  string& alive;
  string& dead;
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

  Screen screen(life.Cols() * 10, life.Rows() * 10, FLAGS_font);
  try {
    screen.Initialize();
    screen.LoadBMP(FLAGS_alive);
    screen.LoadBMP(FLAGS_dead);
  } catch (Screen::Error e) {
    cerr << "Initializing graphics: " << e.what() << endl;
    exit(1);
  }
  GraphicsView view(screen, FLAGS_alive, FLAGS_dead);

  bool running = true;
  bool paused = false;
  while (running) {
    Screen::Event evt = screen.GetEvent();
    if (evt == Screen::QUIT)
        running = false;
    if (evt == Screen::PAUSE)
        paused = (paused ? false : true);
    life.Display(view);
    ostringstream o;
    if (!paused) {
      o << "Generation " << life.Generation();
      life.Evolve();
    } else {
      o << "Paused (Generation " << life.Generation() << ")";
    }
    screen.SetText(o.str());
    screen.Render();
    screen.Delay(250);
  }
  return 0;
}
