#include "life.h"

void Life::Evolve() {
  Cells newmap(map.Rows(), map.Cols());
  for (int i = 0; i < map.Rows(); i++) {
    for (int j = 0; j < map.Cols(); j++) {
      switch (map.CountLiving(i, j)) {
      case 3:
        newmap(i, j) = Cells::ALIVE;
        break;
      case 2:
        newmap(i, j) = map(i, j);
        break;
      default:
        newmap(i, j) = Cells::DEAD;
      }
    }
  }
  map = newmap;
  generation++;
}

void Life::Load(const string& filename) {
  Cells newmap(filename);
  map = newmap;
  generation = 1;
}
