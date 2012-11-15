#ifndef _SCREEN_H_
#define _SCREEN_H_

#include <string>
#include <map>
using namespace std;

#include "SDL.h"
#include "SDL_ttf.h"

// Graphics services for SDL version of Life.
// Maintains a width * height screen and exports just what Life needs
// from SDL. Cf. sdl_main.cc for sample use.
class Screen {
public:
  class Error {
  public:
    Error(const string& text) : error(text) { }
    const string& what() { return error; }
  private:
    const string error;
  };

  Screen(int w, int h, const string& filename) 
      : width(w), height(h),
        fontfile(filename), screen(0), font(0) { }
  ~Screen();

  void Initialize();

  // We maintain a map of bitmaps keyed by filename.
  void LoadBMP(const string& file);
  void ShowBMP(const string& file, int x, int y);

  // Draw black text in the top right.
  void SetText(const string& text);

  void Render() {
    SDL_UpdateRect(screen, 0, 0, width, height);
  }
  void Delay(int milliseconds) {
    SDL_Delay(milliseconds);
  }

  // Export useful events.
  enum Event {
    QUIT,
    PAUSE,
    NONE
  };
  Event GetEvent();

private:
  void BlitSurface(SDL_Surface *surface, int x, int y);

  int width, height;
  const string& fontfile;
  SDL_Surface *screen;
  TTF_Font *font;
  map<string, SDL_Surface*> bitmaps;
};
#endif
