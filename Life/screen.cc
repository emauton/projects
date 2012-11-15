#include "screen.h"

Screen::~Screen() {
  for (auto entry : bitmaps)
    SDL_FreeSurface(entry.second);
  SDL_FreeSurface(screen);
  TTF_CloseFont(font);
  TTF_Quit();
  SDL_Quit();
}

void Screen::Initialize() {
  if (SDL_Init(SDL_INIT_VIDEO) < 0)
    throw Error("SDL_Init: " + string(SDL_GetError()));

  if (TTF_Init() < 0)
    throw Error("TTF_Init: " + string(TTF_GetError()));

  screen = SDL_SetVideoMode(width, height, 32,
                            SDL_HWSURFACE | SDL_DOUBLEBUF | SDL_FULLSCREEN);
  if (screen == NULL)
    throw Error("SDL_SetVideoMode: " + string(SDL_GetError()));

  font = TTF_OpenFont(fontfile.c_str(), 24);
  if (!font)
    throw Error("TTF_OpenFont: " + string(TTF_GetError()));
}

void Screen::LoadBMP(const string& file) {
  SDL_Surface *bmp = SDL_LoadBMP(file.c_str());
  if (bmp == NULL)
    throw Error("SDL_LoadBMP(" + file + "): " + string(SDL_GetError()));
  bitmaps[file] = bmp;
}

void Screen::ShowBMP(const string& bmp, int x, int y) {
  auto it = bitmaps.find(bmp);
  if (it != bitmaps.end())
    BlitSurface(it->second, x, y);
}

void Screen::SetText(const string& text) {
  SDL_Color c = {0, 0, 0, 0};
  SDL_Surface *t = TTF_RenderText_Solid(font, text.c_str(), c);
  BlitSurface(t, width - t->w - 4, 4);
  SDL_FreeSurface(t);
}

Screen::Event Screen::GetEvent() {
  SDL_Event evt;
  Event ret = NONE;

  // The event queue may have lots of new events (mouse moves, etc.)
  while (SDL_PollEvent(&evt)) {
    if (evt.type == SDL_QUIT)
      ret = QUIT;
    if (evt.type == SDL_KEYDOWN) {
      if (evt.key.keysym.sym == SDLK_ESCAPE)
        ret = QUIT;
      if (evt.key.keysym.sym == SDLK_SPACE)
        ret = PAUSE;
    }
  }
  return ret;
}

void Screen::BlitSurface(SDL_Surface *surface, int x, int y) {
  SDL_Rect dest;
  dest.x = x;
  dest.y = y;
  dest.w = surface->w;
  dest.h = surface->h;
  SDL_BlitSurface(surface, NULL, screen, &dest);
}
