# You'll need to build & install SDL, SDL_ttf (and therefore freetype) in $(PREFIX).
PREFIX = /Users/cian/Desktop/Code/C++

# clang under Mac OS X; can do modern C++, e.g. std::unique_ptr.
CXX = c++

# Flags for $(CXX).
CXXFLAGS += -g -Wall -Wextra -std=c++11 -stdlib=libc++ -DGTEST_HAS_TR1_TUPLE=0 -I$(PREFIX)/include

# SDL (see $(PREFIX)/bin/sdl-config)
SDLFLAGS = -I/Users/cian/Desktop/Code/C++/include/SDL -D_GNU_SOURCE=1 -D_THREAD_SAFE
SDLLIBS = -L/Users/cian/Desktop/Code/C++/lib -lSDLmain -lSDL -Wl,-framework,Cocoa

# SDL_ttf + freetype
TTFLIBS = -L/Users/cian/Desktop/Code/C++/lib -lSDL_ttf -lfreetype -lz -lbz2

# gflags
GFLAGLIBS = -L/Users/cian/Desktop/Code/C++/lib -lgflags

# Google test.
GTEST_DIR = $(PREFIX)/gtest-1.6.0
GTFLAGS += -I$(GTEST_DIR)/include

# Remember to add new tests to this list.
TESTS = cells_test life_test utility_test

all : life sdlife $(TESTS)
clean :
	rm -f life sdlife $(TESTS) gtest.a gtest_main.a *.o

# Objects
cells.o : cells.h cells.cc
	$(CXX) $(CXXFLAGS) -c cells.cc

life.o : life.h life.cc
	$(CXX) $(CXXFLAGS) -c life.cc

screen.o : screen.h screen.cc
	$(CXX) $(CXXFLAGS) $(SDLFLAGS) -c screen.cc

utility.o : utility.h utility.cc
	$(CXX) $(CXXFLAGS) -c utility.cc

main.o : life.h main.cc
	$(CXX) $(CXXFLAGS) -c main.cc

sdl_main.o : life.h screen.h sdl_main.cc
	$(CXX) $(CXXFLAGS) $(SDLFLAGS) -c sdl_main.cc

# Binaries

life : main.o cells.o life.o utility.o
	$(CXX) $(CXXFLAGS) $^ -o $@

sdlife : sdl_main.o cells.o life.o screen.o utility.o
	$(CXX) $(CXXFLAGS) $(SDLLIBS) $(TTFLIBS) $(GFLAGLIBS) $^ -o $@

# Tests

cells_test.o : cells_test.cc cells.o utility.o
	$(CXX) $(CXXFLAGS) $(GTFLAGS) -c cells_test.cc

cells_test : cells_test.o cells.o utility.o gtest_main.a
	$(CXX) $(CXXFLAGS) $(GTFLAGS) $^ -o $@

life_test.o : life_test.cc cells.o life.o utility.o
	$(CXX) $(CXXFLAGS) $(GTFLAGS) -c life_test.cc

life_test : life_test.o cells.o life.o utility.o gtest_main.a
	$(CXX) $(CXXFLAGS) $(GTFLAGS) $^ -o $@

utility_test.o : utility_test.cc utility.o
	$(CXX) $(CXXFLAGS) $(GTFLAGS) -c utility_test.cc

utility_test : utility_test.o utility.o gtest_main.a
	$(CXX) $(CXXFLAGS) $(GTFLAGS) $^ -o $@

# Google Test boilerplate from their example Makefile.

# All Google Test headers.
GTEST_HEADERS = $(GTEST_DIR)/include/gtest/*.h \
                $(GTEST_DIR)/include/gtest/internal/*.h

# Builds gtest.a and gtest_main.a.

# Usually you shouldn't tweak such internal variables, indicated by a
# trailing _.
GTEST_SRCS_ = $(GTEST_DIR)/src/*.cc $(GTEST_DIR)/src/*.h $(GTEST_HEADERS)

# For simplicity and to avoid depending on Google Test's
# implementation details, the dependencies specified below are
# conservative and not optimized.  This is fine as Google Test
# compiles fast and for ordinary users its source rarely changes.
gtest-all.o : $(GTEST_SRCS_)
	$(CXX) $(GTFLAGS) -I$(GTEST_DIR) $(CXXFLAGS) -c \
            $(GTEST_DIR)/src/gtest-all.cc

gtest_main.o : $(GTEST_SRCS_)
	$(CXX) $(GTFLAGS) -I$(GTEST_DIR) $(CXXFLAGS) -c \
            $(GTEST_DIR)/src/gtest_main.cc

gtest.a : gtest-all.o
	$(AR) $(ARFLAGS) $@ $^

gtest_main.a : gtest-all.o gtest_main.o
	$(AR) $(ARFLAGS) $@ $^
