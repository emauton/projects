// http://stripe-ctf.com level 0.
// Offers about a 4x speedup over ~same algorithm in Ruby, but is much longer
// and harder to understand.
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"runtime/pprof"
	"strings"
)

var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")

func main() {
	flag.Parse()
	args := flag.Args()

	// A little standalone pprof support.
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}

	var dict string
	if len(args) < 1 {
		dict = "/usr/share/dict/words"
	} else {
		dict = args[0]
	}

	f, err := ioutil.ReadFile(dict)
	if err != nil {
		log.Fatal(err)
	}
	lines := strings.Split(string(f), "\n")

	// Without the capacity hint, runtime is dominated by
	// map "evacuation" as it repeatedly reallocates itself.
	entries := make(map[string]bool, len(lines))
	for _, entry := range lines {
		entries[entry] = true
	}

	input, err := ioutil.ReadAll(bufio.NewReader(os.Stdin))
	if err != nil {
		log.Fatal(err)
	}

	// Lots less readable than the Ruby version's cute gsub.
	// We end up going to pains here to preserve delimiters;
	// some exposure of skipped delimiter text by bufio.Scanner
	// would have made this much neater.
	in_word := false
	begin := 0
	for i := 0; i < len(input); i++ {
		if input[i] == ' ' || input[i] == '\n' {
			if in_word {
				word := string(input[begin:i])
				if entries[strings.ToLower(word)] {
					fmt.Print(word)
				} else {
					fmt.Printf("<%s>", word)
				}
				in_word = false
			}
			fmt.Printf("%c", input[i])
		} else {
			if !in_word {
				in_word = true
				begin = i
			}
		}
	}
}
