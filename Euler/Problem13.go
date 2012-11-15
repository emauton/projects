/* http://projecteuler.net/profile/cian.png
   Problem 13. Work out the first ten digits of the sum of one-hundred 50-digit numbers.
   2012-07-18 */

package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

// ParseNumbers parses the 50-digit input numbers in slices of 10 digits at a time.
func ParseNumbers(filename string) (numbers [][]int64) {
	f, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}
	lines := strings.Split(string(f), "\n")
	numbers = make([][]int64, 0, len(lines))
	for _, l := range lines {
		if len(l) == 0 {
			continue
		}
		parts := make([]int64, 0, 5)
		for i := 0; i < len(l); i += 10 {
			n, err := strconv.ParseInt(l[i:i+10], 10, 64)
			if err != nil {
				panic(err)
			}
			parts = append(parts, n)
		}
		numbers = append(numbers, parts)
	}
	return
}

func main() {
	numbers := ParseNumbers("data/Problem13")

	// We use 64-bit integers to do each 10-digit section of the sum
	// and manually carry over.
	var sum, carry int64
	for j := len(numbers[0]) - 1; j >= 0; j-- {
		sum = carry
		for i := 0; i < len(numbers); i++ {
			sum += numbers[i][j]
		}
		carry = sum / 1e10
	}
	fmt.Println(sum)
}
