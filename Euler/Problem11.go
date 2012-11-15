/* http://projecteuler.net/profile/cian.png
   Problem 11. What is the greatest product of four adjacent numbers on the
               same straight line in the 20 by 20 grid?
   2012-07-16 */

package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func ParseGrid(filename string) (grid [][]int) {
	f, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}
	lines := strings.Split(string(f), "\n")
	grid = make([][]int, 0, len(lines))
	for _, l := range lines {
		if len(l) == 0 {
			continue
		}
		nums := make([]int, 0, len(l))
		for _, s := range strings.Split(l, " ") {
			n, err := strconv.Atoi(s)
			if err != nil {
				panic(err)
			}
			nums = append(nums, n)
		}
		grid = append(grid, nums)
	}
	return
}

func main() {
	grid := ParseGrid("data/Problem11")
	n := len(grid)
	max := 0
	// We traverse the grid as though reading a page left-to-right.
	// We multiply out the "east", "southeast", "south" and "southwest"
	// sets of 4 cells. Because of the way we traverse, this covers the
	// remaining directions.
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			// Crufty and repetitive. Must do better.
			// "east"
			if j <= n-4 {
				mult := 1
				for k := 0; k < 4; k++ {
					mult *= grid[i][j+k]
				}
				if mult > max {
					max = mult
				}
			}
			// "southeast"
			if i <= n-4 && j <= n-4 {
				mult := 1
				for k := 0; k < 4; k++ {
					mult *= grid[i+k][j+k]
				}
				if mult > max {
					max = mult
				}
			}
			// "south"
			if i <= n-4 {
				mult := 1
				for k := 0; k < 4; k++ {
					mult *= grid[i+k][j]
				}
				if mult > max {
					max = mult
				}
			}
			// "southwest"
			if i <= n-4 && j >= 4 {
				mult := 1
				for k := 0; k < 4; k++ {
					mult *= grid[i+k][j-k]
				}
				if mult > max {
					max = mult
				}
			}
		}

	}
	fmt.Println(max)
}
