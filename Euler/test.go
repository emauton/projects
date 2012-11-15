package main

import "fmt"

func main() {
	var f, n int64 = 1083299, 10e16

	table := make(map[int64]bool, 1000000)
	for i := f; i < n; i += f {
		table[i] = true
	}
	fmt.Println(len(table))
}
