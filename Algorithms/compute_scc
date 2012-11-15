#!/usr/bin/python

# Programming question week 4, http://www.algo-class.org/ June 2012.

# The file contains the edges of a directed graph. Vertices are labeled as
# positive integers from 1 to 875714. Every row indicates an edge, the vertex
# label in first column is the tail and the vertex label in second column is
# the head (recall the graph is directed, and the edges are directed from the
# first column vertex to the second column vertex). So for example, the 11th
# row looks liks : "2 47646". This just means that the vertex with label 2 has
# an outgoing edge to the vertex with label 47646

# Your task is to code up the algorithm from the video lectures for computing
# strongly connected components (SCCs), and to run this algorithm on the given
# graph. 

# Output Format: You should output the sizes of the 5 largest SCCs in the given
# graph, in decreasing order of sizes, separated by commas (avoid any spaces).
# So if your algorithm computes the sizes of the five largest SCCs to be 500,
# 400, 300, 200 and 100, then your answer should be "500,400,300,200,100". If
# your algorithm finds less than 5 SCCs, then write 0 for the remaining terms.
# Thus, if your algorithm computes only 3 SCCs whose sizes are 400, 300, and
# 100, then your answer should be "400,300,100,0,0".

# WARNING: This is the most challenging programming assignment of the course.
# Because of the size of the graph you may have to manage memory carefully. The
# best way to do this depends on your programming language and environment, and
# we strongly suggest that you exchange tips for doing this on the discussion
# forums.

import collections
import sys
import random

def return_emptylist():
  return []

def return_false():
  return False

def ParseGraph(filename):
  """Parse a graph into a list of edges for programming Q.4

     Args:
     - filename: the on-disk graph representation
     Returns:
     - edges = [(vertex_1, vertex_2), ...]
  """
  edges = []
  for l in open(filename):
    fields = [int(f) for f in l.split()]
    edges.append(tuple(fields))

  adjacency = collections.defaultdict(return_emptylist)
  reverse_adjacency = collections.defaultdict(return_emptylist)
  for e in edges:
    adjacency[e[0]] = adjacency[e[0]] + [e] 
    reverse_adjacency[e[1]] = reverse_adjacency[e[1]] + [(e[1], e[0])]

  return adjacency, reverse_adjacency, edges

t = 0
s = 0
finishing = {}
leader = {}
explored = collections.defaultdict(return_false)
def ResetState():
  global t, s, finishing, leader, explored
  t = 0
  s = 0
  finishing = {}
  leader = {}
  explored = collections.defaultdict(return_false)

def DFSLoop(edges, labeling, reversed = False):
  global s
  for i in labeling:
    if not explored[i]:
      s = i
      DFS(edges, i, reversed)

forward_adjacency = {}
reverse_adjacency = {}
def DFS(edges, start, reversed = False):
  global t
  if reversed:
    adjacency = reverse_adjacency
  else:
    adjacency = forward_adjacency

  # Iterative (i.e. manually managing a stack) solution.
  stack = []
  stack.append((start, 1))

  while len(stack) > 0:
    current, phase = stack.pop()
    if phase == 1:
      explored[current] = True
      leader[current] = s
      edge_found = False
      for edge in adjacency[current]:
        if not explored[edge[1]]:
          stack.append((current, 1))
          stack.append((edge[1], 1))
          edge_found = True
          break
      if not edge_found:
        stack.append((current, 2))
    if phase == 2:
      t += 1
      finishing[current] = t
      sys.stderr.write('Finished %s\n' % current)

forward_adjacency, reverse_adjacency, edges = ParseGraph(sys.argv[1])

sys.stderr.write('Graph parsed\n')

num_nodes = max([e[0] for e in edges] + [e[1] for e in edges])
labeling = xrange(num_nodes, 0, -1)
DFSLoop(edges, labeling, True)

sys.stderr.write('Reverse DFSLoop done\n')

inverse_finishing = dict((v, k) for k, v in finishing.iteritems())
finish_labeling = [inverse_finishing[i] for i in xrange(num_nodes, 0, -1)]

ResetState()
DFSLoop(edges, finish_labeling)

sys.stderr.write('Forward DFSLoop done\n')

sccs = {}
for i in leader:
  if leader[i] not in sccs:
    sccs[leader[i]] = [i]
  else:
    sccs[leader[i]].append(i) 

for i in sccs:
  print '%s\t%s' % (i, len(sccs[i]))
