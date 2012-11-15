#!/usr/bin/python

# Playing with mapping "The Dark Room" on Youtube as of ~February 2012.

import json

graph = json.load(open('graph.json'))

print "digraph {"

for key in graph.keys():
  print '"%s" [label="%s"]' % (key, graph[key]["name"])
  for label, link_key in graph[key]["link"].items():
    print '"%s" -> "%s" [label="%s"]' % (key, link_key, label)

print "}"
