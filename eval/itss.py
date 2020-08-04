import os, subprocess, multiprocessing, sys
import time
from os import listdir
from os.path import isfile, join
import re
import sys
from functools import reduce
import operator
sys.path.append('.')
from results import Results

global results 
results = Results.data

global style
style = "<style> \
#results { \
  font-family: Arial, Helvetica, sans-serif;\
  border-collapse: collapse;\
  width: 100%;\
}\
#results td, #results th {\
  border: 1px solid #ddd;\
  padding: 8px;\
}\
#results tr:nth-child(even){background-color: #f2f2f2;}\
results tr:hover {background-color: #ddd;}\
#results th {\
  padding-top: 12px;\
  padding-bottom: 12px;\
  text-align: left;\
  background-color: #aaa;\
  color: white;\
}\
#results td.summary { font-weight:bold }\
</style>"

def check(job):
  p = job["problem"]
  fname = job["path"]
  pfile = open(fname, "r")
  
  cmd = "./sandbox 60 stack exec tct-its -- -s runtime "
  bashCommand = cmd + " " + fname
  #print(bashCommand)
  process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
  out, err = process.communicate()
  regex = re.compile(r'WORST_CASE\(\?,O\(([^\s]*)')
  match = re.search(regex, str(out))  
  if match != None:
    g = match.group(1)
    complexity = g[:-2]
    complexity_str = "O(" + complexity + ")"
    degrees = {
      "1" : 0.,
      "log(n)" : .5,
      "n^1" : 1.,
      "log(n)*n^1" : 1.5,
      "n^2" : 2.,
      "log(n)*n^2" : 2.5,
      "log(n)^2*n^2" : 2.75,
      "n^3" : 3.,
      "n^4" : 4.,
    }
    degree = degrees[complexity] if complexity in degrees else 666
    print(fname + ": " + complexity_str + " '" + str(degree) + "'")
    if complexity not in degrees:
      print("unknown '" + complexity + "'")
    job["degree"] = degree
  else:
    print(p + ": ?")
    job["errors"] = True
  return job

def accumulate(jobs):
  summary= {}
  tools = ["TCT", "KoAT", "CoFloCo", "PUBS"]
  for tool in tools:
    summary[tool] = {"solved": 0, "min": 0}
  print("<html>" + style)
  print("<body><table id=\"results\">")
  toolnames = ["<th>" + t + "</th>" for t in tools]
  print("<tr><th>&nbsp;</th>" + reduce(operator.add, toolnames, "") + "</tr>")
  for r in jobs:
    name = r["path"][len("koat-evaluation/examples/"):-5] # drop .koat
    print("<tr><td>" + name + "</td>")
    degrees = {}
    # TCT
    degrees["TCT"] = r["degree"] if "degree" in r else None
    print("<td>" + ("?" if degrees["TCT"] is None else str(degrees["TCT"])) + "</td>")
    
    # other tools
    for tool in tools[1:]:
      tresult = results[tool][name]
      degrees[tool] = tresult["degree"] if "degree" in tresult else None
      print("<td>" + ("?" if degrees[tool] is None else str(degrees[tool])) + "</td>")
    print("</tr>")

    m = min(degrees.values() + [10001])
    for tool in tools:
      summary[tool]["solved"] += 0 if degrees[tool] is None else 1
      summary[tool]["min"] += 0 if degrees[tool] is None or degrees[tool] != m else 1

  solveds = ["<td style=\"summary\">" + str(summary[t]["solved"]) + "</td>" for t in tools]
  trsumm= "<tr>"
  print(trsumm + "<td style=\"summary\">solved</td>" + reduce(operator.add, solveds, "") + "</tr>")
  mins = ["<td>" + str(summary[t]["min"]) + "</td>" for t in tools]
  print(trsumm + "<td>minimal</td>" + reduce(operator.add, mins, "") + "</tr>")

  print("</table></body></html>")

  return summary


if __name__ == "__main__":
  if len(sys.argv) < 2:
    print("example dir needed")
  exampledir = sys.argv[1]

  # collect jobs
  jobs = []
  for subdir, dirs, files in os.walk(exampledir):
    for file in files:
      if ".koat" in file:
        filepath = join(subdir, file)
        jobs.append({"problem":file, "path":filepath})
  
  # check in parallel
  print("<!--")
  numprocs = multiprocessing.cpu_count() - 1
  print("Doing " + str(len(jobs)) + " jobs with " + str(numprocs) + " procs")
  pool = multiprocessing.Pool(numprocs)
  tctresults = pool.map_async(check, jobs)
  pool.close()
  pool.join()
  print("-->")
  summary = accumulate(tctresults.get())

  #results = []
  #for j in jobs:
  #  results.append(check(j))

  l = len(summary)
  print("<!-- SUMMARY: ")
  for t in summary.keys():
    print("  %s: %d/%d solved (%d min)" % (t, summary[t]["solved"], l, summary[t]["min"]))
  print("-->")
  