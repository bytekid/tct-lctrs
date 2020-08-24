import os, subprocess, multiprocessing, sys
import time
from os import listdir, system
from os.path import isfile, join
import re
import sys
from functools import reduce
import operator
sys.path.append('.')
from results import Results
import json

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


htmlresult = ""
def htmlprint(s):
  global htmlresult
  htmlresult = htmlresult + "\n" + s
  print(s)

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
    degree = 666
    for d in degrees.keys():
      if complexity.startswith(d):
        degree = degrees[d]
        break

    print(fname + ": " + complexity_str + " '" + str(degree) + "'")
    if degree == 666:
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
  htmlprint("<html>" + style)
  htmlprint("<body><table id=\"results\">")
  toolnames = ["<th>" + t + "</th>" for t in tools]
  htmlprint("<tr><th>&nbsp;</th>" + reduce(operator.add, toolnames, "") + "</tr>")
  for r in sorted(jobs, key = lambda r: r["path"]):
    name = r["path"][len("koat-evaluation/examples/"):-5] # drop .koat
    htmlprint("<tr><td>" + name + "</td>")
    degrees = {}
    # TCT
    degrees["TCT"] = r["degree"] if "degree" in r else None
    htmlprint("<td>" + ("?" if degrees["TCT"] is None else str(degrees["TCT"])) + "</td>")
    
    # other tools
    for tool in tools[1:]:
      tresult = results[tool][name]
      degrees[tool] = tresult["degree"] if "degree" in tresult else None
      htmlprint("<td>" + ("?" if degrees[tool] is None else str(degrees[tool])) + "</td>")
    htmlprint("</tr>")

    m = min([d for d in degrees.values() if not (d is None)] + [10001])
    for tool in tools:
      summary[tool]["solved"] += 0 if degrees[tool] is None else 1
      summary[tool]["min"] += 0 if degrees[tool] is None or degrees[tool] != m else 1

  solveds = ["<td style=\"summary\">" + str(summary[t]["solved"]) + "</td>" for t in tools]
  trsumm= "<tr>"
  htmlprint(trsumm + "<td style=\"summary\">solved</td>" + reduce(operator.add, solveds, "") + "</tr>")
  mins = ["<td>" + str(summary[t]["min"]) + "</td>" for t in tools]
  htmlprint(trsumm + "<td>minimal</td>" + reduce(operator.add, mins, "") + "</tr>")

  htmlprint("</table></body></html>")

  # dump into json
  res = json.dumps(jobs, sort_keys=True, indent=2)
  githead = subprocess.run(['git', 'log', '--pretty=format:\'%h\'', '-n', '2'], stdout=subprocess.PIPE)
  heads = githead.stdout.decode('utf-8').splitlines()
  print(heads)
  currenthead = heads[0].strip("'")
  lasthead = heads[0].strip("'")
  htmlprint("<!-- git head " + currenthead + " -->")

  resdir = "results"
  rname = currenthead + ".json" # t.strftime('%Y-%m-%d') + 
  if not os.path.exists(resdir):
    os.makedirs(resdir)
  rfile = open(resdir + "/" + rname, "w")
  rfile.write(res)

  global htmlresult
  rfile = open(resdir + "/" + currenthead + ".html", "w")
  rfile.write(htmlresult)

  return summary


if __name__ == "__main__":
  if len(sys.argv) < 2:
    print("example dir needed")
  exampledir = sys.argv[1]

  # collect jobs
  jobs = []
  for subdir, dirs, files in os.walk(exampledir):
    for file in files:
      if file[-5:] == ".koat":
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

  l = len(jobs)
  for t in summary.keys():
    print("  %s: %d/%d solved (%d min)" % (t, summary[t]["solved"], l, summary[t]["min"]))
  