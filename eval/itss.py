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

resdir = "results"
htmlresult = ""
def htmlprint(s):
  global htmlresult
  htmlresult = htmlresult + "\n" + s
  print(s)

def htmlcomment(s):
  htmlprint("<!-- " + s + " -->")

def dump_html(currenthead):
  global htmlresult, resdir
  rfile = open(resdir + "/" + currenthead + ".html", "w")
  rfile.write(htmlresult)

def dump_json(currenthead, jobs):
  global resdir
  data = dict([ (j['path'], j) for j in jobs ])
  res = json.dumps(data, sort_keys=True, indent=2)
  rname = currenthead + ".json" # t.strftime('%Y-%m-%d') + 
  if not os.path.exists(resdir):
    os.makedirs(resdir)
  rfile = open(resdir + "/" + rname, "w")
  rfile.write(res)

def compare_versions(jobs, cmphead):
  global resdir
  cmpjson = resdir + "/" + cmphead + ".json"
  htmlcomment("compare with " + str(cmphead))
  if not os.path.exists(cmpjson):
    print("no comparison result file found")
    return
  
  cmpfile = open(cmpjson, "r") 
  cmpres = cmpfile.read()
  cmpdata = json.loads(cmpres)
  data = dict([ (j['path'], j) for j in jobs ])

  def success(p, d):
    return "degree" in d[p]

  def fail(p, d):
    return not success(p, d)

  def differ(p):
    return success(p, data) and success(p, cmpdata) and data[p]["degree"] != cmpdata[p]["degree"]

  gained = [ p for p in data.keys() if success(p, data) and fail(p, cmpdata)]
  lost = [ p for p in data.keys() if success(p, cmpdata) and fail(p, data)]
  differ = [ (p, data[p]["degree"], cmpdata[p]["degree"]) for p in data.keys() if differ(p)]
  htmlcomment("gained: " + reduce(lambda p, s: s + " " + p, gained, ""))
  htmlcomment("lost: " + reduce(lambda p, s: s + " " + p, lost, ""))
  htmlcomment("different: " + reduce(lambda s, p: s + " " + str(p[0]) + "(" + \
    str(p[1]) + " vs " + str(p[2]) + ")", differ, ""))
  return cmpdata


 
def get_git_heads():
  githead = subprocess.run(['git', 'log', '--pretty=format:\'%h\'', '-n', '2'], stdout=subprocess.PIPE)
  heads = githead.stdout.decode('utf-8').splitlines()
  currenthead = heads[0].strip("'")
  lasthead = heads[1].strip("'")
  htmlcomment("current git head " + currenthead)
  return [currenthead, lasthead]

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
    degreestuples = [
      ("1", 0.),
      ("n^1", 1.),
      ("log(n)*n^1", 1.5),
      ("n^2", 2.),
      ("log(n)*n^2", 2.5),
      ("log(n)^2*n^2", 2.75),
      ("log(n)", .5),
      ("n^3", 3.),
      ("n^4", 4.),
    ]
    degrees = dict(degreestuples)
    degree = 666
    for (d, dd) in degreestuples:
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

def accumulate(jobs, cmphead):
  currenthead, lasthead = get_git_heads()
  if cmphead is None:
    cmphead = lasthead
  dump_json(currenthead, jobs)
  cmpdata = compare_versions(jobs, cmphead)

  summary= {}
  tools = ["TCT", cmphead, "KoAT", "CoFloCo", "PUBS"]
  for tool in tools:
    summary[tool] = {"solved": 0, "min": 0, "const": 0, "lin": 0, "quad": 0, "cub": 0, "log": 0, "exp": 0}
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
    
    # head to compare with
    cmpresult = cmpdata[r["path"]]
    degrees[cmphead] = cmpresult["degree"] if "degree" in cmpresult else None
    cmpdegree = degrees[cmphead]
    htmlprint("<td>" + ("?" if cmpdegree is None else str(cmpdegree)) + "</td>")

    # other tools
    for tool in tools[2:]:
      tresult = results[tool][name]
      degrees[tool] = tresult["degree"] if "degree" in tresult else None
      htmlprint("<td>" + ("?" if degrees[tool] is None else str(degrees[tool])) + "</td>")
    htmlprint("</tr>")

    m = min([d for d in degrees.values() if not (d is None)] + [10001])
    for tool in tools:
      summary[tool]["solved"] += 0 if degrees[tool] is None else 1
      summary[tool]["min"] += 0 if degrees[tool] is None or degrees[tool] != m else 1
      summary[tool]["const"] += 0 if degrees[tool] is None or degrees[tool] != 0 else 1
      summary[tool]["lin"] += 0 if degrees[tool] is None or degrees[tool] > 1 else 1
      summary[tool]["quad"] += 0 if degrees[tool] is None or degrees[tool] > 2 else 1
      summary[tool]["cub"] += 0 if degrees[tool] is None or degrees[tool] > 3 else 1
      summary[tool]["log"] += 0 if degrees[tool] is None or degrees[tool] != 0.5 else 1
      summary[tool]["exp"] += 0 if degrees[tool] is None or degrees[tool] < 10000 else 1

  solveds = ["<td style=\"summary\">" + str(summary[t]["solved"]) + "</td>" for t in tools]
  trsumm= "<tr>"
  htmlprint(trsumm + "<td style=\"summary\">solved</td>" + reduce(operator.add, solveds, "") + "</tr>")
  mins = ["<td>" + str(summary[t]["min"]) + "</td>" for t in tools]
  htmlprint(trsumm + "<td>minimal</td>" + reduce(operator.add, mins, "") + "</tr>")
  consts = ["<td>" + str(summary[t]["const"]) + "</td>" for t in tools]
  htmlprint(trsumm + "<td>constant</td>" + reduce(operator.add, consts, "") + "</tr>")
  lins = ["<td>" + str(summary[t]["lin"]) + "</td>" for t in tools]
  htmlprint(trsumm + "<td>&lt;= linear</td>" + reduce(operator.add, lins, "") + "</tr>")
  quads = ["<td>" + str(summary[t]["quad"]) + "</td>" for t in tools]
  htmlprint(trsumm + "<td>&lt;= quadratic</td>" + reduce(operator.add, quads, "") + "</tr>")
  cubs = ["<td>" + str(summary[t]["cub"]) + "</td>" for t in tools]
  htmlprint(trsumm + "<td>&lt;= cubic</td>" + reduce(operator.add, cubs, "") + "</tr>")
  exps = ["<td>" + str(summary[t]["exp"]) + "</td>" for t in tools]
  htmlprint(trsumm + "<td>exponential</td>" + reduce(operator.add, exps, "") + "</tr>")
  exps = ["<td>" + str(summary[t]["log"]) + "</td>" for t in tools]
  htmlprint(trsumm + "<td>logarithmic</td>" + reduce(operator.add, exps, "") + "</tr>")

  htmlprint("</table></body></html>")

  # dump results into json
  dump_html(currenthead)

  return summary


if __name__ == "__main__":
  if len(sys.argv) < 2:
    print("example dir needed")
  exampledir = sys.argv[1]

  comparehead = sys.argv[2] if len(sys.argv) > 2 else None

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
  summary = accumulate(tctresults.get(), comparehead)

  #results = []
  #for j in jobs:
  #  results.append(check(j))

  l = len(jobs)
  for t in summary.keys():
    print("  %s: %d/%d solved (%d min)" % (t, summary[t]["solved"], l, summary[t]["min"]))
  