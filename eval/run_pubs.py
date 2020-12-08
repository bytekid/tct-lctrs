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

resdir  = "results"
resdirhead = ""
htmlresult = ""

degreestuples = dict([
  ("1", 0.),
  ("X", 1.),
  ("X*log(2,X)", 1.5),
  ("log(2,X)*X", 1.5),
  ("X*X", 2.),
  ("log(2,X)*X^2", 2.5),
  ("log(2,X)", .5),
  ("X*X*X", 3.),
  ("X*X*X*X", 4.),
  ("X^5", 5.),
  ("X^6", 6.),
  ("X^7", 7.),
  ("pow(2,X)", 10000),
])

def htmlprint(s):
  global htmlresult, resdir
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

def get_old_tct_data():
  global resdir
  cmpjson = resdir + "/tctits.json"
  if not os.path.exists(cmpjson):
    print("no comparison result file found")
    return
  cmpfile = open(cmpjson, "r") 
  cmpres = cmpfile.read()
  cmpdata = json.loads(cmpres)
  return cmpdata

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


def outfilename(job):
  fname = job["path"]
  basename = fname[fname.rfind("/")+1:]
  outfile = resdirhead + "/" + str(job["id"])+ "_" + basename.replace(".koat", ".txt")
  return outfile

def check(job):
  global resdirhead, degreestuples
  p = job["problem"]
  fname = job["path"]
  pfile = open(fname, "r")
  
  #print(fname, flush=True)
  cmd = "./sandbox 60 ./pubs_static -computebound ubnormal_withlevelcountenabled -file "
  bashCommand = cmd + " " + fname
  process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
  out, err = process.communicate()

  out = str(out)

  if "failed" in out or "ERROR" in out or "TIMEOUT" in out:
    print("error")
    job["errors"] = True
    return job

  boundstr = "* Non Asymptotic Upper Bound:"
  i = out.find(boundstr)
  j = out.find("\\n", i)
  complexity = out[i + len(boundstr):j].strip() 
  rawcomplexity = complexity
  #print("raw: " + rawcomplexity, flush=True)

  # uniform parameters
  for p in "ABCDEFGHIJKLMNOPQRSTUVW":
    complexity = complexity.replace(p,"X")

  multregex = re.compile(r'[1-9][0-9]*\*')
  match = re.search(multregex, str(complexity))  
  while match != None:
    factor = str(match.group(0))
    complexity = complexity.replace(factor, "")
    match = re.search(multregex, str(complexity))  
  mcomplexity = complexity

  divregex = re.compile(r'\/[1-9][0-9]*')
  match = re.search(divregex, complexity)  
  while match != None:
    complexity = complexity.replace(str(match.group(0)), "")
    match = re.search(divregex, complexity)
  dcomplexity = complexity

  addregex = re.compile(r'-?[1-9][0-9]*\+')
  match = re.search(addregex, complexity)  
  while match != None:
    complexity = complexity.replace(str(match.group(0)), "")
    match = re.search(addregex, complexity)  
  acomplexity = complexity

  subregex = re.compile(r'[-+][1-9][0-9]*')
  match = re.search(subregex, complexity)  
  while match != None:
    complexity = complexity.replace(str(match.group(0)), "")
    match = re.search(subregex, complexity)  
  scomplexity = complexity

  logregex = re.compile(r'log\([3-9][0-9]*,')
  match = re.search(logregex, complexity)  
  while match != None:
    complexity = complexity.replace(str(match.group(0)), "log(2,")
    match = re.search(logregex, complexity)

  logregex = re.compile(r'log\(2,[3-9][0-9]*\)')
  match = re.search(logregex, complexity)  
  while match != None:
    complexity = complexity.replace(str(match.group(0)), "1")
    match = re.search(logregex, complexity)

  def simplify(cstr):
    try:
      val = int(cstr)
      return "1"
    except ValueError:
      rep = [("nat(X)","X"), ("nat(X+X)","X"), ("(X+X)","(X)"), ("(X)","X"), \
        ("X*X+X", "X*X"), ("(X*X)","X*X"), ("(log(2,X))","log(2,X)"), \
        ("(pow(2,X))","pow(2,X)"), ("-X","+X"),("X+X","X"), ("nat(+X)","X"),]
      while [p for (p,r) in rep if p in cstr] != []:
        for (p,r) in rep:
          cstr = cstr.replace(p, r)
      return cstr

  complexity = simplify(complexity)
  
  def maxsimplify(cstr):
    if cstr.startswith("max"):
      cstr2 = cstr[5:-2]
      mlist = cstr2.split(',')
      maxdeg = None
      for d in mlist:
        dd = simplify(d)
        if not dd in degreestuples:
          print("max failed: " + d, dd)
          break
        deg = degreestuples[dd]
        if not maxdeg:
          (cstr, maxdeg) = (dd, deg)
        else:
          (cstr, maxdeg) = (dd, deg) if deg > maxdeg else (cstr, maxdeg)
    return cstr


  maxregex = re.compile(r'max\(\[[^\]]*\]\)')
  match = re.search(maxregex, complexity)  
  if match != None:
    maxexpr = str(match.group(0))
    maxexpr2 = maxsimplify(maxexpr)
    complexity = complexity.replace(maxexpr, maxexpr2)
  

  if complexity in degreestuples:
    degree = degreestuples[complexity]
    print(str(degree), flush=True)
    job["degree"] = degree
  else:
    print(fname)
    print("raw: " + rawcomplexity)
    print("without multiplication: " + mcomplexity)
    print("without division: " + dcomplexity)
    print("without addition: " + acomplexity)
    print("without subtraction: " + scomplexity)
    print("NOT FOUND " + complexity)
    job["degree"] = 666
  return job

if __name__ == "__main__":
  if len(sys.argv) < 2:
    print("example dir needed")
  exampledir = sys.argv[1]

  comparehead = sys.argv[2] if len(sys.argv) > 2 else None

  # collect jobs
  jobs = []
  id = 0
  for subdir, dirs, files in os.walk(exampledir):
    for file in files:
      if file[-4:] == ".ces" and not "cofloco.ces" in file:
        filepath = join(subdir, file)
        jobs.append({"problem":file, "path":filepath, "id":id})
        id = id + 1
  
  # check in parallel
  numprocs = 3 # multiprocessing.cpu_count() - 1
  print("Doing " + str(len(jobs)) + " jobs with " + str(numprocs) + " procs")
  pool = multiprocessing.Pool(numprocs)
  tctresults = pool.map_async(check, jobs)
  pool.close()
  pool.join()
  #results = [check(j) for j in jobs]
  res = {}
  for r in results:
    d = {}
    name = r["path"].replace("koat-evaluation/examples/","").replace(".ces","")
    if "degree" in r:
      d["degree"] = r["degree"]
    if "errors" in r:
      d["errors"] = r["errors"]
    res[name] = d
  print(res)
