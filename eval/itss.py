import os, subprocess, multiprocessing, sys
import time
from os import listdir
from os.path import isfile, join
import re
import sys
sys.path.append('.')
from results import Results

global results 
results = Results.data

def check(job):
  p = job["problem"]
  fname = job["path"]
  pfile = open(fname, "r")
  
  #with tempfile.NamedTemporaryFile() as outfile:
  cmd = "./sandbox 60 stack exec tct-its -- -s runtime "
  bashCommand = cmd + " " + fname
  #print(bashCommand)
  process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
  out, err = process.communicate()
  #if err:
  #  print(err)
  regex = re.compile(r'WORST_CASE\(\?,O\(([^\s]*)')
  match = re.search(regex, str(out))  
  if match != None:
    g = match.group(1)
    complexity = g[:-2]
    complexity_str = "O(" + complexity + ")"
    print(fname + ": " + complexity_str + " '" + g + "'")
    degrees = {
      "1" : 0.,
      "log(n)" : .5,
      "n^1" : 1.,
      "log(n)*n^1" : 1.5,
      "n^2" : 2.,
      "n^3" : 3.,
      "n^4" : 4.,
    }
    degree = degrees[complexity] if complexity in degrees else 666
    if complexity not in degrees:
      print("unknown '" + complexity + "'")
    job["degree"] = degree
  else:
    print(p + ": ?")
    job["errors"] = True
  return job

def accumulate(jobs):
  print("<html><body><table>")
  print("<tr><td>&nbsp;</td><td>TCT</td><td>KoaT</td><td>CoFloCo</td><td>PUBS</td></tr>")
  for r in jobs:
    name = r["path"][len("koat-evaluation/examples/"):-5] # drop .koat
    print("<tr><td>" + name + "<td>")
    # TCT
    tct_degree = str(r["degree"]) if "degree" in r else "?" 
    print("<td>" + tct_degree + "</td>")
    # KoAT
    koat_result = results["KoAT"][name]
    koat_degree = str(koat_result["degree"]) if "degree" in koat_result else "?" 
    print("<td>" + koat_degree + "</td>")
    # CoFloCo
    cofloco_result = results["CoFloCo"][name]
    cofloco_degree = str(cofloco_result["degree"]) if "degree" in cofloco_result else "?" 
    print("<td>" + cofloco_degree + "</td>")
    # PUBS
    pubs_result = results["PUBS"][name]
    pubs_degree = str(pubs_result["degree"]) if "degree" in pubs_result else "?" 
    print("<td>" + pubs_degree + "</td>")
    print("</tr>")
  print("</table></body></html>")

  return [j["degree"] if "degree" in j else None for j in jobs]


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
  numprocs = multiprocessing.cpu_count() - 1
  print("Doing " + str(len(jobs)) + " jobs with " + str(numprocs) + " procs")
  pool = multiprocessing.Pool(numprocs)
  tctresults = pool.map_async(check, jobs)
  pool.close()
  pool.join()
  tctresults = accumulate(tctresults.get())

  #results = []
  #for j in jobs:
  #  results.append(check(j))
  print("%d/%d OK" % (len([r for r in tctresults if r]), len(tctresults)))
  