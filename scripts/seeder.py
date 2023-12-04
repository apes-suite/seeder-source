#!/usr/bin/env python
# This python script generates several jobscripts based on the input parameters in the 
# parameter file (params.list).Following 4 files are required to run this script.
# 1. script_gen.py
# 2. pyratemp.py
# 3. params.list
# 4. <something>.template (A template file that you will have to write) eg quelia.template or juropa.template already exist on the repository.
#
# The following steps should be followed to run this script
# Step1:
#   Create a Template file named <something>.template
#      - The template should be modified with the variables "${NPROCS}$";${PATH_TO_EXECUTABLE}$;
#        and ${ARGUMENT}$ at appropriate places in the template. See "quelia.template" for example.
# Step2:
#   Set all the Parameters in the file called params.list.
# Step3:
#   Run the script : ./script_gen.py
#
#################################################################################################################
import sys
import string
import re
import logging
from subprocess import call
from itertools import product
import subprocess
import os


# get variable name for given line
def get_variable(name, lines,typ):
  for line in lines:
    if not line.startswith('#'):
      if name in line:
        if typ==list:
          temp = re.findall(r"=.*$",line)
          for x in temp:
            temp2 = re.split(r"= ",x)
            val = re.split(r"\,",temp2[1])
            return val
        else:
          temp = re.findall(r"=.*$",line)
          for x in temp:
            temp2 = re.split(r"= ",x)
            val = re.split(r"\,",temp2[1])
            return val[0]


# generates a temporary script which would generate scripts from given template file    
def genscript(filename,template,parameters,values,path_lib,MESH):
  print "Generating Lua Script: %s"%filename
  filename = filename+'.lua'
  st=''
  for i in range(len(parameters)):
    if i==(len(parameters)-1):
      st=st+parameters[i]+'=\''+values[i]+'\''
    else:
      st=st+parameters[i]+'=\''+values[i]+'\','

  st=st+','+'MESH'+'=\''+MESH+'\''
  script=open('script.py','w')
  script.write("#!/usr/bin/env python\n")
  script.write("import sys\n")
  script.write("import logging\n")
  script.write("sys.path[:0]=[\"")
  script.write("\n".join(path_lib))
  script.write("\"]\n")
  script.write("import pyratemp as pyratemplate\n")
  script.write("logfile = open(\"%s\",\"w\")\n"%filename)
  script.write("mytemplate = pyratemplate.Template(filename=\"%s\")\n"%template)
  script.write("logfile.write(mytemplate(%s))\n"%st)
  script.write("logfile.close()")
  script.close()
  success=subprocess.call("python script.py", shell=True)
  if success==1:
    print "!!! ERROR: run_simulation will now exit:\n"
    print"CHECK IF LUA TEMPLETE FILE HAS MORE PARAMETERS TO CHANGE THAN SPECIFIED IN params.list "
    sys.exit(2)
  subprocess.call("rm script.py", shell=True)

# To name the mesh folder
def namemesh(x,num,p_name):
  MESH='mesh'
  for i in range(0,num):
    new_name=p_name[i][:2]
    print 'new_name=',new_name
    MESH=MESH+'_'+new_name+'_'+x[i]
  return MESH


# To name the lua file
def namefile(name,p_name,x,num):
 for i in range(0,num):
   new_name=p_name[i][:2]
   name=name+'_'+new_name+'_'+x[i]
 return name


p_name=[]
array=[]
p_count=1


#Read lua parameters from params_lua.list
if  (len(sys.argv) > 1):
  ifile = sys.argv[1]
else:
  ifile = 'params.list'
param_file = open(ifile, "r")
param_lines = param_file.readlines()
param_file.close()

path_lib = get_variable('PATH_TO_LIB', param_lines,list)
sys.path[:0] = path_lib
import pyratemp as pyratemplate



# Read the line number in params.list where the parameter for lua script starts
# just checks for an "&" sign and stores the line number
pattern = re.compile('&')
for i,line in enumerate(param_lines):
  if pattern.search(line):
    start_line= i+2
#    print start_line

# Read the parameters to be varied in the lua template file
param_file = open(ifile, "r")
for i,line in enumerate(param_file):
    if int(i) >= start_line:
      if not '#' in line:
       p=re.split(r"= ",line)
       p_name.append(re.split(r" = ",line)[0])
       pval=re.split(r"\,",p[1])
       new=map(string.strip,pval)
       array.append(new)
       p_count +=1
param_file.close()

#print array
num=len(p_name)

# Read path to executable
list_path = get_variable('PATH_TO_EXECUTABLE',param_lines,list)

# Read the name of the template file
lua_list_template = get_variable('LUA_TEMPLATE',param_lines,string)


for x in product(*array):
  #print x
  # Create lua files for different parameters
  name1 = lua_list_template.split('.')[0].split('/')
  name=name1[len(name1)-1]
  lua_file=namefile(name,p_name,x,num)
  print 'lua_file=',lua_file
  #Name the mesh folder
  MESH=str(namemesh(x,num,p_name))

  genscript(lua_file,lua_list_template,p_name,x,path_lib,MESH)
  
  # Create mesh folder
  call(['mkdir', MESH])
  # Generate mesh
  call([list_path[0],lua_file+'.lua'])

