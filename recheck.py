# Parameters for the Musubi recheck runs.

import os
import sys
import datetime
import shutil

from clone_and_build_function import *

templateFolder = './templates/'
machineFolder = './machines/'
apesFolder = os.getenv('HOME')+'/apes/'

date = datetime.datetime.now().strftime("%Y-%m-%d__%X")
weekday = datetime.datetime.now().strftime("%A")

# Production directory, keep the past week as history.
prod_dir = 'seeder-runs_' + weekday

run_label = 'SEEDER'

# Cleanup production directory before using it:
shutil.rmtree(prod_dir, ignore_errors=True)
loglevel = 'INFO'

hg_clone_source = 'ssh://geb.sts.nt.uni-siegen.de//var/www/repos/hg/'

from recheck import notify_list
mail_address = notify_list
smtp_server = { 'tunnel' : {'host':'robin.inf.tu-dresden.de'} }

seeder_path = apesFolder + 'seeder/testsuite/'

# name of the shepherd log file
shepherd_out = 'shepherd.log'

# name of the log and rror file of the clone and build function
clone_build_out = 'clone_build.log'
clone_build_err = 'clone_build_error.log'

create_tag_on = False
grep_performance = True 
loris_clone_url = apesFolder + 'loris/'

shepherd_jobs = []

# Use the latest revision
seeder_exe = clone_build( solver          = 'seeder',
                          hg_clone_source = hg_clone_source+'seeder',
                          solver_dir      = 'seeder',
                          clone_build_out = clone_build_out,
                          clone_build_err = clone_build_err,    
                          revision        = 'default'     )

## SEEDER JOB 1 
testcase_path = seeder_path+'triangle/'
shepherd_jobs.append(dict(executable = seeder_exe,
                          template=testcase_path+'seeder.lua',
                          extension='lua',
                          run_exec = True,
                          create_subdir = ['mesh','debug'],
                          prefix = 'triangle_default',
                          label = 'triangle_default',
                          validation = True,
                          val_method = 'identity',
                          val_ref_path = 2849541493,#testcase_path+'ref_mesh/',
                          val_output_filename = 'mesh/',
                          val_md5 = True 
                          ))
## SEEDER JOB 2
testcase_path = seeder_path+'sphere_stl/'
shepherd_jobs.append(dict(executable = seeder_exe,
                          template=testcase_path+'seeder.template',
                          extension='lua',
                          run_exec = True,
                          additional_params = dict(stl_path = testcase_path+'stl/'),
                          create_subdir = ['mesh','debug'],
                          prefix = 'sphere_stl_default',
                          label = 'sphere_stl_default',
                          validation = True,
                          val_method = 'identity',
                          val_ref_path = 836754722, #testcase_path+'ref_mesh/',
                          val_output_filename = 'mesh/',
                          val_md5 = True 
                          ))
## SEEDER JOB 3
testcase_path = seeder_path+'subresolution/'
shepherd_jobs.append(dict(executable = seeder_exe,
                          template=testcase_path+'subres.lua',
                          extension='lua',
                          run_exec = True,
                          create_subdir = ['mesh','debug'],
                          prefix = 'subres_default',
                          label = 'subres_default',
                          validation = True,
                          val_method = 'identity',
                          val_ref_path = 1436679427,#testcase_path+'ref_mesh/',
                          val_output_filename = 'mesh/',
                          val_md5 = True 
                          ))

#### 
