#! /usr/bin/env python
# encoding: utf-8
# Harald Klimach 2011
import sys
import os

APPNAME = 'seeder'
VERSION = '1'

# Add the aotus subdirectory as path to search for modules.
sys.path.append(os.path.join(sys.path[0], 'treelm', 'aotus'))

top = '.'
out = 'build'

def options(opt):
    opt.recurse('treelm')
    opt.add_option('--no_harvesting', action='store_true',
                   default=False,
                   help = 'Do not include harvesting files for compilation.',
                   dest = 'no_harvesting')


def configure(conf):
    # Compiler flags are set in the treelm wscript.
    conf.recurse('treelm')
    conf.setenv('')

    # Avoid some warnings in gfortran:
    if not conf.options.nowarn:
        for key, fenv in conf.all_envs.items():
            if fenv.FC_NAME == 'GFORTRAN':
                fenv.FCFLAGS.append('-Wno-unused-dummy-argument')

    conf.recurse('polynomials')
    conf.setenv('')
    if not conf.options.no_harvesting:
      conf.env.build_hvs = True
    else:
      conf.env.build_hvs = False

    conf.setenv('')
    conf.setenv('ford', conf.env)
    conf.env.ford_mainpage = 'sdr_mainpage.md'


def build(bld):
    from waflib.extras.utest_results import utests

    bld(rule='cp ${SRC} ${TGT}', source=bld.env.COCOSET, target='coco.set')

    bld.add_group()

    seed_ppsources = bld.path.ant_glob('source/*.fpp')
    seed_sources = bld.path.ant_glob('source/*.f90', 
                                        excl='source/seeder.f90')
    seed_sources += seed_ppsources
    seedc_sources = ['external/tribox3.c']
    fxtp_deps = ['fxtp_wrap_obj', 'fxtp_obj', 'fxtp_wrapper']


    if bld.cmd != 'gendoxy':

        bld.recurse('treelm')
        bld.recurse('polynomials')
        bld(
            features = 'c',
            source   = seedc_sources,
            target   = 'cobjs')

        bld(
            features = 'coco fc',
            source   = seed_sources,
            target   = 'objs')

        bld(
            features = 'fc fcprogram',
            source = 'source/seeder.f90',
            use      = ['tem_objs', 'ply_objs', 'aotus', 'objs', 'cobjs', bld.env.mpi_mem_c_obj]+fxtp_deps,
            target   = 'seeder')
        if bld.env.build_hvs and not bld.options.no_harvesting:
            seed_hvs_sources = bld.path.ant_glob('source/sdr_harvesting/*.f90', 
                                                 excl='source/sdr_harvesting/sdr_harvesting.f90')
            bld(
                features = 'fc',
                source   = seed_hvs_sources,
                target   = 'sdr_hvs_objs')
            bld(
                features = 'fc fcprogram',
                source   = 'source/sdr_harvesting/sdr_harvesting.f90',
                use      = ['tem_objs', 'ply_objs', 'aotus', 'objs',
                            bld.env.mpi_mem_c_obj, 'sdr_hvs_objs', 'base64']+fxtp_deps,
                target = 'sdr_harvesting')


        utests(bld, ['aotus', 'tem_objs', 'ply_objs', 'objs'], coco=True)

    else:

        bld(rule='cp ${SRC} ${TGT}',
            source = bld.path.find_node(['treelm', 'source', 'arrayMacros.inc']),
            target = bld.path.find_or_declare('arrayMacros.inc'))
        bld.recurse('polynomials')
        bld.recurse('treelm', 'post_doxy')

        bld(
            features = 'coco',
            source   = seed_ppsources)
     
# Small helping program to help the transition from old mesh files
# to the new Lua based headers.
    if bld.cmd == 'ascii2lua':
        bld( 
            features = 'fc fcprogram',
            source   = 'extras/convertascii2lua.f90',
            use      = ['tem_objs', 'aotus'],
            target   = 'ascii2lua')

#clean build directory and coco completely to create the build from scratch
def cleanall(ctx):
    from waflib import Options
    Options.commands = ['distclean'] + Options.commands
    ctx.exec_command('rm coco')
    
from waflib.Build import BuildContext
class a2l(BuildContext):
    "ascii2lua"
    cmd = 'ascii2lua'
