#! /usr/bin/env python
# encoding: utf-8
# Harald Klimach 2011
import sys
import os

APPNAME = 'seeder'
VERSION = '1'

top = '.'
out = 'build'

def options(opt):
    opt.add_option('--no_harvesting', action='store_true',
                   default=False,
                   help = 'Do not include harvesting files for compilation.',
                   dest = 'no_harvesting')


def configure(conf):
    # Compiler flags are set in the treelm wscript.
    conf.setenv('')

    # Avoid some warnings in gfortran:
    if not conf.options.nowarn:
        for key, fenv in conf.all_envs.items():
            if fenv.FC_NAME == 'GFORTRAN':
                fenv.FCFLAGS.append('-Wno-unused-dummy-argument')

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

    seed_ppsources = bld.path.ant_glob('source/*.fpp')
    seed_sources = bld.path.ant_glob('source/*.f90',
                                     excl='source/seeder.f90')
    seed_sources += seed_ppsources
    seedc_sources = ['external/tribox3.c']
    fxtp_deps = ['fxtp_wrap_obj', 'fxtp_obj', 'fxtp_wrapper']


    if bld.cmd != 'docu':

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
            name = 'seeder',
            source = 'source/seeder.f90',
            use      = ['tem_objs', 'ply_objs', 'aotus', 'objs', 'cobjs',
                        bld.env.mpi_mem_c_obj]+fxtp_deps,
            target   = bld.path.parent.find_or_declare('seeder'))
        if bld.env.build_hvs and not bld.options.no_harvesting:
            seed_hvs_sources = bld.path.ant_glob('source/sdr_harvesting/*.f90',
                                                 excl='source/sdr_harvesting/sdr_harvesting.f90')
            bld(
                features = 'fc',
                source   = seed_hvs_sources,
                target   = 'sdr_hvs_objs')
            bld(
                features = 'fc fcprogram',
                name     = 'sdr_harvesting',
                source   = 'source/sdr_harvesting/sdr_harvesting.f90',
                use      = ['tem_objs', 'ply_objs', 'aotus', 'objs',
                            bld.env.mpi_mem_c_obj, 'sdr_hvs_objs', 'base64']+fxtp_deps,
                target = bld.path.parent.find_or_declare('sdr_harvesting'))


        utests(bld, ['aotus', 'tem_objs', 'ply_objs', 'objs'], preprocessor='coco')

    else:
        from waflib.extras.make_fordoc import gendoc

        spp = bld(
          features = 'includes coco',
          source   = seed_ppsources)

        sdr_preprocessed = []
        for ppm in spp.tasks:
          for f in ppm.outputs:
            sdr_preprocessed.append(f)

        if not bld.env.fordonline:
          sdr_preprocessed.append(bld.env.fordext_aotus)
          sdr_preprocessed.append(bld.env.fordext_tem)

        tgt = bld.path.get_bld().make_node('docu/modules.json')
        bld.env.fordext_sdr = tgt

        bld( rule = gendoc,
             source = sdr_preprocessed,
             src_paths = [bld.path.find_node('source').abspath(),
                          os.path.join(bld.top_dir, 'polynomials', 'source')],
             target = tgt,
             extern = ['aoturl = {0}'.format(bld.env.fordext_aotus),
                       'temurl = {0}'.format(bld.env.fordext_tem)
                      ],
             extern_urls = ['aoturl = {0}'.format(bld.env.fordurl_aotus),
                            'temurl = {0}'.format(bld.env.fordurl_tem)
                           ],
             mainpage = os.path.join(bld.top_dir, 'sdr', 'sdr_mainpage.md')
        )

# Small helping program to help the transition from old mesh files
# to the new Lua based headers.
    if bld.cmd == 'ascii2lua':
        bld(
            features = 'fc fcprogram',
            source   = 'extras/convertascii2lua.f90',
            use      = ['tem_objs', 'aotus'],
            target   = 'ascii2lua')

from waflib.Build import BuildContext
class a2l(BuildContext):
    "ascii2lua"
    cmd = 'ascii2lua'
