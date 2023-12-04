project: Seeder
summary: Mesh generator, providing the mesh with tree information.
src_dir: source/
src_dir: build/ford
src_dir: polynomials/source
exclude_dir: build/ford/treelm
external: aoturl = https://geb.inf.tu-dresden.de/doxy/aotus
external: temurl = https://geb.inf.tu-dresden.de/doxy/treelm
output_dir: docu
page_dir: doc_pages
copy_subdir: media
project_website: https://apes.osdn.io/pages/seeder
graph: true
graph_maxdepth: 4
graph_maxnodes: 32
display: public
display: protected
display: private
sort: permission
source: false
author: University of Siegen
title: Mainpage
print_creation_date: True

Seeder Mesh Generator
=====================

The goal of Seeder is simple: create an octree mesh out of a given geometry.
Typically the geometry is given in the form of
[STL](|page|/features/stl.html)-files.
As simple as this goal is the basic idea for the construction of this mesh.
The user first has to describe the cubical outline of the mesh, which has to
enclose the complete geometry.
This is referred to as `bounding_cube`.
It is the root node of the octree, from which all elements of the mesh are
derived by recursive bisection in all three spatial dimensions.
The generated mesh will be written to the location provided by the `folder`
setting.
This either is simply a prefix to use for all mesh file names or a directory,
if it is a directory, the string has to include the path-separator at the end,
and the directory has to exist.
A third global setting can be used to define a minimial refinement level to
use for all elements that will be part of the final mesh.
This `minlevel` setting provides a method to prescribe a minimal resolution
for the complete domain, if it is not provided the resolution will only be
defined by the geometrical objects you put into the bounding cube.
See the [fully periodic cube](|page|/testsuite_tutorials/ful_per_cube.html)
example for a mesh that uses only these three settings.

Getting Started
---------------

-   [Quick Start: Building and Compiling Seeder](|page|) 
-   [Treelm Requirements](|temurl|/page/requirements.html)
-   [Testsuite](|page|/testsuite_tutorials/index.html): An introduction to the
    usage of the features of Seeder. You are guided through all required steps
    to generate meshes with different configurations.
-   [Transformations](|page|/features/transformation.html) like translation,
    scaling, reflection and rotation can be applied to all geometric objects.
-   Geometric definitions, have to be given one of the
    [attributes](page/attributes/index.html).

    Attributes are defined by their `kind`:

    * [Boundary](|page|/attributes/boundary.html)
    * [Seed](|page|/attributes/seed.html)
    * [Refinement](|page|/attributes/refinement.html) *level*, that describes
      the minimal refinement level by which the object should be resolved
    * and some kind specific further values, like a label for boundary
      conditions


For some further internal details see:

- the [overall algorithm](|page|/sdr_algorithm.html).

- [Octree](|temurl|/page/octree.html) describes the implementation of the
  Octree data structure in a little more detail.

- [Treelm](|temurl|/page) is a common library that implements the octree mesh
  handling for the solvers.

- [Aotus](|aoturl|) is a library that provides the access to Lua scripts as
  configuration files.


License
-------

Seeder is licensed under the terms of the 2-clause BSD license reproduced below.
This means that Seeder is free software and can be used, reproduced, modified,
distributed and redistributed also for commercial purposes under the conditions
of the BSD license.
The only requirement is that some credit to the authors is given by putting this
copyright notice somewhere in your project.

According to good scientific practice, publications on results achieved in whole
or in part due to Seeder should cite at least one paper presenting the Seeder
software.

An appropriate reference is:

@incollection{harlacher_seeder12,
   author = {Harlacher, Daniel F. and Hasert, Manuel and Klimach, Harald and Zimny, Simon and Roller, Sabine},
   affiliation = {German Research School for Simulation Sciences GmbH, and RWTH, Aachen, Germany},
   title = {Tree Based Voxelization of STL {Data}},
   booktitle = {High Performance Computing on Vector Systems 2011},
   editor = {Resch, Michael and Wang, Xin and Bez, Wolfgang and Focht, Erich and Kobayashi, Hiroaki and Roller, Sabine},
   publisher = {Springer Berlin Heidelberg},
   pages = {81-92},
   year = {2012}
}

See each file for respective copyright notices.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
