title: Build and run Seeder 

# Generate a geometry 

Seeder relies mainly on STL files for geometry descriptions.
Use your preferred tool to generate the geometry you want to
create a mesh for.
Most CAD applications allow you to export geometry in the
STL format, but also other 3D modelling tools like for
example *Blender* provide you with that option.

# Build and run Seeder 

-   Checkout: `hg clone https://geb.sts.nt.uni-siegen.de/hg/seeder`

    this will get you the repository including sub-repos aotus and treelm in the
    directory `seeder`.

-   Note: You need MPI in order to compile the application.
    Set the environment variable `FC` to point to the MPI compiler wrapper:

    `export FC=mpif90`

After that you can build and run Seeder. There are some
[further tutorials](testsuite_tutorials/index.html) available to illustrate the
usage of Seeder.

Here is a very quick basic run through:

- Configure `./waf configure`
- Build `./waf build`
- Create a mesh directory in your working path: `mkdir mesh`
- Create a debug directory in your working path: `mkdir debug`
- Change settings in `input/config.lua`. Specify the STL file(s) and set the
  *min* and *max* tree levels. 
- Run `build/seeder input/config.lua`
- The generated mesh is found in the `mesh/` directory after the successful
  execution of Seeder
