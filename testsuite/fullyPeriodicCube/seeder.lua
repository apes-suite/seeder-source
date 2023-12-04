-- This seeder configuration file shows an example for fully periodic cube
-- mesh with very basic mesh information i.e minlevel, folder and 
-- bounding cube
-- ------------------------------------------------------------------------- --

-- Location to write the mesh in.
-- Note the trailing path seperator, needed, if all mesh files should be in a
-- directory. This directory has to exist before running Seeder in this case!
folder = 'mesh/'

-- Some comment, you might want to put into the mesh file for later reference.
comment = 'Periodic Cube'

-- Debug output can be used to output prelimnary tree in restart format
-- and this restart file can be converted to vtk format by Harvester
ebug = { debugMode = true, debugFiles = false, debugMesh = 'debug/' }

-- A minimum level, by which all parts in the computational domain should at
-- least be resolved with. Default is 0.
minlevel = 2

-- Bounding cube: the root node of the octree, defining the complete universe,
-- from which all elements are derived by recursive bisection.
-- The origin is the corner from which on the cube is spanned with the given
-- length in each direction.
PI = 3.1415926
bounding_cube = {
  origin = {0.0, 0.0, 0.0},
  length = PI * 2.0,
}

spatial_object = {
  {
    attribute = { kind = 'seed'},
    geometry = {
      kind = 'canoND',
      object = {
        origin = { 1.0, 1.0, 1.0, },
     }
    }
  }
}
