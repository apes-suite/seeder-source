-- This is the standard seeder configuration, that should document the possible
-- and required configuration options.
-- It should always run out of the box!
-- ------------------------------------------------------------------------- --

-- Location to write the mesh in.
-- Note the trailing path seperator, needed, if all mesh files should be in a
-- directory. This directory has to exist before running Seeder in this case!
folder = 'mesh/'

-- Some comment, you might want to put into the mesh file for later reference.
comment = 'Simple Sample Seeder Mesh'

-- Debug output can be used to output prelimnary tree in restart format
-- and this restart file can be converted to vtu format by Harvester
debug = { debugMode = true, debugFiles = false, debugMesh = 'debug/' }

-- Bounding cube: the root node of the octree, defining the complete universe,
-- from which all elements are derived by recursive bisection.
-- The origin is the corner from which on the cube is spanned with the given
-- length in each direction.
bounding_cube = { origin = {-2.0, -2.0, -2.0},
                  length = 4.0 }

-- A minimum level, by which all parts in the computational domain should at
-- least be resolved with. Default is 0.
minlevel = 6

-- *********************** Table of spatial objects *********************** --
-- Each spatial object is defined by an attribute and some geometric entity
-- attached to this attribute. Attributes might be defined multiple times.
-- Attributes are described by a kind (boundary, seed or refinement), a level
-- and maybe further kind specific values, like a label for the boundary.
-- Geometric objects might by right now:
-- - canoND (point, line, plane or box)
-- - STL
-- - Sphere
-- - Cylinder
-- - Periodic boundaries are special, spatial objects of this kind can only make
-- - use of geometric objects of the kind 'periodic'.
spatial_object = {
  {
    -- Defining a domain boundary
    attribute = { kind = 'boundary', -- or seed, refinement
                  label = 'cube',   -- some label to identify the boundary
                                     -- condition
                  level = 1          -- level to refine this object with,
                                     -- default = 0
                },

      geometry = {
      kind = 'stl',
      object = {{
-- filename is cube stl
        filename = 'cube.stl'

--         filename = 'cylinder.stl'
        }
      }  
    },
    transformation = {
        translation = {0.5,0.0,0.0}, -- translate the geometry
--      deformation = 0.5 -- const scaling in all direction
--      deformation = {0.5,0.5,0.5} -- diff scaling in x,y,z direction
        deformation = {
                      { 0.5*math.cos(45*math.pi/180), -0.5*math.sin(45*math.pi/180), 0.0 },
                       { 0.5*math.sin(45*math.pi/180), 0.5*math.cos(45*math.pi/180), 0.0 },
                       { 0.0, 0.0, 0.5 }
                       } -- rotate the geometry in anti-clockwise direction 
                         -- along z-axis and scale the geometry by factor 0.5
                       
    }
  },
  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = { kind = 'seed' },
    geometry =  {              -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = { origin = {-2.0, -1.0, -1.0} }
               }
  }
}
-- ------------------------------------------------------------------------ --

