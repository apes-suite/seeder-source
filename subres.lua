-- This is a Seeder configuration with subresolution, that should document the
-- possible and required configuration options for meshes with subresolution.
-- It shows a minimal example with a sphere flooded by a color called solid,
-- and the complete cube flooded by the 'none' color.
-- For more details on general settings, see seeder.lua
-- It should always run out of the box!
-- ------------------------------------------------------------------------- --

-- Location to write the mesh in.
-- Note the trailing path seperator, needed, if all mesh files should be in a
-- directory. This directory has to exist before running Seeder in this case!
folder = 'mesh/'

-- Some comment, you might want to put into the mesh file for later reference.
comment = 'Sample Subresolved Seeder Mesh'

-- Bounding cube: the root node of the octree, defining the complete universe,
-- from which all elements are derived by recursive bisection.
-- The origin is the corner from which on the cube is spanned with the given
-- length in each direction.
bounding_cube = { origin = {0.0, 0.0, 0.0},
                  length = 1.0 }

-- A minimum level, by which all parts in the computational domain should at
-- least be resolved with. Default is 0.
minlevel = 2

-- Definition of the subresolution:
subresolution = { polydegree = 7,
                  polyspace  = 'Q', --default: 'q'
                  -- You can prescribe the number of sublevels to resolve
                  -- target elements with:
                  levels = 5, -- default:
                  -- math.ceil(log(real((polydegree+1)*projection.factor),2))
                              --          + 1

                  -- Method of projection to use to get from nodal to modal
                  -- representation of polynomial boundaries.
                  projection = { factor = 2,  -- Oversampling factor
                                              -- default = 1
                                 kind = 'l2p' -- possible: 'fpt', 'l2p'
                                              -- default = 'l2p'
                                 -- See also projection definition in
                                 -- treelm/config.lua.
                               },
                  -- In the value table you can define different values for
                  -- the color filling in subresolved elements.
                  -- It can be omitted, in this case all colors will default to
                  -- fill=1 and void=0.
                  values = {
                             -- One table per color:
                            { label = 'solidsphere',
                               fill = 1.0,  --default: 1.0
                               void = 0.0 } --default: 0.0
                           }
                }

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
--
-- Periodic boundaries are special, spatial objects of this kind can only make
-- use of geometric objects of the kind 'periodic'.
spatial_object = {
  {
    -- Defining a domain boundary
    attribute = {
      kind = 'boundary', -- or seed, refinement
      label = 'sphere',  -- some label to identify the boundary
                         -- condition
      color = 'solidsphere',
      level = minlevel+1, -- level to refine this object with,
                          -- default = 0
      -- This boundary is to be subresolved:
      subresolution = true
    },

    geometry = { -- Example for a sphere definition
      kind = 'sphere',
      object = {
                 { origin = {0.5, 0.5, 0.5}, -- Center of the sphere
                   radius = 0.2,             -- Radius of the sphere
                   only_surface = true       -- Use only surface of the object?
                                             -- default is: false.
                 }
                 -- There might be multiple spheres defined in a single object:
                 -- ,{ origin = {0.0, 0.5, 0.0},
                 --    radius = 0.25
                 --  }
               }
   }
  },
  -- Defining a seed to identify the sphere part, that should be colored with
  {
    attribute = { kind = 'seed', color = 'solidsphere' },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = { origin = {0.5, 0.5, 0.5} }
               }
  },
  -- Defining a seed to identify the part of the computational domain in
  -- the universe cube.
  {
    attribute = { kind = 'seed', color = 'none' },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = { origin = {0.1, 0.1, 0.1} }
               }
  }

}
-- ************************************************************************ --
