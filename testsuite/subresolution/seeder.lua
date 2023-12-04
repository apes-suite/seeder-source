-- Example on the usage of subresolution data.
-- A sphere is discretized by a few elements and subresolved by polynomials
-- where the cubes are intersected by the sphere.
-- ------------------------------------------------------------------------- --

sub_degree = 11
sub_space = 'P'
oversampling = 2
projection_method = 'l2p'

-- Location to write the mesh in.
-- Note the trailing path seperator, needed, if all mesh files should be in a
-- directory. This directory has to exist before running Seeder in this case!
folder = 'mesh-'..sub_space..sub_degree..'_o'..oversampling..'_'

-- Some comment, you might want to put into the mesh file for later reference.
comment = 'Subresolved Sphere'
timing_file = 'timing.res'

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
subresolution = { polydegree = sub_degree,
                  polyspace  = sub_space,
                  -- You can prescribe the number of sublevels to resolve
                  -- target elements with:
                  levels = 1 + math.ceil(math.log((sub_degree+1)*oversampling,2)),

                  -- Method of projection to use to get from nodal to modal
                  -- representation of polynomial boundaries.
                  projection = { factor = oversampling,  -- Oversampling factor
                                              -- default = 1
                                 kind = projection_method
                               },
                  -- In the value table you can define different values for
                  -- the color filling in subresolved elements.
                  -- It can be omitted, in this case all colors will default to
                  -- fill=1 and void=0.
                  values = {
                             -- One table per color:
                            { label = 'solid',
                               fill = 1.0,  --default: 1.0
                               void = 0.0 } --default: 0.0
                           }
                }

-- *********************** Table of spatial objects *********************** --
spatial_object = {
  {
    -- Defining a domain boundary
    attribute = {
      kind = 'boundary', -- or seed, refinement
      label = 'sphere',  -- some label to identify the boundary
                         -- condition
      color = 'solid',
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
    attribute = { kind = 'seed', color = 'solid' },
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
