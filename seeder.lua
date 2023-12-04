-- This is the standard seeder configuration, that should document the possible
-- and required configuration options.
-- It should always run out of the box!
-- ------------------------------------------------------------------------- --

-- Location to write the mesh in.
-- Note the trailing path seperator, needed, if all mesh files should be in a
-- directory. This directory has to exist before running Seeder in this case!
folder = 'mesh/' --default: 'mesh_'

-- Some comment, you might want to put into the mesh file for later reference.
comment = 'Simple Sample Seeder Mesh'

-- Debug output can be used to output prelimnary tree in restart format
-- and this restart file can be converted to vtk format by Harvester
ebug = { debugMode = true, debugMesh = 'debug/' }

-- How detailed should the output be?
-- The higher the level, the more output you'll get from seeder.
logging = { level = 5 }

-- Bounding cube: the root node of the octree, defining the complete universe,
-- from which all elements are derived by recursive bisection.
-- The origin is the corner from which on the cube is spanned with the given
-- length in each direction.
bounding_cube = { origin = {-1.0, -1.0, -1.0},
                  length = 2.0 }

-- A minimum level, by which all parts in the computational domain should at
-- least be resolved with. Default is 0.
minlevel = 6

-- We require elements neighboring boundaries to be refined down to the level
-- of the boundaries themselves. However, this is checked on all levels, and
-- results in some kind of smoothing of the refinements towards the actual
-- boundaries.
-- If this is not desired, this can be deactivated by setting smoothbounds to
-- false.
-- WARNING: If this is set to false, you might end up with elements that only
--          have partial boundaries. To avoid this, you need to configure an
--          appropriate refinement box around the geometry, with the same level
--          as the boundary itself.
-- Default is to use smoothening towards the boundaries (true).
smoothbounds = true

-- To smooth between levels to restrict level jump in the domain to 1
-- Default is set to true
smoothlevels = true

-- If there are colored boundaries in the mesh, they might be resolved on a
-- subelement level by describing the color distribution within the element
-- with a polynomial.
-- To enable this, there needs to be a subresolution table with at least the
-- polynomial degree to use for this subresolution.
-- See subres.lua for a more complete example of a mesh making use of this and
-- some further explanations.
subresolution = { polydegree = 7 }

-- Colors that should be inverted, that is their flooding status gets reversed
-- after the flooding algorithm. This way a seed can be placed in the domain
-- which is NOT to be flooded by the color. For example to define material
-- outside the flow domain, but placing the seed in the simply connected flow
-- domain itself. This is a list of labels, identifying the colors to invert
-- this way. The 'none' color can not be inverted!
-- inverted_colors = {'solid'}

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
    -- Each spatial object is described by an attribute and a geometrical
    -- definition.
    -- Each attribute is uniquely identified by kind, label and color.
    -- That is, if you would like to have different settings of the remaining
    -- options, those need to have different labels as well!
    attribute = {
      -- The kind describes what kind of object we describe here. Seeder knows
      -- spatial objects of the following kinds:
      -- - 'boundary':   objects that limit flooding and thereby the domain.
      -- - 'seed':       objects from where the flooding starts out until it
      --                   reaches boundaries or filled the complete domain.
      -- - 'refinement': objects towards which elements will be refined, without
      --                 limiting domain boundaries at all.
      kind = 'boundary',

      -- A label to connect to refer to this spatial object to. This is mostly
      -- relevant for boundaries, as it is later on used to identify boundary
      -- conditions.
      label = 'solid',

      -- Level by which this element should at least be resolved with.
      -- The object might actually be better resolved, due to other conditions
      -- like refinement boxes or higher resolved other boundaries in the
      -- proximity. This setting does not apply to seed objects, seed objects
      -- never impose a refinement on their own.
      -- Default is 0, thus if no level is defined, the object will not impose
      -- a refinement on its own.
      -- Define level if there is distance_refine because level_offset
      -- in distance_refine is offset from this level
      level = minlevel+2,

      -- Each spatial object might have a color. A color does not have an
      -- effect on refinement objects.
      -- A colored seed defines a flooding type that can be distinguished from
      -- other colors, and non 'none' will be identified by the isColored
      -- property in the mesh.
      -- A colored boundary affects only the flooding of the same color. Thus
      -- a colored seed will be confined by a boundary of the same color.
      -- Boundaries with the color 'none' or 'all' confine all colors.
      -- The 'none' color is treated specially, it will not be written to disk
      -- and elements with only this color do not get the isColored property.
      -- As the 'none' color can only be confined by boundaries, that also
      -- confine all other colors, it is assumed, that this color is used to
      -- describe the complete domain.
      -- Color names can be chosen arbitrarily and their names will be stored
      -- in the colors.lua definition file. They are case insensitive.
      -- Default is 'none'.
      color = 'none',

      -- If a boundary does not have the 'none' color, it might be subresolved
      -- and described by a polynomial beyond staircases offered by the mesh.
      -- Note, that for this to work you need to have a subresolution table
      -- defined, which at least states the polynomial degree to use for the
      -- subresolution. An example can be found in the subres.lua configuration.
      --
      -- subresolution = false,

      -- Attributes are uniquely identified by the settings above, those below
      -- are not considered for uniqueness. Thus be aware, that you need to have
      -- to have different labels, if you want to have attributes with different
      -- settings below!
      calc_dist = false, -- whether to calculate qVal
      flood_diagonal = true, -- whether to flood diagonal elements
                             -- this option only works when calc_dist is true
                             -- when encounter a problem with high order BC
                             -- try to set this option to false.

      store_normal = false, -- If set to true, the wall normals on this boundary
                            -- will be stored on disk.
                            -- Normals point from the wall to the barycenter of
                            -- the boundary fluid element and there length
                            -- correspondents to the distance of the elements
                            -- bary center to the boundary surface.
                            -- Only objects of this boundary condition will be
                            -- considered for the normal computation.

      -- With distance_refine, boundary can be resolved to certain
      -- level with certain distance from geometry
      distance_refine = {
        {
          radius = 0.1, -- distance from geometry
          level_offset = 0 -- level offset from the attribute level to
                           -- refine nodes within given distance
        },
        { -- second distance refinement
          radius = 0.2, -- distance from geometry
          level_offset = -1 -- level offset from the attribute level to
                           -- refine nodes within given distance
        }
      }
    },

-- Each geometrical object might be modified by a transformation table, which
-- allows you to define a translation and deformation of the object in question.
-- This is especially useful to adapt coordinates from STL files to match with
-- your universe definition.

--    transformation = {
--      translation = { 0.25, 0.0, 0.0 },
--      -- WARNING: deformation is only available for some of the geometrical
--      --            objects
--      deformation = 0.5 -- const scaling in all direction
----      deformation  = {1.0,2.0,3.0} -- diff scaling in three directions
----      deformation = {
----                      {1.0,2.0,3.0},
----                      {4.0,5.0,6.0},
----                      {7.0,8.0,9.0}} -- 3x3 matrix defines the rotation
--                                       -- and scaling in directions
--    },

    geometry = {
      -- Each geometry is of a certain kind, here is the definition of a sphere
      -- shown. For further examples look further down.
      kind = 'sphere',
      -- Example for a sphere definition
      object = {
                 { origin = {0.0, 0.0, 0.0}, -- Center of the sphere
                   radius = 0.5,             -- Radius of the sphere
                   -- By default spatial objects are 'solid', that is all
                   -- elements cutted by them are considered. For example
                   -- triangles intersect with their complete plane, and boxes
                   -- with their complete volume.
                   -- If you want to change this and have only the surface of
                   -- an object intersecting elements, the following flag needs
                   -- to be set to true. It is available for most spatial
                   -- objects. Default is: false.
                   only_surface = true -- Here we use only the surface of the
                                       -- sphere to describe the boundary.
                 }
                 -- There might be multiple spheres defined in a single object:
                 -- ,{ origin = {0.0, 0.5, 0.0},
                 --    radius = 0.25
                 --  }
               }

   }
  },
  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = { kind = 'seed' },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = { origin = {-0.0, -0.0, -0.0} }
               }
  }

-- ------------------------------------------------------------------------ --
-- Further example definitions of geometries:
--
-- Example for an STL definition:
--    geometry = { kind = 'stl',
--                 object = {filename = 'sphere.stl'}
--               }


-- Example for cylinder
--    geometry = {
--      kind = 'cylinder',
--      object = {{
--        normal = {1.0,0.0,0.0},
--        rotation = {0.0,0.0,0.0}, --clockwise along y-axis
--        length=2.0,
--        radius = 0.2,
--        origin = {-1.0,0.0,0.0}
--       ,type='solid'
--        }
----       ,{
----        normal = {1.0,0.0,0.0},
----        rotation = {0.0,0.0,0.0}, --clockwise along y-axis
----        length=2.0,
----        radius = 0.1,
----        origin = {-1.0,0.0,0.0}
----        }
--      }
--    }


-- Example for canonical
--     geometry = {
--       kind = 'canoND',
--       object = {
--         origin = {-1.0,-0.5,-.5},
--         vec = {{2.0,0,0,0},
--                {0.0,2.0,0.0}
--         }
--       }
--     }


-- Example for periodic planes
--    geometry = {
--      kind = 'periodic',
--      object = {
--        plane1 = {
--          origin = { -1.0,-1.0,-0.5 },
--          vec = {{0.0,2.0,0.0},
--                 {2.0,0.0,0.0}}
--        },
--        plane2 = {
--          origin = { -1.0,-1.0, 0.5 },
--          vec = {{2.0,0.0,0.0},
--                 {0.0,2.0,0.0}}
--        }
--      }
--    }


-- Example for spacer
--    geometry = {
--      kind = 'spacer',
--      object = {
--        length = {
--          normal = {1.0,0.0,0.0},
--          rotation = {0.0,0.0,0.0}, --clockwise along y-axis
--          filament_gap = 1.0,
--          length=2.0,
--          radius = 0.1,
--          origin = {-1.0,0.0,-1.0/2}
--        },
--        width = {
--          normal = {0.0,0.0,1.0},
--          rotation = {0.0,0.0,0.0},
--          filament_gap = 1.0,
--          length=2.0,
--          radius = 0.1,
--          origin = {-1.0/2.0,0.0,-1.0}
--        },
--          interwoven = true
--      }
--    }
} -- end of spatial objects
-- ************************************************************************ --
