folder = 'mesh/'
printRuntimeInfo = false

comment = 'Simple Sample Seeder Mesh'

-- Debug output can be used to output prelimnary tree in restart format
-- and this restart file can be converted to vtk format by Harvester
debug = { debugMode = true, debugMesh = 'debug/' }

bounding_cube = { origin = {-1.0, -1.0, -1.0},
                  length = 2.0 }

minlevel = 1

-- Currently, seeder does not support non-axis aligned box
-- If you want to intersect non-axis aligned box, use
-- planes to define the box or use hollow box
spatial_object = {
  {
    attribute = { 
      kind = 'refinement', -- or seed, boundary
      level = 5,
    },

    geometry = {
      kind = 'canond',
      object = {
                 { origin = { -1.0, -1.0, -1.0 },
                   length = 0.9
                 }
               }
    } -- geometry
  },

  {
    attribute = { 
      kind = 'boundary', -- or seed, boundary
      label = 'solid',   -- some label to identify the boundary
      level = 5,
    },

    geometry = {
      kind = 'canond',
      object = {
                 { origin = { -0.0, -0.0, -0.0 },
                   vec = { 
                     { 1.0, 0.0, 0.0 },
                     { 0.0, 1.0, 0.0 },
                     { 0.0, 0.0, 1.0 },
                   },
                   only_surface = true -- creates hollow box
                                       -- default is false i.e solid box
                 }
               }
    } -- geometry
  },
  {
    attribute = { kind = 'seed' },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = { origin = {-0.0, -1.0, -1.0} }
               }
  }
} -- end of spatial objects
-- ************************************************************************ --
