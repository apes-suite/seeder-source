folder = 'mesh/'

comment = 'Ellipsoid'
debug = { debugMode = false, debugFiles = false, debugMesh = 'debug/' }

bounding_cube = { origin = {-1.0, -1.0, -1.0},
                  length = 2.0 }
minlevel = 1

spatial_object = {
  {
    attribute = {
      kind = 'boundary', -- or seed, refinement
      label = 'solid',   -- some label to identify the boundary
      level = 8          -- level to refine this object with,
    },
   -- Defining Geometry of the sphere
    geometry = {
      kind = 'ellipsoid',
      object = {
        {
          radius = {0.7, 0.3, 0.4},
          origin = {0.1,0.0,0.0}, -- origin of the sphere
          only_surface = true,      -- Use only surface of the object?
        }
      }, -- object
    }, -- geometry
  },
  {
    attribute = { kind = 'seed' },
    geometry = {
      kind = 'canoND',
      object = { origin = {-0.0, -0.0, -0.0} },
    },
  },
}
-- ------------------------------------------------------------------------ --

