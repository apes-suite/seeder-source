folder = 'mesh/'
printRuntimeInfo = false

comment = 'sphere_in_bounding_box'

debug = { debugMode = true, debugFiles = true, debugMesh = 'debug/' }
timing_file = 'timing.res'

bounding_cube = { origin = {-4.75, -0.75, -0.75},
                  length = 1.5 }

minlevel = 1

spatial_object = {
  {
    attribute = {
      kind = 'boundary',
      label = 'sphere',
      level = 5,
      calc_dist = true,
      store_normal = true,
    },
    geometry = {
      kind = 'stl',
      object = {{
        filename = 'stl/sphere.stl',
        }
      }
    }
  },
  {
    attribute = { kind = 'seed' },
    geometry = {
      kind = 'canoND',
      object = { origin = {-4.0, -0., -0.} }
    }
  }
}
-- ------------------------------------------------------------------------ --

