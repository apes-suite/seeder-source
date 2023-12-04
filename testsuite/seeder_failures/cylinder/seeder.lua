folder = 'mesh/'

debug = {debugMode = true, debugMesh='debug/' }
logging = {level=10}

defLevel = 5
dx=2.0/2.0^defLevel

bounding_cube = {origin = {-1.0,-1.0,-1.0},
                length =   2.0}

minlevel = defLevel

spatial_object = {
  { -- inlet
    attribute = {
      kind = 'boundary',
      label = 'cyl'  
    },
    geometry = {
      kind = 'cylinder',
      object = {{
        radius = 1.0,
        origin = {-0.5+dx,-0.5+dx,-0.5+dx},
        --------------------------
        --        WORKING       --
        --------------------------
--        vec = {0.35, 0.0,0.0},
        --------------------------
        --       NOT WORKING    --
        --------------------------
        vec = {0.2, 0.0,0.0},
        only_surface = false
        }
      }
    }
  },
  { -- the seed point
    attribute = {
      kind = 'seed',
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {0.5,0.5,0.5}
      }
    }
  }
}
