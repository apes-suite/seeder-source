require "common"

comment = 'channel2D'
minlevel = 1
folder = 'mesh/'
level = maxlevel
multi_level = 1  -- 0 = single-level, 1 = multi-level

debug = {debugMode=true, debugFiles=true, debugMesh='debug/'}

bounding_cube = { origin = {-length/2, -length/2, -length/2},
                  length = length }

spatial_object = {
  { attribute = { kind = 'seed', },
    geometry = { kind = 'canoND',
                 object = { origin = { 0.0, 0.0, 0.0 },
               }
    } -- geometry
  }, -- seed
  { attribute = { kind = 'refinement', level = level + multi_level, },
    geometry = {
      kind = 'canoND',
      object = {
        origin = { -length*0.25, -height*0.25, -dx },
        vec = { { length*0.5,  0.0,  0.0 },
                { 0.0,  height*0.5,  0.0 },
                { 0.0,  0.0, 4*dx  },
        }
      } -- object
    }
  }, -- refinement
  { attribute = { kind = 'periodic', level = level },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = {
          origin = { -length/2, -height/2-dx/2, -dx/2},
          vec = { { 0.0, height+dx, 0.0},
                  { length+dx, 0.0, 0.0},}
        }, -- plane 1
        plane2 = {
          origin = { -length/2, -height/2-dx/2, dx+dx/2},
          vec = { { length, 0.0, 0.0},
                  { 0.0, height+dx, 0.0},}
        }, -- plane 2
      } -- object
    } -- geometry
  }, -- periodic table
  { attribute = { kind = 'boundary', label = 'top', level = level },
    geometry = {
      kind = 'canoND',
      object = {
        origin = { -length/2, height/2+dx/2, -dx/2 },
        vec = { { length+dx,0.0,0.0},
                { 0.0,0.0,height},
        }
      } -- object
    } -- geometry
  }, -- top plane
  { attribute = { kind = 'boundary', label = 'bottom', level = level },
    geometry = {
      kind = 'canoND',
      object = {
        origin  = { -length/2, -height/2-dx/2, -dx/2 },
        vec = { { length+dx,0.0,0.0},
                { 0.0,0.0,height},
        }
      } -- object
    } -- geometry
  },-- bottom plane
  { attribute = { kind = 'boundary', label = 'right', level = level },
    geometry = {
      kind = 'canoND',
      object = {
        origin  = { length/2-dx/2, -height/2-dx/2, dx/2 },
        vec = { { 0.0, height+dx, 0.0},
                { 0.0, 0.0,       height+dx},
        }
      } -- object
    } -- geometry
  },-- right plane
  { attribute = { kind = 'boundary', label = 'left', level = level },
    geometry = {
      kind = 'canoND',
      object = {
        origin  = { -length/2+dx/2, -height/2-dx/2, dx/2 },
        vec = { { 0.0, height+dx, 0.0},
                { 0.0, 0.0,       height+dx},
        }
      } -- object
    } -- geometry
  }-- left plane
} -- spatial object
