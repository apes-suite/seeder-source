require "common"

comment = 'channel_cylinder'
minlevel = 1
folder = mesh -- mesh location spacified in common.lua
debug = {debugMode=true, debugFiles=true, debugMesh='debug/'}

bounding_cube = { origin = {-length/2, -length/2, -length/2},
                  length = length }

spatial_object = {
  { attribute = { kind = 'seed', },
    geometry = { kind = 'canoND',
                 object = { origin = { 0.25*length, 0.0, 0.0 },
               }
    }
  }, -- seed
  { attribute = { kind = 'boundary', label = 'periodic', level = level },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = {
          origin = { -length/2, -height/2-dx/2, -dx/2},
          vec = { { 0.0, height+dx, 0.0},
                  { length, 0.0, 0.0},}
        }, -- plane 1
        plane2 = {
          origin = { -length/2, -height/2-dx/2, dx+dx/2},
          vec = { { length, 0.0, 0.0},
                  { 0.0, height+dx, 0.0},}
        }, -- plane 2
      }
    }
  }, -- periodic table
  { attribute = { kind = 'boundary', label = 'outlet', level = level },
    geometry = {
      kind = 'canoND',
      object = {
        origin  = { length/2-dx/2, -height/2-dx/2, dx/2 },
        vec = { { 0.0, height+dx, 0.0},
                { 0.0, 0.0,       height},
        }
      } -- object
    }
  },-- right plane
  { attribute = { kind = 'boundary', label = 'inlet', level = level },
    geometry = {
      kind = 'canoND',
      object = {
        origin  = { -length/2+dx/2, -height/2-dx/2, dx/2 },
        vec = { { 0.0, height+dx, 0.0},
                { 0.0, 0.0,       height},
        }
      } -- object
    }
  },-- left plane
  { attribute = { 
      kind = 'boundary', label = 'top_wall', level = level, calc_dist = qVal_wall,
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = { -length/2, height/2 + wall_shift, -dx/2 },
        vec = { { length,0.0,0.0},
                { 0.0,0.0,height},
        }
      } -- object
    }
  }, -- top plane
  { attribute = { 
      kind = 'boundary', label = 'bottom_wall', level = level, calc_dist = qVal_wall,
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin  = { -length/2, -height/2 - wall_shift, -dx/2 },
        vec = { { length,0.0,0.0},
                { 0.0,0.0,height},
        }
      } -- object
    }
  },-- bottom plane
} -- spatial object

if useCylinder == true then
  table.insert(spatial_object,  {
    attribute = { 
      kind = 'boundary',
      level = level,
      label = 'cylinder',
      calc_dist = qVal_cylinder,
    },
    geometry = {
      kind = 'stl', -- was: sphere
      object = { filename = 'stl/cylinder.stl'
--        { origin = {-length*0.3,-0.01*height,0.},
--          radius = radius }
      }
    },
    transformation = {
      deformation =  { cylinder_radius, cylinder_radius, dx*0.5 },
      translation =  { cylinder_position, 0., dx*0.5 }
    }
    })
end
