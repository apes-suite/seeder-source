-- Musubi configuration file. 
-- This is a LUA script.

-- Simulation name
simulation_name = 'stenosis'
mesh = 'mesh/main/'  -- Mesh information
-- Time step settings
tmax           = 700000  -- total iteration number
interval       = 100000
probe_interval = 50
time = {useIterations = true, max = tmax, interval = 10, min = 1}

umax = 0.0085470085470086

R = 4.0
--Hagen Poiseuille profile
function circ_poise(x,y,z)
  return umax*(1 - ((x^2.0+y^2.0)/R^2.0))
end

scheme = { {

  identify = { label = 'fluid', kind = 'lbm', relaxation = 'bgk', layout = 'd3q19' },
  fluid = { omega = 1.95, rho0 = 1.0 },
  -- Initial condition 
  initial_condition = { density = 1.0, 
                      velocityX = 0.0,
                      velocityY = 0.0,
                      velocityZ = 0.0 } ,

  -- Boundary condition
  boundary_condition = {  
  { label = 'wall', kind = 'wall'},
  { label = 'inlet', 
    kind = 'inlet_ubb', 
    velocityX = 0.0,
    velocityY = 0.0,
    velocityZ = {kind = 'combined', 
                 transient = {predefined='smooth', min_factor=0.0, max_factor=1.0, from_time=0, to_time=50000},
                 spatial =  circ_poise
                },
    density = 1.0
  } ,  
  { label = 'outlet',
    kind  = 'outlet_expol',
    density = 1.0 }
  },

  -- Tracking              
  tracking = { 
  {
    label = 'stenosis100', 
    variable = { {'velocity'}, {'density'}, {'wss'} },   -- options: density, velocity
    shape={kind = 'all' },
    time = {min = 0, max = -1, interval = interval}, 
    format = 'harvester',
    folder = 'tracking_100_final/' 
   },
--   {
--    label = 'recheck', 
--    variable = { {'velMag'} },   -- options: density, velocity
--    shape={kind = 'canoND', object = { center = {0.0, 0.0, -7.5},
--                                       halfvec = { {2.0, 0.0, 0.0}, {0.0, 2.0, 0.0} },
--                                       segments = {99, 99}
--                                     } 
--          },
--    reduction = 'average',
--    time = {min = 0, max = tmax, interval = probe_interval}, 
--    format = 'ascii',
--    folder = '100_probe/' 
--   },
--   {
--    label = 'after',
--    variable = { {'velMag'} },
--    shape = {kind = 'canoND', object = {origin = {0.0, 0.0, 18.0}  }
--            },
--    reduction = 'average',
--    time = {min = 0, max = -1, interval = probe_interval}, 
--    format = 'ascii',
--    folder = '100_probe/'
--   },
--   {
--    label = 'before',
--    variable = { {'velMag'} },
--    shape = {kind = 'canoND', object = {origin = {0.0, 0.0, 4.0}  }
--            },
--    reduction = 'average',
--    time = {min = 0, max = -1, interval = probe_interval}, 
--    format = 'ascii',
--    folder = '100_probe/'
--   }

   
   } 
  }
  
}

restart = { read = 'restart_100/stenosis_lastHeader.lua',
            rite = 'restart_100/',
            time = { min = 0, max = tmax, interval = interval},
            buffer = 8000000 }

--wss: 5e-8, 4e-6
geomIncr = { solidify = true, proximity = true,
             depend =  {useScheme = 'fluid', variable = { {'wss'} },
                        condition ={ { threshold = 4.0e-6, operator = '<='}
                                   }
                       },
             time = {min = 0, max = tmax, interval = 200}
  }
