require "seeder"

interpolation_method = 'quadratic'
--interpolation_method = 'none'
--interpolation_method = 'average'
--inlet_kind='inlet_ubb'
--outlet_kind='outlet_pab'
inlet_kind = 'wall'
outlet_kind = 'wall'

--require "PATH"
tracking_folder = './tracking/'--"TRACKING_FOLDER"
output_folder = './output_d3/'--"OUTPUT_FOLDER"

-- Simulation name
mesh = './mesh/' -- Mesh information
debug = {
  verbosity = 100, 
  debugMode = true, 
  debugFiles = true,
  dumpDependencies = true,
  dumpTreeIDs = true,
  dumpAuxLists = true,
  }
simulation_name = simName

interpolation_method = 'quadratic'

identify = {label = idLabel, layout='d3q19', relaxation = 'bgk'}
u0 = 0.0
amplitude = u_in
dp = 0.001
p0 = 1.
function ini_rho(x, y, z)
  return p0 - dp/length*x+dp*0.5
end

function ini_function(x, y, z)
  return (amplitude - amplitude*4/(height^2)*y^2)  --*(1.-0.2/length*x)
end

originX = -length*0.1
originY = length*0.1
originZ = 00.0
halfwidth = length*0.015
amplitude = 0.001
background = 1.0
function ic_2Dgauss_pulse(x, y, z)
  return background+amplitude*math.exp(-0.5/(halfwidth^2)*(( x - originX )^2+( y - originY )^2))
end

init_allElems = true
-- Time step settigs
tmax =   40 --nIters    -- total iteration number
interval = tmax/tmax
tRamping = tmax/10
time = {useIterations = true,
        min = 1, max = tmax, interval = interval }

fluid = { --#omega_ramping = { predefined='smooth', min_factor = 0.7, max_factor=1, from_time=0, to_time=tRamping}, 
       omega = omega, 
       rho0 = 1.0 }

-- Initial condition 
initial_condition = { density   = ic_2Dgauss_pulse, --1.0, --ini_rho,
                      velocityX = 0.0, --ini_function,
                      velocityY = 0.0,
                      velocityZ = 0.0
                      }
-- Boundary conditions
boundary_condition = {  
{ label = 'east',
   kind = outlet_kind,
   density =  1.0}, 
{ label = 'north', 
   kind = 'wall' },
{ label = 'south', 
   kind = 'wall' },
 }

if usePeriodic ==false then
  table.insert( boundary_condition, 
{ label = 'frontback', 
   kind = 'wall' } )
  table.insert( boundary_condition,{ label = 'west', 
  kind = inlet_kind,
  velocityX = --u_in,
          { kind = 'combined',
                transient= {
                  predefined='smooth', 
                  min_factor = 0.0, 
                  max_factor=1.0, 
                  from_time=0, 
                  to_time=tRamping}, 
                spatial = {
                  predefined='parabol', 
                  shape = { 
                    kind = 'canoND', 
                    object = {
                      origin = {-0.5*length, -(height-0.5*dx)*0.5, -(height-0.5*dx)*0.5},
                      vec = { {0.0, height-0.5*dx, 0.0 } , {0.0, 0.0, height-0.5*dx}}
                      } 
                   }
                  ,amplitude = u_in 
              }         
	      },
   velocityY = 0.0, velocityZ = 0.0} )
  else
  table.insert( boundary_condition,{ label = 'west', 
  kind = inlet_kind,
  velocityX = { kind = 'combined',
                transient= {
                  predefined='smooth', 
                  min_factor = 0.0, 
                  max_factor=1.0, 
                  from_time=0, 
                  to_time=tRamping}, 
                spatial = {
                  predefined='parabol', 
                  shape = { 
                    kind = 'canoND', 
                    object = {
                      origin = {-0.5*length, -(height-0.5*dx)*0.5, 0.0},
                      vec = {0.0, height-0.5*dx, 0.0} 
                      } 
                   }
                  ,amplitude = u_in 
              }         
	      },
   velocityY = 0.0, velocityZ = 0.0} )
end
if useObstacle ==true then
  table.insert( boundary_condition, 
{ label = 'sphere', 
   kind = 'wall' } )
 end

-- Output settings
output = { active = true, -- VTK output activated?
  folder = output_folder..label..'_l'..level,     -- Output location
  vtk = false,             -- VTK output activated?
  wss = true, 
  dumpGhosts = false,
  umpHalos = true,
  time = {min = 0, max = tmax, interval = interval}
}

-- Tracking              
racking = {{
  label = label..'_probePressure_l'..level, 
 variable = {'density'}, 
 shape = {kind = 'canoND', object = {origin ={0.0,0.,0.} } },
 time = {min = 1, max = -1, interval = 5},
  format = 'ascii', folder = tracking_folder      
 }, 
 {
  -- tracking object for getting the error in terms of the l2norm
  label = label..'_hvsXZ_l'..level, 
  variable = { 
     {'density'}, 
     {'velocity'}, {'wss'}},
  shape = {kind = 'canoND', object = {origin ={ -length*0.5,0.,-height*0.5}, 
            vec={{length, 0., 0.}, {0.,0.,height}}, 
            segments = {2*nElemsMax, nElemsMax/2} } },
  time = {min = tmax, max = tmax, interval = interval},
  format = 'harvester',
  folder = tracking_folder
 }, {
  -- tracking object for getting the error in terms of the l2norm
  label = label..'_hvsXY_l'..level, 
  variable = { 
     {'density'}, 
     {'velocity'}, {'wss'}},
  shape = {kind = 'canoND', object = {origin ={ -length*0.5,-height*0.5,0.}, 
            vec={{length, 0., 0.}, {0.,height,0.}}, 
            segments = {2*nElemsMax, nElemsMax/2} } },
  time = {min = tmax, max = tmax, interval = interval},
  format = 'harvester',
  folder = tracking_folder
 }} 

