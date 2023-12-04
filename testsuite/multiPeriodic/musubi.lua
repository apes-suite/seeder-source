require "seeder"
-- Musubi configuration file.. 
-- This is a LUA script.
originX = length/5.0
originY = length/5.0
originZ = 00.0
halfwidth = length/10.
amplitude = 0.1
background = 1.0

function ic_1Dgauss_pulse(x, y, z)
  return background+amplitude*math.exp(-math.log(2.)/(halfwidth^2)*( x - originX )^2)
end
function ic_2Dgauss_pulse(x, y, z)
  return background+amplitude*math.exp(-math.log(2.)/(halfwidth^2)*(( x - originX )^2+( y - originY )^2))
end
-- Initial condition 
initial_condition = { --pressure = {predefined='gausspulse', center={0.0,0.0,0.0}, halfwidth=0.3, amplitude=0.20, background=1.000},
                      pressure = ic_2Dgauss_pulse,
                      velocityX = 0.0,
                      velocityY = 0.0,
                      velocityZ = 0.0 }
-- Simulation name
simulation_name = 'Gaussian_pulse_validation'
mesh = 'mesh/'-- Mesh information
--mesh = { predefined='cube',    -- use the predefined full cube
--         origin = {-1.,-1.,-1.},  -- origin of the cube
--         length = 2.,         -- length of the cube
--         refinementLevel = 5 } -- refinement level to resolve the cube

fluid = { omega = 1.98, rho0 = 1.0 }


-- Local refinement settings
interpolation_method = 'linear'
                     
-- Time step settigs
tmax           =  20    -- total iteration number
time = {min = 0, max = tmax, interval = tmax/10}

-- Tracking              
tracking = {
{ label = 'probe_press', 
  variable = {'pressure'}, 
  shape = {kind='canoND',object = {origin ={0.,0.,0.}}},
  time = {min = 0, max = tmax, interval = 1}, 
  format='ascii', folder='tracking/'
  }
-- ,
-- { label = 'line', 
-- variables = {'state'}, -- state (=pdfs), pressure, pressure etc
-- type='line', line ={ 
--    origin = {-75.,-0.,0.}, 
--    direction = {150.,0,0.}, 
--    segments=300, 
--    distribution='equal'}, 
-- format='harvester', folder='tracking/', 
-- interval = 1, tmin = 0, tmax = 800 } 
  }

