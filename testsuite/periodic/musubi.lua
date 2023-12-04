require "common"

simulation_name = 'channel2D'
debug_mode = false
mesh = 'mesh/'

tmax = 5000
time = {useIterations = true,
        max = tmax, interval = tmax/10}

add_variable = { name='spacetime', ncomponents=1, spacetime=wssA }

tracking = {
  label = 'harvester', 
  folder = 'tracking/',
  variable = {{'pressure'},{'velocity'}}, 
  shape = {kind = 'all' },
  time = {min = tmax, max = tmax, interval = tmax},
  format = 'harvester'      
}
fluid = { omega = omega, rho0 = 1.0 }
interpolation_method = 'quadratic'
initial_condition = { pressure = 1.0, velocityX = 0.0, velocityY = 0.0, velocityZ = 0.0 }
identify = {label = 'channel', layout = 'd3q19'}

boundary_condition = {
{ label = 'right',  kind = 'outlet_expol', pressure = 1.0, },
{ label = 'top', kind = 'wall'},
{ label = 'bottom', kind = 'wall'},
                    } -- boundary table
if usePeriodic then
  table.insert( boundary_condition,
        { label = 'left',   kind = 'inlet_ubb',
          velocityX = {
            kind = 'combined',
            transient= {
              predefined='smooth',
              min_factor=0.0, max_factor=velMax, from_time=0, to_time=tmax/4},
            spatial = {
              predefined='parabol',
              shape = { kind = 'canoND', 
                        object = { origin = { -length/2+dx, -R*dx, dx/2},
                                   vec = { 0.0, 2*R*dx, 0.0 }
                                 }
                      } -- shape table
                      } -- spatial table
                      }, -- velX table
          velocityY = 0.0,
          velocityZ = 0.0,
        })
else
  table.insert( boundary_condition,
        { label = 'left',   kind = 'inlet_ubb',
          velocityX = {
            kind = 'combined',
            transient= {
              predefined='smooth',
              min_factor=0.0, max_factor=velMax, from_time=0, to_time=tmax/4},
            spatial = {
              predefined='parabol',
              shape = { kind = 'canoND', 
                        object = { origin = { -length/2+dx, -R*dx, R*dx},
                                   vec = { {0.0, 2*R*dx, 0.0}, {0.0, 0.0, R*dx} }
                                 }
                      } -- shape table
                      } -- spatial table
                      }, -- velX table
          velocityY = 0.0,
          velocityZ = 0.0,
        })
  table.insert( boundary_condition,
      { label = 'front', kind = 'wall' } )
  table.insert( boundary_condition,
      { label = 'back', kind = 'wall' } )
end
