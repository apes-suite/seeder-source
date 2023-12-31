-- Local variable
-- If true post-process debug output from seeder else post-process mesh files
debug = false

-- Simulation name to append to output file 
simulation_name = 'Cylinder'

-- define the output
if debug then
  restart = { read = 'debug/final100_restart_ProtoData_header_100.lua'  }
else
  mesh = 'mesh/'
end

-- define the output
output_folder = 'mesh/'  -- Output location
output = { format = 'vtk' }   -- Output format
