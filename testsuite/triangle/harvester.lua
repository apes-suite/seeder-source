simulation_name = 'Triangle'

logging = {level=3}

mesh = 'mesh/'
-- define the input
restart = {
          read = 'debug/final100_restart_ProtoData_header_100.lua'
--          read = 'debug/0_restart_ProtoData_header_0.lua'
        }

-- define the output_folder and output table if tracking table is not defined
output_folder = 'harvest/'
output = {  -- some general information
            format = 'vtk',   -- Output format 
         }


NOtracking = {
 folder = 'harvest/',
 label = 'proto',
 variable = {'bcattr_pos_minbcid', 'treeid', 'reflevel'},
 shape = {kind='all'},
 output = {format = 'vtk'}
}
