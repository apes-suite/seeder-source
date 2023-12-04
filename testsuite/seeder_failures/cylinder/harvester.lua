simulation_name = 'Cylinder'

--mesh = 'mesh/lh5/nH400/'
mesh = 'mesh/'
-- define the input
input = {
--          mesh = mesh,     -- only use this statement if you want to display the mesh only
          read = 'debug/100_restart_ProtoData_header_100.lua'
        }

-- define the output
output = {  -- some general information
            folder = 'harvest/',     -- Output location 

           { -- first output

            format = 'VTU',   -- Output format 

           }    

         }

         
