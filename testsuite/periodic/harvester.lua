name = 'periodic'

--mesh = 'mesh/lh5/nH400/'
mesh = 'mesh/'
-- define the input
input = {
--          mesh = mesh,     -- only use this statement if you want to display the mesh only
--          read = 'debug/0_restart_ProtoData_header_0.lua'
          read = 'tracking/channel_harvester_channel2D_lastHeader.lua'
        }

-- define the output
output = {  -- some general information
            folder = 'harvest/',     -- Output location 
--            folder = mesh,     -- Output location 

           { -- first output

            format = 'VTU',   -- Output format 
           }    

         }


