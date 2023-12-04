simulation_name = 'channel'
debug = true
debug = false

if debug then
-- define the input
input = { read = 'debug/100_restart_ProtoData_header_100.lua'  }
-- define the output
output = {  -- some general information 
            folder = 'harvest/',     -- Output location
	  {
	    label ='',
            format = 'VTU',   -- Output format
          }             
        }
else
-- define the input
input = {  mesh = 'mesh/'  }
-- define the output
output = {  -- some general information 
           folder = 'harvest/',     -- Output location
            
           { --first output subset
	     solid = true, -- dump solid elements?
	     format = 'VTU',
             label = 'solid'
           },		
           { --second output subset
	     label ='',
             requestedData={variable={'treeID'}},
             format = 'VTU',   -- Output format
           }             
        }
end


