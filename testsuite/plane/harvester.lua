
-- define the input
input = {
--  read = 'debug/0_restart_ProtoData_header_0.lua'--filename
  mesh = './mesh/'
  }

-- define the output
output = {  -- some general information
    folder = 'harvest/',     -- Output location 
   { 
     format = 'VTU',   -- Output format 
     dump_all = true,
   }    
}
