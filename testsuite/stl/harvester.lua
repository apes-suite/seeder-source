name = 'stl'

--mesh = 'mesh/lh5/nH400/'
mesh = 'mesh/'
-- define the input
input = {
--          mesh = mesh,     -- only use this statement if you want to display the mesh only
          read = 'debug/0_restart_ProtoData_header_0.lua'
        }

-- define the output
output = {  -- some general information
            folder = 'harvest/',     -- Output location 
--            folder = mesh,     -- Output location 

           { -- first output

--             -- output quantities, what shall be outputted???              
--             tracking = { 
--                         label = 'probe_press', 
--                         variable = {{'density_combined',1, dep = {'spc1','spc2'}},{'density',1}},             -- options: density, velocity                        
--                         shape={object='point', point ={0.0,0.0,0.0}} 
--                        },    
            format = 'VTU',   -- Output format 

           }    

         }


