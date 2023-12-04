simulation_name = 'sphere'

mesh = 'mesh/'
input = {
mesh = mesh,
--read = 'debug/0_restart_ProtoData_header_0.lua'
        }

output = {  -- some general information
  folder = 'harvest/',     -- Output location 
  { -- first output
--             -- output quantities, what shall be outputted???              
--             tracking = { 
--                         label = 'probe_press', 
--                         variable = {{'density_combined',1, dep = {'spc1','spc2'}},{'density',1}},             -- options: density, velocity                        
--                         shape={object='point', point ={0.0,0.0,0.0}} 
--                        },    
  dumpAll = true,
  dumpMesh = false,
  output_format = 'VTK',   -- Output format 
  binary = true,
  vrtx = {           -- when this table is defined set use_vrtx = .true.
--                      updated_vertices  = 'prefix_of_new_vertices',            -- when this is given set  new_vrtx = .true.
--                      old_vertices  = 'prefix_of_old_vertices',                -- when this is given set  old_vrtx = .true.
--                      dump_vertices = 'prefix_of_where_to_dump_vertices'       -- when this is given set dump_vrtx = .true.
  } 
  }    

}
