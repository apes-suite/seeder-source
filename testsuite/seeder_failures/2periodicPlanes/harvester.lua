name = 'pure'

-- define the input
input = {
--          read = 'mesh/'
          read = 'debug/0_restart_ProtoData_header_0.lua'
        }

-- define the output
output = { folder = 'harvest/',
           {    
--             requestedData = {{ depVarSys = 'intersection',
--                                variable = { {'density'}, {'velocity'} }
--                             }},
             dumpMesh = false,
             dumpAll = true,
             output_format = 'VTK',   -- Output format 
             binary = true,
             vrtx = {           -- when this table is defined set use_vrtx = .true.
--                     updated_vertices  = 'prefix_of_new_vertices',            -- when this is given set  new_vrtx = .true.
--                     old_vertices  = 'prefix_of_old_vertices',                -- when this is given set  old_vrtx = .true.
--                     dump_vertices = 'prefix_of_where_to_dump_vertices'       -- when this is given set dump_vrtx = .true.
                    } ,
             label = 'mesh',

             --define the shape to be dumped into VTK
--             shape = {kind='all'}
--              shape = {kind = 'canoND', object = {center = {0.0, 0.0, 42.5},
--                                                  halfvec = { {4.0, 0.0, 0.0}, {0.0, 0.0, 50.5} },
--                                                  segments = {99, 1300}
--                                                 } 
--                      },
            }    

}


