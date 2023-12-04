require 'seeder'
--------------------------------------------------------------------------------
-- input settings
input = {
   mesh = './mesh/',
   ead = './t0_0_restart_ProtoData_header_0.lua'
 }
---------------------------------------------------------------------------------------


---------------------------------------------------------------------------------------
-- output settings
output = { -- start output table
  folder = 'vis_', -- Output location
  {  -- Start subset 1 table
    format = 'vtu',
    shape = {
      { kind = 'canoND', 
        object = {{
          origin = {startX, -height*0.5,0.}, 
          vec  = {
            {length,0,0.},
            {0.0, height, 0.0}
          }, 
          segments={ nElemsMax, nElemsMax }
        },{
          origin = {startX, 0., -height*0.5}, 
          vec  = {
            {length,0,0.},
            {0.0, 0.0, height }
          }, 
          segments={ nElemsMax, nElemsMax }
        } 
       }
      }
    }  } -- end subset 1 table
}  -- end output table
