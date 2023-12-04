--package.path = package.path .. ";../../seeder.lua"
--require 'seeder'
--
--name = label..'_l'..level
--filename = 'tracking/'..idLabel..'_'..label..'_hvsXY_l'..level..'_'..simName..'_lastHeader.lua'

-- define the input
input = {
--  read = './0_restart_ProtoData_header_0.lua'--filename
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
