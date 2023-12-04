package.path = package.path .. ";../../seeder.lua"
require 'seeder'

name = comment
showmesh = false

if showmesh then
input = {
mesh = './mesh/'
  }

-- define the output
output = {  -- some general information
    folder = 'output/',     -- Output location 
   { 
     output_format = 'VTK',   -- Output format 
     dumpAll = false,
     dumpMesh = true,
     binary = true,
     vrtx = { } 
   }    
}
else
input = {
  read = './debug/0_restart_ProtoData_header_0.lua'--filename
  }

output = {  -- some general information
    folder = 'output/',     -- Output location 
   { 
     output_format = 'VTK',   -- Output format 
     dumpAll = true,
     dumpMesh = false,
     binary = true,
     vrtx = { } 
   }    
}
end
