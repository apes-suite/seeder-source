-- This file contains common parameters

-- Setting mesh refinement level
-- get the value from BASH environment variable LEVEL
-- OR get level from user input
-- level = os.getenv("LEVEL")
-- doesn't work!!!   os.execute("export qVal=0.1")
-- qVal = os.getenv("qVal") + 0.5
qVal = 0.4
if qVal >= 1.0 then
  qVal = qVal - 1.0
end

level = 7
if level == nil then
  print("Please input the mesh refinement level: (6, 7, 8 or 9)")
  level = io.read("*number")
  print("Mesh refinement level is: ",level)
end

printRuntimeInfo = false
mesh = 'mesh/' -- Mesh files location

length = 8.0    -- physical length of channel
height = length / 4
useCylinder = true
cylinder_radius = 0.51
cylinder_position = 0.0
qVal_wall = false
qVal_cylinder = true

-- Do not change these values.
-- They are taken as a base to calculate the position of the periodic plane
dx = length / 2.^level
wall_shift = ( qVal ) * dx
ly = (height + wall_shift*2) / dx       -- number of elements for channel height
lx = length / dx - 2   -- number of elements for channel height

--print("------- Info from common.lua --------------")
print("-- The channel length has elements of "..lx)
print("-- The channel height has elements of "..ly)
print("-- qVal is "..qVal)
