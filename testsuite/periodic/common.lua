-- This file contains common parameters

-- refine level for channel domain.
-- get the value from BASH environment variable LEVEL
maxlevel = 7
diffusive = true  -- diffusive or acoustic scaling?
usePeriodic = true -- true = 2D channel, false = 3D channel
useObstacle = false -- not used yet

printRuntimeInfo = false

length = 4.0    -- physical length of channel
height = length / 8

-- Do not change these values.
-- They are taken as a base to calculate the position of the periodic plane
dx = length / 2.^maxlevel
ly = height / dx       -- number of elements for channel height
lx = length / dx - 2   -- number of elements for channel height
R = ly / 2    -- channel radius

-- For Grid convergence, keep Re = U_aver * Ly / viscosity to be constant
Reynolds = 10
if diffusive then
  -- For DIFFUSIVE scaling, keep omega constant, adjust velMax accordingly
  omega = 1.821
  velMax = ( 2 * Reynolds / omega - Reynolds ) / 4 / ly
  tmax  =  10000 * ( 4.^(maxlevel - 6 ))
else
  -- For ACOUSTIC scaling, keep vel constant, adjust omega accordingly
  velMax = 0.005
  omega = 2 * Reynolds / ( 4.0 * velMax * ly + Reynolds )
  tmax  =  9000 * ( 2.^(maxlevel - 6 ))
end

vis = ( 1.0 / omega - 0.5 ) / 3.0   -- viscosity
D_slope = 8.0 * vis * velMax / ly / ly * 3 -- density drop slope
--dt = getdtFromOmega( { dx = dx, nu_p = vis_phy, omega = omega} )
dt = Reynolds * dx * dx * vis
physics = { dt = dt, rho0 = 1000}
vel_phy = dx * velMax / dt

-- Analytical solutions
function wssA(x, y, z)
  return vis * 2 * math.abs(y/dx) * velMax / R / R
end
function velA(x, y, z)
  return velMax * ( 1 - (y/dx)^2 / R^2 )
end
print("------- Info from common.lua --------------")
print("-- Use Periodic    ",usePeriodic)
print("-- Use Multi-level ",multi_level)
print("-- Use Obstacle    ",useObstacle)
print("-- The channel length has elements of "..lx)
print("-- The channel height has elements of "..ly)
print("-- The dx is "..dx)
print("-- The dt is "..dt)
print("-- tmax           is "..tmax)
print("-- Omega          is "..omega)
print("-- Viscosity      is "..vis)
print("-- Max velocity   is "..velMax )
print("-- Density slope  is "..D_slope)
print("-----   Physical Parameters   -------------")
print("-- velocity phy   is "..vel_phy )
print("-------------------------------------------")
