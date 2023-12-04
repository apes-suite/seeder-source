-- common header --

level =  5  
refinementLevel = 1  
refinementLevel2 = 2
label = 'channel'
idLabel = 'channel'
u_in = 0.02
-- geometry definition
height =  0.41
length = 1*height
radius = 0.05
usePeriodic = true
useObstacle = false
testIntersection = false
simName = 'channelRefine'

if useObstacle == true then
  refOmega = 1.995
  refLevel = 8
else
  refOmega = 1.8
  refLevel = level
end
--
m = 2^(refLevel - level)
omega = 1./(1/m*(1/refOmega -0.5)+0.5)
maxLevel = level+math.max(refinementLevel, refinementLevel2)

dx     = length/2^level
dxMin  = length/2^maxLevel
dxDash = 0.5*dxMin
viscLB = 1/3*(1/omega-1/2)
-- physical reference values
dxPhys = dx
lPhys = length  -- m
csPhys = 300    -- m/s
csLB = 1/math.sqrt(3)
--acoustic scaling with speeds of sound as reference
-- csPhys = csLB * dxPhys / dtPhys
dtPhys = csLB/csPhys*dxPhys
rho0 = 1  -- kg/m^3
viscPhys = viscLB*dxPhys^2/dtPhys


nElemsMax = 2^maxLevel
if useObstacle == true then
  nIters = nElemsMax*40
else
  nIters = nElemsMax*40
end
nIters = nElemsMax*20


-- Use this file as template. Do not modify this file for running some testcases
outputname = 'channel_2D'
outputpreview = true
folder = 'mesh/' 

-- general refinement levels
-- minrefine: defines minimum refinement level of the entire domain
--
-- maxrefine: defines maximum refinement
-- gets overwritten from madatory STL levels
-- (deprecated - used only for spacer stuff right now)
-- will become default for stl refinement level once
-- it is not mandatory any more
minlevel  = level
-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory

logging = {level=3}

seederLength = length+dx

--debug = {debugMode = true, debugFiles = true, debugMesh = 'debug/'}
size_x = 0.3*length
start_x = -0.15*length
if useObstacle then
  size_x = 0.63*length
  start_x = -0.40*length
  size_y = 3.*height/5.-2.*dxDash
  if testIntersection then
    start_x = -0.35*length
  end
else
  size_y = height/3-5.*dxDash
end
start_y = -size_y/2
size_z = size_y
start_z = start_y

start2_x = -0.32*length
size2_x  = 0.37*size_x
size2_y = size_y*0.60
start2_y = -size2_y/2
size2_z = size2_y
start2_z = start2_y

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
bounding_cube = {origin = {-seederLength*0.5, -seederLength*.5, -seederLength*0.5},
               length = seederLength}


spatial_object = {
  {
    attribute = {
      kind = 'refinement',
      level = refinementLevel+level,
      label='box1'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {start_x, start_y, start_z },
        vec = {{size_x, 0., 0.},
              {0.,size_y, 0.},
              {0., 0., size_z}}
      }
    }
  },
  {
    attribute = {
      kind = 'refinement',
      level = refinementLevel2+level,
      label='box2'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {start2_x, start2_y, start2_z },
        vec = {{size2_x, 0., 0.},
              {0.,size2_y, 0.},
              {0., 0., size2_z}}
      }
    }
  },
  { attribute = { kind = 'seed', },
    geometry = { kind = 'canoND',
                 object = { origin = { 0.0, 0.0, dx*0.5 },
               }
    } -- geometry
  }, -- seed
  {
    attribute = {
      kind = 'boundary',
      label='east'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {length*0.5, -length*0.5, -length*0.5},
        vec = {{0.0, length, 0.},
              {0.,0.0, length}}
      }
    }
  },
  {
    attribute = {
      kind = 'boundary',
      label='west'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {-length*0.5, -length*0.5, -length*0.5},
        vec = {{0.0, length, 0.},
              {0.,0.0, length}}
      }
    }
  },
  {
    attribute = {
      kind = 'boundary',
      label='north'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {-length*0.5, height*0.5+dxDash, -length*0.5},
        vec = {{length, 0.0, 0.},
              {0.,0.0, length}}
      }
    }
  },  
  {
    attribute = {
      kind = 'boundary',
      label='south'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {-length*0.5,-height*0.5-dxDash, -length*0.5},
        vec = {{length, 0.0, 0.},
              {0.,0.0, length}}
      }
    }
  }
}

if useObstacle ==true then
  table.insert(spatial_object,  { 
    attribute = { 
      kind = 'boundary', 
      label = 'sphere'
    },
    geometry = {
      kind = 'sphere', 
      object = {
        { origin = {-length*0.3,-0.01*height,0.},
          radius = radius }
      }
    }
    }) 
end

if usePeriodic == true then
  table.insert(spatial_object, { 
    attribute = { 
      kind = 'periodic', 
    },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = {
          origin = { -length/2, -length/2, dx+dxDash},
          vec = { { length, 0.0, 0.0},
                  { 0.0, length, 0.0},}
        }, -- plane 1
        plane2 = {
          origin = { -length/2,  -length/2, -dxDash},
          vec = { { 0., length, 0.0, 0.0},
                  { length, 0., 0.0},}
        }, -- plane 2
      } -- object
    } -- geometry

  }) 
else
  table.insert(spatial_object, { 
    attribute = { 
      kind = 'boundary', 
      label = 'frontback'
    },
    geometry = {
      kind = 'canoND',
      object = {
        {
          origin = { -length/2, -length/2, height/2+dxDash},
          vec = { { length, 0.0, 0.0},
                  { 0.0, length, 0.0},}
        }, -- plane 1
        {
          origin = { -length/2, -length/2, -height*0.5-dxDash},
          vec = { { length, 0.0, 0.0},
                  { 0.0, length, 0.0},}
        }, -- plane 2
      } -- object
    } -- geometry

  }) 
end

