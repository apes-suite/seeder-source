-- definition of the festo porous medium test case

level = 7
refinement1 = 0
refinement2 = 3
refinement3 = 0
refinement4 = 0
maxLevel=level+math.max(refinement1, refinement2, refinement3, refinement4)
interpolation_method = 'quadratic'

shepherdRun = true
if shepherdRun == false then
  verbose = true
else
  verbose = false
end

qVals = true
simname = 'porous'
-- geometry definition

-- give the bounding box origin and the size of the medium to simulate
-- conversion factor for the porous medium
fac = 1./1000.
debug = {debugMode = true, debugMesh = './t0_'}

porous_origin_original = { -0.806*fac, -0.794*fac, -0.793*fac }
porous_size   = { 1.59*fac, 1.59*fac, 1.59*fac  }
-- 
heightPorous = math.max( porous_size[2], porous_size[3])
heightInlet = heightPorous

length = 25.*porous_size[1]
heightJet    = 1.5*math.max(porous_size[2], porous_size[3])
height = 0.1*length
startX = -3.*porous_size[1]

-- set the bounding cube
cube_length = math.max( length, height )
origin = {startX, -cube_length*0.5, -cube_length*0.5}
bounding_cube = {
  length = cube_length, 
  origin = origin
  }
-------------------------------------------
-- calculate dx
dx     = length/math.pow(2,level)
dxMin  = length/math.pow(2,maxLevel)
dxDash = 0.5*dxMin
nElemsMax = 2^maxLevel
-------------------------------------------
-- backplate position
backPlateX = startX+dx-dxDash
-- determine the position of the porous medium, such 
-- that the end matches exactly with the grid level interface position 
porous_endI = math.ceil((porous_origin_original[1]+porous_size[1]-origin[1])/dx)
new_porous_end = porous_endI*dx + origin[1]
diffX = new_porous_end - (porous_origin_original[1] + porous_size[1]) - dxDash
pipeHeight = 1.5*heightPorous
-- moved the porous medium so that the end of
-- the medium is exactly at the end of an element
-- to have smooth walls
porous_origin = {
  porous_origin_original[1]+diffX,
  porous_origin_original[2],
  porous_origin_original[3]
}
porous_end    = { 
  porous_origin[1] + porous_size[1],
  porous_origin[2] + porous_size[2],
  porous_origin[3] + porous_size[3]}


-------------------------------------------
-- some basic Seeder settings
comment = 'festo: porous medium with outflow area'
folder = 'mesh/'
if shepherdRun then
  stlfolder = '../../input/'
else
  stlfolder = 'input/'
end 
minlevel  = level
-------------------------------------------


-- define the 0th refinement box
ref4_size = { 
 1.8*porous_size[1], 
 1.0*porous_size[2], 
 1.0*porous_size[3]
}
ref4_start = { 
   0.3*porous_size[1],
  -0.5*ref4_size[2],
  -0.5*ref4_size[3]
}

-- define the 0th refinement box
ref3_size = { 
   6.*porous_size[1], 
 1.3*porous_size[2], 
 1.3*porous_size[3]
}
ref3_start = { 
  -0.5*porous_size[1],
  -0.5*ref3_size[2],
  -0.5*ref3_size[3]
}

-- define the 1st refinement box
ref2_size = { 
  1.*porous_size[1], 
  1.5*porous_size[2], 
  1.5*porous_size[3]
}
ref2_start = { 
  -3.*porous_size[1],
  -0.5*ref2_size[2],
  -0.5*ref2_size[3]
}

-- define the 2nd refinement box
ref1_size = { 
  26.*porous_size[1], 
  4.*porous_size[2], 
  4.*porous_size[3]
}
ref1_start = { 
  -5.*porous_size[1],
  -0.5*ref1_size[2],
  -0.5*ref1_size[3]
}

spatial_object = {

  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = { kind = 'seed' },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = {{ origin = 
                 {origin[1] + 5.5*dx, 0.0, 0.0} },
                 { origin = 
                 {origin[1] + length-1.5*dx, 0.0, 0.0} }}
               }
  },
  -- Refinement boxes
  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = { kind = 'refinement',
                  label = 'refinement box 1',
                  level = level+refinement1
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = { 
                   origin = ref1_start,
                   vec = {
                     {ref1_size[1],0.,0.,},
                     {0.,ref1_size[2],0.},
                     {0.,0., ref1_size[3]}
                   }
                 }
               }
  },
  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = { kind = 'refinement',
                  label = 'refinement box 2',
                  level = level+refinement2
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = { 
                   origin = ref2_start,
                   vec = {
                     {ref2_size[1],0.,0.,},
                     {0.,ref2_size[2],0.},
                     {0.,0., ref2_size[3]}
                   }
                 }
               }
  },
  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = { kind = 'refinement',
                  label = 'refinement box 3',
                  level = level+refinement3
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = { 
                   origin = ref3_start,
                   vec = {
                     {ref3_size[1],0.,0.,},
                     {0.,ref3_size[2],0.},
                     {0.,0., ref3_size[3]}
                   }
                 }
               }
  },
  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = { kind = 'refinement',
                  label = 'refinement box 4',
                  level = level+refinement4
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = { 
                   origin = ref4_start,
                   vec = {
                     {ref4_size[1],0.,0.,},
                     {0.,ref4_size[2],0.},
                     {0.,0., ref4_size[3]}
                   }
                 }
               }
  },
--  -- cylinder
--{
--    attribute = { kind = 'boundary',
--                  label = 'cylinder',
--                },
--    geometry = {
--      kind = 'cylinder',
--      object = {{
--        radius = heightPorous,
--        origin = {startX, 0., 0.,},
--        vec = {porous_end[1]-startX,0.0,0.0}, 
--        only_surface = true
--        }}}
--  },  
  -- Inlet
  {
    attribute = { kind = 'boundary',
                  label = 'inlet',
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = {
                   { -- inlet
                   origin = { startX + 2*dx, -porous_size[2]/2, -porous_size[3]/2},
                   vec = {
                     {0.,porous_size[2],0.,},
                     {0.,0., porous_size[3]}
                     }
                   }               }
             }
  },  
  -- Outlet
  {
    attribute = { kind = 'boundary',
                  label = 'outlet',
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = {
                   { -- outlet
                   origin = { 
                     origin[1]+length-0.5*dx, 
                     -cube_length/2, 
                     -cube_length/2},
                   vec = {
                     {0.,cube_length,0.,},
                     {0.,0., cube_length}
                     }
                   }               }
             }
  },
  -- Small channel around the porous medium
  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = { kind = 'boundary',
                  label = 'channel_small',
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = {
                   { -- y-
                   origin = { origin[1], -porous_size[2]/2, -porous_size[3]/2},
                   vec = {
                     {porous_end[1] - origin[1],0.,0.,},
                     {0.,0., porous_size[3]}
                     }
                   },
                   { -- y+
                   origin = { origin[1], porous_size[2]/2, -porous_size[3]/2},
                   vec = {
                     {porous_end[1] - origin[1],0.,0.,},
                     {0.,0., porous_size[3]}
                     }
                   },
                   { -- z-
                   origin = { origin[1], -porous_size[2]/2, -porous_size[3]/2},
                   vec = {
                     {porous_end[1] - origin[1],0.,0.,},
                     {0., porous_size[2],0.}
                     }
                   },                   
                   { -- z+
                   origin = { origin[1], -porous_size[2]/2,  porous_size[3]/2},
                   vec = {
                     {porous_end[1] - origin[1],0.,0.,},
                     {0., porous_size[2],0.}
                     }
                   },
                   -- Pipe hull
                   { -- y-
                   origin = { origin[1], -pipeHeight/2, -pipeHeight/2},
                   vec = {
                     {porous_end[1] - origin[1],0.,0.,},
                     {0.,0., pipeHeight}
                     }
                   },
                   { -- y+
                   origin = { origin[1], pipeHeight/2, -pipeHeight/2},
                   vec = {
                     {porous_end[1] - origin[1],0.,0.,},
                     {0.,0., pipeHeight}
                     }
                   },
                   { -- z-
                   origin = { origin[1], -pipeHeight/2, -pipeHeight/2},
                   vec = {
                     {porous_end[1] - origin[1],0.,0.,},
                     {0., pipeHeight,0.}
                     }
                   },                   
                   { -- z+
                   origin = { origin[1], -pipeHeight/2,  pipeHeight/2},
                   vec = {
                     {porous_end[1] - origin[1],0.,0.,},
                     {0., pipeHeight,0.}
                     }
                   },
                   -- Pipe end plate
                   { -- z+
                   origin = { porous_end[1], -pipeHeight/2, -pipeHeight/2},
                   vec = {
                     {0., 0., pipeHeight }, 
                     {0., (pipeHeight-porous_size[2])/2, 0. } 
                     }
                   },
                   { -- z-
                   origin = { porous_end[1], porous_end[2], -pipeHeight/2},
                   vec = {
                     {0., 0., pipeHeight }, 
                     {0., (pipeHeight-porous_size[2])/2, 0. } 
                     }
                   },                   
                   { -- y+
                   origin = { porous_end[1], -pipeHeight/2, -pipeHeight/2},
                   vec = {
                     {0., pipeHeight, 0. }, 
                     {0., 0., (pipeHeight-porous_size[3])/2 } 
                     }
                   },
                   { -- y-
                   origin = { porous_end[1], -pipeHeight/2, porous_end[3]},
                   vec = {
                     {0., pipeHeight, 0. }, 
                     {0., 0., (pipeHeight-porous_size[3])/2 } 
                     }
                   },
             }
             }
  },
  -- wall at the end of the porous medium 
  {
    attribute = { kind = 'boundary',
                  label = 'backplate',
                },
    geometry = { 
                 kind = 'canoND',
                 object = {
                   { -- z+
                   origin = { backPlateX, origin[2], porous_end[3]},
                   vec = {
                     {0., cube_length, 0. }, 
                     {0., 0., cube_length } 
                     }
                   },
                   { -- z-
                   origin = { backPlateX, origin[2], porous_origin[3]},
                   vec = {
                     {0., cube_length, 0. }, 
                     {0., 0.,-cube_length } 
                     }
                   },
                   { -- y+
                   origin = { backPlateX, porous_end[2], origin[3]},
                   vec = {
                     {0., cube_length, 0. }, 
                     {0., 0., cube_length } 
                     }
                   },
                  { -- y-
                   origin = { backPlateX, porous_origin[2], origin[3]},
                   vec = {
                     {0.,-cube_length, 0. }, 
                     {0., 0., cube_length } 
                     }
                   },
               }
             }
  },
  -- Top out
  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = { kind = 'boundary',
                  label = 'out_top',
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = {
                   { -- y+
                   origin = { origin[1], height*0.5, origin[3] },
                   vec = {
                     {cube_length, 0., 0. },
                     {0.,0., cube_length }
                     }
                   },
               }
             }
  }
,
  -- bottom out
  {
    attribute = { kind = 'boundary',
                  label = 'out_bot',
                },
    geometry = { 
                 kind = 'canoND',
                 object = {
                   { -- y-
                   origin = { origin[1],-height*0.5, origin[3] },
                   vec = {
                     {cube_length, 0., 0. },
                     {0.,0., cube_length }
                     }
                   },
               }
             }
  },
  -- front out
  {
    attribute = { kind = 'boundary',
                  label = 'out_frn',
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = {
                   { -- z+
                   origin = { origin[1], origin[2],  height*0.5},
                   vec = {
                     {cube_length, 0., 0. },
                     {0., cube_length,0. }
                     }
                   },
               }
             }
  }
,
  -- back out
  {
    attribute = { kind = 'boundary',
                  label = 'out_bck',
                },
    geometry = { -- single point definition with a canoND object.
                 kind = 'canoND',
                 object = {
                   { -- z+
                   origin = { origin[1], origin[2], -height*0.5},
                   vec = {
                     {cube_length, 0., 0. },
                     {0., cube_length,0. }
                     }
                   },
               }
             }
  },
--   {
--     attribute = { kind = 'boundary',  -- or seed, refinement
--                   label = 'porous',   -- some label to identify the boundary
--                   level = maxLevel,   -- level to refine this object with,
--                   calc_dist = qVals,
--     },
--                 
--     transformation = { 
--       translation = { 
--             -(porous_origin[1] + porous_size[1]*0.5)+2*diffX,
--             -(porous_origin[2] + porous_size[2]*0.5),
--             -(porous_origin[3] + porous_size[3]*0.5) },
--       deformation = fac
--     },  
-- 
--     geometry = { -- Example for a sphere definition
--       kind = 'stl',
--       object = { filename = stlfolder..'porous.stl'
--                }
--    }
--   }
}


