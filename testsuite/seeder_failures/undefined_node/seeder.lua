-- Use this file as template. Do not modify this file for running some testcases
-- Height of the cylinder
height =  0.41 --m
-- number of elements in height
nHeight = 20
-- length to height ratio
l_h = 5
-- number of elements in length
nLength = nHeight*l_h 
-- element size
dx = height/nHeight
-- length of the channel
length = nLength*dx
-- number of elements in bounding cube
--=number of elements in channel + inlet + outlet
nLength_bnd = nLength+2
-- level required to reach computed dx
level = math.ceil(math.log(nLength_bnd)/math.log(2))
-- length of the bounding cube
length_bnd = (2^level)*dx

-- smallest possible element size
dx_eps = length_bnd/2^20
dx_half = dx*0.5
zpos = dx_half

qVal = true

-- directory to write mesh
folder = 'mesh/'

-- How detailed should the output be?
-- The higher the level, the more output you'll get from seeder.
logging = { level = 3 }

debug = {debugMode = true, debugFiles=false, debugMesh ='debug/'}

-- bounding_cube: two entries: origin and length in this
-- order, if no keys are used
bounding_cube = {origin = {-dx/1.,-dx/1.,-dx/1.},
               length = length_bnd}

-- A minimum level, by which all parts in the computational domain should at
-- least be resolved with. Default is 0.
minlevel = level

-- *********************** Table of spatial objects *********************** --
-- Each spatial object is defined by an attribute and some geometric entity
-- attached to this attribute. Attributes might be defined multiple times.
-- Attributes are described by a kind (boundary, seed or refinement), a level
-- and maybe further kind specific values, like a label for the boundary.
-- Geometric objects might by right now:
-- - canoND (point, line, plane or box)
-- - STL
-- - Sphere
-- - Cylinder
--
-- Periodic boundaries are special, spatial objects of this kind can only make
-- use of geometric objects of the kind 'periodic'.
spatial_object = {
-- Seed point
  {
    attribute = {
      kind = 'seed',  ------seed
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = { dx, height*0.5, zpos },
        vec = {length-dx, 0.0,0.0}
        } --- object
    }
  },
  {
    attribute = {
      kind = 'boundary',  ---kind in attribute is seed/boundary/refinement/periodic
      label = 'north',     -- for north
      calc_dist = qVal
    },
    geometry = {
      kind = 'canoND',    -- kind in geometry is canoND/sphere/stl 
      object = {
        origin = { -dx/2,height+dx/2.0+dx_eps,-dx/2 },
        vec = {{length+2*dx,0.0,0.0},
               {0.0,0.0,2.*dx}}
        }
    }
  },
  {
    attribute = {
      kind = 'boundary',
      label = 'south',   --- for south
      calc_dist = qVal
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {-dx/2.0,-dx/2.0-dx_eps,-dx/2.0},
        vec = {{length-dx/2.0,0.0,0.0},
               {0.0,0.0,2.*dx}}
        }
    }
  },
  {
    attribute = {
      kind = 'boundary',
      label = 'west',     --- for west
      calc_dist = qVal
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {-dx_eps,-dx/2.0,-dx/2.0},
        vec = {{0.0,height+2*dx,0.0},
               {0.0,0.0,2.*dx}}
        }
    }
  },

  {
    attribute = {
      kind = 'boundary',
      label = 'east',     --- for east
      calc_dist = qVal
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {length+dx/2.0+dx_eps,dx/2.0,-dx/2.0},
        vec = {{0.0,height,0.0},
               {0.0,0.0,2.*dx}}
        }
    }
  },

  {
    attribute = {
      kind = 'periodic', --kind is periodic
    },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = {
          origin = {-dx_eps, -dx_eps, dx+dx_eps},
          vec = {{length,0.0,0.0},
               {0.0,height+2*dx_eps,0.0}}
        },  --- plane1
        plane2 = {
          origin = {-dx_eps,-dx_eps,-dx_eps},
          vec = {{0.0,height+2*dx_eps,0.0},
                 {length,0.0,0.0}}
        } --- plane2        
      }
    }
  },
  {
    attribute = {
      kind = 'periodic', --kind is periodic
    },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = {
          origin = {-dx_eps, dx_eps, dx+dx_eps},
          vec = {{length+2*dx_eps,0.0,0.0},
               {0.0,height+2*dx_eps,0.0}}
        },  --- plane1
        plane2 = {
          origin = {-dx_eps,dx_eps,-dx_eps},
          vec = {{0.0,height+2*dx_eps,0.0},
                 {length+2*dx_eps,0.0,0.0}}
        } --- plane2        
      }  
    }
  },
}


