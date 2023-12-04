-- Location to write the mesh in.
-- Note the trailing path seperator, needed, if all mesh files should be in a
-- directory. This directory has to exist before running Seeder in this case!
folder = 'mesh/'

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
bounding_cube = {origin = {-0.1, -0.1, -0.1},
               length = 0.2}


ebug = { debugMode = true, debugMesh = 'debug/' } 

-- Laboratory scale spacer:
-- length=20 cm,width=10 cm,
-- filament, radius = 0.01 cm
-- spacer gap = 0.1 cm ! distance between two parallel filament
-- offset = 0.0 ! offset of spacer along its height in bounding box
-- default offset=0.0. create spacer in bounding box origin
-- boundary_label = 'spacer'. define boundary name
spatial_object = {
  {
    -- Defining a domain boundary
    attribute = {
      kind = 'boundary', -- or seed, refinement
      label = 'solid',   -- some label to identify the boundary condition
      level = 7          -- level to refine this object with, default = 0
    },
    geometry = {
      kind = 'spacer',
      object = {
        length = {
          normal = {1.0,0.0,0.0},
          rotation = {0.0,0.0,0.0}, --clockwise along y-axis
          filament_gap = 0.1,
          length=0.2,
          radius = 0.01,
          origin = {-0.1,0.00,-0.1/2.}
        },
        width = { 
          normal = {0.0,0.0,1.0},
          rotation = {0.0,0.0,0.0}, 
          filament_gap = 0.1,
          length=0.2,
          radius = 0.01,
          origin = {-0.1/2,-0.00,-0.1}
        },
          interwoven = true
      }
    }
  },  
  {
    -- Defining a seed to identify the part of the computational domain in
    -- the universe cube.
    attribute = {
      kind = 'seed'
    },
    geometry = {
      -- single point definition with a canoND object.
      kind = 'canoND',
      object = { origin = {-0.05, -0.05, -0.05}
               }
    }
  }
} -- end of spatial objects

