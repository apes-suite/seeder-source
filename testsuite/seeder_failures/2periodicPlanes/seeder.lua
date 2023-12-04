require "common"

-- Location to write the mesh in.
-- Note the trailing path seperator, needed, if all mesh files should be in a
-- directory. This directory has to exist before running Seeder in this case!
folder = 'mesh/'

debug = {debugMode = true,debugFiles=true,debugMesh='debug/' }

-- bounding cube: two entries: origin and length in this
-- order, if no keys are used
bounding_cube = { origin = bc_origin,
                  length = bc_length }

-- minimum refinement level in fluid domain
minlevel = minrefine

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
      label = 'spacer',   -- some label to identify the boundary condition
      level = minrefine          -- level to refine this object with, default = 0
    },
    geometry = {
      kind = 'spacer',
      object = {
        length = {
          vec = sp_vec1,
          filament_gap = sp_filament_gap1,
          radius = sp_radius1,
          origin = sp_orig1
        },
        width = { 
          vec = sp_vec2,
          filament_gap = sp_filament_gap2,
          radius = sp_radius2,
          origin = sp_orig2
        },
          interwoven = true
      }
    }
  },  
  {
    attribute = {
      kind = 'boundary',
      level = minrefine          -- level to refine this object with, default = 0
    },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = { 
          origin = perNo_orig,
          vec = { perNo_vec1, perNo_vec2 }
        },
        plane2 = {
          origin = perSo_orig,
          vec = { perSo_vec1, perSo_vec2 }
        }
      }  
    }
  },
  {
    attribute = {
      kind = 'boundary',
      level = minrefine          -- level to refine this object with, default = 0
    },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = { 
          origin = perFr_orig,
          vec = { perFr_vec1, perFr_vec2 }
        },
        plane2 = {
          origin = perBa_orig, 
          vec = { perBa_vec1, perBa_vec2 }
        }
      }  
    }
  },
  {
    attribute = {
      kind = 'boundary',
      label = 'inlet',
      level = minrefine          -- level to refine this object with, default = 0
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = out_orig, 
        vec = { out_vec1, out_vec2}
      }
    }
  },
  {
    attribute = {
      kind = 'boundary',
      label = 'outlet',
      level = minrefine          -- level to refine this object with, default = 0
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = in_orig,
        vec = { in_vec1, in_vec2}
      }
    }
  },
  {
    attribute = {
      kind = 'seed',
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = seed_orig
      }
    }
  },
}
      
