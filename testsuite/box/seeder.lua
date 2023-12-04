
printRuntimeInfo = false

comment = 'box'
minlevel = 2
-- Use this file as template. Do not modify this file for running some testcases
-- Location to write the mesh in
-- Note the trailing path seperator, needed, if all mesh files should be
-- directory. This directory has to exist before running Seeder in this
-- case
folder = 'mesh/'

level = 2      --number of levels are two

--Debug output can be used to output preliminary tree in restart format
--and this restart file can be converted to vtu format by harvester.
NOdebug = {debugMode=true, debugFiles=true, debugMesh='debug/'}

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
bounding_cube = { origin = {-2.0, -2.0, -2.0}, length = 4.0 }  

-- Each spatial object is defined by an attribute and some geometric
-- entity attached to this attribute. Attribute might be defined mulitple times.
-- Attributes are described by a kind (boundary ,seed or Refinement) a level
    spatial_object = {
   { attribute = { kind = 'seed', label = 'seed', }, -- kind and lable are 'seed'
    
-- Geometric objects might by right now:
-- canoND (point, line, plane or box)
-- STL
      geometry = {
      kind = 'canoND',
      object = { origin = { 0.0, 0.0, 0.0 },
      }
    }
  }, -- seed
  --------------------------------------------
-- defining the attribute and geometry for the left wall
 
  { attribute = {
      kind = 'boundary', label = 'left_wall',
      level = level, calc_dist = false,
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = { -2.0, -2.0, -1.8 },
        vec = {  {  4.0,  0.0,  0.0 },
                 {  0.0,  4.0,  0.0 },
              },
        only_surface = true,
               } -- object
               },
  },
  --------------------------------------------
-- defining the attribute for the right wall

    { attribute = {               
      kind = 'boundary', label = 'right_wall',
      level = level, calc_dist = false,
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = { -2.0, -2.0, 1.4 },
        vec =  { {  4.0,  0.0, 0.0 },
                 {  0.0,  4.0, 0.0 },
        },
        only_surface = true,
      } -- object
    },
  }, -- right_wall
} -- spatial object
