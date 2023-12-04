-- This seeder configuration file shows an example to create a stenosis.
------------------------------------------------------------------------------
-- Location to write the mesh in
-- Note the trailling path seperator,needed if all mesh files should be in a
-- directory.This directory has to exist before running seeder in this case!
folder = 'mesh/'

-- some comment, you might want to put into the mesh file for later reference.
comment = 'stenosis'

-- A minimum level,by which all part into the computation domain should at 
-- least be resolved with. Default is 0 
minlevel = 10

-- Debug output can be used to output preliminary format in restart format
-- and this restart file can be converted to vtu format by harvester
 ref_level = minlevel 
 Debug = {debugMode = true, debugFiles = false, debugMesh='debug/' }

-- bounding box: two entries: origin and length in this 
-- order, if no keys are used
bounding_cube = {origin = {-4.2, -4.2, -8.2},
                 length = 102.0 }
-- Each spatial object is defined by an the attribute  and some  geometric 
-- entity attached to this attribute. Attribute might be defined multiple times.-- a level
spatial_object = {
  {
    attribute = {
      kind = 'boundary', 
      label = 'inlet',
      level = minlevel 
    },
 -- geometric objects might by right now:
 -- canoND (point, line, plane or box)
 -- STL
    geometry = { 
      kind = 'stl',
      object = {filename = 'stl/inlet.stl'} 
    }
  },
  {
    attribute = {
      kind = 'boundary', 
      label = 'outlet',
      level = minlevel 
    },
    geometry = { 
      kind = 'stl',
      object = {filename = 'stl/outlet.stl'} 
    }
  },
  {
    attribute = {
      kind = 'boundary', 
      label = 'wall',
      level = minlevel 
    },
    geometry = { 
      kind = 'stl',
      object = {filename = 'stl/stenosis.stl'} 
    }
  },
  {
    attribute = { 
      kind = 'seed',
      label = 'seed',
    },
    geometry = {
      kind = 'canoND', 
      object = { origin = {0.0, 0.0, 0.0} }
    }                
   }
}
