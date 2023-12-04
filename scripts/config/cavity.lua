outputname = 'lidcavity'
outputpreview = false

-- mesh folder
folder = 'mesh_${SPACER_LENGTH}$/'

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory
--stl_files = {{filename = 'input/cube/100.stl', 
--              bclabel = 'wall', 
--              format = 'binary', 
--              refinementlevel = 7},
--             {filename = 'input/cube/010.stl', 
--              bclabel = 'wall', 
--              format = 'binary', 
--              refinementlevel = 8}
stl_files = {{filename = 'input/Lidcavity_movingwall.stl', 
              boundary_label = 'movingwall', 
              format = 'binary' 
              }, 
	      {filename = 'input/Lidcavity_fixedwall.stl', 
              boundary_label = 'fixedwall', 
              format = 'binary'}}

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-0.5, -0.5, -0.5},
               length = 1.0
              }
-- refinebox: three entries: origin, length and refinementlevel
refinebox = {{origin = {-0.5, -0.5, -0.5},
            length = {1.0, 1.0, 1.0},
            refinementlevel = 6
            }}               
-- seed: position of seed 
seed = { points = { {boundingbox.origin[1]+0.5,
                     boundingbox.origin[2]+0.5,
                     boundingbox.origin[3]+0.5}
                  }
       }
-- general refinement levels
-- minrefine: defines minimum refinement level of the entire domain
--
-- maxrefine: defines maximum refinement
-- gets overwritten from madatory STL levels
-- (deprecated - used only for spacer stuff right now)
-- will become default for stl refinement level once
-- it is not mandatory any more
minrefine = 6 
maxrefine = 6 

