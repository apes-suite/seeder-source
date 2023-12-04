outputname = '2level'
outputpreview = true 

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory
--stl_files = {{filename = 'input/debug/cube.stl', 
--              bclabel = 'wall', 
--              format = 'binary' }}
--             {filename = 'input/cube/010.stl', 
--              bclabel = 'wall', 
--              format = 'binary', 
--              refinementlevel = 8}
-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {0.0,0.0,0.0},
               length = 10.}
-- refinebox: three entries: origin, length and refinementlevel
refinebox = { origin = {3.5, 3.5, 3.5},
              length = {1.0, 1.00, 1.0},
              refinementlevel = 4   }               
-- seed: position of seed 
seed = { points = { {boundingbox.origin[1]+boundingbox.length*0.6,
                     boundingbox.origin[2]+boundingbox.length*0.6,
                     boundingbox.origin[3]+boundingbox.length*0.6}
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
minrefine = 3 
maxrefine = 3

--geom = {sphere = {{origin = {5.0,3.0,5.0}, radius = 0.5}}} --	 {origin = {0.0,3.0,0.0}, radius = 1.0}}}
