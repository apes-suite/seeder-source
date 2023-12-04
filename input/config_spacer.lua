outputname = 'sphere'
outputpreview = true 

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
--stl_files = {{filename = 'input/boxSpacer_1.stl', 
--              bclabel = 'fixedwall', 
--              format = 'binary', 
--              refinementlevel = 5}} 
--	      {filename = 'input/Lidcavity_movingwall.stl', 
--              bclabel = 'movingwall', 
--              format = 'binary', 
--              refinementlevel = 5} }

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {0.0,0.0,0.0},
               length = 5.}
-- refinebox: three entries: origin, length and refinementlevel
refinebox = {origin = {-2.5, -0.02, -0.5},
            length = {5.0, 0.04, 1.0},
            refinementlevel = 4 
            }               
-- seed: position of seed 
seed = { points = { {boundingbox.origin[1]+2.5,
                     boundingbox.origin[2]+2.5,
                     boundingbox.origin[3]+2.5}
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
minrefine = 4 
maxrefine = 4
nrperiod = 1.0

-- model for spacer geometry
--geom = {spacer = {{length = {20.0, 0.0, 10.0}, radius = 0.1,nrperiods = {10.0, 4.0, 5.0},offset = 0.0,boundary_label='spacer'}}}
--                 {length = {20.0, 0.0, 10.0}, radius = 0.25,nrperiods = {4.0, 4.0, 2.0},offset = 1.1}}}

-- model for sphere geometry
--geom = {sphere = {{pos = {0.0,0.0,0.0}, radius = 0.5, boundary_label='sphere'}}}
