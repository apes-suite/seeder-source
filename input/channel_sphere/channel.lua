outputname = 'channel_sphere'
outputpreview = true 

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory
stl_files = {{filename = 'wall.stl', 
              bclabel = 'wall', 
              format = 'binary'},
             {filename = 'sphere.stl',
              bclabel = 'sphere',
              format = 'binary' },
             {filename = 'inlet.stl', 
              bclabel = 'inlet', 
              format = 'binary' },
             {filename = 'outlet.stl', 
              bclabel = 'outlet', 
              format = 'binary' }}

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-7.99, -8.01, -8.01},
               length = 16.1}
-- refinebox: three entries: origin, length and refinementlevel
refinebox = {origin = {-6.0, -0.5, -0.5},
            length = {7.0, 1.0, 1.0},
            refinementlevel = 7
            }               
-- seed: position of seed 
seed = {boundingbox.origin[1]+12.0,
        boundingbox.origin[2]+8.0,
        boundingbox.origin[3]+8.0
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
maxrefine = minrefine

--spacer = {length = {20.0, 20.0, 20.0},
--          radius = 1.0,
--	  nrperiods = {1.0, 2.0, 1.0},
--	  offset = -5.0}

--sphere = {origin = {0.0,0.0,0.0},
--          radius = 1.0}

--geom = {sphere = {{origin = {0.0,0.0,0.0}, radius = 0.5},
--	 {origin = {0.0,3.0,0.0}, radius = 1.0}}}
