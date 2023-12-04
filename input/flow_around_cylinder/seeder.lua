-- Use this file as template. Do not modify this file for running some testcases

outputname = 'flow_around_cylinder'
outputpreview = true 

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory
stl_files = {
            {filename = 'cylinder.stl',
             bclabel = 'cylinder',
	     format = 'binary',
	     refinementlevel = 8 
	    },
            {filename = 'inflow.stl',
             bclabel = 'west',
	     format = 'binary',
	     refinementlevel = 8 
	    },
            {filename = 'outflow.stl',
             bclabel = 'east',
	     format = 'binary',
	     refinementlevel = 8 
	    },
            {filename = 'walls.stl',
             bclabel = 'walls',
	     format = 'binary',
	     refinementlevel = 8 
	    },

}

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-51.0, -50.0, -50.0},
               length = 102.}

-- seed: position of seed 
seed = {0.0, 0.0, 0.0}

-- refinebox: three entries: origin, length and refinementlevel
refinebox = {
             {origin = {-50.0, -10.0, -10.0},
              length = {100.0, 20.0, 20.0},
              refinementlevel = 8,
	      deformable = false} }

-- general refinement levels
-- minrefine: defines minimum refinement level of the entire domain
--
-- maxrefine: defines maximum refinement
-- gets overwritten from madatory STL levels
-- (deprecated - used only for spacer stuff right now)
-- will become default for stl refinement level once
-- it is not mandatory any more
minrefine = 1 
maxrefine = 3 

