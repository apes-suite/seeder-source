-- Use this file as template. Do not modify this file for running some testcases

outputname = 'mult_geom'
outputpreview = true 

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-0.1, -0.1, -0.1},
               length = 0.2}

-- refinebox: three entries: origin, length and refinementlevel
refinebox = {origin = {-0.1, -0.02, -0.1},
            length = {0.2, 0.04, 0.2},
            refinementlevel = 6
            }               

-- seed: position of seed 
seed = { points = { {boundingbox.origin[1]+0.1,
                     boundingbox.origin[2]+0.1,
                     boundingbox.origin[3]+0.1}
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
minrefine = 1 
maxrefine = 6

-- geom: to define intrinsic geometries.
-- Possible geom: spheres, spacers, planes

-- sphere with radius 0.5 at origin
geom = {
       spacer = {{length = {distance=20.0,axis='x',gap=0.1},
                   width = {distance=10.0,axis='z',gap=0.1},
                   radius = 0.01,
                   offset = 0.0,
	           bclabel = 'spacer'}}
--		   ,
--        plane={
--	       {vecA={0.2,0.0,0.0},
--                vecB={0.0,0.0,0.2},
--                pos={-0.099,0.02,-0.099},
--                bclabel='top',
--                refinementlevel = 6}
--              ,{vecA={0.2,0.0,0.0},
--                vecB={0.0,0.0,0.2},
--                pos={-0.099,-0.02,-0.099},
--                bclabel='bottom',
--	        refinementlevel = 6}
--              ,
--               {vecA={0.2,0.0,0.0},
--                vecB={0.0,0.04,0.0},
--                pos={-0.099,-0.02,-0.099},
--                bclabel='left',
--	        refinementlevel = 6}
--              ,{vecA={0.2,0.0,0.0},
--                vecB={0.0,0.04,0.0},
--                pos={-0.099,-0.02,0.099},
--                bclabel='right',
--	        refinementlevel = 6}
--               ,{vecA={0.0,0.04,0.0},
--                vecB={0.0,0.0,0.2},
--                pos={-0.099,-0.02,-0.099},
--                bclabel='inlet',
--	        refinementlevel = 6}
--               ,{vecA={0.0,0.04,0.0},
--                vecB={0.0,0.0,0.2},
--                pos={-0.099,-0.02,0.099},
--                bclabel='outlet',
--	        refinementlevel = 6}
--		}
       }
