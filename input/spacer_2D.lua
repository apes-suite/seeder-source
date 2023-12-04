outputname = 'spacer'
outputpreview = true 

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-0.1, -0.02, -0.15},
               length = 0.3}
-- refinebox: bounding box for spacer	       
refinebox = {origin = {-0.1, -0.02, -0.1},
            length = {0.2, 0.04, 0.2},
            refinementlevel = 6
            }               
-- refinebox: three entries: origin, length and refinementlevel
-- seed: position of seed 
seed = { points = { {boundingbox.origin[1]+0.1,
                     boundingbox.origin[2]+0.02,
                     boundingbox.origin[3]+0.15}
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
minrefine = 2 
maxrefine = 5

-- Laboratory scale spacer:
-- So far: Spacer length along x, width along z, height along y
-- length=20 cm,width=10 cm,
-- filament, radius = 0.01 cm
-- spacer gap = 0.1 cm ! distance between two parallel filament
-- offset = 0.0 ! offset of spacer along its height in bounding box
-- default offset=0.0. create spacer in bounding box origin
-- boundary_label = 'spacer'. define boundary name

geom = {
 spacer = {{length = {distance=0.2,axis='x',gap=0.1},
          width = {distance=0.2,axis='z',gap=0.1},
          radius = 0.01,
          offset = 0.0, -- offset distance from bounding box origin
          bclabel = 'spacer'}}
	  ,
	periodic = {{plane1={
          vecB={0.2,0.0,0.0},
			    vecA={0.0,0.04,0.0},
			    pos={-0.1,-0.02,-0.1}},
                     plane2={
		            vecA={0.2,0.0,0.0 },
			    vecB={0.0,0.04,0.0},
			    pos={-0.1,-0.02,0.1}},
	             bclabel='periodic_z',
		     refinementlevel=4}
		     }
		     ,
	plane = {
		  {vecA={0.2,0.0,0.0},    
	           vecB={0.0,0.0,0.2},
	           pos={-0.1,-0.019,-0.1},
                   bclabel='south',
                   refinementlevel=4}
		 ,{vecA={0.2,0.0,0.0},    
	           vecB={0.0,0.0,0.2},
	           pos={-0.1,0.02,-0.1},
                   bclabel='north',
                   refinementlevel=4}
	         ,{vecA={0.0,0.04,0.0},
		   vecB={0.0,0.0,0.2},
		   pos={-0.099,-0.02,-0.1},
		   bclabel='inlet',
		   refinementlevel=4}
		 ,{vecA={0.0,0.04,0.0},    
	           vecB={0.0,0.0,0.2},
	           pos={0.099,-0.02,-0.1},
                   bclabel='outlet',
                   refinementlevel=4}
--		 ,{vecA={0.4,0.0,0.0},    
--	           vecB={0.0,0.04,0.0},
--	           pos={-0.2,-0.02,-0.099},
--                   bclabel='bottom',
--                   refinementlevel=4}
--		 ,{vecA={0.4,0.0,0.0},    
--	           vecB={0.0,0.04,0.0},
--	           pos={-0.2,-0.02,0.099},
--                   bclabel='top',
--                   refinementlevel=4}
		  }
		 } 
