outputname = 'periodic'
outputpreview = true 
debug_mode = true
-- general refinement levels
-- minrefine: defines minimum refinement level of the entire domain
--
-- maxrefine: defines maximum refinement
-- gets overwritten from madatory STL levels
-- (deprecated - used only for spacer stuff right now)
-- will become default for stl refinement level once
-- it is not mandatory any more
minrefine = 1 
maxrefine = 4


-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-2.0,-2.0,-2.0},
               length = 4.}
-- refinebox: three entries: origin, length and refinementlevel
refinebox = {{origin = {-2.0, -1.0, -1.0},
            length = {4.0, 2.0, 2.0},
            refinementlevel = maxrefine
            }}               
-- seed: position of seed 
seed = { points = { {boundingbox.origin[1]+2.0,
                     boundingbox.origin[2]+2.0,
                     boundingbox.origin[3]+2.0}
                  }
       }
-- plane definition
-- definition 1
-- plane is defined by two vectors and position in x,y,z coordinate
-- with reference to origin 
-- pos - position to trasform plane from its origin
-- vecA- vector A from its origin ( defines plane length ) 
-- vecB- vector B from its origin ( defines plane width ) 
--  origin-----------> vecA
--        | -        |
--        |   -A+B   |
--        |     -    |
--        |       -  |
--   vecB v----------x 

-- example plane at xy-plane normal to z-axis
-- offset from z-plane at 1.0
-- starting at x = -1.0 and y = -1.0 
-- length and width of plane is 2.0
geom={
     periodic = {{plane1={
                         vecA={0.0,2.0,0.0},
                         vecB={0.0,0.0,2.0},
                         pos={0.5,-1.0,-1.0}},
                  plane2={
                         vecB={0.0,2.0,0.0},
                         vecA={0.0,0.0,2.0},
                         pos={-0.5,-1.0,-1.0}},
                  bclabel = 'periodic',
                  refinementlevel = maxrefine}
                  }
		  ,
     plane={
             {vecA={4.0,0.0,0.0},
	      vecB={0.0,2.0,0.0},
	      pos={-2.0,-1.0,1.},
	      bclabel='top',
              refinementlevel = maxrefine}
             ,{vecA={4.0,0.0,0.0},
	      vecB={0.0,2.0,0.0},
	      pos={-2.0,-1.0,-1.},
	      bclabel='bottom',
              refinementlevel = maxrefine}
             ,{vecA={4.0,0.0,0.0},
	      vecB={0.0,0.0,2.0},
	      pos={-2.0,1.,-1.0},
	      bclabel='north',
              refinementlevel = maxrefine}
             ,{vecA={4.0,0.0,0.0},
	      vecB={0.0,0.0,2.0},
	      pos={-2.0,-1.,-1.0},
	      bclabel='south',
              refinementlevel = maxrefine}
             }
}

