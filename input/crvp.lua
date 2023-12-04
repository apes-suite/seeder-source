outputname = 'crvp'
outputpreview = true 
folder = 'crvp/'

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
-- general refinement levels
-- minrefine: defines minimum refinement level of the entire domain
--
-- maxrefine: defines maximum refinement
-- gets overwritten from madatory STL levels
-- (deprecated - used only for spacer stuff right now)
-- will become default for stl refinement level once
-- it is not mandatory any more
maxrefine = 6
minrefine = maxrefine

boundingbox = {origin = {-5.0,-5.0,-5.0},
               length = 10.}
-- refinebox: three entries: origin, length and refinementlevel
refinebox = {{origin = {-0.25, -0.25, -5.},
            length = {0.5, 0.5, 10.0},
            refinementlevel = maxrefine+2
            },
            {origin = {-1.0, -1.0, -5.0},
            length = {2.0, 2.0, 10.0},
            refinementlevel = maxrefine+1
            }}               
-- seed: position of seed 
seed = { points = { {0.1,0.1,0.25}}
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
     plane={
             {vecA={0.0,10.0,0.0},
              vecB={0.0,0.0,10.0},
              pos={5.0,-5.0,-5.0},
  	      bclabel='east',
              refinementlevel = minLevel}
            ,{vecA={0.0,10.0,0.0},
              vecB={0.0,0.0,10.0},
              pos={-5.0,-5.0,-5.0},
              bclabel='west',
              refinementlevel = minLevel}
            ,{vecA={10.0,0.0,0.0},
              vecB={0.0,0.0,10.0},
              pos={-5.0,5.,-5.0},
              bclabel='north',
              refinementlevel = minLevel}
             ,{vecA={10.0,0.0,0.0},
              vecB={0.0,0.0,10.0},
              pos={-5.0,-5.,-5.0},
              bclabel='south',
              refinementlevel = minLevel}
             },                  
     periodic = {{plane1={
                         vecA={10.0,0.0,0.0},
                         vecB={0.0,10.0,0.0},
                         pos={-5.0,-5.0,0.5}},
                  plane2={
                         vecB={10.0,0.0,0.0},
                         vecA={0.0,10.0,0.0},
                         pos={-5.0,-5.0,0.14}},
                  refinementlevel = maxrefine,
                  bclabel = 'periodic_Z'}
             }}

