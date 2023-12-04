outputname = 'multiPeriodic'
outputpreview = true
folder = 'mesh/'

length=1.0
debug = { debugMode = true, debugFiles = true}

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
maxrefine = 5
minrefine = maxrefine

deltax=2.*length/math.pow(2,maxrefine)

boundingbox = {origin = {-length,-length,-length},
               length = 2.*length}
-- refinebox: three entries: origin, length and refinementlevel
efinebox = {
            {origin = {-0.25, -0.25, -5.},
            length = {0.5, 0.5, 10.0},
            refinementlevel = maxrefine+2
            },
            { origin = {25.0, 25.0, 0.0},
              length = {25.0, 25.0, 64.0},
              refinementlevel = maxrefine+1
            } 
            }               
-- seed: position of seed 
seed = { points = { {0.,0.,deltax*0.5}}
       }
geom={
     periodic = {
                 {plane1={
                         vecA={2.*length,0.0,0.0},
                         vecB={0.0,2.*length,0.0},
                         pos={-length,-length,deltax+deltax/2.}},
                  plane2={
                         vecB={2.*length,0.0,0.0},
                         vecA={0.0,2.*length,0.0},
                         pos={-length,-length,-deltax}}},
                  {plane1={
                         vecB={0.0,0.0,2.*length},
                         vecA={0.0,2.*length,0.0},
                         pos={length,-length,-length}},
                  plane2={
                         vecA={0.0,0.0,2.*length},
                         vecB={0.0,2.*length,0.0},
                         pos={-length,-length,-length}}}
                 ,{plane1={
                         vecA={0.0,0.0,2.*length},
                         vecB={2.*length,0.0,0.0},
                         pos={-length,length,-length}},
                  plane2={
                         vecB={0.0,0.0,2.*length},
                         vecA={2.*length,0.0,0.0},
                         pos={-length,-length,-length}}}
         
             }
   }

