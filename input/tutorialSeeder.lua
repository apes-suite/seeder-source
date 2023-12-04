-- This is a Seeder configuration lua file.
-- It should be used along with the [Seeder Tutorial]
-- For more information about the Tutorial,
-- please look at: 
outputname = 'Tutorial'
outputpreview = true
folder = 'mesh/tutorial'

-- boundingbox: two entries: origin and length in this order
boundingbox = {origin = {0.0,0.0,0.0}, length = 32.0}

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
geom={
     plane={
             {vecA={0.0, 8.0, 0.0},
              vecB={0.0, 0.0, 0.3},
              pos={32.0, 0.0, 0.0},
              bclabel='east'},
             {vecA={0.0, 8.0, 0.0},
              vecB={0.0, 0.0, 0.3},
              pos={0.0, 0.0, 0.0},
              bclabel='west'},
             {vecA={32.0, 0.0, 0.0},
              vecB={0.0, 0.0, 0.3},
              pos={0.0, 8.0, 0.0},
              bclabel='north'},
             {vecA={32.0, 0.0, 0.0},
              vecB={0.0, 0.0, 0.3},
              pos={0.0, 0.0, 0.0},
              bclabel='south'},
           },
     periodic = {{plane1={
                         vecB={32.0, 0.0, 0.0},
                         vecA={0.0, 8.0, 0.0},
                         pos={0.0, 0.0, 0.0}},
                  plane2={
                         vecA={32.0, 0.0, 0.0},
                         vecB={0.0, 8.0, 0.0},
                         pos={0.0, 0.0, 0.3}},
                  bclabel = 'periodicZ',
                  refinementlevel = 8}
                }
  }

-- general refinement levels
-- minrefine: defines minimum refinement level of the entire domain
-- maxrefine: defines maximum refinement
-- gets overwritten from madatory STL levels
minrefine = 1
maxrefine = 3

-- refinebox: three entries: origin, length and refinementlevel
refinebox = {
            origin = {0.0,0.0,0.0},
            length = {32.0, 8.0, 0.3},
            refinementlevel = 8
            }

-- seed: position of seed 
seed = { points = { {2.0, 2.0, 0.15} } }

