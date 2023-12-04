-- Use this file as template. Do not modify this file for running some testcases

outputname = 'example'
outputpreview = true 

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory

stl_files = {{filename = 'input/cube/100.stl', 
              bclabel = 'wall', 
              format = 'binary', 
              refinementlevel = 7},
             {filename = 'input/cube/010.stl', 
              bclabel = 'wall', 
              format = 'binary', 
              refinementlevel = 8}
	      }

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-10.0, -10.0, -10.0},
               length = 20.}

-- refinebox: three entries: origin, length and refinementlevel
refinebox = {origin = {0.0, 1.0, 0.0},
            length = {20.0, 3.0, 10.0},
            refinementlevel = 8
            }               

-- seed: position of seed 
seed = {boundingbox.origin[1]+10.0,
        boundingbox.origin[2]+10.0,
        boundingbox.origin[3]+10.0
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
maxrefine = 4

-- geom: to define intrinsic geometries.
-- Possible geom: spheres, spacers, planes

-- sphere with radius 0.5 at origin
geom = {sphere = {{pos = {0.0,0.0,0.0}
		,radius = 0.5
		, boundary_label='sphere1'}
		,{pos = {0.0,2.0,0.0}
		,radius = 0.5
		, boundary_label='sphere2'}
		,}
		}

-- Laboratory scale spacer:
-- So far: Spacer length along x, width along z, height along y
-- length=20 cm,width=10 cm,
-- filament, radius = 0.01 cm
-- spacer gap = 0.1 cm ! distance between two parallel filament
-- offset = 0.0 ! offset of spacer along its height in bounding box
-- offset distance from bounding box origin
-- default offset=0.0. create spacer in bounding box origin
-- boundary_label = 'spacer'. define boundary name

geom = {
       spacer = {{length = {distance=20.0,axis='x',gap=0.1},
          width = {distance=10.0,axis='z',gap=0.1},
          radius = 0.01,
          offset = 0.0,
	  boundary_label = 'spacer'}}
	  ,
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
       plane={{vecA={2.0,0.0,0.0},
	      vecB={0.0,2.0,0.0},
	      pos={-1.0,-1.0,1.0},
	      bclabel='top',
              refinementlevel = 5}
             ,
	     }}
-- definition 2
-- pos - common point for two triangles in a plane
-- pointA - which defines vecA or 2nd point of 1st triangle
-- pointB - which defined vecB or 2nd point of 2nd triangle
--     pos------------ pointA
--        | -        |
--        |   -A+B   |
--        |     -    |
--        |       -  |
-- pointB |----------x 

-- above example plane defined using points

geom={plane={
             {pointA={1.0,-1.0,1.0},
	      pointB={-1.0,1.0,1.0},
	      pos={-1.0,-1.0,2.0},
	      bclabel='top',
              refinementlevel = 5}
	      }}

