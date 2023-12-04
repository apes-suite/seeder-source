-- Use this file as template. Do not modify this file for running some testcases

outputname = 'channel'
outputpreview = true 

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory


-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-30.0, -30.0, -30.0},
               length = 60.}


-- seed: position of seed 
seed = {boundingbox.origin[1]+30.0,
        boundingbox.origin[2]+30.0,
        boundingbox.origin[3]+30.0
       }

-- general refinement levels
-- minrefine: defines minimum refinement level of the entire domain
--
-- maxrefine: defines maximum refinement
-- gets overwritten from madatory STL levels
-- (deprecated - used only for spacer stuff right now)
-- will become default for stl refinement level once
-- it is not mandatory any more
minrefine = 7 
maxrefine = 7

-- geom: to define intrinsic geometries.
-- Possible geom: spheres, spacers, planes

-- sphere with radius 0.5 at origin

-- Laboratory scale spacer:
-- So far: Spacer length along x, width along z, height along y
-- length=20 cm,width=10 cm,
-- filament, radius = 0.01 cm
-- spacer gap = 0.1 cm ! distance between two parallel filament
-- offset = 0.0 ! offset of spacer along its height in bounding box
-- offset distance from bounding box origin
-- default offset=0.0. create spacer in bounding box origin
-- bclabel = 'spacer'. define boundary name


-- plane definition
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
	    
geom={plane={ 
             {vecA={0.0,60.0,0.0},
	      vecB={0.0,0.0,60.0},
	      pos={-30.0,-30.0,-30.0},
	      bclabel='west',
              refinementlevel = 7},
              {vecA={0.0,60.0,0.0},
	      vecB={0.0,0.0,60.0},
	      pos={30.0,-30.0,-30.0},
	      bclabel='east',
              refinementlevel = 7},
              {vecA={60.0,0.0,0.0},
	      vecB={0.0,60.0,0.0},
	      pos={-30.0,-30.0,10.0},
	      bclabel='top',
              refinementlevel = 7},
             {vecA={60.0,0.0,0.0},
	      vecB={0.0,60.0,0.0},
	      pos={-30.0,-30.0,-10.0},
	      bclabel='bottom',
              refinementlevel = 7},
             {vecA={60.0,0.0,0.0},
	      vecB={0.0,0.0,60.0},
	      pos={-30.0,-10.0,-30.0},
	      bclabel='south',
              refinementlevel = 7},
             {vecA={60.0,0.0,0.0},
	      vecB={0.0,0.0,60.0},
	      pos={-30.0,10.0,-30.0},
	      bclabel='north',
              refinementlevel = 7}
             }}
