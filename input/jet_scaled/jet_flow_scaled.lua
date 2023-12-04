-- Jet flow test case geometry C. Bogey, C. Bailly, and D. Juve 
-- Theoret. Comput. Fluid Dynamics (2003) 16: 273–297

-- Output VTK file name
outputname = 'jetflow'
-- VTK file to be writen in asii or Binary format.
outputpreview = true 

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory
stl_files = {
							{
								filename = 'input/cylinder_case.stl', 
								bclabel = 'inlet', 
								format = 'ascii', -- STL file is ascii in this case 
								refinementlevel = 8 
							},
							{
								filename = 'input/wall_hole.stl', 
								bclabel = 'wall', 
								format = 'ascii', -- STL file is ascii in this case 
								refinementlevel = 8
							},
							{
								filename = 'input/box.stl', 
								bclabel = 'jacket', 
								format = 'ascii', -- STL file is ascii in this case 
								refinementlevel = 8 
							}
}

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-25.0, -60.0, -60.0},
							length = 120.0,
              --origin = {-50.0, -80.0, -80.0},
              --length = 200.0,
							refinementlevel = 8
							}

-- refinebox: three entries: origin, length and refinementlevel
--refinebox = {{origin = {-2.0, -42.0, 10.0},
--            length = {68.0, 84.0, 20.0},
--            refinementlevel = 8 
--            }}               
refinebox = {{origin = {-0.064, -40.1, -8.06},
            length = {64.1, 80.1, 16.1},
            --origin = {-1.0, -41.0, -10.0},
            --length = {66.0, 84.0, 20.0},
            refinementlevel = 8
            }}               

-- seed: position of seed 
--seed = {boundingbox.origin[1]+100.0,
--        boundingbox.origin[2]+50.0,
--        boundingbox.origin[3]+50.0
--       }
-- seed: position of seed 
seed = { points = { {32.0, 
                      0.0, 
                      0.0}
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
maxrefine = 8

-- geom: to define intrinsic geometries.
-- Possible geom: spheres, spacers, planes

-- sphere with radius 0.5 at origin
--geom = {sphere = {{pos = {0.0,0.0,0.0}
--		,radius = 0.5
--		, boundary_label='sphere1'}
--		,{pos = {0.0,2.0,0.0}
--		,radius = 0.5
--		, boundary_label='sphere2'}
--		}}

-- Laboratory scale spacer:
-- So far: Spacer length along x, width along z, height along y
-- length=20 cm,width=10 cm,
-- filament, radius = 0.01 cm
-- spacer gap = 0.1 cm ! distance between two parallel filament
-- offset = 0.0 ! offset of spacer along its height in bounding box
-- offset distance from bounding box origin
-- default offset=0.0. create spacer in bounding box origin
-- boundary_label = 'spacer'. define boundary name

--geom = {spacer = {{length = {distance=20.0,axis='x',gap=0.1},
--          width = {distance=10.0,axis='z',gap=0.1},
--          radius = 0.01,
--          offset = 2.0,
--	  boundary_label = 'spacer'}}}

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
--	    
--geom={
----      sphere = {{pos = {-20.0,0.0,0.0}
----		,radius = 3,5
----		, bclabel='sphere1'}}, 
--       
--      plane={
--              {vecA={0.0,20.0,0.0},
--               vecB={0.0,0.0,20.0},
--               pos={-50,-10,-10.0},
--               bclabel='Input',
--               },
--
--             {vecA={140.0,0.0,0.0},
--               vecB={0.0,20.0,0.0},
--               pos={-50,-10,-10},
--               bclabel='topplane'
--               },
--               {vecA={140.0,0.0,0.0},
--               vecB={0.0,20.0,0.0},
--               pos={-50,-10.0,10.0},
--               bclabel='bottomplane'
--               },
--               {vecA={140.0,0.0,0.0},
--               vecB={0.0,0.0,20.0},
--               pos={-50,-10.0,-10.0},
--               bclabel='Sideplane1'
--              },
--               {vecA={140.0,0.0,0.0},
--               vecB={0.0,0.0,20.0},
--               pos={-50,10.0,-10.0},
--               bclabel='Sideplane2'},
--
--               {vecA={0.0,20.0,0.0},
--               vecB={0.0,0.0,20.0},
--               pos={90,-10,-10.0},
--               bclabel='Output'}}
--
--sphere = {{pos = {3.0,0.0,0.0}
--		,radius = 1.0
--		, bclabel='sphere1',refinementlevel=5}}
--        }

--spacer = {{length = {distance=100.0,axis='x',gap=10},
--          width = {distance=100.0,axis='z',gap=10},
--          radius = 5,
--          offset = 0,
--	  boundary_label = 'spacer'}}`
