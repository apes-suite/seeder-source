outputname = 'spacer'
outputpreview = true 

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 1, fileformat = 'binary'
-- refinementlevel is the level to which the stl shall be refined
-- it is mandatory
stl_files = {{filename = 'input/spacer_Xinlet.stl', 
              boundary_label = 'inlet', 
              format = 'binary', 
              refinementlevel = 13}, 
	      {filename = 'input/spacer_Xoutlet.stl', 
              boundary_label = 'outlet', 
              format = 'binary', 
              refinementlevel = 13}, 
              {filename = 'input/spacer_Ywall.stl', 
              boundary_label = 'wall', 
              format = 'binary', 
              refinementlevel = 13}, 
	      {filename = 'input/spacer_Zperiodic.stl', 
              boundary_label = 'periodic', 
              format = 'binary', 
              refinementlevel = 13}, 
            }

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-10.0, -10.0, -10.0},
               length = 20.}
-- refinebox: bounding box for spacer	       
refinebox = {origin = {-10.0, -0.02, -0.5},
            length = {20.0, 0.04, 1.0},
            refinementlevel = 13
            }               
-- refinebox: three entries: origin, length and refinementlevel
-- seed: position of seed 
seed = { points = { {boundingbox.origin[1]+10,
                     boundingbox.origin[2]+10,
                     boundingbox.origin[3]+10}
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
maxrefine = 13

-- Laboratory scale spacer:
-- So far: Spacer length along x, width along z, height along y
-- length=20 cm,width=10 cm,
-- filament, radius = 0.01 cm
-- spacer gap = 0.1 cm ! distance between two parallel filament
-- offset = 0.0 ! offset of spacer along its height in bounding box
-- default offset=0.0. create spacer in bounding box origin
-- boundary_label = 'spacer'. define boundary name

geom = {spacer = {{length = {distance=20.0,axis='x',gap=0.1},
          width = {distance=10.0,axis='z',gap=0.1},
          radius = 0.01,
          offset = 0.0, -- offset distance from bounding box origin
          boundary_label = 'spacer'}}}
