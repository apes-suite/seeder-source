outputname = 'plate'
outputpreview = true

-- stl_files: for each stl file put a table into
-- this table of tables. Or just a string for
-- the filname, if the other two parameters should
-- get the default: boundary_type = 2, fileformat = 'binary'
stl_files = { {'input/plate.stl', 'wall', 'binary'}} --, {'input/cylinder.stl','wall2','binary'}}

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
boundingbox = {origin = {-5.0, -5.0, -5.0},
               length = 10.0}

minrefine =  2
maxrefine =  minrefine
