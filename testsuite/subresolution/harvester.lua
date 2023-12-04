-- This is a sample configuration file to illustrate the options for the
-- Havester visualization tool.

sub_degree = 11
sub_space = 'P'
oversampling = 2
projection_method = 'l2p'

-- Output name --
-- Name of output file from harvester (time stamp is to be appended to name)
name = 'test' -- default is 'simulation'


logging = {level = 2}
--------------------------------------------------------------------------------
-- Input settings
-- This table describes the data to be read for visualization.

input = {

  -- The file to read.
  mesh = 'mesh-'..sub_space..sub_degree..'_o'..oversampling..'_'

  ,subsampling = {
                  -- the number of subsampling steps, i.e. 1 means we
                  -- increase the number of elements by 8
                  levels = 1 + math.ceil(math.log(sub_degree+1,2)),
                  -- the type of projection we use for the subsampling of the data.
                  projection = sub_space..'LegendrePoint',
                }
 }
---------------------------------------------------------------------------------------


---------------------------------------------------------------------------------------
-- Output settings
-- This table describes, what kind of output to produce from the given input data.

output = {

  folder = 'vis-'..sub_space..sub_degree..'_o'..oversampling..'_',


  -- Output is organized in subsets and multiple of them might be defined to create
  -- various outputs from the given input.

  -- Multiple subsets are used to have different outputs from the same Input, for example
  -- from the same restart file you can have 2 output subsets, 1 output subset producing
  -- VTK and the other Tecplot, or two different subdomains, or a different set of 
  -- requested variables.


  -- Subsets
  {  -- Start subset 1 table

    -- Dump only the solid elements.
    -- If solid = true then nothing else is read in this subset.
    -- Solid elments are the elments
    -- which have boundaries, this is useful for example in visualizing
    -- the walls of a channel flow or a comlex geomtry in case of medical flow for 
    -- example
    -- extract solid does not support subtree i.e shape table
    solid = false,

    -----------------------------------------------------------------------------------

    -- Here in each subtable of the requested data, we define the required variables
    -- and the varible system on which they depend.

    -- A table of requested variables is defined here- those are the physicaly derived
    -- variables, it's possible to have different set of requested variables as seen below,
    -- differnt sets of requested variables typically depend on different variable systems.
    
    -- requestedData = {
    --
    --                {
                        --depVarSys = '2D', -- Might be omitted, the first defined system
                                            -- will then be used. 
                        --  variable = {{'velocity'},{'density'} }
    --                },

    --                {
                        -- If there are multiple variable systems in the restart file,
                        -- then the depVarsys have to be defined for each variable system.

    --                  depVarSys = 'System_B',
    --                  variable = {{'density_B'}, {'velocity_B'},{'pressure_B'}}
    --                }
    --
    --                },
    -----------------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- Label is a name to be appended to the output file name, this is needed 
    -- in case of multple output subsets, it marks which file is coming from 
    -- which output subset.  (in case of one subset it could be left blank as 
    -- default)
    label = '',
    ----------------------------------------------------------------------------

    -----------------------------------------------------------------------------------
    -- OUTPUT FORMAT
    -- This is obligatory, there is no Default for this option!

    format = 'VTU',  -- XML VTK format

    ----------------------------------------------------------------------------
    -- It is possible to extract a spatial subset from the input by using a
    -- tracking shape.
    -- If shape is not defind then 'All' is used as default, resulting in all
    -- elements being output.
    shape = {kind = 'all' }
    -- shape= { kind = 'canoND', object={origin ={dx, dx/2.0, 0.0},
    --          vec = {0.0,0.0,height},segments=2*Nheight}}
    ----------------------------------------------------------------------------
  } -- end subset 1 table
}  -- end output table
