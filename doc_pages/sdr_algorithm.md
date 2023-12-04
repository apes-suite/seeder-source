title: Seeder Algorithm
author: Kannan Masilamani

In a first step, Seeder will create a protoTree, that refines all domain
boundaries with the required resolution. This is done iteratively level by
level, and each node, that is intersected by a domain boundary is further
refined on the next level, if the maximum level of all intersected objects
or global minimum level is not reached yet.
At the end of this process the boundaries of the domain are resolved as
requested everywhere.

After the boundaries are properly resolved, it needs to be decided, what is
actually part of the domain itself. For this elements are marked as flooded
based upon their neighborhood. Obviously this only works properly if there
is some initial starting point to get the flooding running. This is achieved
by the user defined seed objects, which will mark all the elements that they
intersect as flooded initially, as long as the elements do not intersect
boundaries at the same time. Elements which intersect boundaries are never
flooded!.

Before flooding, the neighbors of each leaf node is identified for 
each node faces which is necassary for flooding to find the state of 
neighbor node.

This flooding is done iteratively until a iteration did not change any
element state anymore. All flooded elements identify the computational
domain now.

Finally the computational domain is refined to the desired level everywhere,
as given by refinement objects or the global minimal refinement level and
boundary conditions are assigned.
This data is then written in treelm format to disk at the location, specified
by the user in the configuration file. Note, that if this is a directory,
you should not forget the trailing path separator in the definition and
create the directory beforehand.
