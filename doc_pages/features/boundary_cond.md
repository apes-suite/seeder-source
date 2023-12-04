title: Boundary conditions

Boundary conditions are defined by the label attached to a boundary object.
Multiple boundary objects might have the same label and therefore attached
to the same boundary condition.
If there are multiple boundaries in a given direction of an element, the
one defined first in the configuration, will be used. This allows you to
set the precedence order of boundaries that should be used at any
intersections.

Only boundary labels that actually appear in the final mesh will be written
to the header description of the boundaries in the mesh.
