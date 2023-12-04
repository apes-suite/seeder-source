title: Subresolved boundaries

Boundaries with a color might be resolved beyond the stair-case representation
offered by the mesh by creating a polynomial representation of the color
values within the intersected elements.
For this you need to define a subresolution table, describing at least the
polynomial degree that should be used for this information.
When this table is defined, you can indicate that a (colored) boundary is to
be subresolved by stating `subresolution = true` in its attribute.

Polynomial information will be created per color and is accessible as a
property in the mesh afterwards.
