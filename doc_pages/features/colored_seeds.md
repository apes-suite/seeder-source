title: Colored seeds

Seeds might have colors and thereby define different areas within the mesh.
This is for example useful to ascribe special material parameters to parts
of the domain.
Each colored seed is bounded by a boundary of the same color, or boundaries
which have the color `none` or `all`.
Elements which have a non 'none' color attached to them will have the
isColored property and the additional property information about which colors
are attached to that element will be stored.
There might be an arbitrary number of colors and multiple colors can be
assigned to each element.
Color names can be chosen arbitrarily and are case insensitive.

We achieve this by using a bitfield for each color and basically do the
flooding for each color. With up to three colors there should be little to
no memory overhead imposed by this approach, as each color requires only one
byte in the integer bitfield, that we use (the last byte is reserved for
general node information). Beyond that additional integers will be used as
needed by the number of colors.
In the mesh on the disk, colors are stored in ASCII characters with 7 colors
per byte, and meshes without coloring (`none` color everywhere as only color)
no color information will be written at all.

@Note colored boundaries are subject to the same rules of boundary labels
as none colored boundaries and if they intersect with none colored boundaries
will take precedence over those, if configured first.
As this is typically not desired, you probably want to define your colored
boundary objects after the none colored ones!
