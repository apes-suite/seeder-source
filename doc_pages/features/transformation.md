title: Transformation

Transformation is used to scale, translate, rotate and reflect
the geometrical objects. Trasformation table is defined in the spatial
object table in the lua config file. If the geometry in the spatial
object contains multiple geometries then the transformation defined in that
spatial object is applied to all the geometries.

If both translation and deformation are defined for the geometry object
then the deformation is applied first and then the deformed geometry
is then translated.

* Translation

Translation is a table with three entries defining x,y,z coordinate values
to translate the geometrical object.
Gometry is translated just by adding the position of the geometry with 
given translation vector.

Example:
```lua
spatial_object={
  ...<attribute>...
  ...<geometry>...
  transformation={
    translation={0.0,2.0,0.0} -- translating the object along y-axis by 2.0
  }
}
```
* Deformation

Deformation table can be used to scale, rotate and reflect the geometry.
Deformation cane be defined as const, vector and matrix. In the code,
it is converted to matrix with 3x3. Matrix is multiplied with a geometry
vector to scale, rotate or reflect depends on the given matrix.

* Scaling

If deformation is const, then geometry is scaled in all three directions
with const and it is vector with three entries, then geometry is scaled in
x,y,z directions with different factor.
Example:
* Constant scaling in three direction
```lua
 spatial_object={
   ...<attribute>...
   ...<geometry>...
   transformation={
     deformation = 2.0, -- scaled in all direction by 2.0
   }
}
```
* Different scaling in three direction
```lua
 spatial_object={
   ...<attribute>...
   ...<geometry>...
   transformation={
     deformation = {0.5,2.0,1.5}
   }
}
```
* Reflection
Below example reflect the geometry object in y-axis
```lua
 spatial_object={
   ...<attribute>...
   ...<geometry>...
   transformation={
     deformation = {1.0,-1.0,1.0}
   }
}
```
* Rotation
 Rotation is defined by the deformation table with 3x3 entries.
 Below example rotate the geometry object in z-axis in anti-clockwise
 direction by 45°.
```lua
 spatial_object={
   ...<attribute>...
   ...<geometry>...
   transformation={
     deformation = {
                    { 0.5*math.cos(45*math.pi/180), 
                      -0.5*math.sin(45*math.pi/180), 
                      0.0 },
                    { 0.5*math.sin(45*math.pi/180), 
                      0.5*math.cos(45*math.pi/180), 
                      0.0 },
                    { 0.0, 0.0, 0.5 }
     }
   }
}
```
More information on rotatation matrix can be found in
[Rotation](http://en.wikipedia.org/wiki/Rotation_(mathematics)).
It is also possible to combine scaling, reflection and rotation in the 
deformation matrix.
Example lua file is available at `testsuite/transform/seeder.lua`.
