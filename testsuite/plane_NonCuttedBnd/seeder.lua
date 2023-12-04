-- common header --

minlevel =  5  
-- geometry definition
height =  0.41
length = 1*height

dx     = length/math.pow(2,minlevel)

-- shift the plane to test non-direct boundaries
shift = dx

folder = 'mesh/' 


seederLength = length+dx

debug = {debugMode = true, debugFiles = true, debugMesh = 'debug/'}

-- boundingbox: two entries: origin and length in this
-- order, if no keys are used
bounding_cube = {origin = {-seederLength*0.5, -seederLength*.5, -seederLength*0.5},
               length = seederLength}


spatial_object = {
  { attribute = { kind = 'seed', },
    geometry = { kind = 'canoND',
                 object = { origin = { 0.0, 0.0, dx*0.5 },
               }
    } -- geometry
  }, -- seed
  {
    attribute = {
      kind = 'boundary',
      label='east'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {length*0.5, -length*0.5, -length*0.5},
        vec = {{0.0, length, 0.},
              {0.,0.0, length}}
      }
    }
  },
  {
    attribute = {
      kind = 'boundary',
      label='west'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {-length*0.5, -length*0.5+shift, -length*0.5},
        vec = {{0.0, length, 0.},
              {0.,0.0, length}}
      }
    }
  },
  {
    attribute = {
      kind = 'boundary',
      label='north'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {-length*0.5, height*0.5, -length*0.5},
        vec = {{length, 0.0, 0.},
              {0.,0.0, length}}
      }
    }
  },  
  {
    attribute = {
      kind = 'boundary',
      label='south'
    },
    geometry = {
      kind = 'canoND',
      object = {
        origin = {-length*0.5+shift,-height*0.5, -length*0.5},
        vec = {{length, 0.0, 0.},
              {0.,0.0, length}}
      }
    }
  },
  {
    attribute = { 
      kind = 'boundary', 
      label = 'front'
    },
    geometry = {
      kind = 'canoND',
      object = {
        {
          origin = { -length/2, -length/2, height*0.5},
          vec = { { length, 0.0, 0.0},
                  { 0.0, length, 0.0},}
        }
      } -- object
    } -- geometry
  },  
  {
    attribute = { 
      kind = 'boundary', 
      label = 'back'
    },
    geometry = {
      kind = 'canoND',
      object = {
        {
          origin = { -length/2, -length/2, -height*0.5},
          vec = { { length, 0.0, 0.0},
                  { 0.0, length, 0.0},}
        }
      } -- object
    } -- geometry
  }  
}

