folder = 'mesh/'

length=1.0
debug = { debugMode = true, debugFiles = true, debugMesh = 'debug/'}

minlevel = 4

-- Multilevel and multi-periodic does not work
refinementLevel = 0 
refinementLevel2 = 0

deltax=2.*length/2^(minlevel+refinementLevel)
--use mininum dx to offset the periodic planes 
--so that periodic planes are aligned perfectly parallel to each other
mindeltax=2.*length/2^(minlevel+refinementLevel2)

bounding_cube = {origin = {-length,-length,-length},
                 length = 2.*length}


spatial_object = {
  { attribute = { kind = 'seed'},
    geometry = { 
      kind = 'canoND',
      object = { origin = {0.,0.,deltax*0.5}}
    }
  },
  { attribute = { kind = 'refinement',
                  level = minlevel + refinementLevel
                },
    geometry = { 
      kind = 'canoND',
      object = {
               origin = {-0.25, -0.25, -0.5},
               vec = { {0.5, 0.0, 0.0},
                       {0.0,0.5, 0.0},
                       {0.0,0.0,1.0}}
      }
    }
  },
  { attribute = { kind = 'refinement',
                  level = minlevel + refinementLevel2
                },
    geometry = { 
      kind = 'canoND',
      object = {
        origin = {-0.25, -0.25/2., -0.5},
        vec = { {0.5, 0.0, 0.0},
                {0.0,0.5/2.0, 0.0},
                {0.0,0.0,1.0}}
      }
    }
  },
  { attribute = { kind = 'periodic', label='periodic' },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = {
          origin = { -length,-length,deltax+mindeltax/2.},
          vec = { {2.*length,0.0,0.0},
                  {0.0,2.*length,0.0},}
        }, -- plane 1
        plane2 = {
          origin = { -length,-length,-mindeltax},
          vec = { { 0.0,2.*length,0.0},
                  { 2.*length,0.0,0.0},}
        }, -- plane 2
      } -- object
    } -- geometry
  }, -- periodic table
  { attribute = { kind = 'periodic' },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = {
          origin = { length,-length,-length},
          vec = { {0.0,2.*length,0.0},
                  {0.0,0.0,2.*length},}
        }, -- plane 1
        plane2 = {
          origin = { -length,-length,-length},
          vec = { { 0.0,0.0,2.*length},
                  { 0.0,2.*length,0.0},}
        }, -- plane 2
      } -- object
    } -- geometry
  }, -- periodic table
  { attribute = { kind = 'periodic' },
    geometry = {
      kind = 'periodic',
      object = {
        plane1 = {
          origin = {-length,length,-length },
          vec = { {0.0,0.0,2.*length},
                  {2.*length,0.0,0.0},}
        }, -- plane 1
        plane2 = {
          origin = { -length,-length,-length},
          vec = { { 2.*length,0.0,0.0},
                  { 0.0,0.0,2.*length},}
        }, -- plane 2
      } -- object
    } -- geometry
  } -- periodic table
}

