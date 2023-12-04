simulation_name = 'fullyPeriodicCube'

mesh = 'mesh/'

tracking = {
  {
    label = 'visMesh',
    folder = 'output/',
    variable = { 'level' },
    shape = {
      -- kind = 'all',
      kind = 'canoND',
      object = {
        {
          origin = { 1.0, 1.0, 1.0 },
          segments = { 1 },
        },
      },
    },
    output = { format = 'vtk' },
  },
}
