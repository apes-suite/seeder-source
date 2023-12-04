-- SEEDER INFO

-- bounding cube dimensions in m
bc_origin = {0.0, 0.0, 0.0}
bc_length = 0.02

-- length height and length depth ratio
lh_ratio = 4.0
ld_ratio = 4.0

-- stent diameter in m
stent_dia = 0.000025

-- pore size of the pores in the stent in m^2
pore_size = 0.00000004

-- refinement level (at least 10 to have 1 voxel in strut diameter)
minrefine = 10

dx = bc_length/2^minrefine

-- seed origin
seed_orig = {dx, dx, dx}

-- stent(spacer) input arguments in m
sp_orig1 = {bc_length/2.0, 0.0, 0.0}
sp_vec1 = {0.0, bc_length/ld_ratio, 0.0}
sp_radius1 = stent_dia/2.0
sp_filament_gap1 = math.sqrt(pore_size)+stent_dia

sp_orig2 = {bc_length/2.0, 0.0, 0.0}
sp_vec2 = {0.0, 0.0, bc_length/lh_ratio}
sp_radius2 = stent_dia/2.0
sp_filament_gap2 = math.sqrt(pore_size)+stent_dia

-- boundaries
-- inlet
in_orig = {0.0, 0.0, 0.0}
in_vec1 = {0.0, bc_length/ld_ratio+4*dx, 0.0} 
in_vec2 = {0.0, 0.0, bc_length/lh_ratio+4*dx}

-- outlet
out_orig = {bc_length, 0.0, 0.0}
out_vec1 = {0.0, 0.0, bc_length/lh_ratio+4*dx}
out_vec2 = {0.0, bc_length/ld_ratio+4*dx, 0.0}

-- periodic
-- north
perNo_orig = {0.0, 0.0, 0.0}
perNo_vec1 = {0.0, bc_length/ld_ratio+4*dx, 0.0}
perNo_vec2 = {bc_length+4*dx, 0.0, 0.0}

-- south
perSo_orig = {0.0, 0.0, bc_length/lh_ratio+dx/2}
perSo_vec1 = {bc_length+4*dx, 0.0, 0.0}
perSo_vec2 = { 0.0, bc_length/ld_ratio+4*dx, 0.0}

-- front
perFr_orig = {0.0, 0.0, 0.0}
perFr_vec2 = {0.0, 0.0, bc_length/lh_ratio+4*dx}
perFr_vec1 = {bc_length+4*dx, 0.0, 0.0}

-- back
perBa_orig = {0.0, bc_length/ld_ratio+dx/2, 0.0}
perBa_vec2 = {bc_length+4*dx, 0.0, 0.0}
perBa_vec1 = {0.0, 0.0, bc_length/lh_ratio+4*dx}

-- MUSUBI INFO
