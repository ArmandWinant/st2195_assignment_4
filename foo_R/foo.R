# Function to compute the volume of a sphere with radius r
volume <- function(r) {
  return (4/3 * pi * r^3)
}

# Function to compute the volumes of the spheres with radius r, r^2 and r^3
volume_vector <- function(r) {
  sphere_volumes <- numeric()
  
  for (i in 1:3) {
    radius = r^i
    volume = volume(radius)
    sphere_volumes[i] = volume
  }
  
  return (sphere_volumes)
}

# Radius
r <- 2

# Run volume_vector(r) and print the volumes of the spheres with radius r, r^2 and r^3
volumes <- volume_vector(r)
print(volumes)
