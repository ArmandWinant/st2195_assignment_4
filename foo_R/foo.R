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
  
  return(sphere_volumes)
}

volume_vector_safe <- function(r) {
  tryCatch(
    volume_vector(r),
    error = function(c) "Could not compute the volumes for the given input"
  )
}
# Radius
r <- 2

# Run volume_vector(r) and print the volumes of the spheres with radius r, r^2 and r^3
volumes <- volume_vector_safe(r)
print(volumes)


areaAndPerimeter <- function(h, l) {
  # create a list to contain values
  rectangleAttributes <- list()
  
  # compute area and perimeter
  rectangleAttributes['area'] = h * l
  rectangleAttributes['perimeter'] = 2 * (h + l)
  
  return(rectangleAttributes)
}

rectangleAttributes <- areaAndPerimeter(-1, 6)
print(rectangleAttributes['perimeter'])
print(rectangleAttributes['area'])

fibonacci <- function(n) {
  fibonacciSequence = c(1, 1)
  
  for(i in 3:n) {
    fibonacciSequence[i] = fibonacciSequence[i-2] + fibonacciSequence[i-1]
  }
  
  return(fibonacciSequence)
}


fibonacciIndex <- function(m) {
  i <- 3
  fibonacciMax <- 0
  while (fibonacciMax < m) {
    fibonacciMax = tail(fibonacci(i), n=1)
    i = i + 1
  }
  
  return(i - 1)
}

fibonacciIndex <- function(m) {
  i <- 3

  while(sum(fibonacci(i)) < m) {
    i = i+1
  }
  
  return(i)
}

print(fibonacci(10))
print(fibonacciIndex(5))
