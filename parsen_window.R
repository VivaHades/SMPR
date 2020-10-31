euclDistance = function(u,v){
  sqrt(sum((u-v)^2))
}

gaussKer = function(r){
  ((1/(sqrt(2*pi))) * exp((-1/2) * r * r))
}

distance = function(xl,z, metricFunction = euclDistance){
  l = dim(xl)[1]
  n = dim(xl)[2] - 1
  dist = matrix(NA, l, 4)

  for(i in 1:l){
    dist[i, ] = c( metricFunction(xl[i, 1:n], z), xl[i, 1], xl[i, 2],xl[i, 3])
  }
  return (dist)
}

parsenWindow = function(z, xl, h, ker=gaussKer){
  xl = distance(xl, z)
  
  l = dim(xl)[1]
  n = dim(xl)[2]
  
  classes = xl[1:l, n]
  counts = table(classes)
  counts[1:length(counts)] = 0
  names(counts) = c("setosa", "versicolor", "virginica")
  
  
  for (i in 1:l) {
    counts[xl[i,4]] = counts[xl[i,4]] + ker(xl[i,1]/h)
  }
  
  if(max(counts) != 0){
    class = names(which.max(counts))
    return (class)
  }
  
  return (4)
}

colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species],
     col = colors[iris$Species], main="Parsen window")

xl = iris[,3:5];


for (x in seq(1, 7, 0.1)){
  for (y in seq(0, 2.5, 0.1)){
    z = c(x, y)
    class = parsenWindow(z, xl, h=0.2)
    points(z[1], z[2], pch = 21, col = colors[class])
  }
}

