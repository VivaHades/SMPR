euclDistance = function(u,v){
  sqrt(sum((u-v)^2))
}

NN = function(xl,z){
  l = dim(xl)[1]
  n = dim(xl)[2] - 1
  
  distances = matrix(NA, l, 1)
  
  for(i in 1:l){
    distances[i, ] = euclDistance(xl[i, 1:n], z)
  }
  distances = cbind(xl, distances)
  min = which.min(distances[, 4])
  return (distances[min, 3])
}


colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species],
     col = colors[iris$Species], main="1NN")

x = 1;
y = 0;
xl = iris[,3:5];
while (x < 7) {
  while(y < 2.8) {
    z = c(x, y)
    class = NN(xl, z)
    points(z[1], z[2], pch = 22, col = colors[class])
    y = y + 0.1
  }
  y = 0
  x = x + 0.1
}
