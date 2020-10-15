euclDistance = function(u,v){
  sqrt(sum((u-v)^2))
}
sortObjectsByDist = function(xl,z, metricFunction = euclDistance){
  l = dim(xl)[1]
  n = dim(xl)[2] - 1
  
  distances = matrix(NA, l, 2)
  
  for(i in 1:l){
    distances[i, ] = c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl = xl[order(distances[, 2]), ]
  return (orderedXl)
}

kWNN = function(xl,z,k,q){
  orderedXl = sortObjectsByDist(xl, z)
  n = dim(orderedXl)[2] - 1
  classes = orderedXl[1:k, n+1]
  counts = table(classes)
  counts[1:length(counts)] = 0
  
  for (i in 1:k) {
    counts[classes[i]] <- counts[classes[i]] + q^i
  }
  class = names(which.max(counts))
  return (class)
}


colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species],
     col = colors[iris$Species], main="KWNN")

x = 1;
y = 0;
xl = iris[,3:5];
while (x < 7) {
  while(y < 2.8) {
    z = c(x, y)
    class = kWNN(xl, z, k=6, q=0.7)
    points(z[1], z[2], pch = 22, col = colors[class])
    y = y + 0.1
  }
  y = 0
  x = x + 0.1
}