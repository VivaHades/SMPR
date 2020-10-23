euclDistance = function(u,v){
  sqrt(sum((u-v)^2))
}
sortObjectsByDist = function(xl,z, metricFunction = euclDistance){
  l = dim(xl)[1]       #число элементов в выборке
  n = dim(xl)[2] - 1   #число признаков объекта
  
  distances = matrix(NA, l, 2) #матрица расстояний
  
  for(i in 1:l){
    distances[i, ] = c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl = xl[order(distances[, 2]), ]
  return (orderedXl)
}

kNN = function(xl,z,k){
  orderedXl = sortObjectsByDist(xl, z)
  n = dim(orderedXl)[2] - 1
  classes = orderedXl[1:k, n+1]
  counts = table(classes)
  class = names(which.max(counts))
  return (class)
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

d=matrix(NA, nrow=10, ncol=3,)
d[1,] = c( 0.5,1,"setosa")
d[2,] = c( 0.6, 1.3, "setosa")
d[3,] = c(0.6,1.6, "setosa")
d[4,] = c(0.5, 1.9,"setosa")

d[5,] = c(1, 1,"versicolor")
d[6,] = c(0.9, 1.3, "versicolor")
d[7,] = c(0.9, 1.6, "versicolor")
d[8,] = c(1, 1.9, "versicolor")

#d[9,] = c(0.75,1.4)

colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(d[, 1:2], pch = 21, bg = colors[d$3],
     col = colors[d$3], main="KWNN")


plot(d)

