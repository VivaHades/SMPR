euclDistance = function(u,v){
  sqrt(sum((u-v)^2))
}
sortObjectsByDist = function(xl,z, metricFunction = euclDistance){
  l = dim(xl)[1]       #число элементов в выборке
  n = dim(xl)[2] - 1   #число признаков объекта
  print(l)
  distances = matrix(NA, l, 2) #матрица расстояний
  
  for(i in 1:l){
    print(i)
    distances[i, ] = c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl = xl[order(distances[, 2]), ]
  return (orderedXl)
}

kNN = function(xl,z,k,orderedXl){
  n = dim(orderedXl)[2] - 1
  classes = orderedXl[1:k, n+1]
  counts = table(classes)
  class = names(which.max(counts))
  return (class)
}

LOO = function(xl){
  l = dim(xl)[1]-1
  print(l)
  err = array(0, 150)
  for(j in 1:l){
    orderedXl = sortObjectsByDist(xl[-j,], c(xl[j, 1], xl[j, 2]))
    for(i in 1:l){
      class = kNN(xl[-j,], c(xl[j, 1], xl[j, 2]), i, orderedXl)
      if(class != xl[j, 3]){
        err[j] = err[j] + 1
      }
    }
  }
  return(err)
}
xl=iris[,3:5]
print(LOO(xl))
colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species],
     col = colors[iris$Species], main="KNN")
