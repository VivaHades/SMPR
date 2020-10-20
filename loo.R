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

kNN = function(k,orderedXl){
  n = dim(orderedXl)[2] - 1
  classes = orderedXl[1:k, n+1]
  counts = table(classes)
  class = names(which.max(counts))
  return (class)
}

LOO = function(xl){
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  err = array(0, 150)
  
  for(j in 1:l){
    orderedXl = sortObjectsByDist(xl[-j,], c(xl[j, 1:2]))
    for(i in 1:l){
      class = kNN(i, orderedXl)
      if(class != xl[j, 3]){
        err[i] = err[i] + 1/l
      }
    }
  }
  return(err)
}
xl=iris[,3:5]
plot(LOO(xl), pch=21, type="l")
print(which.min(LOO(xl)))