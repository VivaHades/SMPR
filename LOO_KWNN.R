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

kWNN = function(k,q,orderedXl){
  
  n = dim(orderedXl)[2] - 1
  classes = orderedXl[1:k, n+1]
  counts = table(classes)
  counts[1:length(counts)] = 0
  
  for (i in 1:k) {
    counts[classes[i]] = counts[classes[i]] + q^i
  }
  class = names(which.max(counts))
  return (class)
}

LOO = function(xl){
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  
  a=0.02
  b=0.98
  k=50
  err = matrix(0, nrow=k, ncol=b/a)
  
  for(j in 1:l){
    
    orderedXl = sortObjectsByDist(xl[-j,], c(xl[j, 1:2]))
    for(i in 1:k){
      s=1
      for(q in seq(a, b, a)){
        
        class = kWNN(i, q, orderedXl)
        if(class != xl[j, 3]){
          err[i,s] = err[i,s] + 1/l
        }
        s = s + 1
      }
    }
  }
  return(err)
}

xl=iris[,3:5]
res = LOO(xl)

minERR = which(res == min(res), arr.ind = TRUE)
for (i in 1:length(minERR)){
  if(minERR[i] > 0){
    print(minERR)
    print(min(res))
  }
}
heatmap(res, xlab='q', ylab='k', Colv=NA, Rowv=NA)
