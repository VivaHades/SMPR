# СМПР

## Метод ближайшего соседа
Рассмотрим работу метода на примере выборки ирисов Фишера на двух признаках - длине и ширине лепестков.
Сначала отобразим выборку на плоскости. Для этого создадим вектор цветов, координаты которого будут соответствовать разным видам цветов.
Далее, с помощью функции plot(), отобразим все объекты на плоскости.  
<img src="https://github.com/VivaHades/SMPR/blob/main/XL.png" />

Рассмотрим алгоритм.
Для реализации нам понадобятся две функции - функция вычисления евклидова расстояния, и функция-классификатор - алгоритм  1NN.
```r
euclDistance = function(u,v){     #Евклидово расстояние
  sqrt(sum((u-v)^2))
}

NN = function(xl,z){   #xl-выборка, z - классифицируемая точка
  l = dim(xl)[1]       #число элементов в выборке
  n = dim(xl)[2] - 1   #число признаков объекта
  
  distances = matrix(NA, l, 1)  #массив расстояний
  
  for(i in 1:l){
    distances[i, ] = euclDistance(xl[i, 1:n], z)
  }
  distances = cbind(xl, distances)       #связываем по столбцам массив расстояний и выборку
  min = which.min(distances[, 4])        #ищем и вызвращаем минимальный элемент
  return (distances[min, 3])
}
```
## Метод k-ближайших соседей

Перейдем к рассмотрению алгоритма KNN.

``` r
kNN = function(xl,z,k){                   #k - число соседей 
  orderedXl = sortObjectsByDist(xl, z)    #сортируем по расстоянию
  n = dim(orderedXl)[2] - 1
  classes = orderedXl[1:k, n+1]           #массив k-ближайших соседей
  counts = table(classes)                 #вычисляем количество вхождений каждого класса среди k ближайших
  class = names(which.max(counts))        #вычисляем класс, который вошел в k ближайших больше всего раз
  return (class)                          
}
```
Карты классификации 6NN и 1NN

<img src="https://github.com/VivaHades/SMPR/blob/main/6NN.png" />
<img src="https://github.com/VivaHades/SMPR/blob/main/1NN.png" />
