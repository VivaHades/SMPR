# СМПР
## Постановка задачи
Имеется множество объектов(признаковое описание) **X** и и множество ответов **Y**. Задача состоит в том, чтобы построить алгоритм, который для произвольного объекта из **X** поставит в соответствие ответ из **Y**.
Для построения алгоритмов классификации опираются на гипотезу компактности.
## Гипотеза компактности
Схожим объектам соответствуют схожие ответы.
## Метрические методы
Метрические методы - класс алгоритмов которые строятся на основе метрик. Мы рассмотрим следующие:
 + [1NN]
 + [KNN]
 + [KWNN]
 + [Метод парзеновского окна]
 + [Метод потенциалов]
 
Кроме самих методов, также мы оценим их точность с помощью метода LOO(leave-one-out).

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
## Метод k ближайших соседей

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

Метод можно оптимизировать - не сортировать всю выборку, а  пробежать ее k раз, находя новый минимальный, старый при этом убирать и сразу добавлять в массив classes.

## Метод k взвешенных ближайших соседей
Данный метод - модификация KNN, в которой добавляется весовая функция. Теперь, прежде чем классифицировать объект, мы будем проверять элементы какого класса имеют больший вес. В качестве весовой функции выбрана **q^i**, где q - еще один параметр, который можно подбирать,а i - индекс элемента из k ближайших.
``` r
kWNN = function(xl,z,k,q){
  orderedXl = sortObjectsByDist(xl, z)
  n = dim(orderedXl)[2] - 1
  classes = orderedXl[1:k, n+1]
  counts = table(classes)
  counts[1:length(counts)] = 0 #обнуляем
  
  for (i in 1:k) {
    counts[classes[i]] <- counts[classes[i]] + q^i #вычисляем веса 
  }
  class = names(which.max(counts))
  return (class)
}
```

## Карты классификации 6NN, 6WNN и 1NN

<img src="https://github.com/VivaHades/SMPR/blob/main/6NN.png" />
<img src="https://github.com/VivaHades/SMPR/blob/main/KWNN.png" />
<img src="https://github.com/VivaHades/SMPR/blob/main/1NN.png" />

## LOO для KNN и KWNN ##

Для определения оптимальных параметров существует метод LOO. Суть его заключается в следующем: берем нашу обучающую выборку. Из нее извлекаем один элемент и запускаем алгоритм для всех комбинаций параметров. Если алгоритм классифицирует объект неправильно - добавляем одну ошибку. Повторяем это для всей выборки. В результате - получаем массив, в котором содержится количество ошибок для всех вариантов параметров. Найдя минимальный элемент массива получим наилучшие параметры.
## LOO для KNN ##
```r
LOO = function(xl){
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  err = array(0, 150)#массив ошибок
  
  for(j in 1:l){
    #сортируем все объекты по расстоянию относительно j элементами
    orderedXl = sortObjectsByDist(xl[-j,], c(xl[j, 1:2]))
    for(i in 1:l){
      class = kNN(i, orderedXl)   #определяем класс j объекта для всех k
      if(class != xl[j, 3]){      #сравниваем вычисленные классы с реальным 
        err[i] = err[i] + 1/l     
      }
    }
  }
  return(err)
}
```
<img src="https://github.com/VivaHades/SMPR/blob/main/LOO_KNN.png" />

В результате получаем, что опимальный k для выборки ирисов равен 6.

## LOO для KWNN ##
```r
LOO = function(xl){
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  
  #границы q
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
```
наименьшее число ошибок в результате работы алгоритма получилось для k=25, 30, 32, 33 при q=0.98, 0.96,0.96, 0.96 соответственно.

<img src="https://github.com/VivaHades/SMPR/blob/main/LOO_KWNN.jpeg" />

## Сравнительная таблица методов KNN и KWNN ##

<table>
 
 <tr>
  <td> метод </td>
  <td> параметры </td>
  <td> % ошибок </td>
 </tr>
 <tr>
  <td> KNN </td>
  <td> k=6 </td>
  <td> 3.4 </td>
 </tr>
 <tr>
  <td> KWNN </td>
  <td> k=25, q=0.96 </td>
  <td> 3.33 </td>
 </tr>
 </table>
