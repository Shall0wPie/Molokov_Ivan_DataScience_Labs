is.integer(7)
#это прикол коненчо
round(7) == 7

a <- c(1, 2, 3)
b <- c(1, 2, 3, 4, 5, 6)
a+b
#хмм и тем не менее посчитал...

b.arr <-array(b, dim = c(3, 2))
b.arr

c.arr <-array(a+b, dim = c(2,2))
c.arr

#несовпадение размерности массивов уже не вывозит
b.arr + c.arr

f <- matrix(b, nrow = 2)
g <- matrix(rep(2,6), nrow = 3)
h <- matrix(rep(2,6), nrow = 3)
g * h
g %*% f


lst <- list("Стипендия в этом семестре = ", FALSE, "(((")
names(lst) <- c("Отрицание","Депрессия","Смирение")
lst
