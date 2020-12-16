install.packages("ggplot2")
library(ggplot2)
qplot(data = iris, x = Sepal.Length, y = Petal.Length)
qplot(data = iris, 
      x = Sepal.Length,
      y = Petal.Length,
      color = Species,
      size = Petal.Width,
      alpha = I(0.7))

