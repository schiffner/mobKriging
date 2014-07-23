load_all()

data = iris

m = mobKriging(Sepal.Length ~  Sepal.Width + Petal.Width | Petal.Length, data = data)
