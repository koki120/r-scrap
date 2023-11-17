library(scatterplot3d)

# ラプラス方程式の定義
laplace_solver <- function(x, y) {
  result = 0
  for (n in seq(1, 99, 2)) {
    result = result + (4/n*pi)*sin(n*pi*x/50)*sinh(n*pi*y/50)/sinh(n*pi)
  }
  return(result)
}

# 3次元グラフのプロット
x = y = seq(0, 50, length.out = 80)  


# 2変数関数の値を計算
z = outer(x, y, laplace_solver)

persp(x, y, z, theta=300, phi=20, expand=0.5, ticktype="detailed",border="blue")