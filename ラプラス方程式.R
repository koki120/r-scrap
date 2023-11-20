library(scatterplot3d)

# 問2のラプラス方程式の定義
laplace_solver2 <- function(x, y) {
  result = 0
  for (n in seq(1, 120, 2)) {
    result = result + (4/(n*pi))*sin(n*pi*x/50)*sinh(n*pi*y/50)/sinh(n*pi)
  }
  return(result)
}

# 3次元グラフのプロット
x = y = seq(0, 50, length.out = 80)  


# 2変数関数の値を計算
z = outer(x, y, laplace_solver2)

persp(x, y, z, theta=60, phi=20, expand=0.5, ticktype="detailed",border="blue")



# 問1のラプラス方程式の定義
laplace_solver1 <- function(n) {
  x = 30
  y = 50
  result = 0
  for (m in seq(1, n, 2)) {
    result = result + 4*sin(m*pi*x/50)*sinh(m*pi*y/50)/((m*pi)*sinh(m*pi))
  }
  return(result)
}

for (n in seq(1,100,2)) {
  diff = 1- laplace_solver1(n)
  if (abs(diff) < 0.01) {
    print(sprintf("初めて条件を満たす項は %d です。", (n + 1) / 2))
    break
  }
}

