# データの配列
x1 <- c(12, 12, 11, 7, 8, 9, 14, 11)
x2 <- c(4, 3, 3, 1, 3, 2, 5, 4)
y <- c(22, 24, 21, 19, 19, 22, 24, 23)

# 平均の計算
mean_x1 <- mean(x1)
mean_x2 <- mean(x2)
mean_y <- mean(y)

# 各項と平均の差との２乗和
sum_squared_diff_x1 <- sum((x1 - mean_x1)^2)
sum_squared_diff_x2 <- sum((x2 - mean_x2)^2)
sum_squared_diff_y <- sum((y - mean_y)^2)

sum_squared_diff_x1_x2 <- sum((x1 - mean_x1) * (x2 - mean_x2))
sum_squared_diff_y_x1 <- sum((x1 - mean_x1) * (y - mean_y))
sum_squared_diff_y_x2 <- sum((x2 - mean_x2) * (y - mean_y))


A <- matrix(c(sum_squared_diff_x1, sum_squared_diff_x1_x2, sum_squared_diff_x1_x2, sum_squared_diff_x2), 2, 2, byrow = TRUE)
B <- matrix(c(sum_squared_diff_y_x1, sum_squared_diff_y_x2), 2, 1)
result <- solve(A) %*% B

b1 <- result[1, 1]
b2 <- result[2, 1]
b0 <- mean_y - b1 * mean_x1 - b2 * mean_x2
print("b0,b1,b2")
print(b0)
print(b1)
print(b2)

# 決定変数
sum_squared_diff_y_hat <- sum((mean_y-b0-b1*x1-b2*x2)^2)
R <- sum_squared_diff_y_hat/sum_squared_diff_y
print(sum_squared_diff_y_hat)

mean_epsilon <- mean(y-b0-b1*x1-b2*x2)
sum_squared_diff_epsilon <- sum((y-b0-b1*x1-b2*x2-mean_epsilon)^2)
phi_epsilon = length(x1) - 2 - 1
phi_y = length(x1)- 1
R2 <- 1 - (sum_squared_diff_epsilon/phi_epsilon)/(sum_squared_diff_y/phi_y)
print(R2)

F0 <- (sum_squared_diff_y_hat/2)/(sum_squared_diff_epsilon/phi_epsilon)
print(F0)

