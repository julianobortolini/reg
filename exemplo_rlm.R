# Montgomery, Peck e Vining, 2021, p. 76:
#
# Example 3.1 The Delivery Time Data
# A soft drink bottler is analyzing the vending machine service routes in his distribu
# tion system. He is interested in predicting the amount of time required by the route 
# driver to service the vending machines in an outlet. This service activity includes 
# stocking the machine with beverage products and minor maintenance or housekeep
# ing. The industrial engineer responsible for the study has suggested that the two 
# most important variables affecting the delivery time (y) are the number of cases of 
# product stocked (x1) and the distance walked by the route driver (x2). The engineer 
# has collected 25 observations on delivery time:


ID <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
        11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
        21, 22, 23, 24, 25)

y <- c(16.68, 11.50, 12.03, 14.88, 13.75, 18.11, 8.00, 17.83, 79.24, 21.50,
       40.33, 21.00, 13.50, 19.75, 24.00, 29.00, 15.35, 19.00, 9.50, 35.10,
       17.90, 52.32, 18.75, 19.83, 10.75)

x1 <- c(7, 3, 3, 4, 6, 7, 2, 7, 30, 5,
        16, 10, 4, 6, 9, 10, 6, 7, 3, 17,
        10, 26, 9, 8, 4)

x2 <- c(560, 220, 340, 80, 150, 330, 110, 210, 1460, 605,
        688, 215, 255, 462, 448, 776, 200, 132, 36, 770,
        140, 810, 450, 635, 150)

# Criando a matriz X:

X <- cbind(1, x1, x2)

# Estimação dos betas:

XlX <- t(X) %*% X
XlX
Xly <- t(X) %*% y
Xly
beta_hat <- solve(XlX) %*%  Xly
beta_hat

# Estimação de sigma2

y_hat <- X %*% beta_hat
# ou
# y_hat <- X %*% solve( t(X) %*% X ) %*%  t(X) %*% y
# ou
# H <- X %*% solve( t(X) %*% X ) %*%  t(X)
# y_hat <- H %*% y

e_hat <- y - y_hat
SQRes <- t(e_hat) %*% e_hat
GLRes <- length(y) - length(beta_hat)
QMRes <- SQRes / GLRes
sigma2_hat <- QMRes

# Teste de hipóteses:
# Análise de variância

# FV: total
GLTotal <- length(y) - 1
SQTotal <- t(y) %*% y - (sum(y)^2 / length(y))

# FV: regressão
GLReg <- length(beta_hat) - 1
SQReg <- t(beta_hat) %*% t(X) %*% y - (sum(y)^2 / length(y))
QMReg <- SQReg / GLReg

# FV: resíduos
GLRes <- GLTotal - GLReg
SQRes <- SQTotal - SQReg
QMRes <- SQRes / GLRes
QMRes

# Teste F:
Fc <- QMReg / QMRes
Ftab <- qf(0.95, GLReg, GLRes)
# OU
# Ftab <- qf(0.05, GLReg, GLRes, lower.tail = FALSE)
Fc > Ftab # Rejeita ou não rejeita H0?

# Teste de hipóteses Wald:
# H0: beta_1 = 0 versus H1: beta_1 != 0

C <- solve(XlX)
C11 <- C[2,2]
# OU
# C11 <- C["x1", "x1"]
# OU
C11 <- solve(XlX)[2, 2]
se_b1 <- sqrt(sigma2_hat * C11)
b1 <- beta_hat[2]

tc_b1 <- b1 / se_b1
tc_b1

ttab <- qt(0.975, GLRes)
# OU
# ttab <- qt(0.05/2, GLRes, lower.tail = FALSE)

tc_b1 > ttab # rejeita ou não H0?


# H0: beta_2 = 0 versus H1: beta_2 != 0

C <- solve(XlX)
C22 <- C["x2", "x2"]
# OU
C22 <- solve(XlX)[3, 3]
se_b2 <- sqrt(sigma2_hat * C22)
b2 <- beta_hat[3]

tc_b2 <- b2 / se_b2
tc_b2

ttab <- qt(0.975, GLRes)
# OU
# ttab <- qt(0.05/2, GLRes, lower.tail = FALSE)

tc_b2 > ttab # rejeita ou não H0?


# Intervalo de Confiança para beta_1

talpha2 <- qt(0.975, GLRes)
IC_b1 <- c(b1 - talpha2 * se_b1, b1 + talpha2 * se_b1)
IC_b1

# Intervalo de Confiança para beta_2

talpha2 <- qt(0.975, GLRes)
IC_b2 <- c(b2 - talpha2 * se_b2, b2 + talpha2 * se_b2)
IC_b2

# Intervalo de CONFIANÇA para resposta média

x0 <- c(1, 8, 275)
y0 <- x0 %*% beta_hat
y0

var_y0 <- t(x0) %*% C %*% x0 %*% sigma2_hat
se_y0 <- sqrt(var_y0)
# OU
# var_y_new <- t(x_new) %*% solve(XlX) %*% x_new %*% sigma2_hat

talpha2 <- qt(0.975, GLRes)
IC_y0 <- c(y0 - talpha2 * se_y0,
              y0 + talpha2 * se_y0)
IC_y0

# Predição de novos valores:

x0_new <- c(1, 8, 275)
y0_new <- x0_new %*% beta_hat
var_y0_new <- (1 + t(x0_new) %*% C %*% x0_new) %*% sigma2_hat

talpha2 <- qt(0.975, GLRes)
IC_y0_new <- c(y0_new - talpha2 * sqrt(var_y0_new),
              y0_new + talpha2 * sqrt(var_y0_new))
IC_y0_new



# Comparando os ICs:
IC_y0 # Intervalo de Confiança

diff(IC_y0)

IC_y0_new # Intervalo de Predição
diff(IC_y0_new)
