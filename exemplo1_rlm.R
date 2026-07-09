# Exemplo 1 de RLM - Regressão Linear Múltipla


# Example 3.1 The Delivery Time Data
# A soft drink bottler is analyzing the vending machine service routes in his distribu-
# tion system. He is interested in predicting the amount of time required by the route
# driver to service the vending machines in an outlet. This service activity includes
# stocking the machine with beverage products and minor maintenance or housekeep-
# ing. The industrial engineer responsible for the study has suggested that the two
# most important variables affecting the delivery time (y) are the number of cases of
# product stocked (x1) and the distance walked by the route driver (x2). The engineer
# has collected 25 observations on delivery time, which are shown in Table 3.2.

# Tradução:
# Uma engarrafadora de refrigerantes está analisando as rotas de atendimento às máquinas de venda automática em seu sistema de distribuição. Ele está interessado em prever a quantidade de tempo necessária para que o motorista da rota realize o atendimento das máquinas de venda automática em um ponto de venda. Essa atividade de atendimento inclui abastecer a máquina com produtos de bebida e realizar pequenos serviços de manutenção ou limpeza.
# O engenheiro industrial responsável pelo estudo sugeriu que as duas variáveis mais importantes que afetam o tempo de entrega/atendimento ($y$) são o número de caixas de produtos abastecidas ($x_1$) e a distância percorrida a pé pelo motorista da rota ($x_2$). O engenheiro coletou 25 observações sobre o tempo de entrega/atendimento, as quais são apresentadas na Tabela 3.2.

library(MPV)
# data(package = "MPV")
data(softdrink)

head(softdrink)


# Matrizes do modelo Y = X*beta + e
View(softdrink)
Y <- as.matrix(softdrink$y)
Y

X <- cbind("intercepto" = 1, x1 =  softdrink$x1, x2 = softdrink$x2)
X

## beta_hat = (X'X)^(-1) X'Y # beta estimador

XlX <- t(X) %*% X
XlX

XlX_inv <- solve(XlX)
XlX_inv

XlY <- t(X) %*% Y
XlY

## estimativas de beta_0, beta_1 e beta_2
beta_hat <- XlX_inv %*% XlY
beta_hat


## modelo estimado:
paste("y =", round(beta_hat[1], 4), "+", round(beta_hat[2], 4), "*x1 +", round(beta_hat[3], 4), "*x2")
## y = 2.3412 + 1.6159*x1 + 0.0144*x2

2.34 + 1.61*10 + 0.0144 * 500

# estimação de sigma^2

Y_hat <- X %*% beta_hat
cbind(Y, Y_hat)
## ou
Y_hat <- X %*% solve(t(X) %*% X) %*% t(X) %*% Y
## ou
H <- X %*% solve(t(X) %*% X) %*% t(X) # matriz H (hat)
Y_hat <- H %*% Y

e_hat <- Y - Y_hat
e_hat

cbind(Y, Y_hat, e_hat)

SQRes <- sum(e_hat^2)
SQRes

## ou
SQRes <- t(Y) %*% Y - t(beta_hat) %*% XlY
SQRes

gl_Res <- nrow(Y) - ncol(X)
gl_Res
QMRes <- SQRes / gl_Res
QMRes
sigma2_hat <- QMRes
sigma2_hat

# Teste de hipóteses ANOVA
# H0: beta_1 = beta_2 = 0

## Graus de liberdade
gl_total <- nrow(Y) - 1
gl_total
gl_reg <- ncol(X) - 1
gl_reg
gl_res <- gl_total - gl_reg
gl_res

## somas de quadrados

SQTotal <- t(Y) %*% Y - (sum(Y))^2/nrow(Y)
SQTotal
SQReg <- t(beta_hat) %*% XlY - (sum(Y))^2/nrow(Y)
SQReg

SQRes <- SQTotal - SQReg

## quadrados médios

QMReg <- SQReg / gl_reg
QMReg
QMRes <- SQRes / gl_res
QMRes


## teste F

Fc <- QMReg / QMRes
Fc

Ftab <- qf(0.05, gl_reg, gl_res, lower.tail = FALSE)
Ftab

## Fc > Ftab => Se sim, rejeita H0.
Fc > Ftab


# tabela da análise de variância:
ANOVA <- data.frame(
  "FV" = c("Regressão", "Resíduos", "Total"),
  "GL" = c(gl_reg, gl_res, gl_total),
  "SQ" = c(SQReg, SQRes, SQTotal),
  "QM" = c(QMReg, QMRes, NA),
  "Fc" = c(Fc, NA, NA)
)
ANOVA


# teste t
# H0: beta_1 = 0

## matriz C = XlX_inv
C <- XlX_inv
## variância de beta_hat
var_beta_hat <- as.numeric(sigma2_hat) * C
var_beta_hat
round(var_beta_hat, 6)
## desvio padrão de beta_hat
sd_beta_hat <- sqrt(diag(var_beta_hat))
sd_beta_hat

## estatística de teste t
t_beta_1 <- beta_hat[2] / sd_beta_hat[2]
t_beta_1

t_tab <- qt(0.05/2, gl_res, lower.tail = FALSE)

## t_beta_1 > t_tab => Se sim, rejeita H0.
t_beta_1 > t_tab



# Ajuste pela funcao lm

mod <- lm(y ~ x1 + x2, data = softdrink)
mod

## estimativas dos parâmetros
summary(mod)
## estimativas dos betas
## estimativa do erro padrão residual (sigma_hat = sqrt(sigma2_hat))
## teste F da anova
## teste t para os coeficientes

## teste ANOVA
anova(mod)


## intervalos de confiança para os coeficientes:
confint(mod)

## IC de90%:
confint(mod, level = 0.90)

