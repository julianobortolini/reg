# Exemplo "[A cross-national relationship between sugar consumption and major depression?](https://onlinelibrary.wiley.com/doi/10.1002/da.10054)": calcular a reta de regressão \hat{y} = \hat{\beta}_0 + \hat{\beta}_1 x

x <- c(300, 390, 350, 375, 480, 150)
y <- c(3, 5.2, 4.4, 5, 5.7, 2.3)

mod <- lm(y ~ x)

mod

summary(mod)


confint(mod)

confint(mod, level = 0.90)

plot(x, y)

abline(mod)
