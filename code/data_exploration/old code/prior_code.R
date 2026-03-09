





# Beta P0 prior  (more precise metho)
x <- seq(0, 1, 0.001)
y <- dbeta(x, shape1=1, shape2=1)  
plot(y ~ x)

# Beta P0 prior (sampling method)
vals <- rbeta(n=100000, shape1 = 1, shape2 = 1)
df <- tibble(vals=vals)
ggplot(data=df, aes(x=vals)) +
  geom_density()



# Lognormal R prior
r1 <- 0.05
r2 <- 0.5
x <- seq(0, 3, by=0.01)
y <- dlnorm(x, meanlog=log((r1+r2)/2), sdlog=log( ((r1+r2)/2) - log(r1)) /2 )
plot(y ~ x, type="line")
