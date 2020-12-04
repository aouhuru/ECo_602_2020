summary(body_mass ~ body_length, data = delomys)

shapiro.test(delomys$body_mass)
shapiro.test(delomys$body_length)


