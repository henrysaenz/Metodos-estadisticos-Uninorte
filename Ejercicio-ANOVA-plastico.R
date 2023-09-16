#---------------------------------------------------------------
#Ejercicios de práctica
plastico <- c("A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", "C", "C", "C", "C", "C", "C", "C", "C", "D", "D", "D", "D", "D", "D", "D", "D")
resistencia <- c(135, 175, 97, 169, 213, 171, 115, 143, 275, 170, 154, 133, 219, 187, 220, 185, 169, 239, 184, 222, 253, 179, 280, 193, 115, 105, 93, 85, 120, 74, 87, 63)
data <- data.frame(plastico, resistencia)
summary(data)

boxplot(resistencia~plastico, 
        main = "Nivel de resistencia de acuerdo al grupos de plastico",
        horizontal = TRUE, col = c("lightblue", "blue", "darkblue", "white"))

shapiro.test(data$resistencia[plastico == "A"])
sd(data$resistencia[plastico == "A"])
#Los datos del plastico A se adaptan a una distribucion t-student con un p-valor de 0.91

shapiro.test(data$resistencia[plastico == "B"])
sd(data$resistencia[plastico == "B"])
#Los datos del plastico B se adaptan a una distribucion t-student con un p-valor de 0.81        
      
shapiro.test(data$resistencia[plastico == "C"])
sd(data$resistencia[plastico == "C"])
#Los datos del plastico C se adaptan a una distribucion t-student con un p-valor de 0.51        

shapiro.test(data$resistencia[plastico == "D"])
sd(data$resistencia[plastico == "D"])
#Los datos del plástico D se adaptan a una distribucion t-student con un p-valor de 0.88       

library(car)
leveneTest(data$resistencia, as.factor(data$plastico))
#Con un p-valor de 0.29 no se rechaza la HipÓtesis nula, las varianzas son similares

model <- aov(resistencia~plastico)
print(model)

comp_mult <- TukeyHSD(model) #Para cmparar las medias entre si
print(comp_mult)
#b>a>c>d
#a=b
#c=b, c>a, c>d
#b>c>d
#Se forman 2 grupos homogeneos de medias, t1, t3 y t4 y el otro grupo t2
plot(comp_mult)

names(model)
residuales <- model$residuals
print(residuales)
shapiro.test(residuales)
#Los residuos se ajustan a una distribucion normnalk  t de estuden con un p valor de 0.5976

qqplot(residuales, data$resistencia) #Comparar residuos vs la densidad de los datos originales
