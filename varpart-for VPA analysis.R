library("vegan") 
a=read.table("data.txt",header=TRUE)
b=a[,34]
mod <- varpart(b, ~ plantcover+plantrichness, ~ Invertebrate+Protists+Fungal+Bacteria, ~ climate+ N + P + pH + Sand, ~ spatial , data=a)
plot(mod)
mod
Plant_richness <- model.matrix(~ plantcover+plantrichness, data=a)
aFrac <- rda(b, Plant_richness, data=a)
anova(aFrac, step=9999, perm.max=9999)
Soil_biodiv <- model.matrix(~ Invertebrate+Protists+Fungal+Bacteria, data=a)
aFrac <- rda(b, Soil_biodiv, data=a)
anova(aFrac, step=9999, perm.max=9999)
Environment <- model.matrix(~ climate+ N + P + pH + Sand, data=a)
aFrac <- rda(b, Environment, data=a)
anova(aFrac, step=9999, perm.max=9999)
Space <- model.matrix(~ spatial, data=a)
aFrac <- rda(b, Space, data=a)
anova(aFrac, step=9999, perm.max=9999)

# module#3:
a=read.table("data.txt",header=TRUE)
# b=a[,33]
b=a[,c(13:14,19,29)]
mod <- varpart(b, ~ plantcover+plantrichness, ~ Invertebrate+Protists+Fungal+Bacteria, ~ climate+ N + P + pH + Sand, ~ spatial , data=a)
plot(mod)
mod
Plant_richness <- model.matrix(~ plantcover+plantrichness, data=a)
aFrac <- rda(b, Plant_richness, data=a)
anova(aFrac, step=9999, perm.max=9999)
Soil_biodiv <- model.matrix(~ Invertebrate+Protists+Fungal+Bacteria, data=a)
aFrac <- rda(b, Soil_biodiv, data=a)
anova(aFrac, step=9999, perm.max=9999)
Environment <- model.matrix(~ climate+ N + P + pH + Sand, data=a)
aFrac <- rda(b, Environment, data=a)
anova(aFrac, step=9999, perm.max=9999)
Space <- model.matrix(~ spatial, data=a)
aFrac <- rda(b, Space, data=a)
anova(aFrac, step=9999, perm.max=9999)
# Module#2
a=read.table("data.txt",header=TRUE)
# b=a[,32]
b=a[,c(17,20:23)]
mod <- varpart(b, ~ plantcover+plantrichness, ~ Invertebrate+Protists+Fungal+Bacteria, ~ climate+ N + P + pH + Sand, ~ spatial , data=a)
plot(mod)
mod
Plant_richness <- model.matrix(~ plantcover+plantrichness, data=a)
aFrac <- rda(b, Plant_richness, data=a)
anova(aFrac, step=9999, perm.max=9999)
Soil_biodiv <- model.matrix(~ Invertebrate+Protists+Fungal+Bacteria, data=a)
aFrac <- rda(b, Soil_biodiv, data=a)
anova(aFrac, step=9999, perm.max=9999)
Environment <- model.matrix(~ climate+ N + P + pH + Sand, data=a)
aFrac <- rda(b, Environment, data=a)
anova(aFrac, step=9999, perm.max=9999)
Space <- model.matrix(~ spatial, data=a)
aFrac <- rda(b, Space, data=a)
anova(aFrac, step=9999, perm.max=9999)

#Module#1
a=read.table("data.txt",header=TRUE)
# b=a[,33]
b=a[,c(15,18,24:28,30)]
mod <- varpart(b, ~ plantcover+plantrichness, ~ Invertebrate+Protists+Fungal+Bacteria, ~ climate+ N + P + pH + Sand, ~ spatial , data=a)
plot(mod)
mod
Plant_richness <- model.matrix(~ plantcover+plantrichness, data=a)
aFrac <- rda(b, Plant_richness, data=a)
anova(aFrac, step=9999, perm.max=9999)
Soil_biodiv <- model.matrix(~ Invertebrate+Protists+Fungal+Bacteria, data=a)
aFrac <- rda(b, Soil_biodiv, data=a)
anova(aFrac, step=9999, perm.max=9999)
Environment <- model.matrix(~ climate+ N + P + pH + Sand, data=a)
aFrac <- rda(b, Environment, data=a)
anova(aFrac, step=9999, perm.max=9999)
Space <- model.matrix(~ spatial, data=a)
aFrac <- rda(b, Space, data=a)
anova(aFrac, step=9999, perm.max=9999)
#1
a=read.table("data.txt",header=TRUE)
b=a[,13]
mod <- varpart(b, ~ plantcover+plantrichness, ~ Invertebrate+Protists+Fungal+Bacteria, ~ climate+ N + P + pH + Sand, ~ spatial , data=a)
plot(mod)
mod
Plant_richness <- model.matrix(~ plantcover+plantrichness, data=a)
aFrac <- rda(b, Plant_richness, data=a)
anova(aFrac, step=9999, perm.max=9999)
Soil_biodiv <- model.matrix(~ Invertebrate+Protists+Fungal+Bacteria, data=a)
aFrac <- rda(b, Soil_biodiv, data=a)
anova(aFrac, step=9999, perm.max=9999)
Environment <- model.matrix(~ climate+ N + P + pH + Sand, data=a)
aFrac <- rda(b, Environment, data=a)
anova(aFrac, step=9999, perm.max=9999)
Space <- model.matrix(~ spatial, data=a)
aFrac <- rda(b, Space, data=a)
anova(aFrac, step=9999, perm.max=9999)
#2
a=read.table("data.txt",header=TRUE)
b=a[,14]
mod <- varpart(b, ~ plantcover+plantrichness, ~ Invertebrate+Protists+Fungal+Bacteria, ~ climate+ N + P + pH + Sand, ~ spatial , data=a)
plot(mod)
mod
Plant_richness <- model.matrix(~ plantcover+plantrichness, data=a)
aFrac <- rda(b, Plant_richness, data=a)
anova(aFrac, step=9999, perm.max=9999)
Soil_biodiv <- model.matrix(~ Invertebrate+Protists+Fungal+Bacteria, data=a)
aFrac <- rda(b, Soil_biodiv, data=a)
anova(aFrac, step=9999, perm.max=9999)
Environment <- model.matrix(~ climate+ N + P + pH + Sand, data=a)
aFrac <- rda(b, Environment, data=a)
anova(aFrac, step=9999, perm.max=9999)
Space <- model.matrix(~ spatial, data=a)
aFrac <- rda(b, Space, data=a)
anova(aFrac, step=9999, perm.max=9999)

#total8
a=read.table("data.txt",header=TRUE)
b=a[,42]
mod <- varpart(b, ~ plantcover+plantrichness, ~ Invertebrate+Protists+Fungal+Bacteria, ~ climate+ N + P + pH + Sand, ~ spatial , data=a)
plot(mod)
mod
Plant_richness <- model.matrix(~ plantcover+plantrichness, data=a)
aFrac <- rda(b, Plant_richness, data=a)
anova(aFrac, step=9999, perm.max=9999)
Soil_biodiv <- model.matrix(~ Invertebrate+Protists+Fungal+Bacteria, data=a)
aFrac <- rda(b, Soil_biodiv, data=a)
anova(aFrac, step=9999, perm.max=9999)
Environment <- model.matrix(~ climate+ N + P + pH + Sand, data=a)
aFrac <- rda(b, Environment, data=a)
anova(aFrac, step=9999, perm.max=9999)
Space <- model.matrix(~ spatial, data=a)
aFrac <- rda(b, Space, data=a)
anova(aFrac, step=9999, perm.max=9999)
