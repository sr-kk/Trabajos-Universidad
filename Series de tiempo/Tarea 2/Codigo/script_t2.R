datos = rio::import("Tareas/chil013-crn-noaa.txt")

Y = ts(datos$trsgi)
plot.ts(Y)


summary(lm(Y ~1))
