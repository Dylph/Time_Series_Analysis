library(forecast)
source("lampiran.R")
penumpang=read.table("penumpang.txt", header=T)
penumpang=ts(penumpang,start=c(2007,1),freq=12)
penumpang2017=read.table("penumpang2017.txt", header=T)
penumpang2017=ts(penumpang2017,start=c(2017,1),freq=12)

lambda <- BoxCox.lambda(penumpang, method = c("guerrero", "loglik"), lower = -1,
  upper = 2)
penumpang.transform <- BoxCox(penumpang,lambda)
pt <- penumpang.transform

############### Transformasi ####################
plot(penumpang.transform, main="Transformasi Box-Cox" )
lines(ksmooth(time(pt), pt, "normal", bandwidth=1), lwd=2, col="red")
dev.new()
ts.plot(penumpang,col="blue",main="Time Series Kedatangan")
lines(ksmooth(time(penumpang), penumpang, "normal", bandwidth=1), lwd=2, col="red")

win.graph()
par(mfrow=c(2,1)) 
acf(pt, na.action=na.pass)
pacf(pt, na.action=na.pass)

dev.new()
PD1 <- diff(pt, diff=1)
ts.plot(PD1, col="blue", main="Time Series Plot")
lines(ksmooth(time(PD1), PD1, "normal", bandwidth=1), lwd=2, col="red")

win.graph()
par(mfrow=c(2,1))
acf(PD1, na.action=na.pass)
pacf(PD1, na.action=na.pass)

adf.test(penumpang)
adf.test(PD1)

############### Model ###########################
model1 <- Arima(pt, order = c(1, 1, 0), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model1)

model2 <- Arima(pt, order = c(0, 1, 1), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model2)

model3 <- Arima(pt, order = c(1, 1, 1), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model3)

model4 <- Arima(pt, order = c(2, 1, 0), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model4)

model5 <- Arima(pt, order = c(0, 1, 2), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model5)

auto.arima(pt)

############### Peramalan #######################
pred.data = predict(model1, n.ahead = 12)
pred.data.low = pred.data$pred-1.96*pred.data$se
pred.data.up = pred.data$pred+1.96*pred.data$se
pred.data=pred.data$pred

#trnasformasi balik
pred.data = InvBoxCox(pred.data, lambda, biasadj = FALSE, fvar = NULL)
pred.data.low = InvBoxCox(pred.data.low, lambda, biasadj = FALSE, fvar = NULL)
pred.data.up = InvBoxCox(pred.data.up, lambda, biasadj = FALSE, fvar = NULL)

#Nilai hasil fitting
fit.data = fitted(model1)
fit.data = InvBoxCox(fit.data, lambda, biasadj = FALSE, fvar = NULL)

#plot data
dev.new()
plot.ts(penumpang,
	xlim = c(2007, 2018), ylim = c(0, 280000),
	main = "ARIMA Fitted vs Actual")

#plot data fitting in-sample
lines(fit.data, col="red")

#plot hasil prediksi dengan model ARIMA(1,1,0)
lines(pred.data, col="blue")
lines(pred.data.low, col="red", lty=3)
lines(pred.data.up, col="red", lty=3)
lines(penumpang2017)
abline(v= 2017, lty=4)


