######################################################################
######################################################################
##################### Transformasi  Akar #############################

library(forecast)
library(tseries)

############### Diagnosa Data Asli ####################
source("lampiran.R")
penumpang=read.table("penumpang.txt", header=T)
penumpang=ts(penumpang,start=c(2007,1),freq=12)
penumpang2017=read.table("penumpang2017.txt", header=T)
penumpang2017=ts(penumpang2017,start=c(2017,1),freq=12)

ts.plot(penumpang,col="blue",main="Time Series Kedatangan")
lines(ksmooth(time(penumpang), penumpang, "normal", bandwidth=1), lwd=2, col="red")

adf.test(penumpang)
win.graph()
par(mfrow=c(2,1)) 
acf(penumpang, na.action=na.pass)
pacf(penumpang, na.action=na.pass)


############### Transformasi ####################
pakar <- sqrt(penumpang)
plot(pakar, main="Transformasi Akar" )
lines(ksmooth(time(pt), pt, "normal", bandwidth=1), lwd=2, col="red")

win.graph()
par(mfrow=c(2,1)) 
acf(pakar, na.action=na.pass)
pacf(pakar, na.action=na.pass)

dev.new()
p.diff.akar <- diff(pakar, diff=1)
ts.plot(p.diff.akar, col="blue", main="Time Series Plot")
lines(ksmooth(time(p.diff.akar), p.diff.akar, "normal", bandwidth=1), lwd=2, col="red")

win.graph()
par(mfrow=c(2,1))
acf(p.diff.akar, na.action=na.pass)
pacf(p.diff.akar, na.action=na.pass)

adf.test(p.diff.akar)

############### Model ###########################
auto.arima(pt)

model1 <- Arima(pakar, order = c(1, 1, 0), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model1)

model2 <- Arima(pakar, order = c(0, 1, 1), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model2)

model3 <- Arima(pakar, order = c(1, 1, 1), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model3)

model4 <- Arima(pakar, order = c(2, 1, 0), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model4)

model5 <- Arima(pakar, order = c(0, 1, 2), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model5)

############### Peramalan #######################
pred.data = predict(model1, n.ahead = 12)
pred.data.low = pred.data$pred-1.96*pred.data$se
pred.data.up = pred.data$pred+1.96*pred.data$se
pred.data=pred.data$pred

#trnasformasi balik
pred.data = pred.data^2
pred.data.low = pred.data.low^2
pred.data.up = pred.data.up^2

#Nilai hasil fitting
fit.data = fitted(model1)
fit.data = fit.data^2

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



######################################################################
######################################################################
##################### Transformasi  Boxcox ###########################

lambda <- BoxCox.lambda(penumpang, method = c("guerrero", "loglik"), lower = -1,
  upper = 2)
penumpang.transform <- BoxCox(penumpang,lambda)
pbox <- penumpang.transform

plot(penumpang.transform, main="Transformasi Box-Cox" )
lines(ksmooth(time(pbox), pbox, "normal", bandwidth=1), lwd=2, col="red")

win.graph()
par(mfrow=c(2,1)) 
acf(pbox, na.action=na.pass)
pacf(pbox, na.action=na.pass)

dev.new()
p.diff.box <- diff(pbox, diff=1)
ts.plot(p.diff.box, col="blue", main="Time Series Plot")
lines(ksmooth(time(p.diff.box), p.diff.box, "normal", bandwidth=1), lwd=2, col="red")

win.graph()
par(mfrow=c(2,1))
acf(p.diff.box, na.action=na.pass)
pacf(p.diff.box, na.action=na.pass)

adf.test(p.diff.box)

############### Model ###########################
model1 <- Arima(pbox, order = c(1, 1, 0), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model1)

model2 <- Arima(pbox, order = c(0, 1, 1), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model2)

model3 <- Arima(pbox, order = c(1, 1, 1), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model3)

model4 <- Arima(pbox, order = c(2, 1, 0), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model4)

model5 <- Arima(pbox, order = c(0, 1, 2), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model5)

auto.arima(pbox)

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


######################################################################
######################################################################
##################### Transformasi  Log ##############################

plog <- log(penumpang)
plot(plog, main="Transformasi Akar" )
lines(ksmooth(time(plog), plog, "normal", bandwidth=1), lwd=2, col="red")

win.graph()
par(mfrow=c(2,1)) 
acf(plog, na.action=na.pass)
pacf(plog, na.action=na.pass)

dev.new()
p.diff.log <- diff(plog, diff=1)
ts.plot(p.diff.log, col="blue", main="Time Series Plot")
lines(ksmooth(time(p.diff.log), p.diff.log, "normal", bandwidth=1), lwd=2, col="red")

win.graph()
par(mfrow=c(2,1))
acf(p.diff.log, na.action=na.pass)
pacf(p.diff.log, na.action=na.pass)

adf.test(p.diff.log)

############### Model ###########################
auto.arima(plog)

model1 <- Arima(plog, order = c(1, 1, 0), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model1)

model2 <- Arima(plog, order = c(0, 1, 1), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model2)

model3 <- Arima(plog, order = c(1, 1, 1), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model3)

model4 <- Arima(plog, order = c(2, 1, 0), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model4)

model5 <- Arima(plog, order = c(0, 1, 2), 
	seasonal = list(order = c(0, 0, 0), period = 12), include.mean= F)
summary (model5)

############### Peramalan #######################
pred.data = predict(model1, n.ahead = 12)
pred.data.low = pred.data$pred-1.96*pred.data$se
pred.data.up = pred.data$pred+1.96*pred.data$se
pred.data=pred.data$pred

#trnasformasi balik
pred.data = exp(pred.data)
pred.data.low = exp(pred.data.low)
pred.data.up = exp(pred.data.up)

#Nilai hasil fitting
fit.data = fitted(model1)
fit.data = exp(fit.data)

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


