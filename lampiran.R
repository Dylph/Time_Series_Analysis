acfStat <- function(x,lag=36)
{
	out1=acf(x,lag.max=lag,plot=F,na.action=na.pass)
	acfout=out1$acf
	out2=pacf(x,lag.max=lag,plot=F,na.action=na.pass)
	pacfout=NULL
	pacfout[1]=1
	pacfout=c(pacfout,out2$acf)
	temp1=NULL
	temp1[1]=NULL
	temp2=NULL
	temp2[1]=NULL
	for (i in 1:lag)
	{
		temp1[i+1]=Box.test(x,lag=i,type="Ljung")$statistic
		temp2[i+1]=Box.test(x,lag=i,type="Ljung")$p.value
	}
result=cbind(ACF = acfout, PACF = pacfout, "Q-Stats" = temp1, "P-Value" = temp2)
	rownames(result)= 0:lag
	# print(length(acfout))
	# print(length(pacfout))
	# print(length(temp1))
	# print(length(temp2))
	print(result)
}
 
arima.string <- function(object)
{
    order <- object$arma[c(1,6,2,3,7,4,5)]
    result <- paste("ARIMA(",order[1],",",order[2],",",order[3],")",sep="")
    if(order[7]>1 & sum(order[4:6]) > 0)
        result <- paste(result,"(",order[4],",",order[5],",",order[6],")[",order[7],"]",sep="")
    if(is.element("constant",names(object$coef)) | is.element("intercept",names(object$coef)))
        result <- paste(result,"with non-zero mean")
    else if(is.element("drift",names(object$coef)))
        result <- paste(result,"with drift        ")
    else if(order[2]==0 & order[5]==0)
        result <- paste(result,"with zero mean    ")
    else
        result <- paste(result,"                  ")
    return(result)
}

printstatarima <- function (x, digits = 4,se=T,...){
if (length(x$coef) > 0) {
        cat("\nCoefficients:\n")
        coef <- round(x$coef, digits = digits)
        if (se && nrow(x$var.coef)) {
            ses <- rep(0, length(coef))
            ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
            coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
            coef <- rbind(coef, s.e. = ses)
		statt <- coef[1,]/ses
            pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = F)
		coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
		coef <- t(coef)
        }
        print.default(coef, print.gap = 2)
    }
}
