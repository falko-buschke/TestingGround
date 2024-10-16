OA <- c(17674,16774,16826,15840,14219,12561,11427,10074,9130,8235,7298,6946,6084,5900,5347,4909,4504,3786,3320,2954,2631,2321,2123)

paywall <- c(8163,9373,10956,11203,12787,11815,11979,12341,12503,12387,12417,12690,13389,13718,13792,12799,13649,11797,10918,10083,11237,10171,10556)

year <- 2023:2001

png("Journal growth.png", width=16,height=14, units="cm", res=300)
par(mai=c(0.85,0.95,0.05,0.05))

plot(year,OA, col="orange", pch=16, ylab="Number of papers", xlab="Year",las=1, xlim=c(2000,2025),cex.axis=1.3, cex.lab= 1.5, mgp=c(3.3,0.6,0))
points(year,paywall, col="blue",pch=16)

mod <- nls(OA~I(a*exp(year*b)), start = list(a = 200, b = 0.1),control = nls.control(maxiter = 1000))
yrs <- seq(2000,2023,l=1000)
OA.p <- coef(mod)[1]*exp(yrs*coef(mod)[2])
lines(yrs,OA.p,col="orange")

mod.2 <- (lm(paywall~year + I(year^2)))
pw.p <-  coef(mod.2)[1] + yrs*coef(mod.2)[2] + coef(mod.2)[3]*yrs^2
lines(yrs,pw.p,col="blue")

legend ("topleft", c("Open access", "Paywall"), col=c("orange","blue"), pch=16, lty=1)
text(2018,2000, font=2, col="grey", "Web of Science Category: Ecology")
dev.off()
