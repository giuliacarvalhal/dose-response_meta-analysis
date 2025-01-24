# Set the working directory
setwd("C:/Users/giuli/Desktop/Ensenfentrine")

# Load the libraries
library(dosresmeta)
library(rms)
library(mvtnorm)
library(DoseFinding)
library(aod)
library(readxl)
library(mvmeta)


# Load dataset
ari <- read_excel("C:/Users/giuli/Desktop/Ensenfentrine/ari.xlsx", sheet = "PEAKFEV1LAST")

ari 

# Converter MD (y) e SD de litros para mililitros
ari$y <- ari$y * 1000
ari$sd <- ari$sd * 1000


# Auxiliary function to estimate target doses (to be implemented in dosresmeta pkg)
doseEff <- function(p, dose, Ep, trunc = FALSE){
  max_Ep <- max(Ep)
  EDmax <- dose[which.min(abs(Ep - max_Ep))]
  if (trunc == TRUE && EDmax == max(dose)) return(data.frame(p = NA, ED = NA, Ep = NA))      
  ED <- apply(matrix(p), 1, function(x)
    dose[which.min(abs(Ep[dose < EDmax] - x * max_Ep))])
  data.frame(p, ED, Ep = p * max_Ep)
}

## Obtaining mean differences, variances, and (co)varinace matrices for the all the studies
cov.md <- by(ari, ari$id, function(x) covar.smd(y, sd, n, "md", data = x))
ari$md <- unlist(lapply(cov.md, function(x) x$y))
ari$vmd <- unlist(lapply(cov.md, function(x) x$v))

# Define knots
knots <- quantile(ari$dose, c(.25, .5, .75))

## Dose-response meta-analysis
spl <- dosresmeta(formula = y ~ rcs(dose, knots), id = id, sd = sd, n = n, 
                  covariance = "md", data = ari)

## Summary of the model, print coefficients and covariance matrix
summary(spl)
round(coef(spl), 3)
round(vcov(spl), 2)


# Peak dose, CI, and mean difference
p <- 0.95 # Max effect
newdata <- data.frame(dose = seq(0, max(ari$dose), length.out = 5000))
edp <- with(predict(spl, newdata), 
            doseEff(p = p, dose = newdata$dose, Ep = pred, trunc = FALSE))

peak_dose <- edp$ED  # Peak dose

# Confidence interval for the peak dose using bootstrapping
set.seed(1234)
mvsample <- rmvnorm(10000, mean = coef(spl), sigma = vcov(spl))

# Calculate the dose-response curve for each bootstrap sample
results <- do.call("rbind", apply(mvsample, 1, function(y){
  pred <- rcs(newdata$dose, knots) %*% y
  doseEff(p = p, dose = newdata$dose, Ep = pred, trunc = FALSE)
}))

# Calculate the confidence intervals for the peak dose
ci_peak <- by(results, results$p, function(x) 
  round(quantile(x$ED, c(.025, .975), na.rm = TRUE), 2))

# Output the results
cat("Peak Dose:", edp$ED, "mg/day\n")
cat("95% Confidence Interval for Peak Dose:", ci_peak[[1]], "mg/day\n")

# Generate predictions over a range of doses
dose_range <- seq(0, max(ari$dose), length.out = 100)
newdata_range <- data.frame(dose = dose_range)
preds <- predict(spl, newdata = newdata_range)

# Interpolate the predicted mean difference at the exact peak dose
md_at_peak <- approx(dose_range, preds$pred, xout = peak_dose)$y
ci_md_lb <- approx(dose_range, preds$ci.lb, xout = peak_dose)$y
ci_md_ub <- approx(dose_range, preds$ci.ub, xout = peak_dose)$y

# Output the results
cat("Mean Difference at Peak Dose:", round(md_at_peak, 3), "L\n")
cat("95% Confidence Interval for MD at Peak Dose:", round(c(ci_md_lb, ci_md_ub), 3), "L\n")


## Figure 1
{
  
y_range <- range(edp$Ep)
y_mid <- mean(y_range)

# Define colors
fill_color <- rgb(136,202,252, maxColorValue=255)
dark_color <- rgb(120,180,225, maxColorValue=255)
shadow_color <- rgb(210,235,255, maxColorValue=255)
gray_line <- rgb(200,200,200, maxColorValue=255)

pdf("PEAKFEV1LAST_Dose-response.pdf", width = 8, height = 6)
par(mar = c(5, 4, 4, 4) + 1.5, mfrow = c(1, 1), las = 1, bty = "n")

newdata <- data.frame(dose = seq(0, max(ari$dose), length.out = 5000))
preds <- predict(spl, newdata)

# First, plot the base graph without adding the lines
matplot(newdata$dose, preds$pred, type = "n", 
        ylim = c(-20, 225), xlim = c(0, 13),
        xlab = "Ensifentrine (mg/day)", ylab = "MD of Peak FEV1 (ml): Final Assessment")

# Add shaded area between confidence interval lines
polygon(c(newdata$dose, rev(newdata$dose)), 
        c(preds$ci.lb, rev(preds$ci.ub)), 
        col = shadow_color, border = NA)

# Plot the lines for prediction and confidence intervals
matlines(newdata$dose, cbind(preds$pred, preds$ci.lb, preds$ci.ub), 
         lwd = c(1, 0.5, 0.5), lty = c(1, 2, 3), col = "black")

# Add gray horizontal lines across the plot
abline(h = seq(0, 225, by = 25), col = gray_line, lty = 2)

p <- seq(0, 1, .1)
edp <- with(predict(spl, newdata), {
  doseEff(p = p, dose = newdata$dose, Ep = preds$pred, trunc = FALSE)
})
edp <- cbind(edp, Eprel = edp$Ep / max(edp$Ep))

axis(side = 4, at = edp$Ep[seq(1, 11, 2)], labels = 100 * edp$Eprel[seq(1, 11, 2)],
     pos = 13, las = 1)

text(x = 14.5, y = y_mid, labels = "Relative Efficacy, %", srt = 90, xpd = TRUE)

w <- 1 / ari$vmd[ari$vmd != 0]
with(subset(ari, dose != 0), 
     points(dose, md, pch = 21, bg = fill_color, col = dark_color, cex = 2 * w / max(w), lwd = 0.7))

dev.off()
}


## Study-specific models (for Figure 2)
modi <- lapply(split(ari, ari$id), function(x)
  dosresmeta(formula = y ~ rcs(dose, knots),
             sd = sd, n = n, covariance = "md", data = x)
)

## Figure 2
{
  pdf("PEAKFEV1LAST_Dose-response-PerTrial.pdf", width = 10, height = 6)
  par(mfrow = c(2, 3), las = 1, bty = "n")
  mapply(function(d, m){
    newdata <- data.frame(dose = seq(0, max(d$dose), length.out = 100))
    with(predict(m, newdata), {
      matplot(newdata$dose, cbind(pred, ci.lb, ci.ub), type = "l", 
              ylim = c(-0.1, 0.5), xlim = c(0, 13), lty = c(1, 2, 3), col = "black",
              xlab = "Ensifentrine (mg/day)", ylab = "Mean Difference")
    })
    with(d[-1, ], errbar(dose, md, md + 1.96*sqrt(vmd), md - 1.96*sqrt(vmd), 
                         add = T, pch = 15, lty = 3, cap = .02))
    title(d$author[1])
  }, split(ari, ari$id), modi)
  dev.off()
  
  ## Tabular prediction
  newdata <- data.frame(dose = seq(0, 12, by = 1))
  pred_md <- predict(spl, newdata = newdata, xref = 0, expo = FALSE)
  round(pred_md, 2)
  
  ## Target doses with 'confidence interval'
  p <- c(.5, .95)
  newdata <- data.frame(dose = seq(0, max(ari$dose), length.out = 5000))
  edp <- with(predict(spl, newdata), 
              doseEff(p = p, dose = newdata$dose, Ep = pred, trunc = FALSE))
  round(edp, 2)
  
  set.seed(1234)
  mvsample <- rmvnorm(10000, mean = coef(spl), vcov(spl))
  results <- do.call("rbind", apply(mvsample, 1, function(y){
    pred <- rcs(newdata$dose, knots) %*% y
    doseEff(p = p, dose = newdata$dose, Ep = pred, trunc = F)
  }))
  by(results, results$p, function(x) 
    round(quantile(x$ED, c(.025, .975), na.rm = T), 2))
}

## -----------------------------------------------------------------------------
## Sensitivity analysis
{

## 1) Location of knots
knlist <- combn(quantile(ari$dose, c(.1, .25, .5, .75, .9)), 3, simplify = F)[-c(1, 10)]
#do.call(rbind, knlist)
modi_k <- lapply(knlist, function(k)
  dosresmeta(formula = y ~ rcs(dose, k),
             sd = sd, n = n, covariance = "md", data = ari)
)

pdf("PEAKFEV1LAST_Dose-response_Sensitivity.pdf", width = 15, height = 6)
par(mfrow = c(1, 2))
newdata <- data.frame(dose = seq(0, max(ari$dose), length.out = 500))
par(mar = c(5, 4, 4, 4) + 1.5, las = 1, bty = "n")
with(predict(spl, newdata), {
  plot(newdata$dose, pred, type = "l", col = "white", ylim = c(-0.05, 0.15), 
       xlim = c(0, 13), xlab = "Ensifentrine (mg/day)", ylab = "Mean Difference")
})
mapply(function(m, k){
  with(predict(m, newdata), lines(newdata$dose, pred, lty = k))
}, modi_k, 1:8)
legend(0, 21, lapply(knlist, function(k) 
  paste("knots = ", paste(k, collapse = ", "))), lty = 1:8, bty = "n")
axis(side = 4, at = edp$Ep[seq(1, 11, 2)], lab = 100*edp$Eprel[seq(1, 11, 2)], pos = 31)
mtext("Relative Efficacy, %                                   ",
      side = 4, line = 1, las = 3, padj =  -0.5)
dev.off()

}

## Predictions
newdata <- data.frame(dose = seq(0, max(ari$dose), length.out = 500))
predRCS <- predict(spl, newdata = newdata, xref = 0, expo = FALSE)

# If you want to round and view the predictions
#round(predRCS, 2)

## Graphical comparison
pdf("PEAKFEV1LAST_Dose-response_preds.pdf", width = 8, height = 6)
par(mar = c(5, 4, 4, 4) + 1.5, las = 1, bty = "n")
with(predict(spl, newdata), {
  plot(newdata$dose, pred, type = "l", #ylim = c(-0.2, 0.2), xlim = c(0, 13),
       xlab = "Ensifentrine (mg/day)", ylab = "Mean Difference")
})

# Add points from the original data
w <- 1 / ari$vmd[ari$vmd != 0]
with(subset(ari, dose != 0), points(dose, md, pch = 1, cex = 2 * w / max(w)))

# If desired, add a legend
legend("topright", legend = "Restricted Cubic Spline", lty = 1, bty = "n")
dev.off()


## Estimating ED50 and ED95 with confidence intervals

# Calculate the maximum predicted response
max_response <- max(predict(spl, newdata = newdata)$pred)

# For ED50 (median effective dose)
ed50 <- doseEff(p = 0.5, dose = newdata$dose, Ep = predict(spl, newdata = newdata)$pred)

# For ED95 (95% effective dose)
ed95 <- doseEff(p = 0.95, dose = newdata$dose, Ep = predict(spl, newdata = newdata)$pred)

# Bootstrapping to estimate confidence intervals for ED50 and ED95
set.seed(1234)
mvsample <- rmvnorm(10000, mean = coef(spl), sigma = vcov(spl))

results <- do.call("rbind", apply(mvsample, 1, function(y){
  pred <- rcs(newdata$dose, knots) %*% y
  doseEff(p = c(0.5, 0.95), dose = newdata$dose, Ep = pred)
}))

ci_ed50 <- round(quantile(results$ED[results$p == 0.5], c(.025, .975), na.rm = TRUE), 2)
ci_ed95 <- round(quantile(results$ED[results$p == 0.95], c(.025, .975), na.rm = TRUE), 2)

# Output the results for ED50 and ED95
#cat("ED50 (Median Effective Dose):", ed50$ED, "mg/day\n")
#cat("95% Confidence Interval for ED50:", ci_ed50, "mg/day\n")

cat("ED95 (95% Effective Dose):", ed95$ED, "mg/day\n")
cat("95% Confidence Interval for ED95:", ci_ed95, "mg/day\n")

## Table 4
coefi <- coef(spl)
vcovi <- vcov(spl)

# Create the data frame
tab4 <- data.frame(
  id = "Restricted cubic splines", 
  theta1 = round(coefi, 4),  # Round the numeric values
  v11 = round(diag(vcovi), 4)  # Round the numeric values
)

# View the final table
print(tab4)

