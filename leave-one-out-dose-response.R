# Set the working directory
setwd("C:/Users/giuli/Desktop/Ensenfentrine")

# Load the librdata_setes
library(dosresmeta)
library(rms)
library(mvtnorm)
library(DoseFinding)
library(aod)
library(readxl)
library(mvmeta)
library(tidyverse)
library(scales)
library(directlabels)

# Load dataset
data_set <- read_excel("C:/Users/pathway/data_set.xlsx", sheet = "outcome")

# Auxiliary function to estimate target doses (to be implemented in dosresmeta pkg)
doseEff <- function(p, dose, Ep, trunc = FALSE){
  max_Ep <- max(Ep)
  EDmax <- dose[which.min(abs(Ep - max_Ep))]
  if (trunc == TRUE && EDmax == max(dose)) return(data.frame(p = NA, ED = NA, Ep = NA))      
  ED <- apply(matrix(p), 1, function(x)
    dose[which.min(abs(Ep[dose < EDmax] - x * max_Ep))])
  data.frame(p, ED, Ep = p * max_Ep)
}

## Obtaining mean differences, vdata_setances, and (co)vdata_setance matrices for all the studies
cov.md <- by(data_set, data_set$id, function(x) covar.smd(y, sd, n, "md", data = x))
data_set$md <- unlist(lapply(cov.md, function(x) x$y))
data_set$vmd <- unlist(lapply(cov.md, function(x) x$v))

# Define knots
knots <- quantile(data_set$dose, c(.25, .5, .75))

## Dose-response meta-analysis
spl <- dosresmeta(formula = y ~ rcs(dose, knots), id = id, sd = sd, n = n, 
                  covdata_setance = "md", data = data_set)

## Summary of the model, print coefficients and covdata_setance matrix
summary(spl)
round(coef(spl), 3)
round(vcov(spl), 2)

# Peak dose, CI, and mean difference
p <- 0.95 # Max effect
newdata <- data.frame(dose = seq(0, max(data_set$dose), length.out = 5000))
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
dose_range <- seq(0, max(data_set$dose), length.out = 100)
newdata_range <- data.frame(dose = dose_range)
preds <- predict(spl, newdata = newdata_range)

# Interpolate the predicted mean difference at the exact peak dose
md_at_peak <- approx(dose_range, preds$pred, xout = peak_dose)$y
ci_md_lb <- approx(dose_range, preds$ci.lb, xout = peak_dose)$y
ci_md_ub <- approx(dose_range, preds$ci.ub, xout = peak_dose)$y

# Output the results
cat("Mean Difference at Peak Dose:", round(md_at_peak, 3), "L\n")
cat("95% Confidence Interval for MD at Peak Dose:", round(c(ci_md_lb, ci_md_ub), 3), "L\n")

# Perform leave-one-out dose-response meta-analysis
spl_l1o <- lapply(unique(data_set$id), function(i)
  dosresmeta(formula = y ~ rcs(dose, knots), id = id, sd = sd, n = n, 
             covdata_setance = "md", data = subset(data_set, id != i))
)

# Graphical presentation of leave-one-out analysis with customizations
# Load the necessary library for color palettes
{
library(RColorBrewer)

# Define the colors for each study
study_names <- unique(data_set$author)  # Assuming 'author' contains the study names
colors <- c("#9b6a00","#efbc4d","#E69F00", "#56B4E9","#89c4e7", "#3a799e")
names(colors) <- study_names
  
# Define dose range from 0 to 12 mg/day
dose_range <- seq(0, 12, length.out = 100)
newd <- data.frame(dose = dose_range)

# Create the plot with customizations
p <- newd %>% 
  cbind(do.call("cbind", lapply(spl_l1o, function(m) predict(m, newdata = .)$pred))) %>%
  gather(study, pred, -dose) %>%
  mutate(study = factor(study, levels = 1:length(unique(data_set$id)), labels = unique(data_set$author))) %>% 
  ggplot(aes(dose, pred, col = study, label = study)) +
  geom_line(linewidth = 1) +  # Increase line width
  scale_colour_manual(values = colors) +  # Apply custom color palette
  geom_dl(aes(label = study), method = list(dl.trans(x = x + .5), 'last.qp')) +
  geom_hline(yintercept = seq(0, 120, by =20), linetype = "dashed", color = "gray") +  # Add dashed lines for MD
  labs(x = "Drug (mg/day)", y = "Mean Difference", col = "Excluded study") +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold", size = 14, margin = margin(t = 10, r = 10, b = 10, l = 10)),
    axis.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(20, 20, 20, 20),
    axis.text.y = element_text(margin = margin(r = 5)),
    legend.text = element_text(size = 10)  # Adjust legend text size
  )

# Save the plot using ggsave()
ggsave("outcome_leave_one_out_analysis.tiff", plot = p, width = 8, height = 6, units = "in", dpi = 300)


}
