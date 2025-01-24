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
library(tidyverse)
library(lmtest)
library(gridExtra)
library(knitr)
theme_set(theme_minimal())

# Load dataset
ari <- read_excel("C:/Users/giuli/Desktop/Ensenfentrine/ari_one_stage.xlsx", sheet = "MORNING")

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

# Step 3: Create Slist for use in meta-analysis
Slist <- lapply(cov.md, function(x) x$cov)

# Step 4: Assign names to the Slist elements corresponding to the study IDs
names(Slist) <- unique(ari$id)

# data for plot and prediction
newd <- data.frame(dose = seq(0, 12, length.out = 100))
newd_tab <- data.frame(dose = c(0, 2, 4, 6, 8, 12))


# Splines without exclusion
spl <- dosresmeta(formula = y ~ rcs(dose, knots), id = id, sd = sd, n = n, 
                  covariance = "md", data = ari, Slist = Slist, proc = "1stage", method = "reml")
summary(spl)

# Splines with exclusion
spl_exc <- dosresmeta(formula = y ~ rcs(dose, knots), id = id, sd = sd, n = n, 
                      covariance = "md", data = subset(ari, !(id %in% c(7, 8))), method = "reml",
                      Slist = Slist[!names(Slist) %in% c("7","8" )])
summary(spl_exc)

# Spline without unpublished data
spl_exc2 <- dosresmeta(formula = y ~ rcs(dose, knots),id = id, sd = sd, n = n, covariance = "md",
                        data = subset(ari, !(id %in% c(2, 3, 5,6 ))), proc = "1stage", method = "reml",
                        Slist = Slist[!names(Slist) %in% c("2", "3", "5", "6")]
)
summary(spl_exc2)

# Spline without unpublished data and at least 28 days of follow-up
spl_exc3 <- dosresmeta(formula = y ~ rcs(dose, knots),id = id, sd = sd, n = n, covariance = "md",
                       data = subset(ari, !(id %in% c(2, 3, 5, 6, 9))), proc = "1stage", method = "reml",
                       Slist = Slist[!names(Slist) %in% c("2", "3", "5", "6", "9")]
)
summary(spl_exc3)


pred_spl <- predict(spl, newd)
pred_spl_excl <- predict(spl_exc, newd)
pred_spl_exc2 <- predict(spl_exc2, newd)
#pred_spl_exc3 <- predict(spl_exc3, newd)


# Add a model identifier to each prediction data frame
pred_spl$model <- "One-stage"
pred_spl_excl$model <- "Two-stage"
pred_spl_exc2$model <- "Peer-reviewed"
#pred_spl_exc3$model <- "Peer-reviewed & follow-up >= 28 days"

# Combine all predictions
all_preds <- bind_rows(
  pred_spl %>% mutate(dose = newd$dose),
  pred_spl_excl %>% mutate(dose = newd$dose),
  pred_spl_exc2 %>% mutate(dose = newd$dose),
  #pred_spl_exc3 %>% mutate(dose = newd$dose)
  
)

# Define colors and linetypes for each model
model_colors <- c(
  "One-stage" = "#C5C6C7",            # Blue
  "Two-stage" = "#C5C6C7", # Orange
  "Peer-reviewed" = "#0072B2"
  )

model_linetypes <- c(
  "One-stage" = "dashed",
  "Two-stage" = "dashed",
  "Peer-reviewed" = "solid"
)

# Plot
p <- ggplot(all_preds, aes(x = dose, y = pred, color = model, linetype = model)) +
  geom_line(size = 1) +
  geom_ribbon(
    aes(ymin = ci.lb, ymax = ci.ub, fill = model),
    alpha = 0.2,
    color = NA
  ) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  scale_linetype_manual(values = model_linetypes) +
  geom_hline(
    yintercept = seq(-25, 125, by = 25),
    color = "gray",
    linetype = "dashed"
  ) +
  labs(
    x = "Ensifentrine (mg/day)",
    y = "Mean Difference",
    color = "Model",
    linetype = "Model",
    fill = "Model"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold", size = rel(1.2)),
    axis.text = element_text(face = "bold", size = rel(1)),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = rel(1)),
    legend.text = element_text(size = rel(0.8)),
    plot.margin = margin(20, 20, 20, 20),
    axis.text.y = element_text(margin = margin(r = 5))
  )

# Save the plot as a PDF
ggsave(
  "NEW_MORNING_DOSE-RESPONSE_ONE-STAGE_Four_Models.pdf",
  plot = p,
  device = "pdf",
  width = 8,
  height = 6
)



# comparison predicted RRS
round(predict(spl, newd_tab), 2)
round(predict(spl_exc, newd_tab), 2)


# comparison coefficients
round(rbind(spl = c(coef(spl), vcov(spl)[-2]),
            spl_exc = c(coef(spl_exc), vcov(spl_exc)[-2])), 5)

# Figure 5
# Figure 5: Create the plot
{
pred_spl <- predict(spl, newd)
pred_spl_excl <- predict(spl_exc, newd)

print(head(pred_spl))

y_range <- range(edp$Ep)
y_mid <- mean(y_range)
# Define colors
fill_color <- rgb(136,202,252, maxColorValue=255)
shadow_color <- rgb(210,235,255, maxColorValue=255)
gray_line <- rgb(200,200,200, maxColorValue=255)


p <- ggplot(pred_spl, aes(newd$dose, pred, linetype = "One-stage")) + 
  geom_line(color = "#0072B2") +
  geom_line(data = pred_spl_excl, aes(linetype = "Two-stage"), color = "#D55E00") +
  geom_ribbon(data = pred_spl, aes(ymin = ci.lb, ymax = ci.ub), 
              fill = "#56B4E9", alpha=0.2) +  # Confidence interval shading
  geom_ribbon(data = pred_spl_excl, aes(ymin = ci.lb, ymax = ci.ub), 
              fill = "#E69F00",alpha=0.2) +  # Confidence interval shading for excluded model
  geom_hline(yintercept = seq(0, 225, by = 25), color = gray_line, linetype = "dashed") +  # Add dashed horizontal lines
  
   #scale_x_continuous(limits = c(0, 12)) +  # Adjust x-axis limits
  scale_y_continuous( breaks = seq(0, 225, by = 25),) +  # Adjust y-axis limits
  labs(x = "Ensifentrine (mg/day)", y = "Mean Difference", linetype = "Curve") +
  scale_linetype_manual(values = c(`One-stage` = "solid", `Two-stage` = "dashed")) +
  # Add gray horizontal lines across the plot


  theme_classic()
  theme(
  axis.title = element_text(face = "bold", size= rel(3), margin = margin(t = 10, r = 10, b = 10, l = 10)),
  axis.text = element_text(face = "bold", rel(3)),  # Ensure numbers are visible
  legend.position = "top",
  legend.title = element_text(face = "bold", rel(3)),
  plot.margin = margin(20, 20, 20, 20),  # Increased margins around the plot
  axis.text.y = element_text(margin = margin(r = 5), rel(3)),  # Add margin beside y-axis text
   )

# Save the plot as a PDF
ggsave("MORNING_DOSE-RESPONSE_ONE-STAGE.pdf", plot = p, device = "pdf", width = 8, height = 6)
}

