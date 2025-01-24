# Set the working directory
setwd("C:/Users/giuli/Desktop/Ensenfentrine")

# Load the libraries
library(meta)
library(metafor)
library(readr)
library(grid)  # Ensure the grid package is loaded
library(tidyverse)
library(writexl)
library(mvmeta)
library(readxl)
library(ggplot2)
library(grid)  # Ensure the grid package is loaded
library(ggplot2)
library(grid)

fill_color <- rgb(136,202,252, maxColorValue=255)
shadow_color <- rgb(210,235,255, maxColorValue=255)


# Load and prepare the dataset
sheet_names <- excel_sheets("IMMA_.xlsx")
ma <- lapply(sheet_names, function(x) {
  as.data.frame(read_excel("IMMA_.xlsx", sheet = x))
})
names(ma) <- sheet_names

# Converter MD e SD de litros para mililitros (apenas uma vez)
ma$PEAKFEV1DAY1$mean.e <- ma$PEAKFEV1DAY1$mean.e * 1000
ma$PEAKFEV1DAY1$mean.c <- ma$PEAKFEV1DAY1$mean.c * 1000
ma$PEAKFEV1DAY1$sd.e <- ma$PEAKFEV1DAY1$sd.e * 1000
ma$PEAKFEV1DAY1$sd.c <- ma$PEAKFEV1DAY1$sd.c * 1000

# Meta-analysis for continuous outcomes 
fill_color <- rgb(136,202,252, maxColorValue=255)

{
  object <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, 
                     data = ma$PEAKFEV1DAY1, 
                     method.tau = "REML",
                     sm = "MD",
                     studlab = study)
  
  print (object)
  
  summary(object)
  
  # Forest plot for continuous outcome data with manually added labels
  tiff("PEAKFEV1DAY1_forest_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  forest(object,
         label.c = "Placebo",
         label.e = "Ensifentrine",
         layout = "Revman",
         leftcols = c("studlab", "mean.e", "sd.e", "n.e", 
                      "mean.c", "sd.c", "n.c", "w.random", "effect", "ci"),
         leftlabs = c("Studies", "Mean Δ", "SD", "Total", 
                      "Mean Δ", "SD", "Total", 
                      "Weight", "MD", "95% CI"),
         random = TRUE,
         common = FALSE,
         pooled.events = TRUE,
         test.overall.random = TRUE,
         overall.hetstat = TRUE,
         digits = 2,
         digits.pval = 2,
         digits.sd = 2,
         col.square = fill_color,
         col.square.lines = fill_color,
         col.diamond = "black",
         col.diamond.lines = "black",
         diamond.random = TRUE,
         fs.heading = 12,
         colgap.forest = "5mm",
         label.left = "Favors Ensifentrine",
         label.right = "Favors Placebo",
         print.pval.Q = TRUE,
         print.tau.ci = TRUE,
         xlim = c(-3, 3) # Adjusts the x-axis limits
  )
  
    dev.off()
  
}



### Leave-one-out sensitivity analysis for continuous outcomes
{
  l1o_object <- metainf(object, pooled = "random")
  
  
  tiff("PEAKFEV1DAY1_leave_one_out_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  forest(l1o_object,
         col.square = "#E69F00",
         col.diamond = "gray",
        xlim = c(-3, 3),
         xlab = "Favors Ensifentrine            Favors Placebo",
         rightcols = c("effect", "ci", "I2"))
  dev.off()
}

# 1st step: Run the "baujat()" function and assign to a new object
{
  
  # 2nd step: Create Baujat plot
  tiff("PEAKFEV1DAY1_Baujat_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  baujat_object <- baujat(object,
                          bg="#E69F00")
  dev.off()
}

# Subgroup Meta-analysis for continuous outcomes
{
  # Step 1: Convert the Design variable to a factor with specified levels
  ma$PEAKFEV1DAY1$Design <- factor(ma$PEAKFEV1DAY1$Design, 
                                   levels = c("Parallel", "Crossover")) # Specify your desired order
  
  object <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c,
                     data = ma$PEAKFEV1DAY1,
                     method.tau = "REML",
                     sm = "MD",
                     studlab = study,
                     subgroup = Design)
  
  summary(object)
  

  tiff("Design_PEAKFEV1DAY1_forest_plot.tiff", width = 12, height = 8,units = "in", res = 300)
  forest(object,
         layout = "Revman",
         sortvar = factor(object$subgroup, levels = c("Parallel", "Crossover")), # Sort by factor levels
         leftcols = c("studlab", "mean.e", "sd.e", "n.e", 
                      "mean.c", "sd.c", "n.c", "w.random", "effect", "ci"),
         leftlabs = c("Studies", "Mean Δ", "SD", "Total", 
                      "Mean Δ", "SD", "Total", 
                      "Weight", "MD", "95% CI"),
         rightcols = FALSE,
         just.addcols = "right",
         random = TRUE,
         common = FALSE,
         pooled.events = TRUE,
         pooled.totals = TRUE,
         test.overall.random = TRUE,
         overall.hetstat = TRUE,
         digits = 2,
         digits.pval = 2,
         digits.sd = 2,
         col.square = fill_color,
         col.square.lines = fill_color,
         col.diamond = "black",
         col.diamond.lines = "black",
         diamond.random = TRUE,
         lab.e = "Ensifentrine",
         lab.c = "Placebo",
         fs.heading = 12,
         colgap.forest = "5mm",
         label.left = "Favors Placebo",
         label.right = "Favors Ensifentrine",
         subgroup = TRUE,
         print.subgroup.labels = TRUE,
         col.subgroup = "black",
         subgroup.name = "Design", # Replace with actual subgroup name if needed
         print.subgroup.name = FALSE,
         print.pval.Q = TRUE,
         print.tau.ci = TRUE,
         bysort = FALSE,
         test.effect.subgroup.random = TRUE,
        # subgroup.order = c("Parallel", "Crossover"),
         xlim = c(-300, 300) # Adjusts the x-axis limits
         )
  dev.off()
}


# Meta-regression PLOT
{
  # Select a specific sheet (e.g., the first one)
  selected_data <- ma[["PEAKFEV1DAY1"]]
  meta_data <- selected_data %>%
    mutate(
      yi = mean.i - mean.c,  # Mean difference
      vi = (sd.i^2 / n.i) + (sd.c^2 / n.c)  # Variance
    )
  
  # Meta-regression with follow-up as a moderator
  meta_reg <- rma(yi = yi, vi = vi, mods = ~ follow_up, data = meta_data)
  
  # Summary of meta-regression
  meta_reg_summary <- summary(meta_reg)
  
  # Extract relevant data into a dataframe
  regression_table <- data.frame(
    Coefficient = meta_reg_summary$beta,
    SE = meta_reg_summary$se,
    Z_value = meta_reg_summary$zval,
    p_value = meta_reg_summary$pval,
    CI_lower = meta_reg_summary$ci.lb,
    CI_upper = meta_reg_summary$ci.ub
  )
  
  # Save the summary to a CSV file
  write.csv(regression_table, file = "PEAKFEV1DAY1_meta_regression_summary.csv", row.names = FALSE)
  
  # Predict values across the range of follow-up times
  pred <- predict(meta_reg, newmods = seq(min(meta_data$follow_up), max(meta_data$follow_up), length = 100))
  
  # Define the output PDF file
  pdf(file = "PEAKFEV1DAY1_Meta_regression_plot.pdf", width = 14, height = 8)
  
  par(cex.axis = 1.5,  # Increase size of axis labels
      cex.lab = 1.8,   # Increase size of axis titles (x and y labels)
      cex.main = 2,
      mar = c(5, 6, 4, 2) + 0.1)  # Increase left margin (second number in `mar`)
      # Increase size of the main title
  
  # Create the plot
  plot(meta_data$follow_up, meta_data$yi, 
       xlab = "Follow Up (weeks)", 
       ylab = "Mean Difference", 
       pch = 16, 
       col = fill_color,
       cex = 2,  # Increase size of the points (circles)
       ylim = c(min(pred$ci.lb), max(pred$ci.ub)))
  
  # Add the predicted line with increased width
  lines(seq(min(meta_data$follow_up), max(meta_data$follow_up), length = 100), pred$pred, col = fill_color, lwd = 4)
  
  # Add gray horizontal lines across the plot
  abline(h = seq(0, 80, by = 20), col = "gray", lty = 2)
  
  # Add 95% confidence intervals with increased width and shadow effect
  polygon(c(seq(min(meta_data$follow_up), max(meta_data$follow_up), length = 100), 
            rev(seq(min(meta_data$follow_up), max(meta_data$follow_up), length = 100))),
          c(pred$ci.lb, rev(pred$ci.ub)), 
          col = adjustcolor(shadow_color, alpha.f = 0.3), border = NA)
  
  # Close the PDF device
  dev.off()
}



# Outcome to check for Binary MA is  ####
# Meta-analysis for binary outcomes
{
  # Step 1: Convert the Design variable to a factor with specified levels
  ma$SAE$Design <- factor(ma$SAE$Design, 
                                   levels = c("Parallel", "Crossover")) # Specify your desired order
  
  object <- metabin(e.t, n.t, e.c, n.c,
                    data = ma$SAE,
                    method = "MH",
                    method.tau = "DL",
                    sm = "RR",
                    hakn = FALSE,
                    studlab = study,
                    subgroup = Design)
  
  summary(object)
  
  # Forest Plot
  tiff("AE_forest_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  forest(object,
         pooled.events = TRUE,
         sortvar = factor(object$subgroup, levels = c("Parallel", "Crossover")), # Sort by factor levels
         label.e = "Ensifentrine",
         label.c = "Placebo",
         fixed = FALSE,
         random = TRUE,
         comb.fixed = FALSE,
         test.overall = TRUE,
         digits = 3,
         digits.pval = 3,
         print.Q = TRUE,
         print.pval.Q = TRUE,
         print.tau.ci = TRUE,
         layout = "Revman",
         col.square = fill_color,
         subgroup = TRUE,
         print.subgroup.labels = TRUE,
         col.subgroup = "black",
         subgroup.name = "Design", # Replace with actual subgroup name if needed
         print.subgroup.name = FALSE,
         col.square.lines = fill_color,
         col.diamond.random = "black",
         leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c",
                      "effect", "ci", "w.random"),
         leftlabs = c("Studies", "Events", "Total", "Events", "Total",
                      "RR", "IC 95%", "Weight"),
         label.left = "Favors Placebo",
         col.label.right = "black",
         label.right = "Favors Ensifentrine",
         col.label.left = "black",
         subgroup.order = c("Parallel", "Crossover"),
         xlim = c(0.2, 4) )
  dev.off()
}

### Leave-one-out sensitivity analysis ###
{
  # 1st step: Run the "leave1out()" function and assign to a new object
  l1o_object <- metainf(object, pooled = "random")
  
  # 2nd step: Create leave-one-out plot
  tiff("AE_leave_one_out_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  forest(l1o_object,
         col.square="#E69F00",
         col.diamond="gray",
         xlim=c(0.2,),
         xlab="Favors Placebo            Favors Ensifentrine",
         rightcols = c("effect", "ci", "I2")) #para manter a medida de associação, o IC e o I2 nas colunas a direita do forest plot
  dev.off()
  
  
}

# 1st step: Run the "baujat()" function and assign to a new object
{
  # 2nd step: Create Baujat plot
  tiff("AE_Baujat_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  baujat_object <- baujat(object, pos.studlab = 4,
                          bg="#E69F00")
  dev.off()
}
