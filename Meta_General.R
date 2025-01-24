# Set the working directory
setwd("C:/Users/pathway")

# Load the libraries
library(meta)
library(metafor)
library(readr)
library(tidyverse)
library(writexl)
library(mvmeta)
library(readxl)
library(ggplot2)
library(grid) 

# Color coding
fill_color <- rgb(136,202,252, maxColorValue=255)
shadow_color <- rgb(210,235,255, maxColorValue=255)

# Load and prepare the dataset
sheet_names <- excel_sheets("sheet.xlsx")
ma <- lapply(sheet_names, function(x) {
  as.data.frame(read_excel("sheet.xlsx", sheet = x))
})
names(ma) <- sheet_names

# Meta-analysis for continuous outcomes 
fill_color <- rgb(136,202,252, maxColorValue=255)
{
  object <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, 
                     data = ma$sheet_tab, 
                     method.tau = "REML",
                     sm = "MD",
                     studlab = study)
  summary(object)
  
  # Forest plot for continuous outcome 
  tiff("forest_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  forest(object,
         label.c = "Placebo",
         label.e = "Intervention",
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
         label.left = "Favors Intervention",
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
  
  
  tiff("leave_one_out_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  forest(l1o_object,
         col.square = "#E69F00",
         col.diamond = "gray",
        xlim = c(-3, 3),
         xlab = "Favors Intervention            Favors Placebo",
         rightcols = c("effect", "ci", "I2"))
  dev.off()
}

# 1st step: Run the "baujat()" function and assign to a new object
{
  
  # 2nd step: Create Baujat plot
  tiff("Baujat_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  baujat_object <- baujat(object,
                          bg="#E69F00")
  dev.off()
}

# Subgroup Meta-analysis for continuous outcomes
{

  object <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c,
                     data = ma$sheet_tab,
                     method.tau = "REML",
                     sm = "MD",
                     studlab = study,
                     subgroup = Design)
  
  summary(object)
  

  tiff("Subgroup_forest_plot.tiff", width = 12, height = 8,units = "in", res = 300)
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
         lab.e = "Intervention",
         lab.c = "Placebo",
         fs.heading = 12,
         colgap.forest = "5mm",
         label.left = "Favors Placebo",
         label.right = "Favors Intervention",
         subgroup = TRUE,
         print.subgroup.labels = TRUE,
         col.subgroup = "black",
         subgroup.name = "Design", # Replace with actual subgroup name if needed
         print.subgroup.name = FALSE,
         print.pval.Q = TRUE,
         print.tau.ci = TRUE,
         bysort = FALSE,
         test.effect.subgroup.random = TRUE,
         xlim = c(-300, 300) # Adjusts the x-axis limits
         )
  dev.off()
}


# Meta-analysis for binary outcomes
{
  object <- metabin(e.t, n.t, e.c, n.c,
                    data = ma$sheet_tab,
                    method = "MH",
                    method.tau = "DL",
                    sm = "RR",
                    hakn = FALSE,
                    studlab = study,
                    subgroup = Design)
  
  summary(object)
  
  # Forest Plot
  tiff("binary_forest_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  forest(object,
         pooled.events = TRUE,
         sortvar = factor(object$subgroup, levels = c("Parallel", "Crossover")), # Sort by factor levels
         label.e = "Intervention",
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
         label.right = "Favors Intervention",
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
  tiff("binary_leave_one_out_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  forest(l1o_object,
         col.square="#E69F00",
         col.diamond="gray",
         xlim=c(0.2,),
         xlab="Favors Placebo            Favors Intervention",
         rightcols = c("effect", "ci", "I2")) 
  dev.off()
    
}

# 1st step: Run the "baujat()" function and assign to a new object
{
  # 2nd step: Create Baujat plot
  tiff("sheet_tab_Baujat_plot.tiff", width = 12, height = 8, units = "in", res = 300)
  baujat_object <- baujat(object, pos.studlab = 4,
                          bg="#E69F00")
  dev.off()
}
