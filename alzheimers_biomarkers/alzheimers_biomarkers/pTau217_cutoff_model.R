---
title: "Abeta_ptau217_cutpoint (Example with Simulated Data)"
author: "Azadeh Golduzian"
output:
  html_document:
    toc: true
editor: visual
---

## Goal of This Analysis

The goal of this code is to explore the relationship between plasma pTau217 levels and amyloid-beta (Aβ) pathology, operationalized as the Aβ42/40 ratio. Specifically, we aim to:

- Assess how well pTau217 predicts Aβ positivity using logistic regression models.
- Determine optimal cutpoints for pTau217 that classify individuals as Aβ-positive or -negative.
- Evaluate model performance through internal validation (model fit) and external validation via jackknife and k-fold cross-validation ROC curves.
- Compare pTau217's predictive value against other biomarkers, including plasma pTau181 and CSF pTau181.
- Visualize model contrasts and ROC thresholds to support biomarker threshold selection for future clinical and research use.

> ⚠️ This example uses **simulated data** for demonstration only. No real patient information is included.

---

## Setup

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(tidyr)
library(randomForest)
library(erikmisc)
library(tibble)
library(mice)
library(missForest)
library(knitr)
library(gmodels)
library(vcd)
library(grid)
library(ggalluvial)
library(reshape2)
library(caret)
library(lattice)
library(ggplot2)
library(dplyr)
library(car)
library(broom)
library(emmeans)
library(pROC)
library(rlang)
library(patchwork)

Simulate Example Data

set.seed(2025)
example_data <- data.frame(
  fl_abeta_4240_ratio = runif(200, min = 0.04, max = 0.16),
  fl_plasma__ALL__ptau_217 = rnorm(200, mean = 0.5, sd = 0.12)
) %>%
  mutate(
    fl_abeta_positivity = factor(
      ifelse(fl_abeta_4240_ratio < 0.08, 1, 0),
      levels = c(0, 1)
    )
  )

Fit Logistic Regression Model
outcome <- "fl_abeta_positivity"
predictor <- "fl_plasma__ALL__ptau_217"

clean_data <- example_data %>%
  filter(!is.na(.data[[outcome]]),
         !is.na(.data[[predictor]]))

model <- glm(
  as.formula(paste(outcome, "~", predictor)),
  data = clean_data,
  family = binomial
)
summary(model)

ROC Curve and AUC

roc_res <- e_plot_roc(
  labels_true      = clean_data[[outcome]],
  pred_values_pos  = model$fitted.values,
  label_neg_pos    = c(0, 1),
  sw_plot          = TRUE
)

roc_plot <- roc_res$plot_roc +
  ggtitle("ROC Curve: pTau217 → Abeta Positivity")

roc_plot

Optimal Threshold

thresh <- roc_res$roc_curve_best$thresh
thresh


Contrast Plot
dat_cont <- data.frame(fl_plasma__ALL__ptau_217 = clean_data[[predictor]])
fit_glm_01 <- glm(
  cbind(as.numeric(as.character(clean_data[[outcome]])),
        1 - as.numeric(as.character(clean_data[[outcome]])))
  ~ fl_plasma__ALL__ptau_217,
  data = clean_data,
  family = binomial
)

contr_plot <- e_plot_model_contrasts(
  fit    = fit_glm_01,
  dat_cont = dat_cont
)$plots$fl_plasma__ALL__ptau_217 +
  geom_hline(yintercept = thresh, linetype = "dashed", color = "red") +
  ggtitle("Probability Curve with Cut-Point")

contr_plot

Scatter Plot of Labels vs pTau217
scatter_plot <- ggplot(clean_data, aes(
  x = fl_plasma__ALL__ptau_217,
  y = as.numeric(as.character(fl_abeta_positivity))
)) +
  geom_jitter(height = 0.02, alpha = 0.6) +
  geom_vline(xintercept = (log(thresh / (1 - thresh)) - coef(model)[1]) / coef(model)[2],
             linetype = "dashed", color = "blue") +
  labs(
    title = "Observed Positivity vs pTau217",
    x     = "pTau217",
    y     = "Abeta Positivity"
  ) +
  theme_minimal()

scatter_plot

Combine All Plots

combined <- roc_plot + contr_plot + scatter_plot + 
  plot_layout(ncol = 3)

combined





 


