# generate_plots.R
# Generates high-resolution, publication-quality diagnostic figures for README documentation

suppressPackageStartupMessages({
  library(ISLR2)
  library(DescTools)
  library(car)
  library(mctest)
})

if (!dir.exists("figures")) {
  dir.create("figures")
}

Dataset <- Boston

# Models definition
models <- list(
  "crim + dis + rm + rad" = lm(medv ~ crim + dis + rm + rad, data = Dataset),
  "crim + dis + rm"       = lm(medv ~ crim + dis + rm, data = Dataset),
  "dis + rm + rad"        = lm(medv ~ dis + rm + rad, data = Dataset),
  "rm + rad + crim"       = lm(medv ~ rm + rad + crim, data = Dataset),
  "rad + crim + dis"      = lm(medv ~ rad + crim + dis, data = Dataset),
  "crim + dis"            = lm(medv ~ crim + dis, data = Dataset),
  "crim + rm"             = lm(medv ~ crim + rm, data = Dataset),
  "crim + rad"            = lm(medv ~ crim + rad, data = Dataset),
  "dis + rm"              = lm(medv ~ dis + rm, data = Dataset),
  "dis + rad"             = lm(medv ~ dis + rad, data = Dataset),
  "rm + rad"              = lm(medv ~ rm + rad, data = Dataset),
  "crim"                  = lm(medv ~ crim, data = Dataset),
  "dis"                   = lm(medv ~ dis, data = Dataset),
  "rm"                    = lm(medv ~ rm, data = Dataset),
  "rad"                   = lm(medv ~ rad, data = Dataset)
)

adj_r2 <- sapply(models, function(m) summary(m)$adj.r.squared)

# Sort models by Adjusted R-squared in ascending order for clean horizontal barplot
sorted_idx <- order(adj_r2)
sorted_adj_r2 <- adj_r2[sorted_idx]
sorted_names <- names(sorted_adj_r2)

# ==============================================================================
# 1. FIX: Model Comparison Horizontal Barplot (Spacious margins, perfect labels)
# ==============================================================================
png("figures/model_comparison.png", width = 1150, height = 750, res = 135)
par(mar = c(5, 12, 4, 4), bg = "#fcfcfc")

bar_colors <- ifelse(sorted_names == "rm + rad + crim", "#27ae60", "#2980b9")

bp <- barplot(
  sorted_adj_r2,
  horiz = TRUE,
  names.arg = sorted_names,
  las = 1,
  col = bar_colors,
  border = NA,
  xlim = c(0, 0.85),
  xlab = "Adjusted R-squared",
  main = "Model Selection: Adjusted R-squared Comparison (All 15 Subsets)",
  cex.names = 0.85,
  cex.axis = 0.85,
  font.axis = 2,
  font.lab = 2,
  col.axis = "#2c3e50",
  col.lab = "#2c3e50"
)

# Subtle background grid
grid(nx = NULL, ny = NA, col = "#e2e8f0", lty = "dotted")

# Re-draw bars over grid
barplot(
  sorted_adj_r2,
  horiz = TRUE,
  add = TRUE,
  col = bar_colors,
  border = NA,
  axes = FALSE,
  names.arg = rep("", length(sorted_adj_r2))
)

# Text labels with exact values placed clearly outside bars
for (i in 1:length(sorted_adj_r2)) {
  val <- sorted_adj_r2[i]
  is_best <- (sorted_names[i] == "rm + rad + crim")
  label_txt <- if (is_best) sprintf("%.4f  (Optimal: lm.fit4)", val) else sprintf("%.4f", val)
  text(
    x = val + 0.015,
    y = bp[i],
    labels = label_txt,
    pos = 4,
    cex = if (is_best) 0.85 else 0.78,
    font = if (is_best) 2 else 1,
    col = if (is_best) "#1e824c" else "#2c3e50"
  )
}

# Legend in bottom right corner
legend(
  "bottomright",
  legend = c("Selected Optimal Model (lm.fit4)", "Candidate Specifications"),
  fill = c("#27ae60", "#2980b9"),
  border = NA,
  bty = "o",
  box.col = "#dcdde1",
  bg = "#ffffff",
  cex = 0.85,
  inset = c(0.04, 0.05)
)
dev.off()


# ==============================================================================
# 2. FIX: Cook's Distance Plot (Clear lines, non-merging needles, clean leader lines)
# ==============================================================================
best_fit <- models[["rm + rad + crim"]]
n <- nrow(Dataset)
p <- length(coef(best_fit))

cooksd <- cooks.distance(best_fit)
threshold_cooks <- 4 / (n - p)

png("figures/cooks_distance.png", width = 1150, height = 650, res = 135)
par(mar = c(5, 5, 4, 3), bg = "#fcfcfc")

# Set upper limit with comfortable headroom for labels
ylim_max <- max(cooksd) * 1.35

plot(
  1:n, cooksd,
  type = "n",
  ylim = c(0, ylim_max),
  xlim = c(1, n + 15),
  main = "Diagnostic 1: Cook's Distance (Influential Observations)",
  ylab = "Cook's Distance (Di)",
  xlab = "Observation Index (1 to 506)",
  font.axis = 2,
  font.lab = 2,
  col.axis = "#2c3e50",
  col.lab = "#2c3e50"
)

# Background grid
grid(nx = NULL, ny = NULL, col = "#edf2f7", lty = "dotted")

# Draw thin, elegant needle lines that do not merge into a blob
segments(
  x0 = 1:n, y0 = 0,
  x1 = 1:n, y1 = cooksd,
  col = ifelse(cooksd > threshold_cooks, "#fca5a5", "#cbd5e1"),
  lwd = 1.0
)

# Draw points
points(
  1:n, cooksd,
  pch = 16,
  cex = ifelse(cooksd > threshold_cooks, 0.75, 0.45),
  col = ifelse(cooksd > threshold_cooks, "#dc2626", "#64748b")
)

# Cutoff threshold line
abline(h = threshold_cooks, col = "#ef4444", lty = 2, lwd = 1.8)

# Leader-line annotations for top influential points
# #366: top outlier
arrows(x0 = 366, y0 = 0.285, x1 = 366, y1 = cooksd[366] + 0.008, length = 0.06, col = "#991b1b", lwd = 1.2)
text(x = 366, y = 0.295, labels = paste0("#366 (D = ", sprintf("%.3f", cooksd[366]), ")"), pos = 3, cex = 0.78, font = 2, col = "#991b1b")

# #369: second highest
arrows(x0 = 420, y0 = 0.210, x1 = 369 + 4, y1 = cooksd[369] + 0.004, length = 0.06, col = "#991b1b", lwd = 1.2)
text(x = 420, y = 0.210, labels = paste0("#369 (D = ", sprintf("%.3f", cooksd[369]), ")"), pos = 4, cex = 0.78, font = 2, col = "#991b1b")

# #368: third highest
arrows(x0 = 310, y0 = 0.150, x1 = 368 - 4, y1 = cooksd[368], length = 0.06, col = "#991b1b", lwd = 1.2)
text(x = 310, y = 0.150, labels = paste0("#368 (D = ", sprintf("%.3f", cooksd[368]), ")"), pos = 2, cex = 0.78, font = 2, col = "#991b1b")

# #365: fourth highest
arrows(x0 = 310, y0 = 0.105, x1 = 365 - 4, y1 = cooksd[365], length = 0.06, col = "#991b1b", lwd = 1.2)
text(x = 310, y = 0.105, labels = paste0("#365 (D = ", sprintf("%.3f", cooksd[365]), ")"), pos = 2, cex = 0.78, font = 2, col = "#991b1b")

# Legend in TOP-LEFT (completely free from data points which cluster around 360-420)
legend(
  "topleft",
  legend = c(
    paste0("Threshold: 4/(n - p) = ", round(threshold_cooks, 4)),
    "Influential Cases (D > Threshold)",
    "Typical Observations"
  ),
  col = c("#ef4444", "#dc2626", "#64748b"),
  lty = c(2, NA, NA),
  pch = c(NA, 16, 16),
  pt.cex = c(NA, 0.9, 0.6),
  lwd = c(1.8, NA, NA),
  bty = "o",
  box.col = "#dcdde1",
  bg = "#ffffff",
  cex = 0.85,
  inset = c(0.02, 0.02)
)
dev.off()


# ==============================================================================
# 3. FIX: DFFITS Plot (Clean lines, clear leader lines, ample margins)
# ==============================================================================
dffits_val <- dffits(best_fit)
threshold_dffits <- 2 * sqrt(p / n)

png("figures/dffits.png", width = 1150, height = 650, res = 135)
par(mar = c(5, 5, 4, 3), bg = "#fcfcfc")

# Dynamic Y-limits with ample margins top and bottom
y_min <- min(dffits_val) - 0.35
y_max <- max(dffits_val) + 0.35

plot(
  1:n, dffits_val,
  type = "n",
  ylim = c(y_min, y_max),
  xlim = c(1, n + 15),
  main = "Diagnostic 1B: DFFITS (Difference in Fits Statistics)",
  ylab = "DFFITS Value",
  xlab = "Observation Index (1 to 506)",
  font.axis = 2,
  font.lab = 2,
  col.axis = "#2c3e50",
  col.lab = "#2c3e50"
)

# Background grid
grid(nx = NULL, ny = NULL, col = "#edf2f7", lty = "dotted")

# Zero baseline
abline(h = 0, col = "#94a3b8", lty = 1, lwd = 1.0)

# Positive & Negative Threshold lines
abline(h = threshold_dffits, col = "#f97316", lty = 2, lwd = 1.8)
abline(h = -threshold_dffits, col = "#f97316", lty = 2, lwd = 1.8)

# Vertical needle lines
is_extreme <- abs(dffits_val) > threshold_dffits
segments(
  x0 = 1:n, y0 = 0,
  x1 = 1:n, y1 = dffits_val,
  col = ifelse(is_extreme, "#fed7aa", "#cbd5e1"),
  lwd = 1.0
)

# Points
points(
  1:n, dffits_val,
  pch = 16,
  cex = ifelse(is_extreme, 0.75, 0.45),
  col = ifelse(is_extreme, "#ea580c", "#64748b")
)

# Leader-line annotations for top DFFITS points
# #366: top positive
arrows(x0 = 366, y0 = 1.20, x1 = 366, y1 = dffits_val[366] + 0.03, length = 0.06, col = "#9a3412", lwd = 1.2)
text(x = 366, y = 1.23, labels = paste0("#366 (", sprintf("%.2f", dffits_val[366]), ")"), pos = 3, cex = 0.78, font = 2, col = "#9a3412")

# #369: second positive
arrows(x0 = 425, y0 = 0.90, x1 = 369 + 4, y1 = dffits_val[369], length = 0.06, col = "#9a3412", lwd = 1.2)
text(x = 425, y = 0.90, labels = paste0("#369 (", sprintf("%.2f", dffits_val[369]), ")"), pos = 4, cex = 0.78, font = 2, col = "#9a3412")

# #368: third positive
arrows(x0 = 310, y0 = 0.70, x1 = 368 - 4, y1 = dffits_val[368], length = 0.06, col = "#9a3412", lwd = 1.2)
text(x = 310, y = 0.70, labels = paste0("#368 (", sprintf("%.2f", dffits_val[368]), ")"), pos = 2, cex = 0.78, font = 2, col = "#9a3412")

# #365: most negative DFFITS
arrows(x0 = 310, y0 = -0.75, x1 = 365 - 4, y1 = dffits_val[365], length = 0.06, col = "#9a3412", lwd = 1.2)
text(x = 310, y = -0.75, labels = paste0("#365 (", sprintf("%.2f", dffits_val[365]), ")"), pos = 2, cex = 0.78, font = 2, col = "#9a3412")

# Legend in TOP-LEFT (clean, no data obstruction)
legend(
  "topleft",
  legend = c(
    paste0("Threshold: ±2√(p/n) = ±", round(threshold_dffits, 3)),
    "Influential Fits (|DFFITS| > Threshold)",
    "Typical Fits"
  ),
  col = c("#f97316", "#ea580c", "#64748b"),
  lty = c(2, NA, NA),
  pch = c(NA, 16, 16),
  pt.cex = c(NA, 0.9, 0.6),
  lwd = c(1.8, NA, NA),
  bty = "o",
  box.col = "#dcdde1",
  bg = "#ffffff",
  cex = 0.85,
  inset = c(0.02, 0.02)
)
dev.off()


# ==============================================================================
# 4. Residuals vs Fitted Plot (Clean, crisp)
# ==============================================================================
png("figures/residuals_vs_fitted.png", width = 1000, height = 600, res = 135)
par(mar = c(5, 5, 4, 3), bg = "#fcfcfc")
plot(
  best_fit$fitted.values, best_fit$residuals,
  main = "Diagnostic 3: Residuals vs Fitted Values (Homoscedasticity Check)",
  xlab = "Fitted Values (Predicted medv)",
  ylab = "Ordinary Residuals (e_i)",
  pch = 19,
  col = adjustcolor("#2980b9", alpha.f = 0.6),
  cex = 0.85,
  font.axis = 2, font.lab = 2,
  col.axis = "#2c3e50", col.lab = "#2c3e50"
)
grid(nx = NULL, ny = NULL, col = "#edf2f7", lty = "dotted")
abline(h = 0, col = "#ef4444", lty = 2, lwd = 2)
lines(lowess(best_fit$fitted.values, best_fit$residuals), col = "#16a34a", lwd = 2.5)
legend(
  "topleft",
  legend = c("Zero Residual Baseline", "LOWESS Smoothed Curve"),
  col = c("#ef4444", "#16a34a"),
  lty = c(2, 1),
  lwd = 2,
  bty = "o",
  box.col = "#dcdde1",
  bg = "#ffffff",
  cex = 0.85
)
dev.off()


# ==============================================================================
# 5. Normal Q-Q Plot
# ==============================================================================
png("figures/qq_plot.png", width = 1000, height = 600, res = 135)
par(mar = c(5, 5, 4, 3), bg = "#fcfcfc")
stud_res <- rstudent(best_fit)
qqnorm(
  stud_res,
  main = "Diagnostic 4: Normal Q-Q Plot (Studentized Residuals)",
  pch = 19,
  col = adjustcolor("#8e44ad", alpha.f = 0.6),
  cex = 0.85,
  xlab = "Theoretical Quantiles",
  ylab = "Studentized Residuals (r_i)",
  font.axis = 2, font.lab = 2,
  col.axis = "#2c3e50", col.lab = "#2c3e50"
)
grid(nx = NULL, ny = NULL, col = "#edf2f7", lty = "dotted")
qqline(stud_res, col = "#2563eb", lwd = 2)
legend(
  "topleft",
  legend = c("Studentized Residuals", "Theoretical Normal Reference"),
  col = c("#8e44ad", "#2563eb"),
  pch = c(19, NA),
  lty = c(NA, 1),
  lwd = c(NA, 2),
  bty = "o",
  box.col = "#dcdde1",
  bg = "#ffffff",
  cex = 0.85
)
dev.off()


# ==============================================================================
# 6. Added Variable Plots
# ==============================================================================
png("figures/av_plots.png", width = 1100, height = 700, res = 135)
par(bg = "#fcfcfc")
avPlots(
  best_fit,
  col = "#2563eb",
  col.lines = "#ef4444",
  pch = 19,
  cex = 0.65,
  main = "Diagnostic 5: Added-Variable (Partial Regression) Plots"
)
dev.off()


# ==============================================================================
# 7. VIF Barplot
# ==============================================================================
png("figures/vif_barplot.png", width = 900, height = 550, res = 135)
par(mar = c(5, 5, 4, 3), bg = "#fcfcfc")
vif_vals <- vif(best_fit)
bp_vif <- barplot(
  vif_vals,
  col = "#0d9488",
  border = NA,
  ylim = c(0, 3.5),
  ylab = "Variance Inflation Factor (VIF)",
  xlab = "Predictor Variables",
  main = "Diagnostic 6: Variance Inflation Factor (VIF Multicollinearity Check)",
  font.axis = 2, font.lab = 2,
  col.axis = "#2c3e50", col.lab = "#2c3e50"
)
grid(nx = NA, ny = NULL, col = "#edf2f7", lty = "dotted")
barplot(vif_vals, col = "#0d9488", border = NA, add = TRUE, axes = FALSE)
abline(h = 5, col = "#f97316", lty = 2, lwd = 1.8)
abline(h = 10, col = "#ef4444", lty = 3, lwd = 1.8)
text(bp_vif, vif_vals + 0.18, labels = sprintf("VIF = %.4f", vif_vals), font = 2, col = "#134e4a", cex = 0.85)
legend(
  "topright",
  legend = c("Threshold = 5.0 (Moderate Multicollinearity)", "Threshold = 10.0 (Severe Multicollinearity)"),
  col = c("#f97316", "#ef4444"),
  lty = c(2, 3),
  lwd = 1.8,
  bty = "o",
  box.col = "#dcdde1",
  bg = "#ffffff",
  cex = 0.85
)
dev.off()

cat("All plots successfully regenerated with fixed layouts and zero label collisions in figures/\n")
