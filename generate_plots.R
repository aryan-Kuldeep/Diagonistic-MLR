# generate_plots.R
# Generates high-resolution diagnostic figures for README documentation

suppressPackageStartupMessages({
  library(ISLR2)
  library(DescTools)
  library(car)
  library(mctest)
})

# Create figures directory
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Set consistent plotting aesthetics
par_default <- list(bg = "white", col.axis = "#2c3e50", col.lab = "#2c3e50", font.lab = 2)

Dataset <- Boston

# Models definition
models <- list(
  "crim+dis+rm+rad" = lm(medv ~ crim + dis + rm + rad, data = Dataset),
  "crim+dis+rm"     = lm(medv ~ crim + dis + rm, data = Dataset),
  "dis+rm+rad"      = lm(medv ~ dis + rm + rad, data = Dataset),
  "rm+rad+crim"     = lm(medv ~ rm + rad + crim, data = Dataset),
  "rad+crim+dis"    = lm(medv ~ rad + crim + dis, data = Dataset),
  "crim+dis"        = lm(medv ~ crim + dis, data = Dataset),
  "crim+rm"         = lm(medv ~ crim + rm, data = Dataset),
  "crim+rad"        = lm(medv ~ crim + rad, data = Dataset),
  "dis+rm"          = lm(medv ~ dis + rm, data = Dataset),
  "dis+rad"         = lm(medv ~ dis + rad, data = Dataset),
  "rm+rad"          = lm(medv ~ rm + rad, data = Dataset),
  "crim"            = lm(medv ~ crim, data = Dataset),
  "dis"             = lm(medv ~ dis, data = Dataset),
  "rm"              = lm(medv ~ rm, data = Dataset),
  "rad"             = lm(medv ~ rad, data = Dataset)
)

adj_r2 <- sapply(models, function(m) summary(m)$adj.r.squared)

# 1. Model Comparison Barplot
png("figures/model_comparison.png", width = 1000, height = 600, res = 130)
par(mar = c(8, 5, 4, 2), bg = "#fdfdfd")
colors <- ifelse(names(adj_r2) == "rm+rad+crim", "#27ae60", "#2980b9")
bp <- barplot(adj_r2, las = 2, col = colors, border = NA,
        ylim = c(0, 0.65), ylab = "Adjusted R-squared",
        main = "Model Selection: Adjusted R-squared Comparison",
        cex.names = 0.85, font.axis = 2, font.lab = 2)
grid(nx = NA, ny = NULL, col = "#dcdde1", lty = "dotted")
text(bp, adj_r2 + 0.02, labels = sprintf("%.3f", adj_r2), cex = 0.7, font = 2, col = "#2c3e50")
legend("topleft", legend = c("Selected Optimal Model (lm.fit4)", "Other Candidate Models"),
       fill = c("#27ae60", "#2980b9"), bty = "n", cex = 0.9)
dev.off()

# Best model
best_fit <- models[["rm+rad+crim"]]
n <- nrow(Dataset)
p <- length(coef(best_fit))

# 2. Cook's Distance Plot
png("figures/cooks_distance.png", width = 900, height = 550, res = 130)
par(mar = c(5, 5, 4, 2), bg = "#fdfdfd")
cooksd <- cooks.distance(best_fit)
threshold_cooks <- 4 / (n - p)
plot(cooksd, type = "h", main = "Diagnostic 1: Cook's Distance (Influential Cases)",
     ylab = "Cook's Distance", xlab = "Observation Index", col = "#2c3e50", lwd = 1.5)
points(cooksd, pch = 19, cex = 0.5, col = ifelse(cooksd > threshold_cooks, "#e74c3c", "#3498db"))
abline(h = threshold_cooks, col = "#e74c3c", lty = 2, lwd = 2)
text(x = which(cooksd > 0.1), y = cooksd[which(cooksd > 0.1)],
     labels = which(cooksd > 0.1), pos = 3, cex = 0.8, col = "#c0392b", font = 2)
legend("topright", legend = c(paste0("Cutoff: 4/(n-p) = ", round(threshold_cooks, 4)), "Influential Points"),
       col = c("#e74c3c", "#e74c3c"), lty = c(2, NA), pch = c(NA, 19), bty = "n", cex = 0.85)
dev.off()

# 3. DFFITS Plot
png("figures/dffits.png", width = 900, height = 550, res = 130)
par(mar = c(5, 5, 4, 2), bg = "#fdfdfd")
dffits_val <- dffits(best_fit)
threshold_dffits <- 2 * sqrt(p / n)
plot(dffits_val, type = "h", main = "Diagnostic 1B: DFFITS Statistics",
     ylab = "DFFITS", xlab = "Observation Index", col = "#34495e", lwd = 1.2)
points(dffits_val, pch = 19, cex = 0.5, col = ifelse(abs(dffits_val) > threshold_dffits, "#e67e22", "#3498db"))
abline(h = c(-threshold_dffits, threshold_dffits), col = "#e67e22", lty = 2, lwd = 1.8)
text(x = which(abs(dffits_val) > 0.8), y = dffits_val[which(abs(dffits_val) > 0.8)],
     labels = which(abs(dffits_val) > 0.8), pos = 3, cex = 0.8, col = "#d35400", font = 2)
legend("topright", legend = c(paste0("Threshold: ±2√(p/n) = ±", round(threshold_dffits, 3))),
       col = "#e67e22", lty = 2, bty = "n", cex = 0.85)
dev.off()

# 4. Residuals vs Fitted Plot
png("figures/residuals_vs_fitted.png", width = 900, height = 550, res = 130)
par(mar = c(5, 5, 4, 2), bg = "#fdfdfd")
plot(best_fit$fitted.values, best_fit$residuals,
     main = "Diagnostic 3: Residuals vs Fitted Values (Homoscedasticity Check)",
     xlab = "Fitted Values (Predicted medv)", ylab = "Ordinary Residuals",
     pch = 19, col = adjustcolor("#2980b9", alpha.f = 0.7), cex = 0.9)
abline(h = 0, col = "#e74c3c", lty = 2, lwd = 2)
lines(lowess(best_fit$fitted.values, best_fit$residuals), col = "#27ae60", lwd = 2.5)
legend("topleft", legend = c("Zero Residual Baseline", "LOWESS Smoothed Curve"),
       col = c("#e74c3c", "#27ae60"), lty = c(2, 1), lwd = 2, bty = "n", cex = 0.85)
dev.off()

# 5. Normal Q-Q Plot
png("figures/qq_plot.png", width = 900, height = 550, res = 130)
par(mar = c(5, 5, 4, 2), bg = "#fdfdfd")
stud_res <- rstudent(best_fit)
qqnorm(stud_res, main = "Diagnostic 4: Normal Q-Q Plot (Studentized Residuals)",
       pch = 19, col = adjustcolor("#8e44ad", alpha.f = 0.7), cex = 0.9,
       xlab = "Theoretical Quantiles", ylab = "Studentized Residuals")
qqline(stud_res, col = "#2980b9", lwd = 2)
dev.off()

# 6. Added Variable Plots
png("figures/av_plots.png", width = 1000, height = 650, res = 130)
par(bg = "#fdfdfd")
avPlots(best_fit, col = "#2980b9", col.lines = "#e74c3c", pch = 19, cex = 0.7,
        main = "Diagnostic 5: Added-Variable (Partial Regression) Plots")
dev.off()

# 7. VIF Barplot
png("figures/vif_barplot.png", width = 800, height = 500, res = 130)
par(mar = c(5, 5, 4, 2), bg = "#fdfdfd")
vif_vals <- vif(best_fit)
bp_vif <- barplot(vif_vals, col = "#16a085", border = NA, ylim = c(0, 3),
                  ylab = "VIF Value", xlab = "Predictor Variables",
                  main = "Diagnostic 6: Variance Inflation Factor (VIF)",
                  font.axis = 2, font.lab = 2)
abline(h = 5, col = "#e74c3c", lty = 2, lwd = 2) # Standard warning threshold
abline(h = 10, col = "#c0392b", lty = 3, lwd = 2) # Severe threshold
text(bp_vif, vif_vals + 0.15, labels = sprintf("%.3f", vif_vals), font = 2, col = "#2c3e50")
legend("topright", legend = c("Threshold = 5 (Moderate Collinearity)", "Threshold = 10 (High Collinearity)"),
       col = c("#e74c3c", "#c0392b"), lty = c(2, 3), lwd = 2, bty = "n", cex = 0.85)
dev.off()

cat("All plots successfully generated in figures/\n")
