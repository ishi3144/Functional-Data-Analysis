# Perform PCA
df <- USArrests
df <- na.omit(df)
df <- scale(df)
pca_result <- prcomp(df)

# Extract loadings (eigenvectors)
loadings <- pca_result$rotation

# Calculate percentage of variation explained
eigenvalues <- pca_result$sdev^2
percentage_variation <- eigenvalues / sum(eigenvalues) * 100

# Plot the first four eigenfunctions
par(mfrow = c(2, 2))  # Set up a 2x2 grid for plotting

for (i in 1:4) {
  # Plot the eigenfunction
  plot(loadings[, i], type = "l", main = paste("Eigenfunction", i), xlab = "Index", ylab = "Value", col = "red")
  
  # Add text box with percentage of variation
  text(x = 1, y = max(loadings[, i]) - 0.2, labels = paste("Variation:", round(percentage_variation[i], 2), "%"),pos = 4, col = "blue")
}
