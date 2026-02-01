# code written by Claude, need to edit this to make it actually do what I want -2024-10-13
#
# what it's supposed to do is determine which variables in JKP have the most explanatory power for all the other variables

import numpy as np
import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA

# Load your data (replace this with your actual data loading method)
# data = pd.read_csv('your_data.csv')

# For demonstration, let's create a random dataset
np.random.seed(42)
data = pd.DataFrame(np.random.randn(1000, 100))

# Step 1: Prepare the data
scaler = StandardScaler()
scaled_data = scaler.fit_transform(data)

# Step 2: Perform PCA
pca = PCA()
pca_result = pca.fit_transform(scaled_data)

# Step 3: Analyze the results
explained_variance_ratio = pca.explained_variance_ratio_
cumulative_variance_ratio = np.cumsum(explained_variance_ratio)

# Find number of components needed for 90% variance
n_components_90 = np.argmax(cumulative_variance_ratio >= 0.9) + 1

print(f"Number of components explaining 90% of variance: {n_components_90}")

# Step 4: Interpret the principal components
component_loadings = pd.DataFrame(
    pca.components_.T,
    columns=[f'PC{i+1}' for i in range(pca.n_components_)],
    index=data.columns
)

# Get the top 5 variables contributing to the first few principal components
n_top_variables = 5
n_components_to_check = min(5, n_components_90)

for i in range(n_components_to_check):
    print(f"\nTop {n_top_variables} variables contributing to PC{i+1}:")
    top_variables = component_loadings[f'PC{i+1}'].abs().nlargest(n_top_variables)
    print(top_variables)

# Calculate and print the total explained variance by the top 5 components
top_5_variance = explained_variance_ratio[:5].sum()
print(f"\nTotal variance explained by top 5 components: {top_5_variance:.2%}")
