
# LikertEZ

`LikertEZ` is an R package designed to simplify the analysis of ordinal and Likert-scale survey data. It includes tools for calculating summary statistics, Relative Importance Index (RII), Cronbach’s alpha, visualizations (bar plots, heatmaps), and exports to Excel.

## Installation

You can install the development version of `LikertEZ` like so:

```r
# If installed locally
devtools::install("path/to/LikertEZ")

# (Optional) In the future from GitHub
# devtools::install_github("yourusername/LikertEZ")
```

## Example

This is a basic example showing you how to solve a common problem:

```r
library(LikertEZ)

# Simulate some survey data
set.seed(123)
df <- data.frame(
  Q1 = sample(1:5, 100, replace = TRUE),
  Q2 = sample(1:5, 100, replace = TRUE),
  Q3 = sample(1:5, 100, replace = TRUE)
)

# Summary table
summary_table_all(df)

# Plot one item
plot_item(df$Q1, scale_labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))

# Cronbach’s Alpha
cronbach_alpha(df)

# Export summary to Excel
export_summary_excel(df, filename = "summary_report.xlsx")
```

## Features

- **Summary Statistics**: Mean, Median, Standard Deviation (SD), Minimum, Maximum, Mode
- **Relative Importance Index (RII)**: Measures the importance of survey items.
- **Cronbach's Alpha**: Tests the reliability and internal consistency of a set of items.
- **Visualizations**: Bar plots, heatmaps
- **Export to Excel**: Automatically export summary results to an Excel file

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
