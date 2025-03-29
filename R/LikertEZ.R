# Ordinal Summary Tools

#' Summary for a single ordinal item (Likert-type)
#' @param responses Numeric vector
#' @param max_scale Max Likert scale value (default: 5)
#' @param exact Use Monte Carlo chi-sq when needed (default: TRUE)
#' @param B Number of replicates for simulation
#' @param tidy Return result as data.frame
#' @return List or data.frame with stats
#' @export
summarize <- function(responses, max_scale = 5, exact = TRUE, B = 10000, tidy = FALSE) {
  responses <- na.omit(as.numeric(responses))
  N <- length(responses)

  freq_table <- table(factor(responses, levels = 1:max_scale))
  percent_table <- round(100 * prop.table(freq_table), 2)
  mode_val <- as.numeric(names(freq_table)[which.max(freq_table)])
  expected <- rep(sum(freq_table) / max_scale, max_scale)

  if (exact && any(expected < 5)) {
    chi_test <- chisq.test(freq_table, p = rep(1 / max_scale, max_scale), simulate.p.value = TRUE, B = B)
  } else {
    chi_test <- suppressWarnings(chisq.test(freq_table, p = rep(1 / max_scale, max_scale)))
  }

  weights <- as.numeric(names(freq_table))
  fi <- as.numeric(freq_table)
  RII <- sum(weights * fi) / (max_scale * sum(fi))

  result <- list(
    Summary = list(
      Mean = mean(responses),
      Median = median(responses),
      SD = sd(responses),
      Min = min(responses),
      Max = max(responses),
      Mode = mode_val,
      Missing = sum(is.na(responses))
    ),
    Counts = as.list(freq_table),
    Percentages = as.list(percent_table),
    RII = RII,
    Chi_Square = list(
      Statistic = unname(chi_test$statistic),
      DF = unname(chi_test$parameter),
      P_Value = unname(chi_test$p.value),
      Method = chi_test$method
    )
  )

  if (tidy) {
    df <- as.data.frame(result$Summary)
    names(df) <- names(result$Summary)
    return(df)
  }
  return(result)
}

#' Create a tidy summary table of all items
#' @param data A data.frame of ordinal items
#' @param max_scale Max value on the scale
#' @param scale_labels Optional vector of labels for each scale point
#' @param decimals Number of decimal places for percentages (default = 2)
#' @export
summary_table_all <- function(data, max_scale = 5, scale_labels = NULL, decimals = 2) {
  all_items <- lapply(names(data), function(name) {
    result <- summarize(data[[name]], max_scale = max_scale)
    summary_stats <- result$Summary
    counts <- result$Counts
    percents <- result$Percentages

    df <- data.frame(
      Item = name,
      Mean = summary_stats$Mean,
      Median = summary_stats$Median,
      SD = summary_stats$SD,
      Min = summary_stats$Min,
      Max = summary_stats$Max,
      Mode = summary_stats$Mode
    )

    for (i in 1:max_scale) {
      label <- if (!is.null(scale_labels) && i <= length(scale_labels)) scale_labels[i] else paste0("cat", i)
      col_label <- paste0(label, "_N(%)")
      count <- counts[[as.character(i)]]
      percent <- format(round(percents[[as.character(i)]], decimals), nsmall = decimals)
      df[[col_label]] <- paste0(count, " (", percent, "%)")
    }

    return(df)
  })

  summary_df <- do.call(rbind, all_items)
  rownames(summary_df) <- NULL
  return(summary_df)
}

#' Barplot with RII annotation
#' @export
plot_item <- function(responses, max_scale = 5, scale_labels = NULL) {
  library(ggplot2)
  responses <- na.omit(as.numeric(responses))
  df <- as.data.frame(table(factor(responses, levels = 1:max_scale)))
  names(df) <- c("Response", "Count")
  rii_val <- round(sum(as.numeric(df$Response) * df$Count) / (max_scale * sum(df$Count)), 3)

  if (!is.null(scale_labels)) {
    df$Response <- factor(df$Response, labels = scale_labels)
  }

  ggplot(df, aes(x = Response, y = Count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = Count), vjust = -0.5) +
    labs(title = paste("Response Distribution (RII:", rii_val, ")"),
         x = "Response", y = "Count") +
    theme_minimal()
}

#' Cronbach Alpha for a set of ordinal items
#' @export
cronbach_alpha <- function(data) {
  data <- na.omit(data)
  k <- ncol(data)
  item_var <- apply(data, 2, var)
  total_var <- var(rowSums(data))
  alpha <- (k / (k - 1)) * (1 - sum(item_var) / total_var)
  return(round(alpha, 3))
}

#' Rank items by RII or Mean
#' @export
rank_items <- function(data, method = "rii", max_scale = 5, n = 5, top = TRUE) {
  results <- sapply(data, function(col) {
    col <- as.numeric(col)
    if (method == "rii") {
      freq <- table(factor(col, levels = 1:max_scale))
      weights <- as.numeric(names(freq))
      fi <- as.numeric(freq)
      sum(weights * fi) / (max_scale * sum(fi))
    } else {
      mean(col, na.rm = TRUE)
    }
  })
  sorted <- sort(results, decreasing = top)
  return(head(sorted, n))
}

#' Missing Summary for a Data Frame
#' @export
missing_summary <- function(data) {
  sapply(data, function(col) sum(is.na(col)))
}

#' Weighted RII Calculation
#' @export
rii_weighted <- function(responses, weights, max_scale = 5) {
  responses <- as.numeric(responses)
  weights <- as.numeric(weights)
  valid <- !is.na(responses) & !is.na(weights)
  responses <- responses[valid]
  weights <- weights[valid]
  weighted_sum <- sum(responses * weights)
  max_weighted <- sum(max_scale * weights)
  return(weighted_sum / max_weighted)
}

#' Plot RII Heatmap by Groups
#' @export
plot_rii_heatmap <- function(data, group_col, max_scale = 5) {
  library(dplyr)
  library(ggplot2)
  long_data <- tidyr::pivot_longer(data, -{{group_col}}, names_to = "Item", values_to = "Response")

  rii_df <- long_data %>%
    filter(!is.na(Response)) %>%
    group_by({{group_col}}, Item) %>%
    summarise(RII = sum(as.numeric(Response)) / (max_scale * n()), .groups = 'drop')

  ggplot(rii_df, aes(x = Item, y = as.factor({{group_col}}), fill = RII)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    geom_text(aes(label = round(RII, 2)), color = "black", size = 3) +
    labs(title = "RII Heatmap by Group", x = "Item", y = "Group") +
    theme_minimal()
}

#' Export Summary to Excel
#' @export
export_summary_excel <- function(data, filename = "summary_report.xlsx", max_scale = 5) {
  library(openxlsx)
  wb <- createWorkbook()

  for (item in names(data)) {
    sheet <- summarize(data[[item]], max_scale = max_scale, tidy = TRUE)
    addWorksheet(wb, item)
    writeData(wb, item, sheet)
  }

  saveWorkbook(wb, filename, overwrite = TRUE)
  invisible(NULL)
}

#' Import Likert data from CSV and convert factor columns to numeric
#' @export
import_likert_csv <- function(filepath) {
  df <- read.csv(filepath, stringsAsFactors = FALSE)
  df[] <- lapply(df, function(col) {
    if (is.factor(col)) as.numeric(as.character(col)) else col
  })
  return(df)
}

#' Plot RII values side-by-side for all items
#' @export
plot_rii_bar <- function(data, max_scale = 5, sort = TRUE) {
  library(ggplot2)
  rii_values <- sapply(data, function(col) {
    col <- as.numeric(col)
    freq <- table(factor(col, levels = 1:max_scale))
    weights <- as.numeric(names(freq))
    fi <- as.numeric(freq)
    sum(weights * fi) / (max_scale * sum(fi))
  })

  df <- data.frame(Item = names(rii_values), RII = round(rii_values, 3))
  if (sort) {
    df <- df[order(df$RII, decreasing = TRUE), ]
    df$Item <- factor(df$Item, levels = df$Item)
  }

  ggplot(df, aes(x = Item, y = RII)) +
    geom_bar(stat = "identity", fill = "darkcyan") +
    geom_text(aes(label = RII), vjust = -0.5) +
    ylim(0, 1) +
    labs(title = "RII Comparison Across Items", x = "Item", y = "RII") +
    theme_minimal()
}
