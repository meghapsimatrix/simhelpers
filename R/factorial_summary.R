

#' @title Summarize the sums of squares attributable to each factor in a full
#'   factorial experimental design
#'
#' @description Computes an analysis of variance for a set of factors in a full
#'   factorial experimental design. For each term order within the design (i.e.,
#'   main effects, two-way interactions, three-way interactions), the total sum
#'   of squares attributable to each factor is computed. For terms beyond order
#'   1. Optionally, also computes the total sum of squares for each order
#'   (across all factors) and the total sum of squares attributable to each
#'   factor.
#'
#' @param data data.frame or tibble containing simulation results. Each row
#'   should correspond to a unique set of parameter values.
#' @param y character string corresponding to the outcome variable in
#'   \code{data}.
#' @param factors character vector containing the names of two or more variables
#'   in \code{data} that correspond to factors in the experimental design.
#' @param sum_orders logical indicating whether to compute the total sum of squares
#'   attributable to each factor, with a default of \code{TRUE}.
#' @param include_total logical indicating whether to compute the total sum of squares
#'   for each term order (across all factors), with a default of \code{TRUE}.
#' @param check_balance logical indicating whether to check that the experimental design is balanced, with a default of \code{TRUE}.
#'
#' @export
#'
#' @return A data.frame
#'
#' @examples
#'
#' data("Chen_Pusto")
#' dat <- subset(Chen_Pusto, method == "PET-PEESE")
#'
#' factorial_summary(dat, "bias", c("k","mu","tau","cor_mu","wts"))
#' factorial_summary(dat, "bias", c("k","mu","tau","wts"), include_total = FALSE)
#'

factorial_summary <- function(
    data,
    y,
    factors,
    sum_orders = TRUE,
    include_total = TRUE,
    check_balance = TRUE
) {

  # Validate inputs

  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.")
  }

  if (!is.character(y) || length(y) != 1L || !(y %in% names(data))) {
    stop("`y` must be a character vector of length 1 corresponding to a variable in `data`.")
  }

  if (!is.character(factors) || length(factors) <= 1L) {
    stop("`factors` must be a character vector listing two or more factors.")
  }

  missing_f <- setdiff(factors, names(data))
  if (length(missing_f) > 0L) {
    stop(paste("The following variables are not in `data`:", paste(missing_f, collapse = ", ")))
  }

  if (!is.logical(sum_orders)) {
    stop("`sum_orders` must be a logical value.")
  }

  if (!is.logical(include_total)) {
    stop("`include_total` must be a logical value.")
  }


  data <- data[c(y,factors)]
  # Ensure all terms are factors

  not_factors <- factors[!sapply(data[factors], is.factor)]
  data[not_factors] <- lapply(data[not_factors], as.factor)


  # Check for balance
  factor_formula <- stats::reformulate(paste(factors, collapse = " * "), response = y)
  if (check_balance) {
    balance_check <- stats::replications(factor_formula, data = data)
    imbalanced <- is.list(balance_check)
    if (imbalanced) {
      stop("Data are not from a balanced factorial design.")
    }
  }


  # Analysis of Variance

  factor_contrasts <- rep(list("contr.helmert"), length(factors))
  names(factor_contrasts) <- factors
  aov_table <- summary(stats::aov(factor_formula, data = data, contrasts = factor_contrasts))[[1]]


  # Compute term orders
  rownames(aov_table) <- trimws(rownames(aov_table))
  term_names <- rownames(aov_table)
  if ("Residuals" %in% term_names) {
    factors <- c(factors, "Residuals")
  }

  term_order <- paste(
    "Order",
    ifelse(grepl(":", term_names), lengths(gregexpr(":", term_names)) + 1L, 1L)
  )

  n_factors <- length(factors)
  factor_df <- aov_table[factors,"Df"]

  # Attributable sums of squares
  factor_string <- paste0("(^", factors, "$)|(^", factors, ":)|(:", factors, ":)|(:",factors,"$)")
  factor_map <- lapply(factor_string, grepl, x = term_names)
  if (include_total) {
    factor_map[[n_factors + 1L]] <- rep(TRUE, length(term_names))
  }
  SS_attribution <- lapply(factor_map, \(x) tapply(aov_table$`Sum Sq`[x], term_order[x], sum, simplify = TRUE))
  SS_matrix <- do.call(rbind, SS_attribution)

  if ("Residuals" %in% factors) {
    SS_matrix[factors=="Residuals",-1] <- 0
  }

  # Add total across orders
  if (sum_orders) {
    SS_matrix <- cbind(SS_matrix, Sum = rowSums(SS_matrix))
  }

  # Format output

  SS_table <- as.data.frame(SS_matrix)

  if (include_total) {
    res_labels <- data.frame(
      factor = c(factors,"Total"),
      `d.f.` = c(factor_df, NA_integer_)
    )
  } else {
    res_labels <- data.frame(
      factor = factors,
      `d.f.` = factor_df
    )
  }

  cbind(res_labels, SS_table)

}

