# ConstructPortfolio.R
# -------------------------------------------------------------------------
# construct_portfolio: build a single portfolio from a weight_df and returns_df
#
# SUMMARY (what this file provides)
#   - A single function `construct_portfolio()` that takes tidy returns and
#     a table of formation weights and returns:
#       * portfolio: per-date aggregates (gross_exposure, pnl, port_ret, port_index, counts)
#       * holdings : per-date per-ticker rows with origin_weight (formation position),
#                    pnl, and weight_date (the formation date)
#
# DESIGN / ACCOUNTING CHOICES (important, please read)
#   - This implementation models **buy-and-hold of formation positions**:
#       * `origin_weight` = the (signed) position size chosen at the weight_date
#         (interpretable as number of shares, notional, or a signal unit).
#       * For each test-day t, per-holding P&L is computed as:
#           pnl_{i,t} = origin_weight_i * ret_{i,t}
#         and portfolio pnl is the sum across holdings.
#       * `port_ret` is set equal to `pnl` (i.e., raw P&L in the same units as
#         origin_weight × returns). Whether that is a fractional return depends
#         on how you scaled `origin_weight` when you constructed weight_df.
#   - We **do not** rebalance inside the holding window. Positions (origin_weight)
#     are fixed until the next rebalance (per `rebalance_freq` + `shift_n` logic).
#   - This function therefore **does not** produce daily rebalanced returns.
#
# COLUMNS / RETURNED OBJECTS
#   - portfolio (tibble)
#       date           : Date
#       gross_exposure : sum(abs(origin_weight)) on that date (units of origin_weight)
#       pnl            : sum(origin_weight * ret) on that date
#       port_ret       : currently equals pnl (raw P&L). To get fractional returns,
#                        either supply normalized weights (sum(abs)=1) or divide
#                        pnl by a capital base before building an index.
#       port_index     : cumprod(1 + port_ret) (only meaningful if port_ret is fractional)
#       n_assets, n_hold, n_long, n_short : counts
#
#   - holdings (tibble)
#       date           : return date (Date)
#       ticker         : asset identifier (character)
#       ret            : asset simple return on that date (numeric)
#       pos            : sign of origin_weight (+1 long, -1 short, 0 flat)
#       origin_weight  : formation position size (signed notional / shares / signal units)
#       pnl            : origin_weight * ret for that date
#       origin_weight_date    : the original formation date for that position (Date)
#
# PARAMETERS & BEHAVIOR
#   - date_col, ticker_col, return_col, weight_col: names in your input data.frames
#   - return_type: "simple" (default) or "log" (log returns are converted via expm1)
#   - rebalance_freq: "monthly" (default), "quarterly", or "yearly" (controls bucketing)
#   - shift_n: integer >= 0 (default 0). Controls implementation lag:
#       * shift_n = 0 -> apply weights to returns in the same rebalance period
#       * shift_n = 1 -> apply weights to returns in the next rebalance period
#     NOTE: shift_n is merely a *parameter* (default 0) — set it when calling the
#     function (e.g., shift_n = 1 to avoid look-ahead). The signature uses 0L
#     (integer) as a clear default but 0 and 0L behave identically for logic.
#
# USAGE NOTES / PEDAGOGICAL TIPS
#   - If you want fractional returns (so port_index is a standard cumulative
#     return), either feed weights normalized such that sum(abs(weight)) == 1
#     (or divide pnl by an explicit capital argument), or compute port_ret =
#     pnl / capital before calling cumprod.
#   - If you want to *report* how portfolio fractions drift over time given
#     fixed positions, compute holding_value = origin_weight * cumprod(1+ret)
#     per ticker and then compute weights = holding_value / sum(abs(holding_value))
#     as a post-processing step (this function purposely omits that).
#
# -------------------------------------------------------------------------
# Function definition
construct_portfolio <- function(
    return_df,
    weight_df,
    # column names in inputs (use sensible defaults)
    date_col   = "date",
    ticker_col = "ticker",
    return_col = "return",
    weight_col = "weight",
    # return type: "simple" or "log"
    return_type = c("simple","log"),
    # alignment bucket for weights: "monthly","quarterly","yearly"
    rebalance_freq = c("monthly","quarterly","yearly"),
    # shift weights by N buckets (integer, can be 0)
    shift_n = 0L
) {
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(lubridate)
  })
  
  return_type <- match.arg(return_type)
  rebalance_freq  <- match.arg(rebalance_freq)
  shift_n    <- as.integer(shift_n)
  
  # ---- helpers ----
  to_simple <- function(x) {
    x <- as.numeric(x)
    if (return_type == "log") expm1(x) else x
  }
  
  make_bucket <- function(d) {
    if (rebalance_freq == "monthly") floor_date(d, "month")
    else if (rebalance_freq == "quarterly") floor_date(d, "quarter")
    else floor_date(d, "year")
  }
  
  shift_date <- function(d) {
    if (shift_n == 0L) return(d)
    if (rebalance_freq == "monthly") d %m+% months(shift_n)
    else if (rebalance_freq == "quarterly") d %m+% months(3L * shift_n)
    else d %m+% years(shift_n)
  }
  
  # ---- validate minimal columns ----
  stopifnot(is.data.frame(return_df), is.data.frame(weight_df))
  if (!all(c(date_col, ticker_col, return_col) %in% names(return_df))) {
    stop("return_df must contain date, ticker, and return columns.")
  }
  if (!all(c(date_col, ticker_col, weight_col) %in% names(weight_df))) {
    stop("weight_df must contain date, ticker, and weight columns.")
  }
  
  # ---- prepare returns (long form) ----
  rets <- return_df %>%
    transmute(
      date   = as.Date(.data[[date_col]]),
      ticker = as.character(.data[[ticker_col]]),
      ret    = to_simple(.data[[return_col]])
    ) %>%
    filter(is.finite(ret)) %>%
    mutate(rebalance_period = make_bucket(date))    # internal join key
  
  # ---- prepare weights (align by bucket & optional shift) ----
  origin_w <- weight_df %>%
    transmute(
      date   = as.Date(.data[[date_col]]),
      ticker = as.character(.data[[ticker_col]]),
      origin_weight = as.numeric(.data[[weight_col]])
    ) %>%
    filter(is.finite(origin_weight)) %>%
    mutate(
      date_shifted = shift_date(date),
      rebalance_period = make_bucket(date_shifted)
    ) %>%
    group_by(ticker, rebalance_period) %>%
    slice_tail(n = 1) %>%    # if multiple weights in same bucket, take last
    ungroup() %>%
    select(ticker, rebalance_period, origin_weight, origin_weight_date = date)
  
  # ---- merge returns and weights ----
  dat <- rets %>%
    left_join(origin_w, by = c("ticker","rebalance_period")) %>%
    # missing weights => not held (origin_weight = 0)
    mutate(origin_weight = coalesce(origin_weight, 0)) %>%
    mutate(
      pos = dplyr::case_when(
        origin_weight >  0 ~  1L,
        origin_weight <  0 ~ -1L,
        TRUE               ~  0L
      )
    ) %>%
    # per-holding pnl using signed weight (notional * return)
    mutate(
      pnl = origin_weight * ret
    )
  
  if (nrow(dat) == 0) stop("No rows after merging returns and weights. Check date ranges / buckets.")
  
  # ---- portfolio aggregates per-date ----
  port <- dat %>%
    group_by(date) %>%
    summarise(
      # gross exposure is sum of absolute notional exposure
      gross_exposure = sum(abs(origin_weight), na.rm = TRUE),
      # total (signed) pnl across holdings (same units as weights * returns)
      pnl = sum(pnl, na.rm = TRUE),
      # portfolio return: raw pnl (unscaled). Change to normalized if desired.
      port_ret = pnl,
      #port_ret = ifelse(gross_exposure > 0, pnl / gross_exposure, 0),
      n_assets = n_distinct(ticker),
      n_hold   = sum(origin_weight != 0, na.rm = TRUE),
      n_long   = sum(origin_weight > 0,  na.rm = TRUE),
      n_short  = sum(origin_weight < 0,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date) %>%
    mutate(
      port_index = cumprod(1 + coalesce(port_ret, 0))
    ) %>%
    select(date, gross_exposure, pnl, port_ret, port_index, n_assets, n_hold, n_long, n_short)
  
  # ---- holdings output (origin_weight only) ----
  holdings <- dat %>%
    select(
      date,
      ticker,
      ret,
      pos,
      origin_weight,
      pnl,
      origin_weight_date
    )
  
  # attach the chosen rebalance frequency as attr (optional)
  attr(holdings, "rebalance_frequency_choice") <- rebalance_freq
  attr(holdings, "shift_n") <- shift_n
  
  list(portfolio = port, holdings = holdings)
}
