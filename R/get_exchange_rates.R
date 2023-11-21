#' @name GetExchangeRates
#' @aliases get_exchange_rates_symbol, try_exchange_rates_direct_and_indirect
#' @title Functions get_exchange_rates_symbol, try_exchange_rates_direct_and_indirect
#'   and GetExchangeRates
#' @description get_exchange_rates_symbol: uses getSymbol
#' @description GetExchangeRates: uses getFX and getSymbol
#'
#' @param from_curr character, currency symbol
#' @param to_curr character, currency symbol
#' @param from_date character, date
#' @param to_date character, date
#'
#' @keywords exchange
#' @references Ryan JA, Ulrich JM (2023). _quantmod: Quantitative Financial
#' Modelling Framework_. R package version 0.4.25,
#' <https://CRAN.R-project.org/package=quantmod>.
#'
#' @return data.frame
#' @rdname GetExchangeRates
#' @importFrom quantmod getFX getSymbols
#' @importFrom dplyr bind_rows mutate relocate rowwise
#' @importFrom rlang set_names
#' @importFrom stringr str_replace_all
#' @importFrom tibble rownames_to_column
#' @examples
#' from_curr <- c("CAD", "JPY", "USD")
#' to_curr <- c("USD", "USD", "EUR")
#'
#' \donttest{
#' # Success
#' recent_date <- as.character(Sys.Date() - 7)
#' GetExchangeRates(from_curr = from_curr, to_curr = to_curr, recent_date, recent_date)
#'
#' # last date mismatch day
#' GetExchangeRates(from_curr = from_curr, to_curr = to_curr, "2023-10-27", "2023-10-30")
#'
#' # weekend, warning, gets only FX, fails for getSymbol
#'
#' GetExchangeRates(from_curr = from_curr, to_curr = to_curr, "2023-10-28", "2023-10-28")
#' GetExchangeRates(from_curr = from_curr, to_curr = to_curr, "2023-10-29", "2023-10-29")
#' GetExchangeRates(from_curr = from_curr, to_curr = to_curr, "2023-07-08", "2023-07-09")
#'
#' # fails for FX, > 180 days
#' GetExchangeRates(from_curr = from_curr, to_curr = to_curr, "2023-04-03", "2023-04-05")
#'
#' # failure for getSymbol, when none is USD
#' GetExchangeRates(from_curr = "BRL", to_curr = "COP", "2023-07-07")
#'
#' # getSymbol success
#' get_exchange_rates_symbol(from_curr = from_curr, to_curr = to_curr, "2023-07-03", "2023-07-05")
#'
#' # getSymbol > 180 days ok
#' get_exchange_rates_symbol(from_curr = from_curr, to_curr = to_curr, "2023-04-03", "2023-04-05")
#'
#' # failure, weekend days
#' weekend_failure <- try(get_exchange_rates_symbol(
#'   from_curr = from_curr,
#'   to_curr = to_curr, "2023-07-08", "2023-07-09"
#' ), silent = TRUE)
#' weekend_failure
#'
#' # works
#' try_exchange_rates_direct_and_indirect("2023-07-08", from_curr, to_curr, tries = 8)
#' try_exchange_rates_direct_and_indirect("2023-07-08", "BRL", "USD", tries = 8)
#' try_exchange_rates_direct_and_indirect("2023-07-08", "USD", "BRL", tries = 8)
#'
#' # works indirectly
#' recent_date <- as.character(Sys.Date() - 7)
#' try_exchange_rates_direct_and_indirect(recent_date, "COP", "BRL", tries = 8)
#'
#' # works with FX only, provided, not greater than 180 days
#'
#' GetExchangeRates(from_curr = "COP", to_curr = "BRL", recent_date)
#' }
#' @export
GetExchangeRates <- function(from_curr, to_curr, from_date, to_date = from_date) {
  exchanges <- paste0(from_curr, "/", to_curr)

  result_getFX <- mapply(
    function(from_curr, to_curr) {
      ready_name <- paste0(from_curr, ".", to_curr)
      getFX(paste0(from_curr, "/", to_curr),
        from = from_date,
        to = to_date,
        src = "yahoo",
        auto.assign = FALSE
      ) |>
        as.list() |>
        as.data.frame() |>
        rownames_to_column(var = "date") |>
        set_names(~ (.) |> paste0("FX")) |>
        set_names(~ (.) |> str_replace_all(ready_name, ""))
    },
    from_curr, to_curr,
    SIMPLIFY = F
  )
  names(result_getFX) <- exchanges
  result_getFX <- result_getFX |> bind_rows(.id = "exchange")

  result_getSymbols <- try(get_exchange_rates_symbol(from_curr, to_curr, from_date, to_date), silent = TRUE)
  if (inherits(result_getSymbols, "try-error")) {
    warning("getSymbol unavailable for weekends or between two non-dollar currencies")
    return(result_getFX)
  }
  merge(result_getFX, result_getSymbols,
    by.y = c("exchange", "date_input"), by.x = c("exchange", "dateFX"),
    all = TRUE
  )
}

#' @rdname GetExchangeRates
#' @return data.frame
#' @export
get_exchange_rates_symbol <- function(from_curr, to_curr, from_date, to_date = from_date) {
  exchanges <- paste0(from_curr, "/", to_curr)

  result_getSymbols <- mapply(
    function(from_curr, to_curr) {
      ready_name <- paste0(from_curr, to_curr)
      getSymbols(paste0(ready_name, "=X"),
        src = "yahoo", auto.assign = FALSE,
        from = as.Date(from_date), to = as.Date(to_date)
      ) |>
        as.list() |>
        as.data.frame() |>
        rownames_to_column(var = "date") |>
        set_names(~ (.) |> str_replace_all(".X", "")) |>
        rowwise() |>
        mutate("{ready_name}.avg_low_high_this_fun" := mean(c(
          .data[[paste0(ready_name, ".Low")]],
          .data[[paste0(ready_name, ".High")]]
        ), na.rm = TRUE)) |>
        set_names(~ (.) |> str_replace_all(paste0(ready_name, "."), "")) |>
        set_names(~ (.) |> paste0("_Sy")) |>
        mutate(date_input = ifelse(
          as.Date(date_Sy) < as.Date("2023-10-30"),
          as.character(as.Date(date_Sy) + 1),
          date_Sy
        )) |>
        relocate(date_input, .before = date_Sy) |>
        relocate(avg_low_high_this_fun_Sy, .after = date_input)
    },
    from_curr, to_curr,
    SIMPLIFY = FALSE
  )
  names(result_getSymbols) <- exchanges
  result_getSymbols <- result_getSymbols |>
    bind_rows(.id = "exchange") |>
    as.data.frame()
  result_getSymbols
}

#' @rdname GetExchangeRates
#' @return numeric vector
#' @param tries numeric try how many days before desired date
#' @param date character, desired date
#' @export
try_exchange_rates_direct_and_indirect <- function(date, from_curr, to_curr, tries = 8) {
  date <- date_orig <- as.character(date)
  adjusted_value <- "symbol or exchange not found"
  ready_name <- paste0(from_curr, to_curr)
  i <- 0
  while (i < tries) {
    exchange_df <- try(get_exchange_rates_symbol(
      toupper(from_curr),
      toupper(to_curr), date
    ), silent = TRUE)
    date <- as.character(as.Date(date) - 1)
    i <- i + 1
    if (!inherits(exchange_df, "try-error")) break
  }

  if (inherits(exchange_df, "data.frame")) {
    adjusted_value <- exchange_df$Adjusted_Sy
    names(adjusted_value) <- ready_name
  }

  i <- 0
  date <- date_orig
  if (inherits(exchange_df, "try-error") && from_curr != "USD" && to_curr != "USD") {
    while (i < tries) {
      exchange_df_USD_to_from_curr <- try(get_exchange_rates_symbol(
        toupper("USD"),
        toupper(from_curr), date
      ), silent = TRUE)
      date <- as.character(as.Date(date) - 1)
      i <- i + 1
      if (!inherits(exchange_df_USD_to_from_curr, "try-error")) break
    }

    i <- 0
    date <- date_orig
    if (!inherits(exchange_df_USD_to_from_curr, "try-error") && from_curr != to_curr) {
      while (i < tries) {
        exchange_df_USD_to_to_curr <- try(get_exchange_rates_symbol(
          toupper("USD"),
          toupper(to_curr), date
        ), silent = TRUE)
        date <- as.character(as.Date(date) - 1)
        i <- i + 1
        if (!inherits(exchange_df_USD_to_to_curr, "try-error")) break
      }
    }

    if (!inherits(exchange_df_USD_to_from_curr, "try-error") && !inherits(exchange_df_USD_to_to_curr, "try-error")) {
      adjusted_value <- 1 / (exchange_df_USD_to_from_curr$Adjusted_Sy / exchange_df_USD_to_to_curr$Adjusted_Sy)
      names(adjusted_value) <- ready_name
    }
  }

  return(adjusted_value)
}
