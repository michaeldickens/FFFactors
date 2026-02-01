#
# Created 2022-02-14.
#
# What kind of returns can we expect from a market-neutral idiosyncratic position?
#

us_quotes <- read.csv("resources/French/3_Factors.csv")
industry_quotes <- read.csv("resources/French/Industry_Portfolios_10.csv")

print(paste("Market:", geometric_return(market_rets), ",", sd(market_rets)))

idiosyncratic_return <- function(industry_name) {
  market_rets <- (us_quotes$Mkt.RF + us_quotes$RF)[1:1102]
  industry_rets <- industry_quotes[,industry_name]

  correl <- cor(market_rets, industry_rets)

                                        # Build a linear model of industry rets as a function of market rets, and create
                                        # a long-short position that hedges out market exposure. Note: average return of
                                        # `hedged` equals `intercept(model)`.
  model <- lm(industry_rets ~ market_rets)
  hedged <- industry_rets - (market_rets * coefficients(model)[2])

  geometric_return <- function(rets) {
    return(exp(mean(log(1 + rets/100))) - 1)
  }

  # TODO: Build the portfolio that weights industries by Sharpe ratio (as an
  # approximation of the MVO-optimal portfolio) and then find the idiosyncratic
  # returns relative to that portfolio
  optimized_portfolio_weights <- TODO

  # print(geometric_return(industry_rets))
  print(paste(industry_name, ":", geometric_return(hedged), ",", sd(hedged)))  # idiosyncratic (geometric) return of industry_rets
}


idiosyncratic_return("NoDur")
idiosyncratic_return("Durbl")
idiosyncratic_return("Manuf")
idiosyncratic_return("Enrgy")
idiosyncratic_return("HiTec")
idiosyncratic_return("Telcm")
idiosyncratic_return("Shops")
idiosyncratic_return("Hlth")
idiosyncratic_return("Utils")
idiosyncratic_return("Other")
