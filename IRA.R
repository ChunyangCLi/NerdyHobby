

IRA_traditional_vs_roth_recommender <- function(stock_return = 0.07, cash_time = 30, infltion_rate = 0.02, tax_rate_retire = 0.17, tax_rate_now =- 0.3) {
  # This function tells you if Traditional IRA or Roth IRA is a better deal for you given your input assumptions
  
  # stock_return: average annual investment return for your portfolia
  # cash_time: how many years from now you plan to pull out your IRA
  # inflation_rate: average annual inflation rate
  # tax_rate_retire: tax rate the time you plan to pull out your IRA 
  # Note: The roth withdraw does not change your tax brackets or count as your income. Utah has a flat income tax rate of 4.95%.
  # tax_rate_now: your current income tax rate
  
  tax_amount_traditional <- 10000*(1 + stock_return)^cash_time*tax_rate_retire/(1 + infltion_rate)^cash_time
  tax_amount_roth <- 10000*tax_rate_now
  tax_diff <- tax_amount_traditional - tax_amount_roth
  
  if (tax_diff < 0){
    print(paste0("You should do traditional IRA, becasue you will pay $", abs(tax_diff), " less in tax converted to today's money value for every $10k investment funding."))
  } else {
    print(paste0("You should do Roth IRA, becasue you will pay $", abs(tax_diff), " less in tax converted to today's money value for $10k investment funding.")
)  }
}

# Execute the function. You can change the default values
IRA_traditional_vs_roth_recommender(stock_return = 0.07, cash_time = 30, infltion_rate = 0.02, tax_rate_retire = 0.17, tax_rate_now = 0.3)



