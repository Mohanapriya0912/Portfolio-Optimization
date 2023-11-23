install.packages('ROI')
install.packages('ROI.plugin.glpk')
install.packages('ROI.plugin.quadprog')
install.packages('ROI.plugin.symphony')
#ROI packages loads automatically when Portfolio Analytics commands are run
install.packages("PortfolioAnalytics")
library(PortfolioAnalytics)
library(tidyverse)
library(tidyquant)
library("ggthemes")

#Step 1: Download prices and calculate returns(monthly)
stocks <- tq_get(c("AMZN", "TSLA", "PG", "ENPH", "AAPL"), 
                       from ='2013-12-31') %>% 
  group_by(symbol) %>% 
  tq_transmute(select =adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "ret")

#Step2:Widen the dataframe and convert to an xts object.
stocks_wide <- stocks %>% pivot_wider(names_from = symbol, values_from= ret)%>%
  timetk::tk_xts(date_var = date)

#Step3: Find optimal portfolio weights;
#3.0 Define portfolio specification name
port_spec <- portfolio.spec(colnames(stocks_wide))  # Tells R to use the column names from our xts object (which are stock names)

#3.1 Add constraints. First constraint says to use all assets (the sum of weights is one)
port_spec <- add.constraint(portfolio = port_spec,
                            type = "weight_sum",
                            min_sum= 0.99,
                            max_sum =1.01) # First constraints tells R to use all assets (the sum of weights adds to one)

port_spec <- add.constraint(portfolio = port_spec,
                            type ="box",
                            min =0,
                            max =1)        #Second constraints tells R to use anywhere between 0 and 100% of each assets. We can restrict any one investment to say below 50% here.


#3.2 Add objective. Because we want risk-adjusted return, we need to specify both a risk and a return objective 
port_spec <- add.objective(portfolio = port_spec,
                           type = "risk",
                           name = 'StdDev')
port_spec <- add.objective(portfolio = port_spec,
                           type = "return",
                           name ="mean")
print(port_spec)       

#Step 4: Run optimization (there will be no output except for a warning message. The output is stored in opt object printed below)
opt <- optimize.portfolio(stocks_wide, portfolio = port_spec,
                          optimize_method = "ROI",
                          trace = TRUE, 
                          maxSR = TRUE)
print(opt) #let's see what we have  

#Step 5: Charting
chart.EfficientFrontier(opt, match.col = "StdDev")

#Step 6: Extracting weights
wt <- extractWeights(opt)


#Step 7:  Calculate optimal portfolio returns
wt_1 <- as.data.frame(wt) %>% rownames_to_column(var ="symbol")   # Convert the weights into a dataframe and create a variable name: symbol.

optimal_port <- stocks %>% tq_portfolio(assets_col = symbol,
                                        returns_col = ret,
                                        weights = wt_1,
                                        col_rename = 'pret')

#STEP 8: Portfolio with equal weights
myport <- stocks %>% tq_portfolio(symbol, ret, weights = c(0.2,0.2,0.2,0.2,0.2))

#STEP 9: Computing Risk adjusted portfolio performance measure: Sharpe Ratio
pSR <- myport %>% tq_performance(Ra = portfolio.returns, 
                                 Rb              = NULL,
                                 performance_fun = SharpeRatio,
                                 Rf              = 0,
                                 p               = 0.95,
                                 FUN             = "StdDev") %>% 
  add_column(symbol = "Equal Weighted Portfolio", .before =1)

bSR <- optimal_port %>% tq_performance(Ra = pret, 
                                    Rb              = NULL,
                                    performance_fun = SharpeRatio,
                                    Rf              = 0,
                                    p               = 0.95,
                                    FUN             = "StdDev")%>% 
  add_column(symbol = "Optimal Portfolio", .before =1)

#STEP 10: Computing Risk adjusted portfolio performance measure: Sortino Ratio
pSoR <- myport %>% tq_performance(Ra = portfolio.returns,
                                  MAR =0, 
                                  performance_fun = SortinoRatio) %>% 
                                  add_column(symbol = "Equal Weighted Portfolio", .before =1)

bSoR <- optimal_port %>% tq_performance(Ra = pret,
                                        MAR =0, 
                                        performance_fun = SortinoRatio) %>% 
                                        add_column(symbol = "Optimal Portfolio", .before =1)

#STEP 11: Computing Risk adjusted portfolio performance measure: Omega SharpeRatio
pOS <- myport %>% tq_performance(Ra = portfolio.returns, 
                                 performance_fun = OmegaSharpeRatio) %>% 
  add_column(symbol = "Equal Weighted Portfolio", .before =1)

bOS <- optimal_port %>% tq_performance(Ra = pret, 
                                    performance_fun = OmegaSharpeRatio)%>% 
  add_column(symbol = "Optimal Portfolio", .before =1)

#STEP 12: Combining the results 
pbSR <- rbind(pSR, bSR)
pbSoR <- rbind(pSoR, bSoR)
pbOS <- rbind(pOS, bOS)

together <- inner_join(pbSR, pbSoR) %>% inner_join(pbOS) 

#STEP 13: Re-shaping the table into a dataframe suitable for plotting
together1 <- together %>% 
  pivot_longer(!symbol, names_to = "measure", values_to = "value")

#STEP 14: Plotting the calculates measures
ggplot(together1, aes(measure, abs(value), fill = symbol)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(abs(value), 4)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3)+
  scale_fill_manual(values = c("lightblue","#C3D7A4"))+
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Risk-Adjusted Performance Measures - Equal weighted Portfolio vs Optimal weighted Portfolio",
       x = "Measure",
       y = "Value (Absolute)") +
  theme_tufte()
