### EM Algo for Binom Mix
### Ian Arriaga-MacKenzie

# Libraries
library(tidyverse)
library(RColorBrewer)
# Colorblind friendly colors
colval = brewer.pal(8, "Dark2")

# Number of coins in mix
uniqcoin = 2

# Proportions for each coin type, normalized
coin_mix_prenorm = c(.4, .6)
coin_mix = coin_mix_prenorm/sum(coin_mix_prenorm)

# Probability of heads for each coin type
coin_props = c(.5, .3)

# Number of coins to pull from 'bucket'
number_coins = 1000
# Number of times to flip each coin
number_flips = 100

# Generate coin matrix
coin_which = rmultinom(number_coins, 1, coin_mix)
for (i in 1:uniqcoin){
  coin_which[i,] = replace(coin_which[i,], which(coin_which[i,] == 0), NA)
  coin_which[i,] = replace(coin_which[i,], which(coin_which[i,] == 1), rbinom(number_coins, number_flips, coin_props[i])[which(coin_which[i,] == 1)])
}
# Rename coin matrix and create masked vector
coin_outframe = data.frame(t(coin_which)) %>% 
  rename_with(~ gsub("X", "Coin_", .x))
coin_outframe$Masked = rowSums(coin_outframe, na.rm = T)
head(coin_outframe, 20)

# Create data frame with true population values (not input values)
coin_summary = coin_outframe %>% 
  summarise(across(starts_with("Coin"), list(headprop = ~ mean(., na.rm = T)/number_flips,
                                             coinmix = ~ sum(!is.na(.))/number_coins)))
coin_summary

# Masked coin vector
coin_masked = coin_outframe$Masked
# Log likelihood value
loglikval = 0
# EM Algo termination threshold
threshval = 1e-6
# Number of iterations
emiter = 0

# Initial guesses for mixture proportion and head proportion
coinmix_guess = data.frame(g1 = 0.7,
                           g2 = 0.3)
headprop_guess = data.frame(g1 = 0.4,
                            g2 = 0.8)

# EM Algorithm
while (TRUE) {
  
  # Update iterations and log likelihood
  emiter = emiter + 1
  prev_logval = loglikval
  
  # Binomial distribution
  binomsum = drop(as.matrix(coinmix_guess)) * sapply(headprop_guess, function(x) dbinom(coin_masked, number_flips, x))
  coinprobs = data.frame(binomsum / rowSums(binomsum))
  
  # Coin proportion estimate
  coinmix_guess = coinprobs %>% 
    summarise_all(mean)
  # Head proportion estimate for each coin
  headprop_guess = colSums(coin_masked * coinprobs) / colSums(number_flips * coinprobs)
  
  # Log likelihood value
  loglikval = sum(log(binomsum))
  
  # Terminate at threshold value
  if ((abs(prev_logval - loglikval)) < threshval) return()
}

# Bind estimate outputs
headprop_guess = as.data.frame(t(headprop_guess))
coin_guess = cbind(coinmix_guess, headprop_guess, loglikval, emiter)
names(coin_guess) = c("Coin_1_mix", "Coin_2_mix", "Coin_1_head", "Coin_2_head", "LL_Val", "Iterations")

# EM Estimates
coin_guess
# True Population Values
coin_summary

# X limits for plot
xlims = c(min(coin_outframe$Masked) : max(coin_outframe$Masked))
# Plotting variables
binomplot = data.frame(
  xvar = xlims,
  denone = dbinom(xlims, number_flips, prob = headprop_guess$g1)*max(table(coin_outframe$Masked))/max(dbinom(xlims, number_flips, prob = headprop_guess$g1))*(coinmix_guess$g1),
  dentwo = dbinom(xlims, number_flips, prob = headprop_guess$g2)*max(table(coin_outframe$Masked))/max(dbinom(xlims, number_flips, prob = headprop_guess$g2))*(coinmix_guess$g2)
) %>% 
  mutate(dencomb = denone + dentwo)

# Visual representation of data and guesses.
ggplot() +
  geom_histogram(data = data.frame(coin_outframe$Masked), aes(x = coin_outframe$Masked), binwidth = 1)+
  geom_line(data = binomplot, aes(x = xvar, y = denone), color = colval[1], size = 3) +
  geom_line(data = binomplot, aes(x = xvar, y = dentwo), color = colval[2], size = 3) +
  geom_line(data = binomplot, aes(x = xvar, y = dencomb), color = colval[3], size = 3) +
  labs(x = "Number of Heads from 10 Coin Flips",
       y = "Count",
       title = "Binomial Mixture Results") +
  theme_bw()




