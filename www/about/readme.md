
\

BuyTheDips started as a hobby project made to help with crypto investing. The core algorithm statistically determines what “dips” are in the popular saying “buy the dip” based on historical performance, thereby reducing ambiguity. Buying these dips and selling at these peaks will yield greater returns on your investment. Whether you sell or not at the peaks is entirely up to you, but a portfolio that buys at dips will definitely outperform a scheduled-time-purchases (e.g., DCA) by 6-20% depending on your "dip" threshold levels. Currently, BuyTheDips has three main information platforms: **BuyTheDips** [**@BitcoinDips**](https://twitter.com/bitcoindips), **Mole** [**@BitcoinDipsMole**](https://twitter.com/bitcoindipsMole), and my latest feauture, **BuyTheDips** [**%>% dashboard**](https://buythedips.io/).

\

## 1. BuyTheDips %>% dashboard is running in beta.

On the [**BuyTheDips %>% dashboard**](https://buythedips.io/), you'll be have to grab live z-scores based on current price and trading volume. For archived updates on the dashboard please visit the [**versions**](https://buythedips.io/versions.html) page.

\

### Version 1.3.0 | December 3rd, 2021.

- Happy Friday! We've had tons of traffic this week to the dashboard. It was a crazy week for the servers. Sorry if you've experiences outages.
- Added Mole's limits to the dashboard. 
- Enter your budget and Mole will suggest limit buy and sell points based on current z scores. They're essentially **!getspicy** of the twitter version, but the twitter version only updates every 20 minutes while the dashboard one updates every 5 minutes.
- People seem to really enjoy the animations on the main twitter bot. I'm trying to think of ways I can incorporate this into the website. 

\

### Version 1.2.2 | November 25th, 2021.

- Good news: Days since chart is should now be stable and reliable! 
- Most changes were made on the backend this week that increases performance and decreases loading time. In other words, I cleaned up my code lol.
- I added a 'full screen' option to the historical charts. 
- I've been working on the portfolio manager that uses BuyTheDip metrics. I'm pretty happy with how things are going, and I'm hoping that it'll help crypto investors and traders make better decisions. Let me know on Twitter if you'd like to be a beta tester for this feature when its ready. 

\

### Version 1.2.0 | November 16th, 2021.

- Thank you all for your interest in the **BuyTheDips** dashboard! 
- I'm sorry that the server has been unstable. The cold-hard-truth is that I made the app on Amazon AWS free-trial server which has limited capabilities. I'm bashing on my keyboard as fast as I can.
- The initial release a week ago helped better understand server requirements moving forward. Unlike your traditional blog pages, the **BuyTheDips dashboard** app is actually quite CPU heavy with each user. So the server crashes/slows-down everytime there's an uptick of traffic.  
- To unload some server CPU usage, the historical charts were moved off the main app and they are uploaded every hour as *html widgets*. You can still filter them and scroll them, but you can't change their color anymore. 
- Few additional indicators and metrics were added (explained below).
- Features have become slightly more mobile friendly.
- I think I will be able to add even more cryptos in the future updates, but I think I will prioritize stabilitizing the server first.

\

### 1.1 Dashboard plots

![](https://ahjustsea.github.io/BuyTheDip/dashboard_v1.3.0.png)


#### 1.1.1 z-score movements in the last 48 hours.
- Displays historical z-scores (z-price) in the last 48 hours. The others, z-volume and performance, are available on the "Historicals" tab available on desktop.

\

#### 1.1.2 current performance on 14d volume adjusted avg.
- Shows how your crypto is performing currently when compared to the 14-day volume-adjusted moving average price. 

\

#### 1.1.3 z-score in %in% current price (z-price).
- These are z-scores calculated on current price. See below for explanations on z-scores.
- Hover on cryptos to see Mole's suggestions on dips.

\

#### 1.1.4 z-score in %in% current trading volume (z-volume).
- These are z-scores can be calculated on trading volume as well. The same guidelines apply. z-volume > 2.0 is volume that is significantly higher than expected, and z-volume less than -2.0 is significantly less. Calculated from historical 14 day moving averages of trading volume.

\

#### 1.1.5 scatter %in% price (z-price vs. z-volume).
- Scatter plot of items 1.1.3 and 1.1.4
- This is my new favourite way to see cryptos action now. 
- White box in the middle (x and y boundaries @ -2 and 2) is essentially what I call **"the-safezone-of-stability"**. 
- If your crypto is in this zone, it's price and trading volume is as expected (e.g., regular, normal). 
- Once outside this box, and it starts moving towards each corners and/or quadrants, you may consider the following scenarios (with a grain of salt, of course):
1. Right top: Significant increase in volume, significant increase in price. Possible interpretation: large buying pressure outweighing sell limit pools. People may be FOMO buying?
2. Left top: Significant increase in volume, significant decrease in price. Possible interpretation: Large selling pressure (Panic sell?).
3. Left bottom: Significant decrease in volume, significant decrease in price. Possible interpretation: Price is decreasing, but volume is very low. Probably has something to do with liquidity pool (lack of buy limit orders).
4. Right bottom: Significant decrease in volume, significant increase in price. Possible interpretation: Price is increasing, but volume is significant low like quadrant 3. Some people say that you should never trust price movements that isn't accompanied by volume, especially if it's due to low pool liquidity.
- When a crypto starts to "**moon**", it'll first make significant lateral movement towards the right and then tread upwards into the to the right-top corner. 

\

#### 1.1.6 scatter %in% volatility (volatility in price vs. volatility in trading volume).
- Scatter plot of items 1.1.7 and 1.1.8
- Percent volatility (i.e., the tendency to change rapidly and unpredictably) is calculated on both crypto prices and trading volume, and then plotted against each other.
- the faded white boxes show IQR ranges for each metric. Where the boxes overlap in the middle, is where you expect % volatility values to be. Outside of this area indicates that current volatility is higher or lower than the norm. 

\

#### 1.1.7 & 1.1.8 volatility %in% price and trading volume.
- Percent volatility (i.e., the tendency to change rapidly and unpredictably) is calculated on both crypto prices and trading volume.
- Presented as bar graphs.

\

### 1.2 Historical plots (available on desktop)

- 2-week historical plots of z-scores of price and trading volume, and performance can be seen here.
- There is a little filtering bug with the embedded widgets. Please open up the full screen versions to filter down to your favourite crypto.
- Updates every hour.

\

### 1.3 Days Since (available on desktop)

- Displays days since mooning or dipping.
- Please note that I started tracking crypto metrics on Oct 30th, 2021. So if it hasn't dipped or mooned since then, I don't have a value. Sorry.

\

### 1.4 Limits (available on desktop)

- Using z-scores, Mole makes limit buys and sells suggestions and how buying or selling at these points would affect your breakeven scores (avg coin price; optional input). 
- Many of my early twitter followers will know this feature as Mole's !getspicy, but the backend data for the twitter version only updates every 20 minutes while the dashboard one updates every 5 minutes. I may implement !getdips as an option in the future.
- Current strategies available: unbiased, significant, laidback, bell, aggressive.

\

## 2. Features on Twitter

Documentation on **BuyTheDips** [**@BitcoinDips**](https://twitter.com/bitcoindips) and **Mole** [**@BitcoinDipsMole**](https://twitter.com/bitcoindipsMole) has been moved to [**here**](https://buythedips.io/aboutMole.html)

\


## 3. Brief explanation on BuyTheDips Metrics

\

### 3.1 z-scores based on **price** (aka z-price).

z-scores are current price metric calculated from trading volume-adjusted two-week moving averages and standard deviations. 

1. Z-score greater than **+2** means that the current price is significantly **HIGHER** than what's expected from the last two weeks. In this case, *you may consider ***selling*** some coins to take some profits* assuming that your average cost per coin is lower than the current price. If you're looking to buy at z-score > 2, you will be paying a premium.

2. Z-score less than **-2** means that the current price is significantly **LOWER** than what's expected from the last two weeks. In this case, *you may consider ***buying*** more coins as it is a good time to buy*, and prices may go back to norm. 

3. Z-score between +2 and -2 means that the current price is very similar to the average price of last two weeks. In other words, the price is **fair**. 

\

### 3.2 z-scores based on **trading volume** (aka z-volume). 

- Available exclusively on **BuyTheDips %>% dashboard**.
- The same metric can be calculated for trading volume, and the logic stays the same. Large sudden increases in trading volume will register as z-volume > 2.0, and sudden drops will register as z-volume < -2.0. Some investors believe that large movements in volume will precedes increases in price, though it's not always the case. I've actually more cases of the opposite (z-volume increase after z-price) which is an indication of either FOMO or rug-pull/large profit taking.


\

### 3.3 All this is cool but does following z-scores work?

- Surprisingly yes, it works pretty well but you have to be disciplined to some extend. Buying cryptos when z-scores are less than -2 or -3 can make you 6-20% additional returns on your investment. 
- Detailed information on how BuyTheDips makes more on your investment can be found [**here**](http://buythedips.io/zscore_investing.html).

scenario |	times purchased |	bitcoin |	total ivnested (\$) |	current BTCUSD |	portfolio (\$) | profit (\$) |	profit (%)
-----: | -----: | -----: | -----: | -----: | -----: | -----: | -----:
Scheduled purchase (\$100 every two weeks; aka DCA) |	77 |	0.8605088 |	\$7,700.00 |	\$60,000.00 |	\$51,630.53 |	\$43,930.53 |	570.5%
BuyTheDips @ z-score -2 |	1000 |	0.9195261 |	\$7,700.00 |	\$60,000.00 |	\$55,171.56 |	\$47,471.56 |	616.5%
BuyTheDips @ z-score -3 |	139 |	1.0259890 |	\$7,700.00 |	\$60,000.00 |	\$61,559.34 |	\$53,859.34 |	699.5%

![](https://ahjustsea.github.io/BuyTheDip/BuyTheDip.png)


\


## 4. Reach out for suggestions

\

- You can find me on reddit (u/Ahjustsea) or on twitter (@bitcoinDips).
- If you would like to support BuyTheDips, you can make a donation below. Any support will be greatly appreciated. Thank you!

\

**BTC**: bc1qcay52dwm8h7qugj4tkzjcg0wrzhqf9jsh62sgn 

**ETH**: 0xA1e608C40C88B83e3325a9f25E0523BE8bC977d2 

**ADA**: Ae2tdPwUPEZMc9Jp6RVE4SfCKpQCQD4LMZKiSaoyCtSzTE5JNrZNrvBiyTT 

**SOL**: 8oETiSiMJhR1iMSPqWQDZ24qhToxfdBZHG5AP9Jfimv6 

**XLM**: GBC7BWLJ7LPGZGNSBOQQAGA4IQB3OINZ3WT7FHUQ3XRAWFJQC56X34XL 

**USDT**: 0xA1e608C40C88B83e3325a9f25E0523BE8bC977d2

**DOGE**: DKwBg5xx946AMYrSQL1ZecGazetWHRe7Rs 

**SHIB**: 0xA1e608C40C88B83e3325a9f25E0523BE8bC977d2 

**r/cc MOON**: 0x1a5fce959e9e6574cba3893f9d3abcc127d8c82b 
