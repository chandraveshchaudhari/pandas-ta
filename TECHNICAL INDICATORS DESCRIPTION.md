# [Indicators Description by Category]
Patterns that are **not bold**, require TA-Lib to be installed: ```pip install TA-Lib```

### [Candles](#candles-64)
  * [2crows](#2crows)
  * [3blackcrows](#3blackcrows)
  * [3inside](#3inside)
  * [3linestrike](#3linestrike)
  * [3outside](#3outside)
  * [3starsinsouth](#3starsinsouth)
  * [3whitesoldiers](#3whitesoldiers)
  * [abandonedbaby](#abandonedbaby)
  * [advanceblock](#advanceblock)
  * [belthold](#belthold)
  * [breakaway](#breakaway)
  * [closingmarubozu](#closingmarubozu)
  * [concealbabyswall](#concealbabyswall)
  * [counterattack](#counterattack)
  * [darkcloudcover](#darkcloudcover)
  * **[doji](#doji)**
  * [dojistar](#dojistar)
  * [dragonflydoji](#dragonflydoji)
  * [engulfing](#engulfing)
  * [eveningdojistar](#eveningdojistar)
  * [eveningstar](#eveningstar)
  * [gapsidesidewhite](#gapsidesidewhite)
  * [gravestonedoji](#gravestonedoji)
  * [hammer](#hammer)
  * [hangingman](#hangingman)
  * [harami](#harami)
  * [haramicross](#haramicross)
  * [highwave](#highwave)
  * [hikkake](#hikkake)
  * [hikkakemod](#hikkakemod)
  * [homingpigeon](#homingpigeon)
  * [identical3crows](#identical3crows)
  * [inneck](#inneck)
  * **[inside](#inside)**
  * [invertedhammer](#invertedhammer)
  * [kicking](#kicking)
  * [kickingbylength](#kickingbylength)
  * [ladderbottom](#ladderbottom)
  * [longleggeddoji](#longleggeddoji)
  * [longline](#longline)
  * [marubozu](#marubozu)
  * [matchinglow](#matchinglow)
  * [mathold](#mathold)
  * [morningdojistar](#morningdojistar)
  * [morningstar](#morningstar)
  * [onneck](#onneck)
  * [piercing](#piercing)
  * [rickshawman](#rickshawman)
  * [risefall3methods](#risefall3methods)
  * [separatinglines](#separatinglines)
  * [shootingstar](#shootingstar)
  * [shortline](#shortline)
  * [spinningtop](#spinningtop)
  * [stalledpattern](#stalledpattern)
  * [sticksandwich](#sticksandwich)
  * [takuri](#takuri)
  * [tasukigap](#tasukigap)
  * [thrusting](#thrusting)
  * [tristar](#tristar)
  * [unique3river](#unique3river)
  * [upsidegap2crows](#upsidegap2crows)
  * [xsidegap3methods](#xsidegap3methods)
  * _Heikin-Ashi_: **[ha](#ha)**
  * _Z Score_: **[cdl_z](#cdl_z)**

### [Cycles](#cycles-1)
  * [_Even Better Sinewave_: **ebsw**](#ebsw)
### [Momentum](#momentum-41)
  * _Awesome Oscillator_: **[ao](#ao)**
  * _Absolute Price Oscillator_: **[apo](#apo)**
  * _Bias_: **[bias](#bias)**
  * _Balance of Power_: **[bop](#bop)**
  * _BRAR_: **[brar](#brar)**
  * _Commodity Channel Index_: **[cci](#cci)**
  * _Chande Forecast Oscillator_: **[cfo](#cfo)**
  * _Center of Gravity_: **[cg](#cg)**
  * _Chande Momentum Oscillator_: **[cmo](#cmo)**
  * _Coppock Curve_: **[coppock](#coppock)**
  * _Correlation Trend Indicator_: **[cti](#cti)**
      * A wrapper for ```ta.linreg(series, r=True)```
  * _Directional Movement_: **[dm](#dm)**
  * _Efficiency Ratio_: **[er](#er)**
  * _Elder Ray Index_: **[eri](#eri)**
  * _Fisher Transform_: **[fisher](#fisher)**
  * _Inertia_: **[inertia](#inertia)**
  * _KDJ_: **[kdj](#kdj)**
  * _KST Oscillator_: **[kst](#kst)**
  * _Moving Average Convergence Divergence_: **[macd](#macd)**
  * _Momentum_: **[mom](#mom)**
  * _Pretty Good Oscillator_: **[pgo](#pgo)**
  * _Percentage Price Oscillator_: **[ppo](#ppo)**
  * _Psychological Line_: **[psl](#psl)**
  * _Percentage Volume Oscillator_: **[pvo](#pvo)**
  * _Quantitative Qualitative Estimation_: **[qqe](#qqe)**
  * _Rate of Change_: **[roc](#roc)**
  * _Relative Strength Index_: **[rsi](#rsi)**
  * _Relative Strength Xtra_: **[rsx](#rsx)**
  * _Relative Vigor Index_: **[rvgi](#rvgi)**
  * _Schaff Trend Cycle_: **[stc](#stc)**
  * _Slope_: **[slope](#slope)**
  * _SMI Ergodic_: **[smi](#smi)**
  * _Squeeze_: **[squeeze](#squeeze)**
      * Default is John Carter's. Enable Lazybear's with ```lazybear=True```
  * _Squeeze Pro_: **[squeeze_pro](#squeeze_pro)**
  * _Stochastic Oscillator_: **[stoch](#stoch)**
  * _Stochastic RSI_: **[stochrsi](#stochrsi)**
  * _TD Sequential_: **[td_seq](#td_seq)**
      * Excluded from ```df.ta.strategy()```.
  * _Trix_: **[trix](#trix)**
  * _True strength index_: **[tsi](#tsi)**
  * _Ultimate Oscillator_: **[uo](#uo)**
  * _Williams %R_: **[willr](#willr)**

### [Overlap](#overlap-33)
  * _Arnaud Legoux Moving Average_: **[alma](#alma)**
  * _Double Exponential Moving Average_: **[dema](#dema)**
  * _Exponential Moving Average_: **[ema](#ema)**
  * _Fibonacci's Weighted Moving Average_: **[fwma](#fwma)**
  * _Gann High-Low Activator_: **[hilo](#hilo)**
  * _High-Low Average_: **[hl2](#hl2)**
  * _High-Low-Close Average_: **[hlc3](#hlc3)**
      * Commonly known as 'Typical Price' in Technical Analysis literature
  * _Hull Exponential Moving Average_: **[hma](#hma)**
  * _Holt-Winter Moving Average_: **[hwma](#hwma)**
  * _Ichimoku Kinkō Hyō_: **[ichimoku](#ichimoku)**
      * Returns two DataFrames. For more information: ```help(ta.ichimoku)```.
      * ```lookahead=False``` drops the Chikou Span Column to prevent potential data leak.
  * _Jurik Moving Average_: **[jma](#jma)**
  * _Kaufman's Adaptive Moving Average_: **[kama](#kama)**
  * _Linear Regression_: **[linreg](#linreg)**
  * _McGinley Dynamic_: **[mcgd](#mcgd)**
  * _Midpoint_: **[midpoint](#midpoint)**
  * _Midprice_: **[midprice](#midprice)**
  * _Open-High-Low-Close Average_: **[ohlc4](#ohlc4)**
  * _Pascal's Weighted Moving Average_: **[pwma](#pwma)**
  * _WildeR's Moving Average_: **[rma](#rma)**
  * _Sine Weighted Moving Average_: **[sinwma](#sinwma)**
  * _Simple Moving Average_: **[sma](#sma)**
  * _Ehler's Super Smoother Filter_: **[ssf](#ssf)**
  * _Supertrend_: **[supertrend](#supertrend)**
  * _Symmetric Weighted Moving Average_: **[swma](#swma)**
  * _T3 Moving Average_: **[t3](#t3)**
  * _Triple Exponential Moving Average_: **[tema](#tema)**
  * _Triangular Moving Average_: **[trima](#trima)**
  * _Variable Index Dynamic Average_: **[vidya](#vidya)**
  * _Volume Weighted Average Price_: **[vwap](#vwap)**
      * **Requires** the DataFrame index to be a DatetimeIndex
  * _Volume Weighted Moving Average_: **[vwma](#vwma)**
  * _Weighted Closing Price_: **[wcp](#wcp)**
  * _Weighted Moving Average_: **[wma](#wma)**
  * _Zero Lag Moving Average_: **[zlma](#zlma)**

### [Performance](#performance-3)
  * _Draw Down_: **[drawdown](#drawdown)**
  * _Log Return_: **[log_return](#log_return)**
  * _Percent Return_: **[percent_return](#percent_return)**

### [Statistics](#statistics-11)
  * _Entropy_: **[entropy](#entropy)**
  * _Kurtosis_: **[kurtosis](#kurtosis)**
  * _Mean Absolute Deviation_: **[mad](#mad)**
  * _Median_: **[median](#median)**
  * _Quantile_: **[quantile](#quantile)**
  * _Skew_: **[skew](#skew)**
  * _Standard Deviation_: **[stdev](#stdev)**
  * _Think or Swim Standard Deviation All_: **[tos_stdevall](#tos_stdevall)**
  * _Variance_: **[variance](#variance)**
  * _Z Score_: **[zscore](#zscore)**

### [Trend](#trend-18)
  * _Average Directional Movement Index_: **[adx](#adx)**
    * Also includes **[dmp](#dmp)** and **[dmn](#dmn)** in the resultant DataFrame.
  * _Archer Moving Averages Trends_: **[amat](#amat)**
  * _Aroon & Aroon Oscillator_: **[aroon](#aroon)**
  * _Choppiness Index_: **[chop](#chop)**
  * _Chande Kroll Stop_: **[cksp](#cksp)**
  * _Decay_: **[decay](#decay)**
      * Formally: **[linear_decay](#linear_decay)**
  * _Decreasing_: **[decreasing](#decreasing)**
  * _Detrended Price Oscillator_: **[dpo](#dpo)**
      * Set ```lookahead=False``` to disable centering and remove potential data leak.
  * _Increasing_: **[increasing](#increasing)**
  * _Long Run_: **[long_run](#long_run)**
  * _Parabolic Stop and Reverse_: **[psar](#psar)**
  * _Q Stick_: **[qstick](#qstick)**
  * _Short Run_: **[short_run](#short_run)**
  * _Trend Signals_: **[tsignals](#tsignals)**
  * _TTM Trend_: **[ttm_trend](#ttm_trend)**
  * _Vertical Horizontal Filter_: **[vhf](#vhf)**
  * _Vortex_: **[vortex](#vortex)**
  * _Cross Signals_: **[xsignals](#xsignals)**

### [Volatility](#volatility-14)
  * _Aberration_: **[aberration](#aberration)**
  * _Acceleration Bands_: **[accbands](#accbands)**
  * _Average True Range_: **[atr](#atr)**
  * _Bollinger Bands_: **[bbands](#bbands)**
  * _Donchian Channel_: **[donchian](#donchian)**
  * _Holt-Winter Channel_: **[hwc](#hwc)**
  * _Keltner Channel_: **[kc](#kc)**
  * _Mass Index_: **[massi](#massi)**
  * _Normalized Average True Range_: **[natr](#natr)**
  * _Price Distance_: **[pdist](#pdist)**
  * _Relative Volatility Index_: **[rvi](#rvi)**
  * _Elder's Thermometer_: **[thermo](#thermo)**
  * _True Range_: **[true_range](#true_range)**
  * _Ulcer Index_: **[ui](#ui)**


### [Volume](#volume-15)
  * _Accumulation/Distribution Index_: **[ad](#ad)**
  * _Accumulation/Distribution Oscillator_: **[adosc](#adosc)**
  * _Archer On-Balance Volume_: **[aobv](#aobv)**
  * _Chaikin Money Flow_: **[cmf](#cmf)**
  * _Elder's Force Index_: **[efi](#efi)**
  * _Ease of Movement_: **[eom](#eom)**
  * _Klinger Volume Oscillator_: **[kvo](#kvo)**
  * _Money Flow Index_: **[mfi](#mfi)**
  * _Negative Volume Index_: **[nvi](#nvi)**
  * _On-Balance Volume_: **[obv](#obv)**
  * _Positive Volume Index_: **[pvi](#pvi)**
  * _Price-Volume_: **[pvol](#pvol)**
  * _Price Volume Rank_: **[pvr](#pvr)**
  * _Price Volume Trend_: **[pvt](#pvt)**
  * _Volume Profile_: **[vp](#vp)**



## **Candles** (64)

### 2crows
### 3blackcrows
### 3inside
### 3linestrike
### 3outside
### 3starsinsouth
### 3whitesoldiers
### abandonedbaby
### advanceblock
### belthold
### breakaway
### closingmarubozu
### concealbabyswall
### counterattack
### darkcloudcover
### **doji** : A candle body is Doji, when it's shorter than 10% of the
average of the 10 previous candles' high-low range.

Sources:
    TA-Lib: 96.56% Correlation

Calculation:
    Default values:
        length=10, percent=10 (0.1), scalar=100
    ABS = Absolute Value
    SMA = Simple Moving Average

    BODY = ABS(close - open)
    HL_RANGE = ABS(high - low)

    DOJI = scalar IF BODY < 0.01 * percent * SMA(HL_RANGE, length) ELSE 0

### dojistar
### dragonflydoji
### engulfing
### eveningdojistar
### eveningstar
### gapsidesidewhite
### gravestonedoji
### hammer
### hangingman
### harami
### haramicross
### highwave
### hikkake
### hikkakemod
### homingpigeon
### identical3crows
### inneck
### **inside** : An Inside Bar is a bar that is engulfed by the prior highs and lows of it's
previous bar. In other words, the current bar is smaller than it's previous bar.
Set asbool=True if you want to know if it is an Inside Bar. Note by default
asbool=False so this returns a 0 if it is not an Inside Bar, 1 if it is an
Inside Bar and close > open, and -1 if it is an Inside Bar but close < open.

Sources:
    https://www.tradingview.com/script/IyIGN1WO-Inside-Bar/

Calculation:
    Default Inputs:
        asbool=False
    inside = (high.diff() < 0) & (low.diff() > 0)

    if not asbool:
        inside *= candle_color(open_, close)
### invertedhammer
### kicking
### kickingbylength
### ladderbottom
### longleggeddoji
### longline
### marubozu
### matchinglow
### mathold
### morningdojistar
### morningstar
### onneck
### piercing
### rickshawman
### risefall3methods
### separatinglines
### shootingstar
### shortline
### spinningtop
### stalledpattern
### sticksandwich
### takuri
### tasukigap
### thrusting
### tristar
### unique3river
### upsidegap2crows
### xsidegap3methods
### _Heikin-Ashi_: **ha** : The Heikin-Ashi technique averages price data to create a Japanese
candlestick chart that filters out market noise. Heikin-Ashi charts,
developed by Munehisa Homma in the 1700s, share some characteristics
with standard candlestick charts but differ based on the values used
to create each candle. Instead of using the open, high, low, and close
like standard candlestick charts, the Heikin-Ashi technique uses a
modified formula based on two-period averages. This gives the chart a
smoother appearance, making it easier to spots trends and reversals,
but also obscures gaps and some price data.

Sources:
    https://www.investopedia.com/terms/h/heikinashi.asp
Calculation:
    HA_OPEN[0] = (open[0] + close[0]) / 2
    HA_CLOSE = (open[0] + high[0] + low[0] + close[0]) / 4

    for i > 1 in df.index:
        HA_OPEN = (HA_OPEN[i−1] + HA_CLOSE[i−1]) / 2

    HA_HIGH = MAX(HA_OPEN, HA_HIGH, HA_CLOSE)
    HA_LOW = MIN(HA_OPEN, HA_LOW, HA_CLOSE)

    How to Calculate Heikin-Ashi

    Use one period to create the first Heikin-Ashi (HA) candle, using
    the formulas. For example use the high, low, open, and close to
    create the first HA close price. Use the open and close to create
    the first HA open. The high of the period will be the first HA high,
    and the low will be the first HA low. With the first HA calculated,
    it is now possible to continue computing the HA candles per the formulas.
### _Z Score_: **cdl_z** : Normalizes OHLC Candles with a rolling Z Score.

Source: Kevin Johnson

Calculation:
    Default values:
        length=30, full=False, ddof=1
    Z = ZSCORE

    open  = Z( open, length, ddof)
    high  = Z( high, length, ddof)
    low   = Z(  low, length, ddof)
    close = Z(close, length, ddof)


### **Cycles** (1)
### _Even Better Sinewave_: **ebsw** : Even Better SineWave (EBSW) *beta*

This indicator measures market cycles and uses a low pass filter to remove noise.
Its output is bound signal between -1 and 1 and the maximum length of a detected
trend is limited by its length input.

Written by rengel8 for Pandas TA based on a publication at 'prorealcode.com' and
a book by J.F.Ehlers.

### This implementation seems to be logically limited. It would make sense to
implement exactly the version from prorealcode and compare the behaviour.


Sources:
    https://www.prorealcode.com/prorealtime-indicators/even-better-sinewave/
    J.F.Ehlers 'Cycle Analytics for Traders', 2014



### **Momentum** (41)
### _Awesome Oscillator_: **ao** : Awesome Oscillator (AO)

The Awesome Oscillator is an indicator used to measure a security's momentum.
AO is generally used to affirm trends or to anticipate possible reversals.

Sources:
    https://www.tradingview.com/wiki/Awesome_Oscillator_(AO)
    https://www.ifcm.co.uk/ntx-indicators/awesome-oscillator

Calculation:
    Default Inputs:
        fast=5, slow=34
    SMA = Simple Moving Average
    median = (high + low) / 2
    AO = SMA(median, fast) - SMA(median, slow)
### _Absolute Price Oscillator_: **apo** : Absolute Price Oscillator (APO)

The Absolute Price Oscillator is an indicator used to measure a security's
momentum.  It is simply the difference of two Exponential Moving Averages
(EMA) of two different periods. Note: APO and MACD lines are equivalent.

Sources:
    https://www.tradingtechnologies.com/xtrader-help/x-study/technical-indicator-definitions/absolute-price-oscillator-apo/

Calculation:
    Default Inputs:
        fast=12, slow=26
    SMA = Simple Moving Average
    APO = SMA(close, fast) - SMA(close, slow)
### _Bias_: **bias** : Bias (BIAS)

Rate of change between the source and a moving average.

Sources:
    Few internet resources on definitive definition.
    Request by Github user homily, issue #46

Calculation:
    Default Inputs:
        length=26, MA='sma'

    BIAS = (close - MA(close, length)) / MA(close, length)
         = (close / MA(close, length)) - 1

### _Balance of Power_: **bop** : Balance of Power (BOP)

Balance of Power measure the market strength of buyers against sellers.

Sources:
    http://www.worden.com/TeleChartHelp/Content/Indicators/Balance_of_Power.htm

Calculation:
    BOP = scalar * (close - open) / (high - low)
### _BRAR_: **brar** : BRAR (BRAR)

BR and AR

Sources:
    No internet resources on definitive definition.
    Request by Github user homily, issue #46

Calculation:
    Default Inputs:
        length=26, scalar=100
    SUM = Sum

    HO_Diff = high - open
    OL_Diff = open - low
    HCY = high - close[-1]
    CYL = close[-1] - low
    HCY[HCY < 0] = 0
    CYL[CYL < 0] = 0
    AR = scalar * SUM(HO, length) / SUM(OL, length)
    BR = scalar * SUM(HCY, length) / SUM(CYL, length)
### _Commodity Channel Index_: **cci** : Commodity Channel Index (CCI)

Commodity Channel Index is a momentum oscillator used to primarily identify
overbought and oversold levels relative to a mean.

Sources:
    https://www.tradingview.com/wiki/Commodity_Channel_Index_(CCI)

Calculation:
    Default Inputs:
        length=14, c=0.015
    SMA = Simple Moving Average
    MAD = Mean Absolute Deviation
    tp = typical_price = hlc3 = (high + low + close) / 3
    mean_tp = SMA(tp, length)
    mad_tp = MAD(tp, length)
    CCI = (tp - mean_tp) / (c * mad_tp)
### _Chande Forecast Oscillator_: **cfo** : Chande Forcast Oscillator (CFO)

The Forecast Oscillator calculates the percentage difference between the actual
price and the Time Series Forecast (the endpoint of a linear regression line).

Sources:
    https://www.fmlabs.com/reference/default.htm?url=ForecastOscillator.htm

Calculation:
    Default Inputs:
        length=9, drift=1, scalar=100
    LINREG = Linear Regression

    CFO = scalar * (close - LINERREG(length, tdf=True)) / close

### _Center of Gravity_: **cg** : Center of Gravity (CG)

The Center of Gravity Indicator by John Ehlers attempts to identify turning
points while exhibiting zero lag and smoothing.

Sources:
    http://www.mesasoftware.com/papers/TheCGOscillator.pdf

Calculation:
    Default Inputs:
        length=10

### _Chande Momentum Oscillator_: **cmo** : Chande Momentum Oscillator (CMO)

Attempts to capture the momentum of an asset with overbought at 50 and
oversold at -50.

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/chande-momentum-oscillator-cmo/
    https://www.tradingview.com/script/hdrf0fXV-Variable-Index-Dynamic-Average-VIDYA/

Calculation:
    Default Inputs:
        drift=1, scalar=100

    # Same Calculation as RSI except for this step
    CMO = scalar * (PSUM - NSUM) / (PSUM + NSUM)
### _Coppock Curve_: **coppock** : Coppock Curve (COPC)

Coppock Curve (originally called the "Trendex Model") is a momentum indicator
is designed for use on a monthly time scale.  Although designed for monthly
use, a daily calculation over the same period can be made, converting the
periods to 294-day and 231-day rate of changes, and a 210-day weighted
moving average.

Sources:
    https://en.wikipedia.org/wiki/Coppock_curve

Calculation:
    Default Inputs:
        length=10, fast=11, slow=14
    SMA = Simple Moving Average
    MAD = Mean Absolute Deviation
    tp = typical_price = hlc3 = (high + low + close) / 3
    mean_tp = SMA(tp, length)
    mad_tp = MAD(tp, length)
    CCI = (tp - mean_tp) / (c * mad_tp)
### _Correlation Trend Indicator_: **cti** :Correlation Trend Indicator (CTI)

The Correlation Trend Indicator is an oscillator created by John Ehler in 2020.
It assigns a value depending on how close prices in that range are to following
a positively- or negatively-sloping straight line. Values range from -1 to 1.
This is a wrapper for ta.linreg(close, r=True).

    * A wrapper for ```ta.linreg(series, r=True)```
### _Directional Movement_: **dm** : Directional Movement (DM)

The Directional Movement was developed by J. Welles Wilder in 1978 attempts to
determine which direction the price of an asset is moving. It compares prior
highs and lows to yield to two series +DM and -DM.

Sources:
    https://www.tradingview.com/pine-script-reference/#fun_dmi
    https://www.sierrachart.com/index.php?page=doc/StudiesReference.php&ID=24&Name=Directional_Movement_Index

Calculation:
    Default Inputs:
        length=14, mamode="rma", drift=1
            up = high - high.shift(drift)
        dn = low.shift(drift) - low

        pos_ = ((up > dn) & (up > 0)) * up
        neg_ = ((dn > up) & (dn > 0)) * dn

        pos_ = pos_.apply(zero)
        neg_ = neg_.apply(zero)

        # Not the same values as TA Lib's -+DM
        pos = ma(mamode, pos_, length=length)
        neg = ma(mamode, neg_, length=length)

### _Efficiency Ratio_: **er** : Efficiency Ratio (ER)

The Efficiency Ratio was invented by Perry J. Kaufman and presented in his book "New Trading Systems and Methods". It is designed to account for market noise or volatility.

It is calculated by dividing the net change in price movement over N periods by the sum of the absolute net changes over the same N periods.

Sources:
    https://help.tc2000.com/m/69404/l/749623-kaufman-efficiency-ratio

Calculation:
    Default Inputs:
        length=10
    ABS = Absolute Value
    EMA = Exponential Moving Average

    abs_diff = ABS(close.diff(length))
    volatility = ABS(close.diff(1))
    ER = abs_diff / SUM(volatility, length)
### _Elder Ray Index_: **eri** : Elder Ray Index (ERI)

Elder's Bulls Ray Index contains his Bull and Bear Powers. Which are useful ways
to look at the price and see the strength behind the market. Bull Power
measures the capability of buyers in the market, to lift prices above an average
consensus of value.

Bears Power measures the capability of sellers, to drag prices below an average
consensus of value. Using them in tandem with a measure of trend allows you to
identify favourable entry points. We hope you've found this to be a useful
discussion of the Bulls and Bears Power indicators.

Sources:
    https://admiralmarkets.com/education/articles/forex-indicators/bears-and-bulls-power-indicator

Calculation:
    Default Inputs:
        length=13
    EMA = Exponential Moving Average

    BULLPOWER = high - EMA(close, length)
    BEARPOWER = low - EMA(close, length)
### _Fisher Transform_: **fisher** : Fisher Transform (FISHT)

Attempts to identify significant price reversals by normalizing prices over a
user-specified number of periods. A reversal signal is suggested when the the
two lines cross.

Sources:
    TradingView (Correlation >99%)

Calculation:
    Default Inputs:
        length=9, signal=1
    HL2 = hl2(high, low)
    HHL2 = HL2.rolling(length).max()
    LHL2 = HL2.rolling(length).min()

    HLR = HHL2 - LHL2
    HLR[HLR < 0.001] = 0.001

    position = ((HL2 - LHL2) / HLR) - 0.5

    v = 0
    m = high.size
    FISHER = [npNaN for _ in range(0, length - 1)] + [0]
    for i in range(length, m):
        v = 0.66 * position[i] + 0.67 * v
        if v < -0.99: v = -0.999
        if v >  0.99: v =  0.999
        FISHER.append(0.5 * (nplog((1 + v) / (1 - v)) + FISHER[i - 1]))

    SIGNAL = FISHER.shift(signal)
### _Inertia_: **inertia** : Inertia (INERTIA)

Inertia was developed by Donald Dorsey and was introduced his article
in September, 1995. It is the Relative Vigor Index smoothed by the Least
Squares Moving Average. Postive Inertia when values are greater than 50,
Negative Inertia otherwise.

Sources:
    https://www.investopedia.com/terms/r/relative_vigor_index.asp

Calculation:
    Default Inputs:
        length=14, ma_length=20
    LSQRMA = Least Squares Moving Average

    INERTIA = LSQRMA(RVI(length), ma_length)
### _KDJ_: **kdj** : KDJ (KDJ)

The KDJ indicator is actually a derived form of the Slow
Stochastic with the only difference being an extra line
called the J line. The J line represents the divergence
of the %D value from the %K. The value of J can go
beyond [0, 100] for %K and %D lines on the chart.

Sources:
    https://www.prorealcode.com/prorealtime-indicators/kdj/
    https://docs.anychart.com/Stock_Charts/Technical_Indicators/Mathematical_Description#kdj

Calculation:
    Default Inputs:
        length=9, signal=3
    LL = low for last 9 periods
    HH = high for last 9 periods

    FAST_K = 100 * (close - LL) / (HH - LL)

    K = RMA(FAST_K, signal)
    D = RMA(K, signal)
    J = 3K - 2D

### _KST Oscillator_: **kst** : 'Know Sure Thing' (KST)

The 'Know Sure Thing' is a momentum based oscillator and based on ROC.

Sources:
    https://www.tradingview.com/wiki/Know_Sure_Thing_(KST)
    https://www.incrediblecharts.com/indicators/kst.php

Calculation:
    Default Inputs:
        roc1=10, roc2=15, roc3=20, roc4=30,
        sma1=10, sma2=10, sma3=10, sma4=15, signal=9, drift=1
    ROC = Rate of Change
    SMA = Simple Moving Average
    rocsma1 = SMA(ROC(close, roc1), sma1)
    rocsma2 = SMA(ROC(close, roc2), sma2)
    rocsma3 = SMA(ROC(close, roc3), sma3)
    rocsma4 = SMA(ROC(close, roc4), sma4)

    KST = 100 * (rocsma1 + 2 * rocsma2 + 3 * rocsma3 + 4 * rocsma4)
    KST_Signal = SMA(KST, signal)

### _Moving Average Convergence Divergence_: **macd** Moving Average Convergence Divergence (MACD)

The MACD is a popular indicator to that is used to identify a security's trend.
While APO and MACD are the same calculation, MACD also returns two more series
called Signal and Histogram. The Signal is an EMA of MACD and the Histogram is
the difference of MACD and Signal.

Sources:
    https://www.tradingview.com/wiki/MACD_(Moving_Average_Convergence/Divergence)
    AS Mode: https://tr.tradingview.com/script/YFlKXHnP/

Calculation:
    Default Inputs:
        fast=12, slow=26, signal=9
    EMA = Exponential Moving Average
    MACD = EMA(close, fast) - EMA(close, slow)
    Signal = EMA(MACD, signal)
    Histogram = MACD - Signal

    if asmode:
        MACD = MACD - Signal
        Signal = EMA(MACD, signal)
        Histogram = MACD - Signal
### _Momentum_: **mom** : Momentum (MOM)

Momentum is an indicator used to measure a security's speed (or strength) of
movement.  Or simply the change in price.

Sources:
    http://www.onlinetradingconcepts.com/TechnicalAnalysis/Momentum.html

Calculation:
    Default Inputs:
        length=1
    MOM = close.diff(length)
### _Pretty Good Oscillator_: **pgo** : Pretty Good Oscillator (PGO)

The Pretty Good Oscillator indicator was created by Mark Johnson to measure the distance of the current close from its N-day Simple Moving Average, expressed in terms of an average true range over a similar period. Johnson's approach was to
use it as a breakout system for longer term trades. Long if greater than 3.0 and
short if less than -3.0.

Sources:
    https://library.tradingtechnologies.com/trade/chrt-ti-pretty-good-oscillator.html

Calculation:
    Default Inputs:
        length=14
    ATR = Average True Range
    SMA = Simple Moving Average
    EMA = Exponential Moving Average

    PGO = (close - SMA(close, length)) / EMA(ATR(high, low, close, length), length)
### _Percentage Price Oscillator_: **ppo** : Percentage Price Oscillator (PPO)

The Percentage Price Oscillator is similar to MACD in measuring momentum.

Sources:
    https://www.tradingview.com/wiki/MACD_(Moving_Average_Convergence/Divergence)

Calculation:
    Default Inputs:
        fast=12, slow=26
    SMA = Simple Moving Average
    EMA = Exponential Moving Average
    fast_sma = SMA(close, fast)
    slow_sma = SMA(close, slow)
    PPO = 100 * (fast_sma - slow_sma) / slow_sma
    Signal = EMA(PPO, signal)
    Histogram = PPO - Signal

### _Psychological Line_: **psl** : Psychological Line (PSL)

The Psychological Line is an oscillator-type indicator that compares the
number of the rising periods to the total number of periods. In other
words, it is the percentage of bars that close above the previous
bar over a given period.

Sources:
    https://www.quantshare.com/item-851-psychological-line

Calculation:
    Default Inputs:
        length=12, scalar=100, drift=1

    IF NOT open:
        DIFF = SIGN(close - close[drift])
    ELSE:
        DIFF = SIGN(close - open)

    DIFF.fillna(0)
    DIFF[DIFF <= 0] = 0

    PSL = scalar * SUM(DIFF, length) / length

### _Percentage Volume Oscillator_: **pvo** : Percentage Volume Oscillator (PVO)

Percentage Volume Oscillator is a Momentum Oscillator for Volume.

Sources:
    https://www.fmlabs.com/reference/default.htm?url=PVO.htm

Calculation:
    Default Inputs:
        fast=12, slow=26, signal=9
    EMA = Exponential Moving Average

    PVO = (EMA(volume, fast) - EMA(volume, slow)) / EMA(volume, slow)
    Signal = EMA(PVO, signal)
    Histogram = PVO - Signal

### _Quantitative Qualitative Estimation_: **qqe** : Quantitative Qualitative Estimation (QQE)

The Quantitative Qualitative Estimation (QQE) is similar to SuperTrend but uses a Smoothed RSI with an upper and lower bands. The band width is a combination of a one period True Range of the Smoothed RSI which is double smoothed using Wilder's smoothing length (2 * rsiLength - 1) and multiplied by the default factor of 4.236. A Long trend is determined when the Smoothed RSI crosses the previous upperband and a Short trend when the Smoothed RSI crosses the previous lowerband.

Based on QQE.mq5 by EarnForex Copyright © 2010, based on version by Tim Hyder (2008), based on version by Roman Ignatov (2006)

Sources:
    https://www.tradingview.com/script/IYfA9R2k-QQE-MT4/
    https://www.tradingpedia.com/forex-trading-indicators/quantitative-qualitative-estimation
    https://www.prorealcode.com/prorealtime-indicators/qqe-quantitative-qualitative-estimation/

Calculation:
    Default Inputs:
        length=14, smooth=5, factor=4.236, mamode="ema", drift=1
### _Rate of Change_: **roc** : Rate of Change (ROC)

Rate of Change is an indicator is also referred to as Momentum (yeah, confusingly).
It is a pure momentum oscillator that measures the percent change in price with the
previous price 'n' (or length) periods ago.

Sources:
    https://www.tradingview.com/wiki/Rate_of_Change_(ROC)

Calculation:
    Default Inputs:
        length=1
    MOM = Momentum
    ROC = 100 * MOM(close, length) / close.shift(length)
### _Relative Strength Index_: **rsi** : Relative Strength Index (RSI)

The Relative Strength Index is popular momentum oscillator used to measure the
velocity as well as the magnitude of directional price movements.

Sources:
    https://www.tradingview.com/wiki/Relative_Strength_Index_(RSI)

Calculation:
    Default Inputs:
        length=14, scalar=100, drift=1
    ABS = Absolute Value
    RMA = Rolling Moving Average

    diff = close.diff(drift)
    positive = diff if diff > 0 else 0
    negative = diff if diff < 0 else 0

    pos_avg = RMA(positive, length)
    neg_avg = ABS(RMA(negative, length))

    RSI = scalar * pos_avg / (pos_avg + neg_avg)

### _Relative Strength Xtra_: **rsx** : Relative Strength Xtra (rsx)

The Relative Strength Xtra is based on the popular RSI indicator and inspired
by the work Jurik Research. The code implemented is based on published code
found at 'prorealcode.com'. This enhanced version of the rsi reduces noise and
provides a clearer, only slightly delayed insight on momentum and velocity of
price movements.

Sources:
    http://www.jurikres.com/catalog1/ms_rsx.htm
    https://www.prorealcode.com/prorealtime-indicators/jurik-rsx/

Calculation:
    Refer to the sources above for information as well as code example.
### _Relative Vigor Index_: **rvgi** : Relative Vigor Index (RVGI)

The Relative Vigor Index attempts to measure the strength of a trend relative to
its closing price to its trading range.  It is based on the belief that it tends
to close higher than they open in uptrends or close lower than they open in
downtrends.

Sources:
    https://www.investopedia.com/terms/r/relative_vigor_index.asp

Calculation:
    Default Inputs:
        length=14, swma_length=4
    SWMA = Symmetrically Weighted Moving Average
    numerator = SUM(SWMA(close - open, swma_length), length)
    denominator = SUM(SWMA(high - low, swma_length), length)
    RVGI = numerator / denominator
### _Schaff Trend Cycle_: **stc** : Schaff Trend Cycle (STC)

The Schaff Trend Cycle is an evolution of the popular MACD incorportating two
cascaded stochastic calculations with additional smoothing.

The STC returns also the beginning MACD result as well as the result after the
first stochastic including its smoothing. This implementation has been extended
for Pandas TA to also allow for separatly feeding any other two moving Averages
(as ma1 and ma2) or to skip this to feed an oscillator (osc), based on which the
Schaff Trend Cycle should be calculated.

Feed external moving averages:
Internally calculation..
    stc = ta.stc(close=df["close"], tclen=stc_tclen, fast=ma1_interval, slow=ma2_interval, factor=stc_factor)
becomes..
    extMa1 = df.ta.zlma(close=df["close"], length=ma1_interval, append=True)
    extMa2 = df.ta.ema(close=df["close"], length=ma2_interval, append=True)
    stc = ta.stc(close=df["close"], tclen=stc_tclen, ma1=extMa1, ma2=extMa2, factor=stc_factor)

The same goes for osc=, which allows the input of an externally calculated oscillator, overriding ma1 & ma2.


Sources:
    Implemented by rengel8 based on work found here:
    https://www.prorealcode.com/prorealtime-indicators/schaff-trend-cycle2/

Calculation:
    STCmacd = Moving Average Convergance/Divergance or Oscillator
    STCstoch = Intermediate Stochastic of MACD/Osc.
    2nd Stochastic including filtering with results in the
    STC = Schaff Trend Cycle

### _Slope_: **slope** : Slope

Returns the slope of a series of length n. Can convert the slope to angle.
Default: slope.

Sources: Algebra I

Calculation:
    Default Inputs:
        length=1
    slope = close.diff(length) / length

    if as_angle:
        slope = slope.apply(atan)
        if to_degrees:
            slope *= 180 / PI
### _SMI Ergodic_ **smi** : SMI Ergodic Indicator (SMI)

The SMI Ergodic Indicator is the same as the True Strength Index (TSI) developed
by William Blau, except the SMI includes a signal line. The SMI uses double
moving averages of price minus previous price over 2 time frames. The signal
line, which is an EMA of the SMI, is plotted to help trigger trading signals.
The trend is bullish when crossing above zero and bearish when crossing below
zero. This implementation includes both the SMI Ergodic Indicator and SMI
Ergodic Oscillator.

Sources:
    https://www.motivewave.com/studies/smi_ergodic_indicator.htm
    https://www.tradingview.com/script/Xh5Q0une-SMI-Ergodic-Oscillator/
    https://www.tradingview.com/script/cwrgy4fw-SMIIO/

Calculation:
    Default Inputs:
        fast=5, slow=20, signal=5
    TSI = True Strength Index
    EMA = Exponential Moving Average

    ERG = TSI(close, fast, slow)
    Signal = EMA(ERG, signal)
    OSC = ERG - Signal
### _Squeeze_: **squeeze** : Squeeze (SQZ)

The default is based on John Carter's "TTM Squeeze" indicator, as discussed
in his book "Mastering the Trade" (chapter 11). The Squeeze indicator attempts
to capture the relationship between two studies: Bollinger Bands® and Keltner's
Channels. When the volatility increases, so does the distance between the bands,
conversely, when the volatility declines, the distance also decreases. It finds
sections of the Bollinger Bands® study which fall inside the Keltner's Channels.

Sources:
    https://tradestation.tradingappstore.com/products/TTMSqueeze
    https://www.tradingview.com/scripts/lazybear/
    https://tlc.thinkorswim.com/center/reference/Tech-Indicators/studies-library/T-U/TTM-Squeeze

Calculation:
    Default Inputs:
        bb_length=20, bb_std=2, kc_length=20, kc_scalar=1.5, mom_length=12,
        mom_smooth=12, tr=True, lazybear=False,
    BB = Bollinger Bands
    KC = Keltner Channels
    MOM = Momentum
    SMA = Simple Moving Average
    EMA = Exponential Moving Average
    TR = True Range

    RANGE = TR(high, low, close) if using_tr else high - low
    BB_LOW, BB_MID, BB_HIGH = BB(close, bb_length, std=bb_std)
    KC_LOW, KC_MID, KC_HIGH = KC(high, low, close, kc_length, kc_scalar, TR)

    if lazybear:
        HH = high.rolling(kc_length).max()
        LL = low.rolling(kc_length).min()
        AVG  = 0.25 * (HH + LL) + 0.5 * KC_MID
        SQZ = linreg(close - AVG, kc_length)
    else:
        MOMO = MOM(close, mom_length)
        if mamode == "ema":
            SQZ = EMA(MOMO, mom_smooth)
        else:
            SQZ = EMA(momo, mom_smooth)

    SQZ_ON  = (BB_LOW > KC_LOW) and (BB_HIGH < KC_HIGH)
    SQZ_OFF = (BB_LOW < KC_LOW) and (BB_HIGH > KC_HIGH)
    NO_SQZ = !SQZ_ON and !SQZ_OFF
    * Default is John Carter's. Enable Lazybear's with ```lazybear=True```
### _Squeeze Pro_: **squeeze_pro** : Squeeze PRO(SQZPRO)

This indicator is an extended version of "TTM Squeeze" from John Carter.
The default is based on John Carter's "TTM Squeeze" indicator, as discussed
in his book "Mastering the Trade" (chapter 11). The Squeeze indicator attempts
to capture the relationship between two studies: Bollinger Bands® and Keltner's
Channels. When the volatility increases, so does the distance between the bands,
conversely, when the volatility declines, the distance also decreases. It finds
sections of the Bollinger Bands® study which fall inside the Keltner's Channels.

Sources:
    https://usethinkscript.com/threads/john-carters-squeeze-pro-indicator-for-thinkorswim-free.4021/
    https://www.tradingview.com/script/TAAt6eRX-Squeeze-PRO-Indicator-Makit0/

Calculation:
    Default Inputs:
        bb_length=20, bb_std=2, kc_length=20, kc_scalar_wide=2,
        kc_scalar_normal=1.5, kc_scalar_narrow=1, mom_length=12,
        mom_smooth=6, tr=True,
    BB = Bollinger Bands
    KC = Keltner Channels
    MOM = Momentum
    SMA = Simple Moving Average
    EMA = Exponential Moving Average
    TR = True Range

    RANGE = TR(high, low, close) if using_tr else high - low
    BB_LOW, BB_MID, BB_HIGH = BB(close, bb_length, std=bb_std)
    KC_LOW_WIDE, KC_MID_WIDE, KC_HIGH_WIDE = KC(high, low, close, kc_length, kc_scalar_wide, TR)
    KC_LOW_NORMAL, KC_MID_NORMAL, KC_HIGH_NORMAL = KC(high, low, close, kc_length, kc_scalar_normal, TR)
    KC_LOW_NARROW, KC_MID_NARROW, KC_HIGH_NARROW = KC(high, low, close, kc_length, kc_scalar_narrow, TR)

    MOMO = MOM(close, mom_length)
    if mamode == "ema":
        SQZPRO = EMA(MOMO, mom_smooth)
    else:
        SQZPRO = EMA(momo, mom_smooth)

    SQZPRO_ON_WIDE  = (BB_LOW > KC_LOW_WIDE) and (BB_HIGH < KC_HIGH_WIDE)
    SQZPRO_ON_NORMAL  = (BB_LOW > KC_LOW_NORMAL) and (BB_HIGH < KC_HIGH_NORMAL)
    SQZPRO_ON_NARROW  = (BB_LOW > KC_LOW_NARROW) and (BB_HIGH < KC_HIGH_NARROW)
    SQZPRO_OFF_WIDE = (BB_LOW < KC_LOW_WIDE) and (BB_HIGH > KC_HIGH_WIDE)
    SQZPRO_NO = !SQZ_ON_WIDE and !SQZ_OFF_WIDE
### _Stochastic Oscillator_: **stoch** : Stochastic (STOCH)

The Stochastic Oscillator (STOCH) was developed by George Lane in the 1950's.
He believed this indicator was a good way to measure momentum because changes in
momentum precede changes in price.

It is a range-bound oscillator with two lines moving between 0 and 100.
The first line (%K) displays the current close in relation to the period's
high/low range. The second line (%D) is a Simple Moving Average of the %K line.
The most common choices are a 14 period %K and a 3 period SMA for %D.

Sources:
    https://www.tradingview.com/wiki/Stochastic_(STOCH)
    https://www.sierrachart.com/index.php?page=doc/StudiesReference.php&ID=332&Name=KD_-_Slow

Calculation:
    Default Inputs:
        k=14, d=3, smooth_k=3
    SMA = Simple Moving Average
    LL  = low for last k periods
    HH  = high for last k periods

    STOCH = 100 * (close - LL) / (HH - LL)
    STOCHk = SMA(STOCH, smooth_k)
    STOCHd = SMA(FASTK, d)

### _Stochastic RSI_: **stochrsi** : Stochastic (STOCHRSI)

"Stochastic RSI and Dynamic Momentum Index" was created by Tushar Chande and Stanley Kroll and published in Stock & Commodities V.11:5 (189-199)

It is a range-bound oscillator with two lines moving between 0 and 100.
The first line (%K) displays the current RSI in relation to the period's
high/low range. The second line (%D) is a Simple Moving Average of the %K line.
The most common choices are a 14 period %K and a 3 period SMA for %D.

Sources:
    https://www.tradingview.com/wiki/Stochastic_(STOCH)

Calculation:
    Default Inputs:
        length=14, rsi_length=14, k=3, d=3
    RSI = Relative Strength Index
    SMA = Simple Moving Average

    RSI = RSI(high, low, close, rsi_length)
    LL  = lowest RSI for last rsi_length periods
    HH  = highest RSI for last rsi_length periods

    STOCHRSI  = 100 * (RSI - LL) / (HH - LL)
    STOCHRSIk = SMA(STOCHRSI, k)
    STOCHRSId = SMA(STOCHRSIk, d)
### _TD Sequential_: **td_seq** : TD Sequential (TD_SEQ)

Tom DeMark's Sequential indicator attempts to identify a price point where an
uptrend or a downtrend exhausts itself and reverses.

Sources:
    https://tradetrekker.wordpress.com/tdsequential/

Calculation:
    Compare current close price with 4 days ago price, up to 13 days. For the
    consecutive ascending or descending price sequence, display 6th to 9th day
    value.
    * Excluded from ```df.ta.strategy()```.
### _Trix_: **trix** : Trix (TRIX)

TRIX is a momentum oscillator to identify divergences.

Sources:
    https://www.tradingview.com/wiki/TRIX

Calculation:
    Default Inputs:
        length=18, drift=1
    EMA = Exponential Moving Average
    ROC = Rate of Change
    ema1 = EMA(close, length)
    ema2 = EMA(ema1, length)
    ema3 = EMA(ema2, length)
    TRIX = 100 * ROC(ema3, drift)
### _True strength index_: **tsi** : True Strength Index (TSI)

The True Strength Index is a momentum indicator used to identify short-term
swings while in the direction of the trend as well as determining overbought
and oversold conditions.

Sources:
    https://www.investopedia.com/terms/t/tsi.asp

Calculation:
    Default Inputs:
        fast=13, slow=25, signal=13, scalar=100, drift=1
    EMA = Exponential Moving Average
    diff = close.diff(drift)

    slow_ema = EMA(diff, slow)
    fast_slow_ema = EMA(slow_ema, slow)

    abs_diff_slow_ema = absolute_diff_ema = EMA(ABS(diff), slow)
    abema = abs_diff_fast_slow_ema = EMA(abs_diff_slow_ema, fast)

    TSI = scalar * fast_slow_ema / abema
    Signal = EMA(TSI, signal)
### _Ultimate Oscillator_: **uo** : Ultimate Oscillator (UO)

The Ultimate Oscillator is a momentum indicator over three different
periods.  It attempts to correct false divergence trading signals.

Sources:
    https://www.tradingview.com/wiki/Ultimate_Oscillator_(UO)

Calculation:
    Default Inputs:
        fast=7, medium=14, slow=28,
        fast_w=4.0, medium_w=2.0, slow_w=1.0, drift=1
    min_low_or_pc  = close.shift(drift).combine(low, min)
    max_high_or_pc = close.shift(drift).combine(high, max)

    bp = buying pressure = close - min_low_or_pc
    tr = true range = max_high_or_pc - min_low_or_pc

    fast_avg = SUM(bp, fast) / SUM(tr, fast)
    medium_avg = SUM(bp, medium) / SUM(tr, medium)
    slow_avg = SUM(bp, slow) / SUM(tr, slow)

    total_weight = fast_w + medium_w + slow_w
    weights = (fast_w * fast_avg) + (medium_w * medium_avg) + (slow_w * slow_avg)
    UO = 100 * weights / total_weight

### _Williams %R_: **willr** : William's Percent R (WILLR)

William's Percent R is a momentum oscillator similar to the RSI that
attempts to identify overbought and oversold conditions.

Sources:
    https://www.tradingview.com/wiki/Williams_%25R_(%25R)

Calculation:
    Default Inputs:
        length=20
    LL = low.rolling(length).min()
    HH = high.rolling(length).max()

    WILLR = 100 * ((close - LL) / (HH - LL) - 1)


| _Moving Average Convergence Divergence_ (MACD) |
|:--------:|
| ![Example MACD](/images/SPY_MACD.png) |

<br/>

### **Overlap** (33)

### _Arnaud Legoux Moving Average_: **alma** : Arnaud Legoux Moving Average (ALMA)

The ALMA moving average uses the curve of the Normal (Gauss) distribution, which
can be shifted from 0 to 1. This allows regulating the smoothness and high
sensitivity of the indicator. Sigma is another parameter that is responsible for
the shape of the curve coefficients. This moving average reduces lag of the data
in conjunction with smoothing to reduce noise.

Implemented for Pandas TA by rengel8 based on the source provided below.

Sources:
    https://www.prorealcode.com/prorealtime-indicators/alma-arnaud-legoux-moving-average/
### _Double Exponential Moving Average_: **dema** : Double Exponential Moving Average (DEMA)

The Double Exponential Moving Average attempts to a smoother average with less
lag than the normal Exponential Moving Average (EMA).

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/double-exponential-moving-average-dema/

Calculation:
    Default Inputs:
        length=10
    EMA = Exponential Moving Average
    ema1 = EMA(close, length)
    ema2 = EMA(ema1, length)

    DEMA = 2 * ema1 - ema2

### _Exponential Moving Average_: **ema** : Exponential Moving Average (EMA)

The Exponential Moving Average is more responsive moving average compared to the
Simple Moving Average (SMA).  The weights are determined by alpha which is
proportional to it's length.  There are several different methods of calculating
EMA.  One method uses just the standard definition of EMA and another uses the
SMA to generate the initial value for the rest of the calculation.

Sources:
    https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_averages
    https://www.investopedia.com/ask/answers/122314/what-exponential-moving-average-ema-formula-and-how-ema-calculated.asp

Calculation:
    Default Inputs:
        length=10, adjust=False, sma=True
    if sma:
        sma_nth = close[0:length].sum() / length
        close[:length - 1] = np.NaN
        close.iloc[length - 1] = sma_nth
    EMA = close.ewm(span=length, adjust=adjust).mean()
### _Fibonacci's Weighted Moving Average_: **fwma** : Fibonacci's Weighted Moving Average (FWMA)

Fibonacci's Weighted Moving Average is similar to a Weighted Moving Average
(WMA) where the weights are based on the Fibonacci Sequence.

Source: Kevin Johnson

Calculation:
    Default Inputs:
        length=10,

    def weights(w):
        def _compute(x):
            return np.dot(w * x)
        return _compute

    fibs = utils.fibonacci(length - 1)
    FWMA = close.rolling(length)_.apply(weights(fibs), raw=True)

### _Gann High-Low Activator_: **hilo** : Gann HiLo Activator(HiLo)

The Gann High Low Activator Indicator was created by Robert Krausz in a 1998
issue of Stocks & Commodities Magazine. It is a moving average based trend
indicator consisting of two different simple moving averages.

The indicator tracks both curves (of the highs and the lows). The close of the
bar defines which of the two gets plotted.

Increasing high_length and decreasing low_length better for short trades,
vice versa for long positions.

Sources:
    https://www.sierrachart.com/index.php?page=doc/StudiesReference.php&ID=447&Name=Gann_HiLo_Activator
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/simple-moving-average-sma/
    https://www.tradingview.com/script/XNQSLIYb-Gann-High-Low/

Calculation:
    Default Inputs:
        high_length=13, low_length=21, mamode="sma"
    EMA = Exponential Moving Average
    HMA = Hull Moving Average
    SMA = Simple Moving Average # Default

    if "ema":
        high_ma = EMA(high, high_length)
        low_ma = EMA(low, low_length)
    elif "hma":
        high_ma = HMA(high, high_length)
        low_ma = HMA(low, low_length)
    else: # "sma"
        high_ma = SMA(high, high_length)
        low_ma = SMA(low, low_length)

    # Similar to Supertrend MA selection
    hilo = Series(npNaN, index=close.index)
    for i in range(1, m):
        if close.iloc[i] > high_ma.iloc[i - 1]:
            hilo.iloc[i] = low_ma.iloc[i]
        elif close.iloc[i] < low_ma.iloc[i - 1]:
            hilo.iloc[i] = high_ma.iloc[i]
        else:
            hilo.iloc[i] = hilo.iloc[i - 1]
### _High-Low Average_: **hl2**
### _High-Low-Close Average_: **hlc3**
    * Commonly known as 'Typical Price' in Technical Analysis literature
### _Hull Exponential Moving Average_: **hma** : Hull Moving Average (HMA)

The Hull Exponential Moving Average attempts to reduce or remove lag in moving
averages.

Sources:
    https://alanhull.com/hull-moving-average

Calculation:
    Default Inputs:
        length=10
    WMA = Weighted Moving Average
    half_length = int(0.5 * length)
    sqrt_length = int(sqrt(length))

    wmaf = WMA(close, half_length)
    wmas = WMA(close, length)
    HMA = WMA(2 * wmaf - wmas, sqrt_length)
### _Holt-Winter Moving Average_: **hwma** : HWMA (Holt-Winter Moving Average)

Indicator HWMA (Holt-Winter Moving Average) is a three-parameter moving average
by the Holt-Winter method; the three parameters should be selected to obtain a
forecast.

This version has been implemented for Pandas TA by rengel8 based
on a publication for MetaTrader 5.

Sources:
    https://www.mql5.com/en/code/20856

Calculation:
    HWMA[i] = F[i] + V[i] + 0.5 * A[i]
    where..
    F[i] = (1-na) * (F[i-1] + V[i-1] + 0.5 * A[i-1]) + na * Price[i]
    V[i] = (1-nb) * (V[i-1] + A[i-1]) + nb * (F[i] - F[i-1])
    A[i] = (1-nc) * A[i-1] + nc * (V[i] - V[i-1])
### _Ichimoku Kinkō Hyō_: **ichimoku** : Ichimoku Kinkō Hyō (ichimoku)

Developed Pre WWII as a forecasting model for financial markets.

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/ichimoku-ich/

Calculation:
    Default Inputs:
        tenkan=9, kijun=26, senkou=52
    MIDPRICE = Midprice
    TENKAN_SEN = MIDPRICE(high, low, close, length=tenkan)
    KIJUN_SEN = MIDPRICE(high, low, close, length=kijun)
    CHIKOU_SPAN = close.shift(-kijun)

    SPAN_A = 0.5 * (TENKAN_SEN + KIJUN_SEN)
    SPAN_A = SPAN_A.shift(kijun)

    SPAN_B = MIDPRICE(high, low, close, length=senkou)
    SPAN_B = SPAN_B.shift(kijun)
    * Returns two DataFrames. For more information: ```help(ta.ichimoku)```.
    * ```lookahead=False``` drops the Chikou Span Column to prevent potential data leak.
### _Jurik Moving Average_: **jma** : Jurik Moving Average Average (JMA)

Mark Jurik's Moving Average (JMA) attempts to eliminate noise to see the "true"
underlying activity. It has extremely low lag, is very smooth and is responsive
to market gaps.

Sources:
    https://c.mql5.com/forextsd/forum/164/jurik_1.pdf
    https://www.prorealcode.com/prorealtime-indicators/jurik-volatility-bands/

Calculation:
    Default Inputs:
        length=7, phase=0
### _Kaufman's Adaptive Moving Average_: **kama** : Kaufman's Adaptive Moving Average (KAMA)

Developed by Perry Kaufman, Kaufman's Adaptive Moving Average (KAMA) is a moving average
designed to account for market noise or volatility. KAMA will closely follow prices when
the price swings are relatively small and the noise is low. KAMA will adjust when the
price swings widen and follow prices from a greater distance. This trend-following indicator
can be used to identify the overall trend, time turning points and filter price movements.

Sources:
    https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:kaufman_s_adaptive_moving_average
    https://www.tradingview.com/script/wZGOIz9r-REPOST-Indicators-3-Different-Adaptive-Moving-Averages/

Calculation:
    Default Inputs:
        length=10
### _Linear Regression_: **linreg** : Linear Regression Moving Average (linreg)

Linear Regression Moving Average (LINREG). This is a simplified version of a
Standard Linear Regression. LINREG is a rolling regression of one variable. A
Standard Linear Regression is between two or more variables.

Source: TA Lib

Calculation:
    Default Inputs:
        length=14
    x = [1, 2, ..., n]
    x_sum = 0.5 * length * (length + 1)
    x2_sum = length * (length + 1) * (2 * length + 1) / 6
    divisor = length * x2_sum - x_sum * x_sum

    lr(series):
        y_sum = series.sum()
        y2_sum = (series* series).sum()
        xy_sum = (x * series).sum()

        m = (length * xy_sum - x_sum * y_sum) / divisor
        b = (y_sum * x2_sum - x_sum * xy_sum) / divisor
        return m * (length - 1) + b

    linreg = close.rolling(length).apply(lr)
### _McGinley Dynamic_: **mcgd** : McGinley Dynamic Indicator

The McGinley Dynamic looks like a moving average line, yet it is actually a
smoothing mechanism for prices that minimizes price separation, price whipsaws,
and hugs prices much more closely. Because of the calculation, the Dynamic Line
speeds up in down markets as it follows prices yet moves more slowly in up
markets. The indicator was designed by John R. McGinley, a Certified Market
Technician and former editor of the Market Technicians Association's Journal
of Technical Analysis.

Sources:
    https://www.investopedia.com/articles/forex/09/mcginley-dynamic-indicator.asp

Calculation:
    Default Inputs:
        length=10
        offset=0
        c=1

    def mcg_(series):
        denom = (constant * length * (series.iloc[1] / series.iloc[0]) ** 4)
        series.iloc[1] = (series.iloc[0] + ((series.iloc[1] - series.iloc[0]) / denom))
        return series.iloc[1]
    mcg_cell = close[0:].rolling(2, min_periods=2).apply(mcg_, raw=False)
    mcg_ds = close[:1].append(mcg_cell[1:])
### _Midpoint_: **midpoint**
### _Midprice_: **midprice**
### _Open-High-Low-Close Average_: **ohlc4**
### _Pascal's Weighted Moving Average_: **pwma** : Pascal's Weighted Moving Average (PWMA)

Pascal's Weighted Moving Average is similar to a symmetric triangular window
except PWMA's weights are based on Pascal's Triangle.

Source: Kevin Johnson

Calculation:
    Default Inputs:
        length=10

    def weights(w):
        def _compute(x):
            return np.dot(w * x)
        return _compute

    triangle = utils.pascals_triangle(length + 1)
    PWMA = close.rolling(length)_.apply(weights(triangle), raw=True)
### _WildeR's Moving Average_: **rma** : wildeR's Moving Average (RMA)

The WildeR's Moving Average is simply an Exponential Moving Average (EMA) with
a modified alpha = 1 / length.

Sources:
    https://tlc.thinkorswim.com/center/reference/Tech-Indicators/studies-library/V-Z/WildersSmoothing
    https://www.incrediblecharts.com/indicators/wilder_moving_average.php

Calculation:
    Default Inputs:
        length=10
    EMA = Exponential Moving Average
    alpha = 1 / length
    RMA = EMA(close, alpha=alpha)
### _Sine Weighted Moving Average_: **sinwma** : Sine Weighted Moving Average (SWMA)

A weighted average using sine cycles. The middle term(s) of the average have the
highest weight(s).

Source:
    https://www.tradingview.com/script/6MWFvnPO-Sine-Weighted-Moving-Average/
    Author: Everget (https://www.tradingview.com/u/everget/)

Calculation:
    Default Inputs:
        length=10

    def weights(w):
        def _compute(x):
            return np.dot(w * x)
        return _compute

    sines = Series([sin((i + 1) * pi / (length + 1)) for i in range(0, length)])
    w = sines / sines.sum()
    SINWMA = close.rolling(length, min_periods=length).apply(weights(w), raw=True)

### _Simple Moving Average_: **sma** : Simple Moving Average (SMA)

The Simple Moving Average is the classic moving average that is the equally
weighted average over n periods.

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/simple-moving-average-sma/

Calculation:
    Default Inputs:
        length=10
    SMA = SUM(close, length) / length
### _Ehler's Super Smoother Filter_: **ssf** : Ehler's Super Smoother Filter (SSF) © 2013

John F. Ehlers's solution to reduce lag and remove aliasing noise with his
research in aerospace analog filter design. This indicator comes with two
versions determined by the keyword poles. By default, it uses two poles but
there is an option for three poles. Since SSF is a (Resursive) Digital Filter,
the number of poles determine how many prior recursive SSF bars to include in
the design of the filter. So two poles uses two prior SSF bars and three poles
uses three prior SSF bars for their filter calculations.

Sources:
    http://www.stockspotter.com/files/PredictiveIndicators.pdf
    https://www.tradingview.com/script/VdJy0yBJ-Ehlers-Super-Smoother-Filter/
    https://www.mql5.com/en/code/588
    https://www.mql5.com/en/code/589

Calculation:
    Default Inputs:
        length=10, poles=[2, 3]

### _Supertrend_: **supertrend** : Supertrend (supertrend)

Supertrend is an overlap indicator. It is used to help identify trend
direction, setting stop loss, identify support and resistance, and/or
generate buy & sell signals.

Sources:
    http://www.freebsensetips.com/blog/detail/7/What-is-supertrend-indicator-its-calculation

Calculation:
    Default Inputs:
        length=7, multiplier=3.0
    Default Direction:
	Set to +1 or bullish trend at start

    MID = multiplier * ATR
    LOWERBAND = HL2 - MID
    UPPERBAND = HL2 + MID

    if UPPERBAND[i] < FINAL_UPPERBAND[i-1] and close[i-1] > FINAL_UPPERBAND[i-1]:
        FINAL_UPPERBAND[i] = UPPERBAND[i]
    else:
        FINAL_UPPERBAND[i] = FINAL_UPPERBAND[i-1])

    if LOWERBAND[i] > FINAL_LOWERBAND[i-1] and close[i-1] < FINAL_LOWERBAND[i-1]:
        FINAL_LOWERBAND[i] = LOWERBAND[i]
    else:
        FINAL_LOWERBAND[i] = FINAL_LOWERBAND[i-1])

    if close[i] <= FINAL_UPPERBAND[i]:
        SUPERTREND[i] = FINAL_UPPERBAND[i]
    else:
        SUPERTREND[i] = FINAL_LOWERBAND[i]
### _Symmetric Weighted Moving Average_: **swma** : Symmetric Weighted Moving Average (SWMA)

Symmetric Weighted Moving Average where weights are based on a symmetric
triangle.  For example: n=3 -> [1, 2, 1], n=4 -> [1, 2, 2, 1], etc...
This moving average has variable length in contrast to TradingView's fixed
length of 4.

Source:
    https://www.tradingview.com/study-script-reference/#fun_swma

Calculation:
    Default Inputs:
        length=10

    def weights(w):
        def _compute(x):
            return np.dot(w * x)
        return _compute

    triangle = utils.symmetric_triangle(length - 1)
    SWMA = close.rolling(length)_.apply(weights(triangle), raw=True)
### _T3 Moving Average_: **t3** : Tim Tillson's T3 Moving Average (T3)

Tim Tillson's T3 Moving Average is considered a smoother and more responsive
moving average relative to other moving averages.

Sources:
    http://www.binarytribune.com/forex-trading-indicators/t3-moving-average-indicator/

Calculation:
    Default Inputs:
        length=10, a=0.7
    c1 = -a^3
    c2 = 3a^2 + 3a^3 = 3a^2 * (1 + a)
    c3 = -6a^2 - 3a - 3a^3
    c4 = a^3 + 3a^2 + 3a + 1

    ema1 = EMA(close, length)
    ema2 = EMA(ema1, length)
    ema3 = EMA(ema2, length)
    ema4 = EMA(ema3, length)
    ema5 = EMA(ema4, length)
    ema6 = EMA(ema5, length)
    T3 = c1 * ema6 + c2 * ema5 + c3 * ema4 + c4 * ema3
### _Triple Exponential Moving Average_: **tema** : Triple Exponential Moving Average (TEMA)

A less laggy Exponential Moving Average.

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/triple-exponential-moving-average-tema/

Calculation:
    Default Inputs:
        length=10
    EMA = Exponential Moving Average
    ema1 = EMA(close, length)
    ema2 = EMA(ema1, length)
    ema3 = EMA(ema2, length)
    TEMA = 3 * (ema1 - ema2) + ema3
### _Triangular Moving Average_: **trima** : Triangular Moving Average (TRIMA)

A weighted moving average where the shape of the weights are triangular and the
greatest weight is in the middle of the period.

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/triangular-moving-average-trima/
    tma = sma(sma(src, ceil(length / 2)), floor(length / 2) + 1)  # Tradingview
    trima = sma(sma(x, n), n)  # Tradingview

Calculation:
    Default Inputs:
        length=10
    SMA = Simple Moving Average
    half_length = round(0.5 * (length + 1))
    SMA1 = SMA(close, half_length)
    TRIMA = SMA(SMA1, half_length)
### _Variable Index Dynamic Average_: **vidya** : Variable Index Dynamic Average (VIDYA)

Variable Index Dynamic Average (VIDYA) was developed by Tushar Chande. It is
similar to an Exponential Moving Average but it has a dynamically adjusted
lookback period dependent on relative price volatility as measured by Chande
Momentum Oscillator (CMO). When volatility is high, VIDYA reacts faster to
price changes. It is often used as moving average or trend identifier.

Sources:
    https://www.tradingview.com/script/hdrf0fXV-Variable-Index-Dynamic-Average-VIDYA/
    https://www.perfecttrendsystem.com/blog_mt4_2/en/vidya-indicator-for-mt4

Calculation:
    Default Inputs:
        length=10, adjust=False, sma=True
    if sma:
        sma_nth = close[0:length].sum() / length
        close[:length - 1] = np.NaN
        close.iloc[length - 1] = sma_nth
    EMA = close.ewm(span=length, adjust=adjust).mean()
### _Volume Weighted Average Price_: **vwap** : Volume Weighted Average Price (VWAP)

The Volume Weighted Average Price that measures the average typical price
by volume.  It is typically used with intraday charts to identify general
direction.

Sources:
    https://www.tradingview.com/wiki/Volume_Weighted_Average_Price_(VWAP)
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/volume-weighted-average-price-vwap/
    https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:vwap_intraday

Calculation:
    tp = typical_price = hlc3(high, low, close)
    tpv = tp * volume
    VWAP = tpv.cumsum() / volume.cumsum()
    * **Requires** the DataFrame index to be a DatetimeIndex
### _Volume Weighted Moving Average_: **vwma** : Volume Weighted Moving Average (VWMA)

Volume Weighted Moving Average.

Sources:
    https://www.motivewave.com/studies/volume_weighted_moving_average.htm

Calculation:
    Default Inputs:
        length=10
    SMA = Simple Moving Average
    pv = close * volume
    VWMA = SMA(pv, length) / SMA(volume, length)
### _Weighted Closing Price_: **wcp** : Weighted Closing Price (WCP)

Weighted Closing Price is the weighted price given: high, low
and double the close.

Sources:
    https://www.fmlabs.com/reference/default.htm?url=WeightedCloses.htm

Calculation:
    WCP = (2 * close + high + low) / 4

### _Weighted Moving Average_: **wma** : Weighted Moving Average (WMA)

The Weighted Moving Average where the weights are linearly increasing and
the most recent data has the heaviest weight.

Sources:
    https://en.wikipedia.org/wiki/Moving_average#Weighted_moving_average

Calculation:
    Default Inputs:
        length=10, asc=True
    total_weight = 0.5 * length * (length + 1)
    weights_ = [1, 2, ..., length + 1]  # Ascending
    weights = weights if asc else weights[::-1]

    def linear_weights(w):
        def _compute(x):
            return (w * x).sum() / total_weight
        return _compute

    WMA = close.rolling(length)_.apply(linear_weights(weights), raw=True)

### _Zero Lag Moving Average_: **zlma** : Zero Lag Moving Average (ZLMA)

The Zero Lag Moving Average attempts to eliminate the lag associated
with moving averages.  This is an adaption created by John Ehler and Ric Way.

Sources:
    https://en.wikipedia.org/wiki/Zero_lag_exponential_moving_average

Calculation:
    Default Inputs:
        length=10, mamode=EMA
    EMA = Exponential Moving Average
    lag = int(0.5 * (length - 1))

    SOURCE = 2 * close - close.shift(lag)
    ZLMA = MA(kind=mamode, SOURCE, length)


| _Simple Moving Averages_ (SMA) and _Bollinger Bands_ (BBANDS) |
|:--------:|
| ![Example Chart](/images/TA_Chart.png) |

<br/>

### **Performance** (3)

Use parameter: cumulative=**True** for cumulative results.

### _Draw Down_: **drawdown** : Drawdown (DD)

Drawdown is a peak-to-trough decline during a specific period for an investment,
trading account, or fund. It is usually quoted as the percentage between the
peak and the subsequent trough.

Sources:
    https://www.investopedia.com/terms/d/drawdown.asp

Calculation:
    PEAKDD = close.cummax()
    DD = PEAKDD - close
    DD% = 1 - (close / PEAKDD)
    DDlog = log(PEAKDD / close)
### _Log Return_: **log_return** : Log Return

Calculates the logarithmic return of a Series.
See also: help(df.ta.log_return) for additional **kwargs a valid 'df'.

Sources:
    https://stackoverflow.com/questions/31287552/logarithmic-returns-in-pandas-dataframe

Calculation:
    Default Inputs:
        length=1, cumulative=False
    LOGRET = log( close.diff(periods=length) )
    CUMLOGRET = LOGRET.cumsum() if cumulative
### _Percent Return_: **percent_return** : Percent Return

Calculates the percent return of a Series.
See also: help(df.ta.percent_return) for additional **kwargs a valid 'df'.

Sources:
    https://stackoverflow.com/questions/31287552/logarithmic-returns-in-pandas-dataframe

Calculation:
    Default Inputs:
        length=1, cumulative=False
    PCTRET = close.pct_change(length)
    CUMPCTRET = PCTRET.cumsum() if cumulative

| _Percent Return_ (Cumulative) with _Simple Moving Average_ (SMA) |
|:--------:|
| ![Example Cumulative Percent Return](/images/SPY_CumulativePercentReturn.png) |
<br/>

### **Statistics** (11)

### _Entropy_: **entropy** : Entropy (ENTP)

Introduced by Claude Shannon in 1948, entropy measures the unpredictability
of the data, or equivalently, of its average information. A die has higher
entropy (p=1/6) versus a coin (p=1/2).

Sources:
    https://en.wikipedia.org/wiki/Entropy_(information_theory)

Calculation:
    Default Inputs:
        length=10, base=2

    P = close / SUM(close, length)
    E = SUM(-P * npLog(P) / npLog(base), length)
### _Kurtosis_: **kurtosis** : Rolling Kurtosis

Sources:

Calculation:
    Default Inputs:
        length=30
    KURTOSIS = close.rolling(length).kurt()

### _Mean Absolute Deviation_: **mad** : Rolling Mean Absolute Deviation

Sources:

Calculation:
    Default Inputs:
        length=30
    mad = close.rolling(length).mad()
### _Median_: **median** : Rolling Median

Rolling Median of over 'n' periods. Sibling of a Simple Moving Average.

Sources:
    https://www.incrediblecharts.com/indicators/median_price.php

Calculation:
    Default Inputs:
        length=30
    MEDIAN = close.rolling(length).median()
### _Quantile_: **quantile** : Rolling Quantile

Sources:

Calculation:
    Default Inputs:
        length=30, q=0.5
    QUANTILE = close.rolling(length).quantile(q)

### _Skew_: **skew** : Rolling Skew

Sources:

Calculation:
    Default Inputs:
        length=30
    SKEW = close.rolling(length).skew()
### _Standard Deviation_: **stdev** : Rolling Standard Deviation

Sources:

Calculation:
    Default Inputs:
        length=30
    VAR = Variance
    STDEV = variance(close, length).apply(np.sqrt)
### _Think or Swim Standard Deviation All_: **tos_stdevall** : TD Ameritrade's Think or Swim Standard Deviation All (TOS_STDEV)

A port of TD Ameritrade's Think or Swim Standard Deviation All indicator which
returns the standard deviation of data for the entire plot or for the interval
of the last bars defined by the length parameter.

Sources:
    https://tlc.thinkorswim.com/center/reference/thinkScript/Functions/Statistical/StDevAll

Calculation:
    Default Inputs:
        length=None (All), stds=[1, 2, 3], ddof=1
    LR = Linear Regression
    STDEV = Standard Deviation

    LR = LR(close, length)
    STDEV = STDEV(close, length, ddof)
    for level in stds:
        LOWER = LR - level * STDEV
        UPPER = LR + level * STDEV
### _Variance_: **variance** : Rolling Variance

Sources:

Calculation:
    Default Inputs:
        length=30
    VARIANCE = close.rolling(length).var()

### _Z Score_: **zscore** : Rolling Z Score

Sources:

Calculation:
    Default Inputs:
        length=30, std=1
    SMA = Simple Moving Average
    STDEV = Standard Deviation
    std = std * STDEV(close, length)
    mean = SMA(close, length)
    ZSCORE = (close - mean) / std

| _Z Score_ |
|:--------:|
| ![Example Z Score](/images/SPY_ZScore.png) |
<br/>

### **Trend** (18)

### _Average Directional Movement Index_: **adx** : Average Directional Movement (ADX)

Average Directional Movement is meant to quantify trend strength by measuring
the amount of movement in a single direction.

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/average-directional-movement-adx/
    TA Lib Correlation: >99%

Calculation:
    DMI ADX TREND 2.0 by @TraderR0BERT, NETWORTHIE.COM
        //Created by @TraderR0BERT, NETWORTHIE.COM, last updated 01/26/2016
        //DMI Indicator
        //Resolution input option for higher/lower time frames
        study(title="DMI ADX TREND 2.0", shorttitle="ADX TREND 2.0")

        adxlen = input(14, title="ADX Smoothing")
        dilen = input(14, title="DI Length")
        thold = input(20, title="Threshold")

        threshold = thold

        //Script for Indicator
        dirmov(len) =>
            up = change(high)
            down = -change(low)
            truerange = rma(tr, len)
            plus = fixnan(100 * rma(up > down and up > 0 ? up : 0, len) / truerange)
            minus = fixnan(100 * rma(down > up and down > 0 ? down : 0, len) / truerange)
            [plus, minus]

        adx(dilen, adxlen) =>
            [plus, minus] = dirmov(dilen)
            sum = plus + minus
            adx = 100 * rma(abs(plus - minus) / (sum == 0 ? 1 : sum), adxlen)
            [adx, plus, minus]

        [sig, up, down] = adx(dilen, adxlen)
        osob=input(40,title="Exhaustion Level for ADX, default = 40")
        col = sig >= sig[1] ? green : sig <= sig[1] ? red : gray

        //Plot Definitions Current Timeframe
        p1 = plot(sig, color=col, linewidth = 3, title="ADX")
        p2 = plot(sig, color=col, style=circles, linewidth=3, title="ADX")
        p3 = plot(up, color=blue, linewidth = 3, title="+DI")
        p4 = plot(up, color=blue, style=circles, linewidth=3, title="+DI")
        p5 = plot(down, color=fuchsia, linewidth = 3, title="-DI")
        p6 = plot(down, color=fuchsia, style=circles, linewidth=3, title="-DI")
        h1 = plot(threshold, color=black, linewidth =3, title="Threshold")

        trender = (sig >= up or sig >= down) ? 1 : 0
        bgcolor(trender>0?black:gray, transp=85)

        //Alert Function for ADX crossing Threshold
        Up_Cross = crossover(up, threshold)
        alertcondition(Up_Cross, title="DMI+ cross", message="DMI+ Crossing Threshold")
        Down_Cross = crossover(down, threshold)
        alertcondition(Down_Cross, title="DMI- cross", message="DMI- Crossing Threshold")
    * Also includes **dmp** and **dmn** in the resultant DataFrame.
### _Archer Moving Averages Trends_: **amat**
### _Aroon & Aroon Oscillator_: **aroon** : Aroon & Aroon Oscillator (AROON)

Aroon attempts to identify if a security is trending and how strong.

Sources:
    https://www.tradingview.com/wiki/Aroon
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/aroon-ar/

Calculation:
    Default Inputs:
        length=1, scalar=100

    recent_maximum_index(x): return int(np.argmax(x[::-1]))
    recent_minimum_index(x): return int(np.argmin(x[::-1]))

    periods_from_hh = high.rolling(length + 1).apply(recent_maximum_index, raw=True)
    AROON_UP = scalar * (1 - (periods_from_hh / length))

    periods_from_ll = low.rolling(length + 1).apply(recent_minimum_index, raw=True)
    AROON_DN = scalar * (1 - (periods_from_ll / length))

    AROON_OSC = AROON_UP - AROON_DN
### _Choppiness Index_: **chop** : Choppiness Index (CHOP)

The Choppiness Index was created by Australian commodity trader
E.W. Dreiss and is designed to determine if the market is choppy
(trading sideways) or not choppy (trading within a trend in either
direction). Values closer to 100 implies the underlying is choppier
whereas values closer to 0 implies the underlying is trending.

Sources:
    https://www.tradingview.com/scripts/choppinessindex/
    https://www.motivewave.com/studies/choppiness_index.htm

Calculation:
    Default Inputs:
        length=14, scalar=100, drift=1
    HH = high.rolling(length).max()
    LL = low.rolling(length).min()

    ATR_SUM = SUM(ATR(drift), length)
    CHOP = scalar * (LOG10(ATR_SUM) - LOG10(HH - LL))
    CHOP /= LOG10(length)
### _Chande Kroll Stop_: **cksp** : Chande Kroll Stop (CKSP)

The Tushar Chande and Stanley Kroll in their book
“The New Technical Trader”. It is a trend-following indicator,
identifying your stop by calculating the average true range of
the recent market volatility. The indicator defaults to the implementation
found on tradingview but it provides the original book implementation as well,
which differs by the default periods and moving average mode. While the trading
view implementation uses the Welles Wilder moving average, the book uses a
simple moving average.

Sources:
    https://www.multicharts.com/discussion/viewtopic.php?t=48914
    "The New Technical Trader", Wikey 1st ed. ISBN 9780471597803, page 95

Calculation:
    Default Inputs:
        p=10, x=1, q=9, tvmode=True
    ATR = Average True Range

    LS0 = high.rolling(p).max() - x * ATR(length=p)
    LS = LS0.rolling(q).max()

    SS0 = high.rolling(p).min() + x * ATR(length=p)
    SS = SS0.rolling(q).min()
### _Decay_: **decay** : Decay

Creates a decay moving forward from prior signals like crosses. The default is
"linear". Exponential is optional as "exponential" or "exp".

Sources:
    https://tulipindicators.org/decay

Calculation:
    Default Inputs:
        length=5, mode=None

    if mode == "exponential" or mode == "exp":
        max(close, close[-1] - exp(-length), 0)
    else:
        max(close, close[-1] - (1 / length), 0)
    * Formally: **linear_decay**
### _Decreasing_: **decreasing** : Decreasing

Returns True if the series is decreasing over a period, False otherwise.
If the kwarg 'strict' is True, it returns True if it is continuously decreasing
over the period. When using the kwarg 'asint', then it returns 1 for True
or 0 for False.

Calculation:
    if strict:
        decreasing = all(i > j for i, j in zip(close[-length:], close[1:]))
    else:
        decreasing = close.diff(length) < 0

    if asint:
        decreasing = decreasing.astype(int)

### _Detrended Price Oscillator_: **dpo** : Detrend Price Oscillator (DPO)

Is an indicator designed to remove trend from price and make it easier to
identify cycles.

Sources:
    https://www.tradingview.com/scripts/detrendedpriceoscillator/
    https://www.fidelity.com/learning-center/trading-investing/technical-analysis/technical-indicator-guide/dpo
    http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:detrended_price_osci

Calculation:
    Default Inputs:
        length=20, centered=True
    SMA = Simple Moving Average
    t = int(0.5 * length) + 1

    DPO = close.shift(t) - SMA(close, length)
    if centered:
        DPO = DPO.shift(-t)
    * Set ```lookahead=False``` to disable centering and remove potential data leak.
### _Increasing_: **increasing** : Increasing

Returns True if the series is increasing over a period, False otherwise.
If the kwarg 'strict' is True, it returns True if it is continuously increasing
over the period. When using the kwarg 'asint', then it returns 1 for True
or 0 for False.

Calculation:
    if strict:
        increasing = all(i < j for i, j in zip(close[-length:], close[1:]))
    else:
        increasing = close.diff(length) > 0

    if asint:
        increasing = increasing.astype(int)
### _Long Run_: **long_run**
### _Parabolic Stop and Reverse_: **psar** : Parabolic Stop and Reverse (psar)

Parabolic Stop and Reverse (PSAR) was developed by J. Wells Wilder, that is used
to determine trend direction and it's potential reversals in price. PSAR uses a
trailing stop and reverse method called "SAR," or stop and reverse, to identify
possible entries and exits. It is also known as SAR.

PSAR indicator typically appears on a chart as a series of dots, either above or
below an asset's price, depending on the direction the price is moving. A dot is
placed below the price when it is trending upward, and above the price when it
is trending downward.

Sources:
    https://www.tradingview.com/pine-script-reference/#fun_sar
    https://www.sierrachart.com/index.php?page=doc/StudiesReference.php&ID=66&Name=Parabolic

Calculation:
    Default Inputs:
        af0=0.02, af=0.02, max_af=0.2

### _Q Stick_: **qstick** : Q Stick

The Q Stick indicator, developed by Tushar Chande, attempts to quantify and
identify trends in candlestick charts.

Sources:
    https://library.tradingtechnologies.com/trade/chrt-ti-qstick.html

Calculation:
    Default Inputs:
        length=10
    xMA is one of: sma (default), dema, ema, hma, rma
    qstick = xMA(close - open, length)
### _Short Run_: **short_run**
### _Trend Signals_: **tsignals** : Trend Signals

Given a Trend, Trend Signals returns the Trend, Trades, Entries and Exits as
boolean integers. When 'asbool=True', it returns Trends, Entries and Exits as
boolean values which is helpful when combined with the vectorbt backtesting
package.

A Trend can be a simple as: 'close' > 'moving average' or something more complex
whose values are boolean or integers (0 or 1).

Examples:
ta.tsignals(close > ta.sma(close, 50), asbool=False)
ta.tsignals(ta.ema(close, 8) > ta.ema(close, 21), asbool=True)

Source: Kevin Johnson

Calculation:
    Default Inputs:
        asbool=False, trend_reset=0, trade_offset=0, drift=1

    trades = trends.diff().shift(trade_offset).fillna(0).astype(int)
    entries = (trades > 0).astype(int)
    exits = (trades < 0).abs().astype(int)
### _TTM Trend_: **ttm_trend** : TTM Trend (TTM_TRND)

This indicator is from John Carters book “Mastering the Trade” and plots the
bars green or red. It checks if the price is above or under the average price of
the previous 5 bars. The indicator should hep you stay in a trade until the
colors chance. Two bars of the opposite color is the signal to get in or out.

Sources:
    https://www.prorealcode.com/prorealtime-indicators/ttm-trend-price/

Calculation:
    Default Inputs:
        length=6
    averageprice = (((high[5]+low[5])/2)+((high[4]+low[4])/2)+((high[3]+low[3])/2)+((high[2]+low[2])/2)+((high[1]+low[1])/2)+((high[6]+low[6])/2)) / 6

    if close > averageprice:
        drawcandle(open,high,low,close) coloured(0,255,0)

    if close < averageprice:
        drawcandle(open,high,low,close) coloured(255,0,0)
### _Vertical Horizontal Filter_: **vhf** : Vertical Horizontal Filter (VHF)

VHF was created by Adam White to identify trending and ranging markets.

Sources:
    https://www.incrediblecharts.com/indicators/vertical_horizontal_filter.php

Calculation:
    Default Inputs:
        length = 28
    HCP = Highest Close Price in Period
    LCP = Lowest Close Price in Period
    Change = abs(Ct - Ct-1)
    VHF = (HCP - LCP) / RollingSum[length] of Change
### _Vortex_: **vortex** : Vortex

Two oscillators that capture positive and negative trend movement.

Sources:
    https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:vortex_indicator

Calculation:
    Default Inputs:
        length=14, drift=1
    TR = True Range
    SMA = Simple Moving Average
    tr = TR(high, low, close)
    tr_sum = tr.rolling(length).sum()

    vmp = (high - low.shift(drift)).abs()
    vmn = (low - high.shift(drift)).abs()

    VIP = vmp.rolling(length).sum() / tr_sum
    VIM = vmn.rolling(length).sum() / tr_sum

### _Cross Signals_: **xsignals** : Cross Signals (XSIGNALS)

Cross Signals returns Trend Signal (TSIGNALS) results for Signal Crossings. This
is useful for indicators like RSI, ZSCORE, et al where one wants trade Entries
and Exits (and Trends).

Cross Signals has two kinds of modes: above and long.

The first mode 'above', default True, xsignals determines if the signal first
crosses above 'xa' and then below 'xb'. If 'above' is False, xsignals determines
if the signal first crosses below 'xa' and then above 'xb'.

The second mode 'long', default True, passes the long trend result into
tsignals so it can determine the appropriate Entries and Exits. When 'long' is
False, it does the same but for the short side.

Example:
# These are two different outcomes and depends on the indicator and it's
# characteristics. Please check BOTH outcomes BEFORE making an Issue.
rsi = df.ta.rsi()
# Returns tsignal DataFrame when RSI crosses above 20 and then below 80
ta.xsignals(rsi, 20, 80, above=True)
# Returns tsignal DataFrame when RSI crosses below 20 and then above 80
ta.xsignals(rsi, 20, 80, above=False)

Source: Kevin Johnson

Calculation:
    Default Inputs:
        asbool=False, trend_reset=0, trade_offset=0, drift=1

    trades = trends.diff().shift(trade_offset).fillna(0).astype(int)
    entries = (trades > 0).astype(int)
    exits = (trades < 0).abs().astype(int)

| _Average Directional Movement Index_ (ADX) |
|:--------:|
| ![Example ADX](/images/SPY_ADX.png) |


### **Volatility** (14)

### _Aberration_: **aberration** : Aberration

A volatility indicator similar to Keltner Channels.

Sources:
    Few internet resources on definitive definition.
    Request by Github user homily, issue #46

Calculation:
    Default Inputs:
        length=5, atr_length=15
    ATR = Average True Range
    SMA = Simple Moving Average

    ATR = ATR(length=atr_length)
    JG = TP = HLC3(high, low, close)
    ZG = SMA(JG, length)
    SG = ZG + ATR
    XG = ZG - ATR
### _Acceleration Bands_: **accbands** : Acceleration Bands (ACCBANDS)

Acceleration Bands created by Price Headley plots upper and lower envelope
bands around a simple moving average.

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/acceleration-bands-abands/

Calculation:
    Default Inputs:
        length=10, c=4
    EMA = Exponential Moving Average
    SMA = Simple Moving Average
    HL_RATIO = c * (high - low) / (high + low)
    LOW = low * (1 - HL_RATIO)
    HIGH = high * (1 + HL_RATIO)

    if 'ema':
        LOWER = EMA(LOW, length)
        MID = EMA(close, length)
        UPPER = EMA(HIGH, length)
    else:
        LOWER = SMA(LOW, length)
        MID = SMA(close, length)
        UPPER = SMA(HIGH, length)
### _Average True Range_: **atr** : Average True Range (ATR)

Averge True Range is used to measure volatility, especially volatility caused by
gaps or limit moves.

Sources:
    https://www.tradingview.com/wiki/Average_True_Range_(ATR)

Calculation:
    Default Inputs:
        length=14, drift=1, percent=False
    EMA = Exponential Moving Average
    SMA = Simple Moving Average
    WMA = Weighted Moving Average
    RMA = WildeR's Moving Average
    TR = True Range

    tr = TR(high, low, close, drift)
    if 'ema':
        ATR = EMA(tr, length)
    elif 'sma':
        ATR = SMA(tr, length)
    elif 'wma':
        ATR = WMA(tr, length)
    else:
        ATR = RMA(tr, length)

    if percent:
        ATR *= 100 / close
### _Bollinger Bands_: **bbands** : Bollinger Bands (BBANDS)

A popular volatility indicator by John Bollinger.

Sources:
    https://www.tradingview.com/wiki/Bollinger_Bands_(BB)

Calculation:
    Default Inputs:
        length=5, std=2, mamode="sma", ddof=0
    EMA = Exponential Moving Average
    SMA = Simple Moving Average
    STDEV = Standard Deviation
    stdev = STDEV(close, length, ddof)
    if "ema":
        MID = EMA(close, length)
    else:
        MID = SMA(close, length)

    LOWER = MID - std * stdev
    UPPER = MID + std * stdev

    BANDWIDTH = 100 * (UPPER - LOWER) / MID
    PERCENT = (close - LOWER) / (UPPER - LOWER)
### _Donchian Channel_: **donchian** : Donchian Channels (DC)

Donchian Channels are used to measure volatility, similar to
Bollinger Bands and Keltner Channels.

Sources:
    https://www.tradingview.com/wiki/Donchian_Channels_(DC)

Calculation:
    Default Inputs:
        lower_length=upper_length=20
    LOWER = low.rolling(lower_length).min()
    UPPER = high.rolling(upper_length).max()
    MID = 0.5 * (LOWER + UPPER)
### _Holt-Winter Channel_: **hwc** : HWC (Holt-Winter Channel)

Channel indicator HWC (Holt-Winters Channel) based on HWMA - a three-parameter
moving average calculated by the method of Holt-Winters.

This version has been implemented for Pandas TA by rengel8 based on a
publication for MetaTrader 5 extended by width and percentage price position
against width of channel.

Sources:
    https://www.mql5.com/en/code/20857

Calculation:
    HWMA[i] = F[i] + V[i] + 0.5 * A[i]
    where..
    F[i] = (1-na) * (F[i-1] + V[i-1] + 0.5 * A[i-1]) + na * Price[i]
    V[i] = (1-nb) * (V[i-1] + A[i-1]) + nb * (F[i] - F[i-1])
    A[i] = (1-nc) * A[i-1] + nc * (V[i] - V[i-1])

    Top = HWMA + Multiplier * StDt
    Bottom = HWMA - Multiplier * StDt
    where..
    StDt[i] = Sqrt(Var[i-1])
    Var[i] = (1-d) * Var[i-1] + nD * (Price[i-1] - HWMA[i-1]) * (Price[i-1] - HWMA[i-1])

### _Keltner Channel_: **kc** : Keltner Channels (KC)

A popular volatility indicator similar to Bollinger Bands and
Donchian Channels.

Sources:
    https://www.tradingview.com/wiki/Keltner_Channels_(KC)

Calculation:
    Default Inputs:
        length=20, scalar=2, mamode=None, tr=True
    TR = True Range
    SMA = Simple Moving Average
    EMA = Exponential Moving Average

    if tr:
        RANGE = TR(high, low, close)
    else:
        RANGE = high - low

    if mamode == "ema":
        BASIS = sma(close, length)
        BAND = sma(RANGE, length)
    elif mamode == "sma":
        BASIS = sma(close, length)
        BAND = sma(RANGE, length)

    LOWER = BASIS - scalar * BAND
    UPPER = BASIS + scalar * BAND

### _Mass Index_: **massi** : Mass Index (MASSI)

The Mass Index is a non-directional volatility indicator that utilitizes the
High-Low Range to identify trend reversals based on range expansions.

Sources:
    https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:mass_index
    mi = sum(ema(high - low, 9) / ema(ema(high - low, 9), 9), length)

Calculation:
    Default Inputs:
        fast: 9, slow: 25
    EMA = Exponential Moving Average
    hl = high - low
    hl_ema1 = EMA(hl, fast)
    hl_ema2 = EMA(hl_ema1, fast)
    hl_ratio = hl_ema1 / hl_ema2
    MASSI = SUM(hl_ratio, slow)

### _Normalized Average True Range_: **natr** : Normalized Average True Range (NATR)

Normalized Average True Range attempt to normalize the average true range.

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/normalized-average-true-range-natr/

Calculation:
    Default Inputs:
        length=20
    ATR = Average True Range
    NATR = (100 / close) * ATR(high, low, close)

### _Price Distance_: **pdist** : Price Distance (PDIST)

Measures the "distance" covered by price movements.

Sources:
    https://www.prorealcode.com/prorealtime-indicators/pricedistance/

Calculation:
    Default Inputs:
        drift=1

    PDIST = 2(high - low) - ABS(close - open) + ABS(open - close[drift])
### _Relative Volatility Index_: **rvi** : Relative Volatility Index (RVI)

The Relative Volatility Index (RVI) was created in 1993 and revised in 1995.
Instead of adding up price changes like RSI based on price direction, the RVI
adds up standard deviations based on price direction.

Sources:
    https://www.tradingview.com/wiki/Keltner_Channels_(KC)

Calculation:
    Default Inputs:
        length=14, scalar=100, refined=None, thirds=None
    EMA = Exponential Moving Average
    STDEV = Standard Deviation

    UP = STDEV(src, length) IF src.diff() > 0 ELSE 0
    DOWN = STDEV(src, length) IF src.diff() <= 0 ELSE 0

    UPSUM = EMA(UP, length)
    DOWNSUM = EMA(DOWN, length

    RVI = scalar * (UPSUM / (UPSUM + DOWNSUM))
### _Elder's Thermometer_: **thermo** : Elders Thermometer (THERMO)

Elder's Thermometer measures price volatility.

Sources:
    https://www.motivewave.com/studies/elders_thermometer.htm
    https://www.tradingview.com/script/HqvTuEMW-Elder-s-Market-Thermometer-LazyBear/

Calculation:
    Default Inputs:
    length=20, drift=1, mamode=EMA, long=2, short=0.5
    EMA = Exponential Moving Average

    thermoL = (low.shift(drift) - low).abs()
    thermoH = (high - high.shift(drift)).abs()

    thermo = np.where(thermoH > thermoL, thermoH, thermoL)
    thermo_ma = ema(thermo, length)

    thermo_long = thermo < (thermo_ma * long)
    thermo_short = thermo > (thermo_ma * short)
    thermo_long = thermo_long.astype(int)
    thermo_short = thermo_short.astype(int)
### _True Range_: **true_range** : True Range

An method to expand a classical range (high minus low) to include
possible gap scenarios.

Sources:
    https://www.macroption.com/true-range/

Calculation:
    Default Inputs:
        drift=1
    ABS = Absolute Value
    prev_close = close.shift(drift)
    TRUE_RANGE = ABS([high - low, high - prev_close, low - prev_close])

### _Ulcer Index_: **ui** : Ulcer Index (UI)

The Ulcer Index by Peter Martin measures the downside volatility with the use of
the Quadratic Mean, which has the effect of emphasising large drawdowns.

Sources:
    https://library.tradingtechnologies.com/trade/chrt-ti-ulcer-index.html
    https://en.wikipedia.org/wiki/Ulcer_index
    http://www.tangotools.com/ui/ui.htm

Calculation:
    Default Inputs:
        length=14, scalar=100
    HC = Highest Close
    SMA = Simple Moving Average

    HCN = HC(close, length)
    DOWNSIDE = scalar * (close - HCN) / HCN
    if kwargs["everget"]:
        UI = SQRT(SMA(DOWNSIDE^2, length) / length)
    else:
        UI = SQRT(SUM(DOWNSIDE^2, length) / length)

| _Average True Range_ (ATR) |
|:--------:|
| ![Example ATR](/images/SPY_ATR.png) |

<br/>

### **Volume** (15)

### _Accumulation/Distribution Index_: **ad** : Accumulation/Distribution (AD)

Accumulation/Distribution indicator utilizes the relative position
of the close to it's High-Low range with volume.  Then it is cumulated.

Sources:
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/accumulationdistribution-ad/

Calculation:
    CUM = Cumulative Sum
    if 'open':
        AD = close - open
    else:
        AD = 2 * close - high - low

    hl_range = high - low
    AD = AD * volume / hl_range
    AD = CUM(AD)
### _Accumulation/Distribution Oscillator_: **adosc** : Accumulation/Distribution Oscillator or Chaikin Oscillator

Accumulation/Distribution Oscillator indicator utilizes
Accumulation/Distribution and treats it similarily to MACD
or APO.

Sources:
    https://www.investopedia.com/articles/active-trading/031914/understanding-chaikin-oscillator.asp

Calculation:
    Default Inputs:
        fast=12, slow=26
    AD = Accum/Dist
    ad = AD(high, low, close, open)
    fast_ad = EMA(ad, fast)
    slow_ad = EMA(ad, slow)
    ADOSC = fast_ad - slow_ad
### _Archer On-Balance Volume_: **aobv** : 
### _Chaikin Money Flow_: **cmf** Chaikin Money Flow (CMF)

Chailin Money Flow measures the amount of money flow volume over a specific
period in conjunction with Accumulation/Distribution.

Sources:
    https://www.tradingview.com/wiki/Chaikin_Money_Flow_(CMF)
    https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:chaikin_money_flow_cmf

Calculation:
    Default Inputs:
        length=20
    if 'open':
        ad = close - open
    else:
        ad = 2 * close - high - low

    hl_range = high - low
    ad = ad * volume / hl_range
    CMF = SUM(ad, length) / SUM(volume, length)

### _Elder's Force Index_: **efi** : Elder's Force Index (EFI)

Elder's Force Index measures the power behind a price movement using price
and volume as well as potential reversals and price corrections.

Sources:
    https://www.tradingview.com/wiki/Elder%27s_Force_Index_(EFI)
    https://www.motivewave.com/studies/elders_force_index.htm

Calculation:
    Default Inputs:
        length=20, drift=1, mamode=None
    EMA = Exponential Moving Average
    SMA = Simple Moving Average

    pv_diff = close.diff(drift) * volume
    if mamode == 'sma':
        EFI = SMA(pv_diff, length)
    else:
        EFI = EMA(pv_diff, length)
### _Ease of Movement_: **eom** : Ease of Movement (EOM)

Ease of Movement is a volume based oscillator that is designed to measure the
relationship between price and volume flucuating across a zero line.

Sources:
    https://www.tradingview.com/wiki/Ease_of_Movement_(EOM)
    https://www.motivewave.com/studies/ease_of_movement.htm
    https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:ease_of_movement_emv

Calculation:
    Default Inputs:
        length=14, divisor=100000000, drift=1
    SMA = Simple Moving Average
    hl_range = high - low
    distance = 0.5 * (high - high.shift(drift) + low - low.shift(drift))
    box_ratio = (volume / divisor) / hl_range
    eom = distance / box_ratio
    EOM = SMA(eom, length)

### _Klinger Volume Oscillator_: **kvo** : Klinger Volume Oscillator (KVO)

This indicator was developed by Stephen J. Klinger. It is designed to predict
price reversals in a market by comparing volume to price.

Sources:
    https://www.investopedia.com/terms/k/klingeroscillator.asp
    https://www.daytrading.com/klinger-volume-oscillator

Calculation:
    Default Inputs:
        fast=34, slow=55, signal=13, drift=1
    EMA = Exponential Moving Average

    SV = volume * signed_series(HLC3, 1)
    KVO = EMA(SV, fast) - EMA(SV, slow)
    Signal = EMA(KVO, signal)

### _Money Flow Index_: **mfi** : Money Flow Index (MFI)

Money Flow Index is an oscillator indicator that is used to measure buying and
selling pressure by utilizing both price and volume.

Sources:
    https://www.tradingview.com/wiki/Money_Flow_(MFI)

Calculation:
    Default Inputs:
        length=14, drift=1
    tp = typical_price = hlc3 = (high + low + close) / 3
    rmf = raw_money_flow = tp * volume

    pmf = pos_money_flow = SUM(rmf, length) if tp.diff(drift) > 0 else 0
    nmf = neg_money_flow = SUM(rmf, length) if tp.diff(drift) < 0 else 0

    MFR = money_flow_ratio = pmf / nmf
    MFI = money_flow_index = 100 * pmf / (pmf + nmf)

### _Negative Volume Index_: **nvi** : Negative Volume Index (NVI)

The Negative Volume Index is a cumulative indicator that uses volume change in
an attempt to identify where smart money is active.

Sources:
    https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:negative_volume_inde
    https://www.motivewave.com/studies/negative_volume_index.htm

Calculation:
    Default Inputs:
        length=1, initial=1000
    ROC = Rate of Change

    roc = ROC(close, length)
    signed_volume = signed_series(volume, initial=1)
    nvi = signed_volume[signed_volume < 0].abs() * roc_
    nvi.fillna(0, inplace=True)
    nvi.iloc[0]= initial
    nvi = nvi.cumsum()
### _On-Balance Volume_: **obv** : On Balance Volume (OBV)

On Balance Volume is a cumulative indicator to measure buying and selling
pressure.

Sources:
    https://www.tradingview.com/wiki/On_Balance_Volume_(OBV)
    https://www.tradingtechnologies.com/help/x-study/technical-indicator-definitions/on-balance-volume-obv/
    https://www.motivewave.com/studies/on_balance_volume.htm

Calculation:
    signed_volume = signed_series(close, initial=1) * volume
    obv = signed_volume.cumsum()

### _Positive Volume Index_: **pvi** : Positive Volume Index (PVI)

The Positive Volume Index is a cumulative indicator that uses volume change in
an attempt to identify where smart money is active.
Used in conjunction with NVI.

Sources:
    https://www.investopedia.com/terms/p/pvi.asp

Calculation:
    Default Inputs:
        length=1, initial=1000
    ROC = Rate of Change

    roc = ROC(close, length)
    signed_volume = signed_series(volume, initial=1)
    pvi = signed_volume[signed_volume > 0].abs() * roc_
    pvi.fillna(0, inplace=True)
    pvi.iloc[0]= initial
    pvi = pvi.cumsum()

### _Price-Volume_: **pvol** : Price-Volume (PVOL)

Returns a series of the product of price and volume.

Calculation:
    if signed:
        pvol = signed_series(close, 1) * close * volume
    else:
        pvol = close * volume

### _Price Volume Rank_: **pvr** : Price Volume Rank

The Price Volume Rank was developed by Anthony J. Macek and is described in his
article in the June, 1994 issue of Technical Analysis of Stocks & Commodities
Magazine. It was developed as a simple indicator that could be calculated even
without a computer. The basic interpretation is to buy when the PV Rank is below
2.5 and sell when it is above 2.5.

Sources:
    https://www.fmlabs.com/reference/default.htm?url=PVrank.htm

Calculation:
    return 1 if 'close change' >= 0 and 'volume change' >= 0
    return 2 if 'close change' >= 0 and 'volume change' < 0
    return 3 if 'close change' < 0 and 'volume change' >= 0
    return 4 if 'close change' < 0 and 'volume change' < 0

### _Price Volume Trend_: **pvt** : Price-Volume Trend (PVT)

The Price-Volume Trend utilizes the Rate of Change with volume to
and it's cumulative values to determine money flow.

Sources:
    https://www.tradingview.com/wiki/Price_Volume_Trend_(PVT)

Calculation:
    Default Inputs:
        drift=1
    ROC = Rate of Change
    pv = ROC(close, drift) * volume
    PVT = pv.cumsum()

### _Volume Profile_: **vp** : Volume Profile (VP)

Calculates the Volume Profile by slicing price into ranges.
Note: Value Area is not calculated.

Sources:
    https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:volume_by_price
    https://www.tradingview.com/wiki/Volume_Profile
    http://www.ranchodinero.com/volume-tpo-essentials/
    https://www.tradingtechnologies.com/blog/2013/05/15/volume-at-price/

Calculation:
    Default Inputs:
        width=10

    vp = pd.concat([close, pos_volume, neg_volume], axis=1)
    if sort_close:
        vp_ranges = cut(vp[close_col], width)
        result = ({range_left, mean_close, range_right, pos_volume, neg_volume} foreach range in vp_ranges
    else:
        vp_ranges = np.array_split(vp, width)
        result = ({low_close, mean_close, high_close, pos_volume, neg_volume} foreach range in vp_ranges
    vpdf = pd.DataFrame(result)
    vpdf['total_volume'] = vpdf['pos_volume'] + vpdf['neg_volume']

| _On-Balance Volume_ (OBV) |
|:--------:|
| ![Example OBV](/images/SPY_OBV.png) |

<br/><br/>

# **Performance Metrics** &nbsp; _BETA_
_Performance Metrics_ are a **new** addition to the package and consequentially are likely unreliable. **Use at your own risk.** These metrics return a _float_ and are _not_ part of the _DataFrame_ Extension. They are called the Standard way. For Example:

```python
import pandas_ta as ta
result = ta.cagr(df.close)
```

### Available Metrics
### _Compounded Annual Growth Rate_: **cagr**
### _Calmar Ratio_: **calmar_ratio** : The Calmar Ratio is the percent Max Drawdown Ratio 'typically' over
    the past three years.

### _Downside Deviation_: **downside_deviation** : Downside Deviation for the Sortino ratio.
    Benchmark rate is assumed to be annualized. Adjusted according for the
    number of periods per year seen in the data.

### _Jensen's Alpha_: **jensens_alpha** : Jensen's 'Alpha' of a series and a benchmark.
### _Log Max Drawdown_: **log_max_drawdown** : Log Max Drawdown of a series.
### _Max Drawdown_: **max_drawdown** : Maximum Drawdown from close. Default: 'dollar'.
### _Pure Profit Score_: **pure_profit_score**
### _Sharpe Ratio_: **sharpe_ratio**
### _Sortino Ratio_: **sortino_ratio**
### _Volatility_: **volatility**
