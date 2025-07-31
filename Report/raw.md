**Research Proposal**

**Title: Presidential Impact on Stock Market Volatility and Trading
Volume**

**Research Questions**

1.  **Comparative Volatility Influence**: Did President Trump's
    presidency (2017--2020) correspond to significantly higher stock
    market volatility and trading volume than other administrations
    (Clinton, Bush, Obama)?

2.  **Sector-Specific Impacts**: Were trade-exposed or
    deregulation-sensitive sectors (e.g. technology, industrials) more
    volatile under Trump compared to defensive sectors and to their
    performance under previous presidents?

**Hypotheses**

- H1: Average volatility (VIX or daily return variability) under Trump
  exceeded that of other recent presidents.

- H2: Average trading volumes---and frequency of volume spikes---were
  higher during Trump's term.

- H3: Trade-sensitive sectors experienced disproportionately larger
  volatility and volume increases under Trump than defensive sectors,
  and more so than under earlier presidencies.

**Literature Review (brief)\*\***

**Trump and Market Volatility:** Empirical studies (e.g. Nishimura
et al. 2021) show Trump\'s tweets significantly increased realized
volatility and jump risk in returns due to sudden market
responses [[ScienceDirect+6IDEAS/RePEc+6UR Scholarship
Repository+6]{.underline}](https://ideas.repec.org/a/bla/jfnres/v44y2021i3p497-512.html?utm_source=chatgpt.com). Christian
Hoven's thesis confirms company-specific tweets raised abnormal
volatility and trading volume by \~19% [[ScienceDirect+2UR Scholarship
Repository+2Doria+2]{.underline}](https://scholarship.richmond.edu/honors-theses/1484/?utm_source=chatgpt.com). Research
also finds disagreement in social media conversations mentioning Trump
and firms correlates with heightened volatility and volume before
inauguration [[Investopedia+5centaur.reading.ac.uk+5ftp.aeaweb.org+5]{.underline}](https://centaur.reading.ac.uk/88906/1/trump191215.pdf?utm_source=chatgpt.com).\
**Election Uncertainty:** Studies across U.S. presidential cycles show
elevated election uncertainty leads to higher volatility, but systematic
comparison across presidencies is
rare [[ScienceDirect](https://www.sciencedirect.com/science/article/pii/S1544612320302300?utm_source=chatgpt.com)[ScienceDirect](https://www.sciencedirect.com/science/article/pii/S0378426612003603?utm_source=chatgpt.com)]{.underline}.\
**Global Comparisons:** Limited peer-reviewed research exists comparing
Trump-style shocks to market effects from other populist leaders or
political events.

**Gap:** No existing peer-reviewed study has systematically compared
volatility and trading volume across multiple U.S. presidencies
controlling for macro shocks, nor sector-level differences across
administrations. Sectoral volatility comparisons across presidents are
virtually unexplored.

**Ideal Data (High-Resolution Event Studies)**

- **Event-Level Presidential Communication:** Timestamped, rich data on
  Trump's tweets, press announcements, policy declarations; and
  equivalent significant communications for prior presidents. Include
  sentiment and topic coding.

- **High-Frequency Market Data:** Intraday price and volume
  (minute-level or tick-level) for indices and stocks, enabling
  detection of immediate reactions---volatility jumps and volume
  surges---following each presidential signal.

- **Volatility and Policy Uncertainty Indices:** Intraday or daily VIX
  and implied volatility metrics, alongside Economic Policy Uncertainty
  (EPU) and news-based uncertainty measures to capture market fear and
  policy flux.

- **Sector & Asset-Class Coverage:** Minute-level trading data for
  sector indices, industry portfolios, bond, currency, and commodity
  markets to examine how volatility and activity across asset classes
  and sectors respond to presidential actions.

- **Multi-Presidency Baseline Data:** Comparable data across
  administrations (2000--2024) including global indices, to benchmark
  Trump's market effects versus past U.S. presidents and other global
  leaders.

**Realistic Data (WRDS Access) & Compromises**

We will use WRDS for feasible data extraction, noting tradeoffs:

- **CRSP Daily Stock Data**: Daily returns and trading volume for U.S.
  stocks and indices (e.g. S&P 500 or CRSP value‑weighted index).
  Enables measurement of daily volatility and volume by term.

- **Sector Classification**: Industry codes via Compustat or WRDS
  Fama--French industry portfolios permit aggregation to sector-level
  returns and volumes.

- **Volatility Metrics**: Daily VIX values (if available via WRDS
  OptionMetrics) or computed realized volatility (e.g. 30-day rolling
  standard deviations).

- **Economic Policy Uncertainty (EPU)**: Public index merged as control
  variable to account for broader political uncertainty.

**Compromises**:

- **Frequency Limits**: Analysis is based on daily---not
  intraday---data. Immediate intraday spikes from tweets or
  announcements will be blurred into same-day aggregates.

- **Causality Attribution**: Without exact event timestamps aligned to
  market micro-moves, inference is correlational. Major macro shocks
  (e.g. COVID-19, financial crisis) may confound term-level comparisons.
  We will mitigate by excluding or controlling for crisis periods and
  focusing on volatility regimes.

- **Communication Data Absence**: WRDS does not include presidential
  tweet text or timestamped communications. Queries must rely on
  indirect proxies (e.g. periods with many tweets) or publicly available
  tweet logs, limiting precise event coding.

- **Sector Volatility Comparisons across Terms**: Sector indices
  aggregated from CRSP or Fama--French portfolios provide daily
  resolution but lack the granularity to trace within-day shocks.

**Data Strategy Summary**

  ------------------------------------------------------------------------------
  **Data Type**         **Ideal**           **Available via   **Compromise**
                                            WRDS**            
  --------------------- ------------------- ----------------- ------------------
  Presidential events   Timestamped tweets  None              Use historical
  (communication)       & speeches          quantitative; may counts/proxies; no
                                            supplement        full event dataset
                                            manually          

  Market pricing &      Tick/minute data    Only daily        Must measure daily
  volume (intraday)                         CRSP/sector       volatility spikes,
                                            returns & volume  not intraday

  Volatility indices    Intraday VIX        Daily VIX or      Only daily-level
                                            realized          inference
                                            volatility        

  Sector & asset-class  Minute-level for    Daily sector      No intraday sector
  breakdowns            multiple asset      portfolios and    or cross-asset
                        classes             volume            precision

  Global                Data from multiple  U.S. data only    Limited
  market/presidential   countries/leaders   via WRDS; may     international
  data                                      supplement        comparison
                                            globally          
  ------------------------------------------------------------------------------

**Preliminary Analysis Plan**

1.  **Time-series plots** of daily volatility and volume by presidency
    (2000--2024), using daily VIX or realized volatility and volume,
    shading presidential terms.

2.  **Summary statistics**: average volatility and volume metrics per
    term, cumulative extreme movement counts (e.g. days \> ±2% returns),
    testing differences (t-tests).

3.  **Sector comparisons**: calculate sector daily volatility and volume
    changes between presidential terms (e.g. tech vs. utilities),
    presenting percentage change and relative differences.

4.  **Simplified event snapshots**: illustrate a few major known
    Trump\'s announcements (e.g., March 2018 tariffs) vs comparable
    prior events (e.g., 2008 policy shocks), plotting daily returns and
    volatility around those dates.

**Implications**

- Provides evidence whether Trump-era administration induced elevated
  volatility and trading volume compared to prior presidencies.

- Identifies sectors disproportionately impacted under Trump, enriching
  risk management and trading strategy insights.

- Establishes groundwork for political risk assessment tied to
  leadership communication style.

- Offers policymakers contextual evidence on how public messaging
  affects financial stability, supporting guidelines around
  market-sensitive communication.

- Informs investors on allocation strategies sensitive to political
  regime volatility regimes.
