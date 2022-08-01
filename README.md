# Capital stock estimates user guide 

This repository provides users with the ability to produce estimates of capital stock from a set of inputs. This introduces more transparency of how
capital stock estimates are calculated and allows users to examine the impacts of different assumptions and to forecast future estimates of capital. 

## Prerequisites 

To produce capital stocks estimates it is necessary to have R installed on your PC, the listed packages, capital stocks R scripts, and a set of [spreadsheet inputs](https://www.ons.gov.uk/releases/introducingthecapitalstocksuserguide).

The capital stocks system is compatible with R ***version 4.2.1*** (2022). Scripts and packages compatible with version 3.4.0 are available [here](https://github.com/ONSdigital/Capstocks_previous).

To run the capital stocks code requires the following packages:

- dplyr 
- readr 
- readxl 
- testthat 
- tibble 
- tidyr 
- forecast 
- stringr 
- sqldf 
- cellranger 
- RSQLite 
- writexl

Alongside the above packages, the following capital stock packages are needed: 

- [capstock](https://github.com/ONSdigital/Capstock_package)
- [pimIO](https://github.com/ONSdigital/PimIO_package)
- [prepim](https://github.com/ONSdigital/Prepim_package)
- [pimir](https://github.com/ONSdigital/Pimir_package)

To add these packages to your library simply copy the above folders into your R library, so that you have a capstock, pimIO, prepim and pimir folder in your library.

The following R scripts from this repository are required: 

- Aggall.R 
- Aggregate.R 
- bespokeDataCalculations.R 
- Chain.R 
- chainingScript.R 
- miscCapStocksFunctions.R 
- PIM_inputs.R 
- Produce capital stock estimates.R 
- Run_pim.R 
- Unchain.R 

The following [spreadsheets](https://www.ons.gov.uk/economy/nationalaccounts/uksectoraccounts/methodologies/capitalstocksuserguideuk) are required:

- piminput.xlsx
- cvmcoveragetable.xlsx
- hierarchiessectorindustryasset.xlsx
- splitcofog.xlsx

## Running capital stock system 

The capital stocks R scripts should be saved in the same folder. ‘Produce capital stock estimates.R’ requires the user to input three parameters, so it will be
necessary to open the file and add the following locations at the top of the R script: 

- inputDir – the location of input data spreadsheets. For example: *inputDir <- “C:/Capital stock/”* 
- scriptsPath - the location of capital stocks R scripts 
- libraryPath – the location of the library containing the appropriate packages. This can be left blank if you do not 

With these parameters set, running all the lines of code in ‘Produce capital stock estimates.R’ will call the other scripts, producing estimates of capital for the
given input parameters. 

A set of input spreadsheets are published in the [article](https://www.ons.gov.uk/releases/introducingthecapitalstocksuserguide), which provides further details on the production of capital stocks estimates and starting at the end of 2022 these will be published
alongside our bi-annual publications of capital stock. Changing values in the spreadsheets will enable the calculation of capital stock estimates under different
assumptions. In addition, estimates of future values can be added to produce forecasts of future estimates of capital. The ‘Input Spreadsheets’ section details what
are included in these spreadsheets and then ‘R scripts’ describes how estimates of capital are calculated.

To specify where you want outputs written to, add the address of the directory under 'outputDir' in the 'Run_parameters' worksheet in PIM_input.xlsx.

If a user only wants to change a specific set of assumptions, only the relevant worksheets need altering. For example, with life lengths the user might want to
change estimates of mean, maximum life length and perhaps coefficient of variation. Most asset life lengths were reviewed in 2019 and further information on how
they were estimated in this [methodology article](https://www.ons.gov.uk/economy/nationalaccounts/uksectoraccounts/articles/nationalaccountsarticles/changestothecapitalstockestimationmethodsforbluebook2019).

If a user wants to forecast future estimates of capital on a consistent basis to existing estimates, the following steps need to be completed in the PIM input
spreadsheet: 

- If chained estimates are required, update ‘toChainTo’ in the ‘run_parameters’ worksheet with the period that chained estimates are required up to. 

- Forecasted estimates of 'GFCF_CP' and 'Price_index' are required. The headings for future quarters need to be in the format 'Y2025Q1' and all quarters will need
to have estimates. There are numerous series so the user might want to use some code or Excel formula to apply growth rates across a range of series. 

- Future estimates of life lengths are required. If these are not set the latest life length parameters will be carried forward.

To produce capital estimates all the lines in 'Produce capital stock estimates.R' need to be run. It can take a couple of hours to produce estimates of capital.

## Input Spreadsheets 

The following four input spreadsheets are required to run the capital stocks R system.

### 1 PIM input 

The PIM input spreadsheet contains 11 worksheets that are used to calculate estimates of capital stock. 

#### 1.1 Run parameters 

The first worksheet ‘run_parameters’ requires the user to set four parameters.  

The first parameter is the reference period (refPeriod), which will calculate constant price and chain-volume measures (CVMs) for the selected period. The quarter
needs to be in the format ‘Y’ then the year in four digits, followed by ‘Q’ and which quarter (for example ‘Y2019Q4). Although the format suggests an ability to
reference to any quarter, the system sometimes references prices by year and at other times to the fourth quarter 

Prior to the PIM gross fixed capital formation (GFCF) are referenced to the year selected and after the PIM flows are referenced to the year and stocks to the
fourth quarter. In a future Blue Book we will look to consistently reference to a selected quarter.  

The ‘toChainFrom’ and ‘toChainTo’ parameters set the periods where CVMs should be calculated. 

When setting ‘toChainFrom’ it is important to note that any period set before 1995Q1 may not produce estimates. 

The final parameter that is required is the output directory (‘outputDir’), which should be an existing folder location where you want the outputs to be saved. 

#### 1.2 GFCF CP 

This contains current price estimates of GFCF from 1828 to the latest period, broken down by sector, industry, and asset. Please note that currently these estimates
can differ from published estimates of GFCF, but we are working to align these estimates. Differences include the sectoral breakdown of private sector estimates,
general government estimates and estimates prior to 1997. Further details on differences can be found in the [‘Introducing the capital stocks user guide’](https://www.ons.gov.uk/releases/introducingthecapitalstocksuserguide).

A user who is only interested in selected series can remove other series of GFCF.

#### 1.3 Price index 

This worksheet contains price indices from 1828 to the latest period, also broken down by sector, asset, and industry. The price indices are used to deflate current
price estimates so that the PIM runs on constant prices expressed in prices of the reference year. The constant price estimates from the PIM are then reflated using
the same price indices. 

#### 1.4 Depreciation/retirement profiles 

Depreciation/retirement profiles (dep_ret_profiles) are broken down by sector, industry, and asset. Currently we only use different profiles for assets as we have
no evidence that these differ by industry and sector. 

Profile type (profileType) determines whether the user wants to produce an age-price profile directly, or an age-efficiency profile. As the names suggest the age
price profile describes the relationship between age and price, whereas age-efficiency, between age and efficiency. The age-price profile will naturally decline at
a faster rate, for example if an asset has not lost efficiency after a couple of years, the price will still have declined.  

The profile function name (profileFunctionName) defines the shape of the profile. The profile can suggest a decline in value/efficiency weighted towards the
beginning of an asset’s life (geometric, declining balance), an equal loss in value/efficiency in every year (linear), or value/efficiency losses at the end of an
asset’s life (hyperbolic). 

The retirement distribution (retirementDistName) determines how the cohort of assets will be retired around the mean asset life. Retirement distribution can be set
to normal, Weibull, gamma and log normal distributions. 

The right truncate parameter ensures that retirement distributions account for maximum asset lives constraining the right tail of the retirement distribution. 

Age-price profiles provide a relationship between age and price for a single asset and these need to be combined with a retirement profile that describe retirement
for a cohort of assets. Combination method 1 produces an age-price profile for each possible asset life and the probability that an asset will last for that
specified period is used as its weight. By weighting age-price profiles for every single asset life, a depreciation profile for a cohort of assets is produced. 

Combination method 2 assumes that all assets depreciate in line with the longest-lived asset. Therefore, an equivalent asset with the mean life and maximum life
will have the same value in the quarter before both assets reach the mean asset life, however the asset with a mean asset life would then be retired as it hit the
mean asset life and consequently its value would fall to zero. 

The discount rate (discountRate) and inflation rate (inflationRate) are used to produce a real rate of return. This is used to enable the conversion of age
efficiency profiles into age-price profiles and vice-versa. A user that wants to enter only a real discount rate can enter this into the discount rate and simply
set inflation to zero. If the inputs are expressed in quarters the discount rate and inflation rate need to be on a quarterly basis. 

The ‘offSet’ parameter determines when during each period GFCF takes place, to ensure that depreciation occurs from that point. We assume that on average GFCF
occurs halfway through each quarter, but this can be set from the start of the quarter (0) to the end of the quarter (1). 

The profile function parameter (profileFunctionParam) determines the exact shape of the age-price/efficiency profile. For example, for hyperbolic profiles the
closer the parameter is set to one the greater the value/efficiency is weighted towards the end of the asset’s life. 

#### 1.5 Asset life lengths 

The next four worksheets relate to asset life parameters: mean asset lives (AverageLifeLengths), minimum asset lives (Min), maximum asset lives (Max) and
coefficient of variations (CoVs). The asset lives are expressed in the periods used (for example, if using quarterly estimates multiply annual asset lives by four).
Asset lives are assigned to the period in which GFCF occurs, therefore GFCF for an asset in 1990 with a maximum asset life of 80 quarters will all be retired in
2010. The coefficient of variation determines how concentrated retirement is around the mean asset life. 

The estimates of coefficients of variation for other buildings and structures are currently too high and this is scheduled to be corrected in Blue Book 2023. 

#### 1.6 Other changes in volumes 

There are six categories for other changes in volumes (OCIV): 

- K1. Economic appearance of assets 

- K3. Catastrophic losses – these are unanticipated losses, for example war losses 

- K4. Uncompensated seizures 

- K5. Other changes in volumes not elsewhere classified 

- K61. Changes in sector allocation and structure 

- K62. Changes in classification of assets and liabilities 

Other changes in volume need to be recorded in current prices and these adjustments are added to GFCF estimates for the period recorded. 

#### 1.7 Reclassification 

Reclassifications are used to transfer assets from one sector to another and can only be used where all the stock in a sector, industry and asset combination is
reclassified. The advantages of reclassifications over adjustments are that the original life length of assets is preserved. 

#### 1.8 Other 

The ‘other’ spreadsheet is not used to produce estimates of capital stock: but must contain data up to the period the user requires to produce estimates. 

### 2 Asset, industry and sector hierarchies 

This spreadsheet (hierarchies_sector_industry_asset) includes the hierarchies by asset, industry, and sectors, which are used to produce aggregations. This
worksheet also includes the mapping of classifications of functions of governments (COFOGs) to [standard industry classifications (SICs)](https://onsdigital.github.io/dp-classification-tools/standard-industrial-classification/ONS_SIC_hierarchy_view.html).

### 3 CVM coverage table 

This spreadsheet (CVM_coverage_table) allows the user to specify which series CVM estimates should be produced for. Asset, industry, and sector descriptions should
be in the same format as in the hierarchies_sector_industry_asset spreadsheet. 

### 4 Splitting COFOGs across SICs 

Gross fixed capital formation is broken down into classification of functions of government (COFOGs) and these are then mapped to standard industry classifications
(SICs). Most COFOGs map one-to-one, but a couple map to multiple SICs and this split file apportions the relevant COFOGs across the relevant SICs. 

## R scripts 

### Produce capital stocks estimates 

The ‘Produce capital stocks estimates’ script contains three input parameters: ‘inputDir’, ‘scriptsPath’ and ‘libraryPath’. The script reads in the packages
required and then calls the following scripts to produce capital stocks estimates.  

### PIM inputs 

The ‘PIM_inputs’ script reads in all the worksheets from the PIM input spreadsheet (further information on the inputs are found in the spreadsheets section) and
reshapes them into the format required to run the PIM. 

### Run PIM 

The ‘Run_pim’ script takes the input data and uses it to produce capital stocks estimates; with the outputs of the PIM written to a rds file. 

Note that estimates are produced from 1828, however estimates of capital are only reliable where there is a sufficiently long series of GFCF estimates. The maximum
asset life of dwellings are 80 years, therefore estimates of the capital of dwelllings before 1908 will not include all the appropriate investment contributing to
the capital stock. Estimates of GFCF prior to 1997 will not be of the same quality as estimates from 1997 onwards. 

To view the various estimates in the rds files, it is necessary to use the unnest function, for example: 

df <- unnest(df) 

The rds file contains the following columns: 

*Note KPs are incorrectly labelled as CVMs. CVMs are calculated in chaining and all csv outputs are correctly labelled* 

Sector, Industry, Asset, refYear, Period, PriceIndex, gfcf_ociv, GrossStockCVM, NetStockCVM, ProductiveStockCVM, TotalChangesInVolumeCV
TotalOtherChangesInVolumeCVM, TotalOtherChangesInVolumeCP, ConsumptionOfFixedCapitalCVM, NetFixedCapitalFormationCVM, GrossStockCP, NetStockCP,
*ProductiveStockCP, TotalChangesInVolumeCP, ConsumptionOfFixedCapitalCP, NetFixedCapitalFormationCP, NominalHoldingGL, RealHoldingGL, NeutralHoldingGL,
ReturnToCapital, CapitalServicesCP, CapitalServicesCVM*, Year

The columns in italics relate to capital services, but these will not be consistent with published estimates of capital stock as we currently apply different
assumptions.

The rest of this section details the calculations that are made to produce the estimates of capital stocks. 

The first step is to convert current price estimates of GFCF and other changes in volumes into constant price (KP) estimates: 

Constant price GFCF =  Current price GFCF/Price index  (1) 

This is followed by combining GFCF to other changes in volume: 

GFCF KP + OCIV KP = GFCF KP  + K1 KP  + K3 KP  + K4 KP  + K5 KP  +  

K61 KP   + K62 KP		(2) 

To calculate estimates of capital we then need to produce the retirement and depreciation profiles. 

The retirement profile is calculated for each vintage of GFCF using the coefficient of variation, along with the minimum, mean and maximum asset lives. Each vintage
of GFCF is multiplied by the corresponding retirement profile to produce gross stock estimates by vintage. These vintages are then combined to produce estimates of
gross capital stock. 

To calculate a depreciation profile, it is necessary to combine the appropriate age-price profile with the retirement profile. 

If the profile name in configurations is set to db (declining balance) the profile is calculated using the following equation: 

Depreciation = (1-rateOfDecay/retirementAge)Period 

The rate of decay is the parameter contained in configurations, retirement age is the maximum asset life, and the profile is calculated for each period up to its
retirement age. 

If the profile name is set to hyp (hyperbolic), the profile is calculated using: - 

Depreciation = period*(1-shape)/(maximum-shape*period)	(3) 

Period is calculated from the shortest possible asset life to the maximum life of the asset. The shape is the parameter of the profile between 0 to 1, with the
greater value of the parameter the more heavily weighted to depreciating at the end of the asset’s life. Maximum is the maximum asset life. 

The coding currently only offsets retirement profiles and in a future Blue Book we will add offset to the age-efficiency/price profiles. 

If age-efficiency profiles have been calculated the next step is to convert age-efficiency profiles to age-price profiles (except for geometric profiles where age
price profiles are the same as age-efficiency profiles), using ‘ProfileConversion.R’. 

A discount rate is calculated over the course of the asset’s life: 

Discount rate = (discountRate)-maxAge 

An asset price is then calculated by multiplying the age efficiency profile by the inflation and discount rate. 

assetPrice t = Age efficiency profile t * (1+inflation rate t) * (1+discount rate t) 

The age price profile is then calculated 

priceProfile t = assetPrice t / (initial asset Price t * (1+inflationRate t )) 

For assets using combination method 1: age-price profiles are calculated for each possible asset life. The cohort profiles are then calculated by weighting the age
price profiles using the probability that the asset will have the respective life lengths. 

Combination 2 assumes assets all depreciate in the same way as those with the maximum asset life, so only this age-price profile needs to be calculated. This age
price profile is then multiplied by the retirement profile to give a depreciation profile. 

To produce net capital stock estimates, each vintage of GFCF is multiplied by the appropriate depreciation profile. Net capital stocks estimates are then produced
by accumulating each vintage of GFCF for each quarter. 

The PIM produces several further series. 

Constant price estimates of changes in net stock: 

TotalChangesInVolume KP t = NetStock KP t – NetStock KP t-1 

Note: The coding incorrectly labels KP data as CVM prior to the chaining script 

Constant price estimates of other changes in volume: 

TotalOtherChangesInVolume KP = K1 KP + K3 KP + K4 KP+ K5 KP + K61 KP+ K62 KP 

Current price estimates of other changes in volume: 

TotalOtherChangesInVolume CP = K1 CP + K3 CP + K4 CP + K5 CP + K61 CP + K62 CP 

Constant price estimates of capital consumption: 

ConsumptionOfFixedCapital KP = gfcf KP + TotalOtherChangesInVolume KP – TotalChangesInVolume KP 

Net capital formation is calculated by subtracting capital consumption from GFCF: 

NetFixedCapitalFormation KP = gfcf KP – ConsumptionOFixedCapital KP 

Current price estimates for gross stock, net stock, productive stock, total changes in volume, capital consumption and net fixed capital formation are then
calculated by reflating estimates using the same price indices used to deflate GFCF estimates. 

### Write PIM outputs 

This writes both current and constant price estimates of net capital stock, gross capital stock and capital consumption for all series. 

### Unchain 

The first step in unchain is to combine the PIM outputs with terminal costs. Terminal costs are immediately consumed and therefore not run through the PIM. 

Constant price estimates of both stocks and capital consumption are re-referenced to ensure that current price equal constant prices in the reference year. This
step will be removed when prices are referenced to the final quarter, rather than the reference year. 

Estimates of current year’s prices and previous year’s prices are then calculated for each series, further details can be found in a [methodology article on chain
linking](https://www.ons.gov.uk/economy/nationalaccounts/uksectoraccounts/methodologies/chainlinkingmethodsusedwithintheuknationalaccounts#unchaining). 

The final step carried out in this script is to move any reclassified capital to the appropriate sector. 

Another rds file is written containing estimates after reclassifications have been accounted for. 

### Aggregate 

For classifications of functions of government (COFOGs) allocated to two standard industry classifications (), these are split into COFOG series, which are 
mapped to their respective SICs. 

In some instances negative estimates of capital stock can be produced. Given that it is not possible to have negative estimates of capital stock these are set to 
zero. Negative estimates can occur due to either mismeasurement of GFCF, deflators, or incorrect assumptions (for example asset lives). We are currently 
investigating instances of negative capital stock and are looking to identify the cause of these negative values so that the relevant series can be corrected. 

Estimates of capital consumption for cultivated assets are set to zero in line with 3.140 of the [European System of Accounts 2010](https://ec.europa.eu/eurostat/documents/3859598/5925693/KS-02-13-269-EN.PDF/44cd9d01-bc64-40e5-bd40-d17df0c69334). 

The estimates at this stage are all at the same level of detail as the GFCF series and aggregated estimates are now calculated.

### Chain 

This script produces chained volume estimates for selected series: further details about chaining can be found in a [methodology article on chain-linking](https://www.ons.gov.uk/economy/nationalaccounts/uksectoraccounts/methodologies/chainlinkingmethodsusedwithintheuknationalaccounts#unchaining). 

Both annual and quarterly CVM estimates are calculated for selected series.

## Outputs

Annual and quarterly estimates of net capital stock, gross capital stock and capital consumption are produced.

Estimates for the outputs of the PIM are also saved, along with the PIM outputs after taking into account reclassifications. These provide details of each series
broken down by sector, asset and industry.

The outputs from the PIM input file will differ from published estimates of capital stock, given the PIM input file aggregates weapons systems and other machinery 
and equipment.

There are also some small differences in published current price estimates for land imporvements that we are currently investigating and looking to rectify.

Estimates for CVM's may differ due to rounding at different stages and using a different function to benchmark quarterly to annual estimates. We will look to align 
these estimates in future.

## Questions 

If you have any questions, please email capstocks@ons.gov.uk 
