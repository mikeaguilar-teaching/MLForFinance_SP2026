Machine Learning For Finance 
FECON 490 - Duke Economics
Prof. Aguilar | mike.aguilar@duke.edu 
Spring 2026


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Files presented roughly in the order in which they are presented in class. 

Data, Supporting, and Investment Process Examples

PackageLoads.R
- Purpose: load and initialize any packages that will be required during this project
- Location: ./Supporting

DataAcquisition-SP500-Monthly.qmd
- Purpose: Load SP500 prices from via API, then clean and engineer into returns
- Location: ./Data
- Output: SP500_Mthly_Data.Rdata, SP500_Mthly_Prices.csv, SP500_Mthly_SimpleReturns.csv, SP500_Mthly_LogReturns.csv

DataAcquistion-SP500-Daily.qmd
- Purpose: Load SP500 prices from via API, then clean and engineer into returns
- Location: ./Data
- Output: SP500_Daily_Data.Rdata, SP500_Daily_Prices.csv, SP500_Daily_SimpleReturns.csv, SP500_Daily_LogReturns.csv

FeatureEngineering-SP500-Mthly-Momentum.qmd
- Purpose: Use monthly data to create momentum and related features
- Location: ./Data 
- Input: SP500_Mthly_SimpleReturns.csv
- Output:  SP500_Mthly_Momentum_Features.Rdata, SP500_Mthly_Momentum_Features.csv, SP500_Mthly_Momentum.csv

EDA-SP500-Mthly-Momentum.qmd
- Purpose: conduct basic exploratory data analysis (EDA) on monthly momentum within the SP500
- Location: ./Data
- Input: SP500_Mthly_SimpleReturns.csv, SP500_Mthly_Momentum.csv

SignalTesting_Static.R
- Purpose: Function used to evaluate a static trading signal on a single cross section of assets
- Location: ./Supporting

SignalTesting-Static-Demo.R
- Purpose: Demonstrate how to use the SignalTestingStatic function and interpret it's output
- Location: ./Supporting
- Output: ./Supporting/Output/SignalTestingStatic

SignalTesting_Dynamic.R
- Purpose: Function used to evaluate a dynamic trading signal over a panel of assets over time
- Location: ./Supporting

SignalTesting-Dynamic-Demo.R
- Purpose: Demonstrate how to use the SignalTestingDynamic function and interpret it's output
- Location: ./Supporting
- Output: ./Supporting/Output/SignalTestingDynamic

DailyPerformanceStats.R
- Purpose: compute performance and risk diagnostics for an asset relative to a benchmark
- Location: ./Supporting

DailyPerformanceStats-Demo.qmd
- Purpose: Demonstrate the DailyPerformanceStats function
- Location: ./Supporting

SP500-Mthly-Momentum-Signal-Static.R
- Purpose: Construct and evaluate a simple momentum strategy on Mthly SP500 data
- Location: ./Momentum
- Inputs: SP500_Mthly_SimpleReturns.csv, SP500_Mthly_Momentum.csv

ConstructPortfolio.R
- Purpose: function to facilitate the creation of a portfolio based upon weights_t-1 and ret_t
- Location: ./Supporting

ConstructPortfolio-Demo.qmd
- Purpose: Demonstrate how to use ConstructPortfolio
- Location: ./Supporting

SP500_Mthly_Momentum_Stratetgy-Static.qmd
- Purpose: Create a strategy based upon the momentum signal
- Location: ./Momentum
- Inputs: SP500_Daily_SimpleReturns.csv, SP500Mthly_Momentum.csv

SP500_Mthly_Momentum_Portfolio.qmd
- Purpose: Creates a portfolio by combining two strategies
- Location: ./Momentum

SP500_Mthly_Momentum_Strategy_Dynamic.qmd
- Purpose: Create a strategy based on the momentum signal, and roll that over time
- Location: ./Momentum
- Inputs: SP500_Daily_SimpleReturns.csv, SP500Mthly_Momentum.csv
		
	
		
	
		

Clustering
Unsupervised -> Clustering -> Clustering-KMeans-Signal-Momentum.qmd
Unsupervised -> Clustering -> Clustering-Hierarchical-Signal-Momentum.qmd
Unsupervised -> Clustering -> Clustering-GMM-Signal-Momentum.qmd 
Unsupervised -> Clustering -> Clustering-MST-Signal-Momentum.qmd

Generative Models
Unsupervised -> GenerativeModels -> Generative-Parameteric-Signal-Momentum.qmd
Unsupervised -> GenerativeModels -> Generative-KDE-Signal-Momentum.qmd
Unsupervised -> GenerativeModels -> Generative-GAN-Signal-Momentum.qmd

Dimension Reduction
Unsupervised -> DimensionReduction -> DimensionReduction-PCA-Signal-Momentum.qmd
Unsupervised -> DimensionReduction -> DimensionReduction-Autoencoder-Signal-Momentum.qmd

Classification
Supervised -> Classification -> Classification-Logistic-Signal-Momentum.qmd
Supervised -> Classification -> Classification-SVM-Signal-Momentum.qmd
Supervised -> Classification -> Classification-KNN-Signal-Momentum.qmd
Supervised -> Classification -> Classification-DecisionTree-Signal-Momentum.qmd

Regression
Supervised -> Regression -> Regression-Linear-Signal-Momentum.qmd
Supervised -> Regression -> Regression-Regularization-Signal-Momentum.qmd
Supervised -> Regression -> Regression-RandomForest-Signal-Momentum.qmd
Supervised -> Regression -> Regression-NeuralNet-Signal-Momentum.qmd
Supervised -> Regression -> Regression-GradientBoosting-Signal-Momentum.qmd

Time Series
TimeSeries -> TimeSeriesEDA-SP500Monthly.qmd
TimeSeries -> TimeSeriesClustering-DTWBaryCenterAvg-Signal-Momentum.qmd
TimeSeries -> TimeSeriesClassificaiton-HMM-Signal-Momentum.qmd
TimeSeries -> TimeSeriesRegression-EWMA-Signal-Momentum.qmd
TimeSeries -> TimeSeriesRegression-ARMA-Signal-Momentum.qmd
TimeSeries -> TimeSeriesRegression-LSTM-Signal-Momentum.qmd
TimeSeries -> TimeSeriesRegression-RandomForest-Signal-Momentum.qmd
TimeSeries -> TimeSeriesRegression-GRU-Signal-Momentum.qmd
TimeSeries -> TimeSeriesRegression-GradientBoosting-Signal-Momentum.qmd
TimeSeries -> TimeSeriesRegression-NBEATS-Signal-Momentum.qmd



