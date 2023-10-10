# Exploratory Data Analysis Library to be Used in R
R Library to perform Exploratory Data Analysis  
</br>
### Contents   
*AED_var_continua.R -* functions for continuous variables analysis.    

*Analise_normalidade.R -* functions for normallity hypothesis testing.   

*Analise_outliers.R -* functions for outlier detection.   
</br>
### Details  
- *analise_valores_faltantes* : check if there are missing values in each dataset variable.  
- *analise_numerica_var_continua* : calculate measusres of position, spread, assimetry and kurtosis.  
- *histograma* : plot histogram using Ggplot2.
- *density_plot* : plot density graph using Ggplot2.
- *box_plot* : plot box-plot using Ggplot2.  
- *teste_normalidade_shapiro* : perform Shapiro-Wilk test to check normallity hypothesis.  
- *teste_normalidade_anderson_darling* : perform Anderson-Darling test to check normallity hypothesis.
- *qqplot* : draw qqplot to perform graphical identification of normallity hypothesis.
- *detecta_outlier_teste_rosner* : perform Rosner test to detect the presence of outliers.  
- *transforma_variavel* :  perform variable transformations in order to became normally distributed. The possible transformations are:
  -   Lambert W x F
  -   Box Cox
  -   Yeo-Johnson
  -   Ordered Quantile
  -   Logarithmic
  -   Squared root
  -   Exponential
  -   Hyperbolic Arcsine 
