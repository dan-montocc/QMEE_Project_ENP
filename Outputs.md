__Regression Outputs Summary File__

Call:
lm(formula = logBiomass ~ DO_B + Depth, data = fish_therm_sub3)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.31023 -0.23812 -0.03378  0.24737  1.55233 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -3.32237    0.33080 -10.044  < 2e-16 ***
DO_B         0.11556    0.02314   4.993 1.08e-06 ***
Depth        0.93297    0.11430   8.162 1.36e-14 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4464 on 264 degrees of freedom
Multiple R-squared:  0.2208,	Adjusted R-squared:  0.2149 
F-statistic: 37.41 on 2 and 264 DF,  p-value: 4.977e-15


Call:
lm(formula = logBiomass ~ DO_B * Depth * ThermalGuild, data = fish_therm_sub4)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.33732 -0.21195 -0.01246  0.23313  1.55673 

Coefficients: (2 not defined because of singularities)
                                 Estimate Std. Error t value Pr(>|t|)  
(Intercept)                       -7.8165     3.0747  -2.542   0.0116 *
DO_B                               0.6238     0.5067   1.231   0.2194  
Depth                              2.4372     1.1562   2.108   0.0360 *
ThermalGuildcool                   7.1552     3.8917   1.839   0.0672 .
ThermalGuildcool/warm              3.2727     3.8917   0.841   0.4012  
ThermalGuildwarm                   1.2560     0.8545   1.470   0.1428  
DO_B:Depth                        -0.1813     0.1888  -0.960   0.3377  
DO_B:ThermalGuildcool             -0.8270     0.6397  -1.293   0.1972  
DO_B:ThermalGuildcool/warm        -0.3310     0.6397  -0.517   0.6053  
DO_B:ThermalGuildwarm             -0.1092     0.1561  -0.700   0.4846  
Depth:ThermalGuildcool            -2.5901     1.4968  -1.730   0.0848 .
Depth:ThermalGuildcool/warm       -1.0098     1.4968  -0.675   0.5005  
Depth:ThermalGuildwarm                 NA         NA      NA       NA  
DO_B:Depth:ThermalGuildcool        0.3029     0.2453   1.235   0.2182  
DO_B:Depth:ThermalGuildcool/warm   0.1089     0.2453   0.444   0.6576  
DO_B:Depth:ThermalGuildwarm            NA         NA      NA       NA  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4304 on 251 degrees of freedom
Multiple R-squared:  0.2963,	Adjusted R-squared:  0.2599 
F-statistic:  8.13 on 13 and 251 DF,  p-value: 1.354e-13



Call:
lm(formula = logBiomass ~ DO_B + Temp_B + Depth + Sal_B, data = all_fish_sub2)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.99273 -0.18445 -0.01944  0.21710  0.93336 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept) -1.783997   0.912053  -1.956   0.0538 . 
DO_B         0.067000   0.041519   1.614   0.1104   
Temp_B      -0.016151   0.014071  -1.148   0.2544   
Depth        0.854308   0.256645   3.329   0.0013 **
Sal_B       -0.003950   0.006735  -0.587   0.5591   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.388 on 83 degrees of freedom
Multiple R-squared:  0.3043,	Adjusted R-squared:  0.2707 
F-statistic: 9.074 on 4 and 83 DF,  p-value: 3.944e-06