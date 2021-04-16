__Linear regression model output summary tables__

_Hypothesis 1_

```{r}
Call:
lm(formula = logBiomass ~ Depth + NH4 + ChlA + Sal_B + Temp_B + 
    DO_B + Turb, data = all_fish_sub1)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.93566 -0.13590  0.00273  0.19784  0.92413 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
(Intercept) -1.052827   1.144031  -0.920   0.3602  
Depth        0.624799   0.343769   1.817   0.0729 .
NH4         -0.106421   0.042302  -2.516   0.0139 *
ChlA        -0.059200   0.038080  -1.555   0.1240  
Sal_B       -0.009440   0.007028  -1.343   0.1831  
Temp_B      -0.009908   0.014331  -0.691   0.4914  
DO_B         0.066180   0.042601   1.553   0.1243  
Turb         0.013928   0.013579   1.026   0.3082  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3781 on 79 degrees of freedom
Multiple R-squared:  0.3635,	Adjusted R-squared:  0.3071 
F-statistic: 6.445 on 7 and 79 DF,  p-value: 5.19e-06
```

```{r}
Analysis of Variance Table

Model 1: logBiomass ~ Depth + NH4 + ChlA + Sal_B + Temp_B + DO_B + Turb
Model 2: logBiomass ~ Depth + NH4 + ChlA + Sal_B + Turb
  Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
1     79 11.293                              
2     81 12.070 -2  -0.77729 2.7188 0.07213 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

```{r}
Call:
lm(formula = logBiomass ~ (Depth + NH4 + ChlA + Sal_B + Temp_B + 
    DO_B + Turb) * ThermalGuild, data = fish_therm_sub2)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.42308 -0.20607 -0.01632  0.23323  1.48606 

Coefficients:
                               Estimate Std. Error t value Pr(>|t|)   
(Intercept)                  -2.3070008  1.3095736  -1.762  0.07950 . 
Depth                         0.3787574  0.3824957   0.990  0.32314   
NH4                          -0.0191065  0.0297705  -0.642  0.52167   
ChlA                         -0.0064498  0.0431471  -0.149  0.88131   
Sal_B                        -0.0047748  0.0080317  -0.594  0.55278   
Temp_B                        0.0074191  0.0167152   0.444  0.65758   
DO_B                          0.1394820  0.0512293   2.723  0.00699 **
Turb                          0.0002443  0.0157326   0.016  0.98763   
ThermalGuildcool/warm        -0.1859582  1.8520167  -0.100  0.92011   
ThermalGuildwarm              1.1854115  1.8472536   0.642  0.52172   
Depth:ThermalGuildcool/warm   0.4910590  0.5409307   0.908  0.36496   
Depth:ThermalGuildwarm        0.2693283  0.5405489   0.498  0.61880   
NH4:ThermalGuildcool/warm    -0.0292177  0.0421018  -0.694  0.48842   
NH4:ThermalGuildwarm         -0.0219338  0.0420844  -0.521  0.60275   
ChlA:ThermalGuildcool/warm   -0.0261075  0.0610192  -0.428  0.66917   
ChlA:ThermalGuildwarm         0.0185032  0.0607981   0.304  0.76115   
Sal_B:ThermalGuildcool/warm   0.0025013  0.0113585   0.220  0.82590   
Sal_B:ThermalGuildwarm       -0.0121967  0.0113547  -1.074  0.28392   
Temp_B:ThermalGuildcool/warm -0.0213980  0.0236389  -0.905  0.36634   
Temp_B:ThermalGuildwarm      -0.0395614  0.0236324  -1.674  0.09553 . 
DO_B:ThermalGuildcool/warm   -0.0547594  0.0724492  -0.756  0.45055   
DO_B:ThermalGuildwarm        -0.0613284  0.0715314  -0.857  0.39216   
Turb:ThermalGuildcool/warm    0.0110049  0.0222492   0.495  0.62136   
Turb:ThermalGuildwarm         0.0075987  0.0221688   0.343  0.73210   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4274 on 223 degrees of freedom
Multiple R-squared:  0.3509,	Adjusted R-squared:  0.2839 
F-statistic: 5.241 on 23 and 223 DF,  p-value: 1.066e-11
```

