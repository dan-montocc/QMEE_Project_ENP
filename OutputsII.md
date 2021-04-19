# Linear regression model output summary tables #

__Hypothesis 1__

```{r}

Call:
lm(formula = logBiomass ~ Depth + NH4 + ChlA + Sal_B + Temp_B + 
    DO_B + Turb, data = all_fish_scale, na.action = na.exclude)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.95139 -0.19852 -0.00666  0.21802  0.92347 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.10606    0.04067   2.608   0.0108 *  
Depth        0.25727    0.06128   4.198 6.86e-05 ***
NH4         -0.07526    0.05019  -1.500   0.1376    
ChlA        -0.07085    0.05591  -1.267   0.2087    
Sal_B       -0.01600    0.04216  -0.380   0.7052    
Temp_B      -0.05232    0.05052  -1.036   0.3035    
DO_B         0.08389    0.05173   1.622   0.1088    
Turb         0.04697    0.05378   0.873   0.3851    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3836 on 81 degrees of freedom
Multiple R-squared:  0.3432,	Adjusted R-squared:  0.2864 
F-statistic: 6.046 on 7 and 81 DF,  p-value: 1.093e-05
```

_Full model comparison with "key" variables of interest missing_

```{r}
Analysis of Variance Table

Model 1: logBiomass ~ Depth + NH4 + ChlA + Sal_B + Temp_B + DO_B + Turb
Model 2: logBiomass ~ Depth + NH4 + ChlA + Sal_B + Turb
  Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
1     81 11.922                              
2     83 13.075 -2   -1.1531 3.9171 0.02378 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

_Thermal Guild separated Model_

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
_Species Richness Model_

```{r}
Call:
glm(formula = SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B + 
    DO_B + Temp_B + Turb, family = poisson, data = all_fish_sub2)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-4.6817  -0.4612   0.0930   0.6894   1.6574  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.2962178  0.8070284   4.084 4.42e-05 ***
Depth       -0.1922647  0.2444736  -0.786   0.4316    
NH4          0.0009230  0.0299149   0.031   0.9754    
ChlA        -0.0005773  0.0268672  -0.021   0.9829    
Sal_B       -0.0129301  0.0050714  -2.550   0.0108 *  
DO_B         0.0031180  0.0296853   0.105   0.9163    
Temp_B       0.0009588  0.0100481   0.095   0.9240    
Turb         0.0025901  0.0094676   0.274   0.7844    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 91.713  on 86  degrees of freedom
Residual deviance: 81.927  on 79  degrees of freedom
AIC: 484.75

Number of Fisher Scoring iterations: 4
```

__Hypothesis 2__

_Full model with Year as random effect_

```{r}
Linear mixed model fit by REML ['lmerMod']
Formula: Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA + TURB +  
    TN + DIN + TP + SRP + TOC + AvgWaterDepth + Avg.PlantCover +      (1 | Year)
   Data: Peri_dat_scale

REML criterion at convergence: 995

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-2.55811 -0.55563 -0.04887  0.55552  2.30851 

Random effects:
 Groups   Name        Variance Std.Dev.
 Year     (Intercept) 161.6    12.71   
 Residual             307.2    17.53   
Number of obs: 121, groups:  Year, 10

Fixed effects:
               Estimate Std. Error t value
(Intercept)     43.1660     4.3427   9.940
TEMP_B           0.8737     2.1581   0.405
DO_B             2.9536     2.0643   1.431
SAL_B            5.5155     3.5790   1.541
NO3              1.2521     2.7302   0.459
CHLA            -3.4005     2.3209  -1.465
TURB            -4.2639     2.3397  -1.822
TN               4.0761     2.4289   1.678
DIN             -2.7623     2.5730  -1.074
TP               1.4679     2.0461   0.717
SRP              3.7112     2.0270   1.831
TOC             -0.4470     3.3654  -0.133
AvgWaterDepth    0.5319     2.6294   0.202
Avg.PlantCover  -2.8348     1.9365  -1.464

```

_ANOVA of full and subset model_

```{r}
Data: Peri_dat_scale
Models:
Perisubmod: Avg.PeriphytonCover ~ TEMP_B + SAL_B + NO3 + CHLA + TURB + TN + 
Perisubmod:     DIN + TP + SRP + TOC + AvgWaterDepth + (1 | Year)
Perifullmod: Avg.PeriphytonCover ~ TEMP_B + DO_B + SAL_B + NO3 + CHLA + TURB + 
Perifullmod:     TN + DIN + TP + SRP + TOC + AvgWaterDepth + Avg.PlantCover + 
Perifullmod:     (1 | Year)
            npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
Perisubmod    14 1073.5 1112.7 -522.76   1045.5                     
Perifullmod   16 1073.3 1118.0 -520.66   1041.3 4.2016  2     0.1224

```