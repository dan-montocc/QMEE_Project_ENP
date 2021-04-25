# Linear regression model output summary tables #

__Hypothesis 1__

_Full Model (scaled) with all fish data_

```{r}
Call:
lm(formula = logBiomass ~ Depth + NH4 + ChlA + Sal_B + Temp_B + 
    DO_B + Turb, data = all_fish_scale, na.action = na.exclude)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.93566 -0.13590  0.00273  0.19784  0.92413 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  0.10646    0.04053   2.627   0.0104 *
Depth        0.15630    0.08599   1.817   0.0729 .
NH4         -0.13479    0.05358  -2.516   0.0139 *
ChlA        -0.08670    0.05577  -1.555   0.1240  
Sal_B       -0.08808    0.06558  -1.343   0.1831  
Temp_B      -0.03516    0.05086  -0.691   0.4914  
DO_B         0.08230    0.05298   1.553   0.1243  
Turb         0.05099    0.04971   1.026   0.3082  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3781 on 79 degrees of freedom
Multiple R-squared:  0.3635,	Adjusted R-squared:  0.3071 
F-statistic: 6.445 on 7 and 79 DF,  p-value: 5.19e-06
```

_Full Model (NOT scaled) with all fish data_

```{r}
Call:
lm(formula = logBiomass ~ Depth + NH4 + ChlA + Sal_B + Temp_B + 
    DO_B + Turb, data = all_fish_sub1, na.action = na.exclude)

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

_Coefficient estimates upper and lower bounds (NOT scaled)_

```{r}
                  2.5 %       97.5 %
(Intercept) -3.32996328  1.224309144
Depth       -0.05945600  1.309054427
NH4         -0.19062053 -0.022221526
ChlA        -0.13499586  0.016596120
Sal_B       -0.02342956  0.004550191
Temp_B      -0.03843388  0.018617446
DO_B        -0.01861560  0.150975597
Turb        -0.01310039  0.040956766
```
_ANOVA: Full model comparison with "key" variables of interest missing (biomass)_

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

_Thermal Guild (scaled) separated Model_

```{r}
Call:
lm(formula = logBiomass ~ -1 + ThermalGuild + (Depth + NH4 + 
    ChlA + Sal_B + Temp_B + DO_B + Turb):ThermalGuild, data = fish_therm_scale)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.42308 -0.20607 -0.01632  0.23323  1.48606 

Coefficients:
                              Estimate Std. Error t value Pr(>|t|)    
ThermalGuildcool             -0.567510   0.047201 -12.023  < 2e-16 ***
ThermalGuildcool/warm        -0.482779   0.047201 -10.228  < 2e-16 ***
ThermalGuildwarm             -0.348321   0.046924  -7.423 2.39e-12 ***
ThermalGuildcool:Depth        0.094538   0.095471   0.990  0.32314    
ThermalGuildcool/warm:Depth   0.217107   0.095471   2.274  0.02391 *  
ThermalGuildwarm:Depth        0.161763   0.095337   1.697  0.09114 .  
ThermalGuildcool:NH4         -0.039602   0.061706  -0.642  0.52167    
ThermalGuildcool/warm:NH4    -0.100162   0.061706  -1.623  0.10595    
ThermalGuildwarm:NH4         -0.085065   0.061655  -1.380  0.16906    
ThermalGuildcool:ChlA        -0.009727   0.065073  -0.149  0.88131    
ThermalGuildcool/warm:ChlA   -0.049101   0.065073  -0.755  0.45131    
ThermalGuildwarm:ChlA         0.018178   0.064600   0.281  0.77866    
ThermalGuildcool:Sal_B       -0.044089   0.074163  -0.594  0.55278    
ThermalGuildcool/warm:Sal_B  -0.020993   0.074163  -0.283  0.77739    
ThermalGuildwarm:Sal_B       -0.156711   0.074113  -2.114  0.03558 *  
ThermalGuildcool:Temp_B       0.025723   0.057953   0.444  0.65758    
ThermalGuildcool/warm:Temp_B -0.048466   0.057953  -0.836  0.40388    
ThermalGuildwarm:Temp_B      -0.111441   0.057922  -1.924  0.05563 .  
ThermalGuildcool:DO_B         0.165943   0.060948   2.723  0.00699 ** 
ThermalGuildcool/warm:DO_B    0.100795   0.060948   1.654  0.09958 .  
ThermalGuildwarm:DO_B         0.092980   0.059394   1.565  0.11889    
ThermalGuildcool:Turb         0.000977   0.062928   0.016  0.98763    
ThermalGuildcool/warm:Turb    0.044995   0.062928   0.715  0.47534    
ThermalGuildwarm:Turb         0.031371   0.062473   0.502  0.61606    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4274 on 223 degrees of freedom
Multiple R-squared:  0.6496,	Adjusted R-squared:  0.6118 
F-statistic: 17.22 on 24 and 223 DF,  p-value: < 2.2e-16
```

_Thermal Guild (NOT scaled) separated Model_

```{r}
Call:
lm(formula = logBiomass ~ -1 + ThermalGuild + (Depth + NH4 + 
    ChlA + Sal_B + Temp_B + DO_B + Turb):ThermalGuild, data = fish_therm_sub2)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.42308 -0.20607 -0.01632  0.23323  1.48606 

Coefficients:
                               Estimate Std. Error t value Pr(>|t|)   
ThermalGuildcool             -2.3070008  1.3095736  -1.762  0.07950 . 
ThermalGuildcool/warm        -2.4929589  1.3095736  -1.904  0.05824 . 
ThermalGuildwarm             -1.1215892  1.3028287  -0.861  0.39022   
ThermalGuildcool:Depth        0.3787574  0.3824957   0.990  0.32314   
ThermalGuildcool/warm:Depth   0.8698164  0.3824957   2.274  0.02391 * 
ThermalGuildwarm:Depth        0.6480856  0.3819556   1.697  0.09114 . 
ThermalGuildcool:NH4         -0.0191065  0.0297705  -0.642  0.52167   
ThermalGuildcool/warm:NH4    -0.0483242  0.0297705  -1.623  0.10595   
ThermalGuildwarm:NH4         -0.0410402  0.0297458  -1.380  0.16906   
ThermalGuildcool:ChlA        -0.0064498  0.0431471  -0.149  0.88131   
ThermalGuildcool/warm:ChlA   -0.0325573  0.0431471  -0.755  0.45131   
ThermalGuildwarm:ChlA         0.0120534  0.0428338   0.281  0.77866   
ThermalGuildcool:Sal_B       -0.0047748  0.0080317  -0.594  0.55278   
ThermalGuildcool/warm:Sal_B  -0.0022735  0.0080317  -0.283  0.77739   
ThermalGuildwarm:Sal_B       -0.0169715  0.0080263  -2.114  0.03558 * 
ThermalGuildcool:Temp_B       0.0074191  0.0167152   0.444  0.65758   
ThermalGuildcool/warm:Temp_B -0.0139789  0.0167152  -0.836  0.40388   
ThermalGuildwarm:Temp_B      -0.0321423  0.0167061  -1.924  0.05563 . 
ThermalGuildcool:DO_B         0.1394820  0.0512293   2.723  0.00699 **
ThermalGuildcool/warm:DO_B    0.0847227  0.0512293   1.654  0.09958 . 
ThermalGuildwarm:DO_B         0.0781536  0.0499228   1.565  0.11889   
ThermalGuildcool:Turb         0.0002443  0.0157326   0.016  0.98763   
ThermalGuildcool/warm:Turb    0.0112491  0.0157326   0.715  0.47534   
ThermalGuildwarm:Turb         0.0078429  0.0156186   0.502  0.61606   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4274 on 223 degrees of freedom
Multiple R-squared:  0.6496,	Adjusted R-squared:  0.6118 
F-statistic: 17.22 on 24 and 223 DF,  p-value: < 2.2e-16
```

_Coefficient estimates upper and lower bounds (NOT scaled)_

```{r}
                                   2.5 %        97.5 %
ThermalGuildcool             -4.88772370  0.2737221751
ThermalGuildcool/warm        -5.07368187  0.0877640019
ThermalGuildwarm             -3.68902034  1.4458418734
ThermalGuildcool:Depth       -0.37501129  1.1325260611
ThermalGuildcool/warm:Depth   0.11604774  1.6235850919
ThermalGuildwarm:Depth       -0.10461863  1.4007899093
ThermalGuildcool:NH4         -0.07777396  0.0395609806
ThermalGuildcool/warm:NH4    -0.10699163  0.0103433054
ThermalGuildwarm:NH4         -0.09965908  0.0175785804
ThermalGuildcool:ChlA        -0.09147809  0.0785784392
ThermalGuildcool/warm:ChlA   -0.11758554  0.0524709823
ThermalGuildwarm:ChlA        -0.07235738  0.0964641963
ThermalGuildcool:Sal_B       -0.02060248  0.0110528828
ThermalGuildcool/warm:Sal_B  -0.01810116  0.0135541974
ThermalGuildwarm:Sal_B       -0.03278858 -0.0011543843
ThermalGuildcool:Temp_B      -0.02552094  0.0403590640
ThermalGuildcool/warm:Temp_B -0.04691891  0.0189610995
ThermalGuildwarm:Temp_B      -0.06506436  0.0007797086
ThermalGuildcool:DO_B         0.03852646  0.2404376004
ThermalGuildcool/warm:DO_B   -0.01623289  0.1856782443
ThermalGuildwarm:DO_B        -0.02022725  0.1765345055
ThermalGuildcool:Turb        -0.03075925  0.0312477605
ThermalGuildcool/warm:Turb   -0.01975439  0.0422526243
ThermalGuildwarm:Turb        -0.02293600  0.0386218618
```

_Species Richness (scaled) Model_

```{r}
Call:
glm(formula = SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B + 
    DO_B + Temp_B + Turb, family = poisson, data = Rich_fish_scale)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-4.6817  -0.4612   0.0930   0.6894   1.6574  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.6415688  0.0286761  92.117   <2e-16 ***
Depth       -0.0480956  0.0611558  -0.786   0.4316    
NH4          0.0011690  0.0378893   0.031   0.9754    
ChlA        -0.0008455  0.0393499  -0.021   0.9829    
Sal_B       -0.1206535  0.0473222  -2.550   0.0108 *  
DO_B         0.0038772  0.0369141   0.105   0.9163    
Temp_B       0.0034024  0.0356581   0.095   0.9240    
Turb         0.0094821  0.0346597   0.274   0.7844    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 91.713  on 86  degrees of freedom
Residual deviance: 81.927  on 79  degrees of freedom
AIC: 484.75

Number of Fisher Scoring iterations: 4
```

_Species Richness (NOT scaled) Model_

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

_Coefficient estimates upper and lower bounds (NOT scaled)_

```{r}
                  2.5 %       97.5 %
(Intercept)  1.71197514  4.875798970
Depth       -0.67207221  0.286371624
NH4         -0.05806973  0.059216153
ChlA        -0.05407322  0.051328525
Sal_B       -0.02293355 -0.003050598
DO_B        -0.05504497  0.061343506
Temp_B      -0.01876354  0.020628790
Turb        -0.01641241  0.020725373
```

_ANOVA: Full model comparison with "key" variables of interest missing (species richness)_

```{r}
Analysis of Deviance Table

Model 1: SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B + DO_B + Temp_B + 
    Turb
Model 2: SpeciesRichness ~ Depth + NH4 + ChlA + Sal_B + Turb
  Resid. Df Resid. Dev Df  Deviance
1        79     81.927             
2        81     81.941 -2 -0.013273
```


__Hypothesis 2__

_Full model (scaled) with Year as random effect_

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

Note: Not scaled not provided due to fit warning from lmer() function

_ANOVA: Full and subset model comparison_

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

_Coefficient estimates upper and lower bounds (scaled)_

```{r}
  2.5 %     97.5 %
.sig01          6.95697423 21.0129066
.sigma         14.54102223 18.9440965
(Intercept)    34.33421323 51.9125398
TEMP_B         -3.14854675  4.8841180
DO_B           -0.89915071  6.7877873
SAL_B          -1.21864684 12.1707007
NO3            -3.83142613  6.3277267
CHLA           -7.71435464  0.9164950
TURB           -8.62380458  0.0826486
TN             -0.48447342  8.6264851
DIN            -7.55321469  2.0194899
TP             -2.38198254  5.2711508
SRP            -0.05374752  7.5040495
TOC            -6.80034092  5.8122398
AvgWaterDepth  -4.36782531  5.4181972
Avg.PlantCover -6.44842950  0.8466749
```

_nlme Package Summary Output_

```{r}
Linear mixed-effects model fit by REML
  Data: Peri_dat_scale 
       AIC      BIC    logLik
  1027.026 1069.792 -497.5132

Random effects:
 Formula: ~1 | Year
        (Intercept) Residual
StdDev:    12.71405 17.52827

Fixed effects:  Avg.PeriphytonCover ~ TEMP_B + SAL_B + NO3 + CHLA + TURB + TN +      DIN + TP + SRP + TOC + AvgWaterDepth + Avg.PlantCover + DO_B 
                  Value Std.Error DF   t-value p-value
(Intercept)    43.16600  4.342720 98  9.939853  0.0000
TEMP_B          0.87369  2.158098 98  0.404845  0.6865
SAL_B           5.51553  3.578983 98  1.541089  0.1265
NO3             1.25215  2.730247 98  0.458620  0.6475
CHLA           -3.40045  2.320869 98 -1.465164  0.1461
TURB           -4.26391  2.339725 98 -1.822398  0.0714
TN              4.07613  2.428936 98  1.678156  0.0965
DIN            -2.76231  2.573017 98 -1.073567  0.2857
TP              1.46787  2.046106 98  0.717398  0.4748
SRP             3.71117  2.026957 98  1.830908  0.0702
TOC            -0.44698  3.365420 98 -0.132816  0.8946
AvgWaterDepth   0.53188  2.629422 98  0.202279  0.8401
Avg.PlantCover -2.83478  1.936519 98 -1.463854  0.1464
DO_B            2.95358  2.064254 98  1.430823  0.1557
```


__Hypothesis 3__

_Full Biomass Model (scaled)_

```{r}
Call:
lm(formula = logBiomass ~ Temp_B + DO_B + Sal_B + ChlA + Turb + 
    AvgWaterDepth + Avg.PeriphytonCover + +Avg.PlantCover, data = Fish_Peri_dat_scale)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.78335 -0.22887 -0.02811  0.21931  1.05536 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)          0.110757   0.042145   2.628  0.01046 *  
Temp_B              -0.056762   0.051968  -1.092  0.27831    
DO_B                 0.068680   0.050835   1.351  0.18086    
Sal_B               -0.228193   0.065069  -3.507  0.00078 ***
ChlA                 0.017277   0.049858   0.347  0.72994    
Turb                -0.008046   0.052111  -0.154  0.87772    
AvgWaterDepth       -0.118760   0.055065  -2.157  0.03432 *  
Avg.PeriphytonCover  0.014651   0.047958   0.306  0.76085    
Avg.PlantCover       0.106514   0.048690   2.188  0.03190 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3816 on 73 degrees of freedom
Multiple R-squared:  0.3727,	Adjusted R-squared:  0.304 
F-statistic: 5.422 on 8 and 73 DF,  p-value: 2.287e-05
```

_Full Biomass Model (NOT scaled)_

```{r}
Call:
lm(formula = logBiomass ~ Temp_B + DO_B + Sal_B + ChlA + Turb + 
    AvgWaterDepth + Avg.PeriphytonCover + +Avg.PlantCover, data = Peri_all_dat_sub1)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.78335 -0.22887 -0.02811  0.21931  1.05536 

Coefficients:
                      Estimate Std. Error t value Pr(>|t|)    
(Intercept)          0.7484636  0.5578512   1.342  0.18386    
Temp_B              -0.0163409  0.0149606  -1.092  0.27831    
DO_B                 0.0566548  0.0419346   1.351  0.18086    
Sal_B               -0.0245082  0.0069885  -3.507  0.00078 ***
ChlA                 0.0113108  0.0326403   0.347  0.72994    
Turb                -0.0019975  0.0129364  -0.154  0.87772    
AvgWaterDepth       -0.0079837  0.0037018  -2.157  0.03432 *  
Avg.PeriphytonCover  0.0007753  0.0025379   0.306  0.76085    
Avg.PlantCover       0.0122911  0.0056185   2.188  0.03190 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3816 on 73 degrees of freedom
Multiple R-squared:  0.3727,	Adjusted R-squared:  0.304 
F-statistic: 5.422 on 8 and 73 DF,  p-value: 2.287e-05
```

_Coefficient estimates upper and lower bounds (NOT scaled)_

```{r}
                           2.5 %       97.5 %
(Intercept)         -0.363332151  1.860259430
Temp_B              -0.046157351  0.013475566
DO_B                -0.026920712  0.140230366
Sal_B               -0.038436246 -0.010580099
ChlA                -0.053741292  0.076362930
Turb                -0.027779602  0.023784690
AvgWaterDepth       -0.015361298 -0.000606097
Avg.PeriphytonCover -0.004282607  0.005833266
Avg.PlantCover       0.001093419  0.023488831
```

_ANOVA: Full and subset model comparison (biomass)_

```{r}
Analysis of Variance Table

Model 1: logBiomass ~ Temp_B + DO_B + Sal_B + ChlA + Turb + AvgWaterDepth + 
    Avg.PeriphytonCover + +Avg.PlantCover
Model 2: logBiomass ~ Temp_B + DO_B + Sal_B + ChlA + Turb + AvgWaterDepth
  Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
1     73 10.632                              
2     75 11.534 -2  -0.90139 3.0944 0.05129 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

_Functional Group Model (scaled)_

```{r}
Call:
lm(formula = logBiomass ~ -1 + FunctionalGroup + (AvgWaterDepth + 
    ChlA + Sal_B + Avg.PlantCover + Avg.PeriphytonCover + Temp_B + 
    DO_B + Turb):FunctionalGroup, data = Peri_fish_scale)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.55360 -0.25663 -0.00361  0.27194  2.23732 

Coefficients:
                                                Estimate Std. Error t value Pr(>|t|)    
FunctionalGroupalgivore                        -1.362068   0.062086 -21.938  < 2e-16 ***
FunctionalGroupdetrivore                       -0.735753   0.066959 -10.988  < 2e-16 ***
FunctionalGroupinvertivore                     -0.174275   0.057937  -3.008  0.00283 ** 
FunctionalGroupomnivore                        -1.024797   0.057937 -17.688  < 2e-16 ***
FunctionalGrouppiscivore                       -0.926880   0.067720 -13.687  < 2e-16 ***
FunctionalGroupplanktivore                     -1.539622   0.089044 -17.290  < 2e-16 ***
FunctionalGroupalgivore:AvgWaterDepth          -0.157136   0.079372  -1.980  0.04854 *  
FunctionalGroupdetrivore:AvgWaterDepth         -0.111566   0.092121  -1.211  0.22670    
FunctionalGroupinvertivore:AvgWaterDepth       -0.126279   0.073423  -1.720  0.08636 .  
FunctionalGroupomnivore:AvgWaterDepth          -0.073572   0.073423  -1.002  0.31704    
FunctionalGrouppiscivore:AvgWaterDepth         -0.004356   0.085677  -0.051  0.95948    
FunctionalGroupplanktivore:AvgWaterDepth       -0.028788   0.129910  -0.222  0.82476    
FunctionalGroupalgivore:ChlA                    0.173121   0.070028   2.472  0.01392 *  
FunctionalGroupdetrivore:ChlA                  -0.017615   0.071853  -0.245  0.80648    
FunctionalGroupinvertivore:ChlA                 0.075168   0.068501   1.097  0.27327    
FunctionalGroupomnivore:ChlA                    0.039404   0.068501   0.575  0.56551    
FunctionalGrouppiscivore:ChlA                  -0.029736   0.073893  -0.402  0.68762    
FunctionalGroupplanktivore:ChlA                 0.047636   0.188098   0.253  0.80022    
FunctionalGroupalgivore:Sal_B                  -0.201675   0.091280  -2.209  0.02781 *  
FunctionalGroupdetrivore:Sal_B                 -0.237002   0.097594  -2.428  0.01568 *  
FunctionalGroupinvertivore:Sal_B               -0.172091   0.085030  -2.024  0.04376 *  
FunctionalGroupomnivore:Sal_B                   0.021121   0.085030   0.248  0.80398    
FunctionalGrouppiscivore:Sal_B                 -0.131612   0.114399  -1.150  0.25076    
FunctionalGroupplanktivore:Sal_B               -0.091939   0.155183  -0.592  0.55394    
FunctionalGroupalgivore:Avg.PlantCover          0.052513   0.068839   0.763  0.44608    
FunctionalGroupdetrivore:Avg.PlantCover         0.037116   0.072176   0.514  0.60741    
FunctionalGroupinvertivore:Avg.PlantCover       0.087396   0.066565   1.313  0.19009    
FunctionalGroupomnivore:Avg.PlantCover          0.075226   0.066565   1.130  0.25923    
FunctionalGrouppiscivore:Avg.PlantCover         0.094808   0.078710   1.205  0.22923    
FunctionalGroupplanktivore:Avg.PlantCover       0.043478   0.117877   0.369  0.71247    
FunctionalGroupalgivore:Avg.PeriphytonCover    -0.084253   0.068756  -1.225  0.22128    
FunctionalGroupdetrivore:Avg.PeriphytonCover    0.096874   0.076141   1.272  0.20414    
FunctionalGroupinvertivore:Avg.PeriphytonCover -0.009118   0.064448  -0.141  0.88758    
FunctionalGroupomnivore:Avg.PeriphytonCover     0.021450   0.064448   0.333  0.73947    
FunctionalGrouppiscivore:Avg.PeriphytonCover    0.175629   0.084304   2.083  0.03797 *  
FunctionalGroupplanktivore:Avg.PeriphytonCover  0.150258   0.119114   1.261  0.20801    
FunctionalGroupalgivore:Temp_B                 -0.068788   0.076065  -0.904  0.36646    
FunctionalGroupdetrivore:Temp_B                -0.147739   0.079312  -1.863  0.06336 .  
FunctionalGroupinvertivore:Temp_B              -0.042831   0.072245  -0.593  0.55367    
FunctionalGroupomnivore:Temp_B                 -0.097856   0.072245  -1.354  0.17648    
FunctionalGrouppiscivore:Temp_B                -0.190247   0.083485  -2.279  0.02330 *  
FunctionalGroupplanktivore:Temp_B              -0.025241   0.098601  -0.256  0.79811    
FunctionalGroupalgivore:DO_B                   -0.045539   0.072484  -0.628  0.53026    
FunctionalGroupdetrivore:DO_B                  -0.048210   0.086408  -0.558  0.57726    
FunctionalGroupinvertivore:DO_B                 0.115878   0.069945   1.657  0.09850 .  
FunctionalGroupomnivore:DO_B                    0.082917   0.069945   1.185  0.23666    
FunctionalGrouppiscivore:DO_B                   0.015918   0.073284   0.217  0.82818    
FunctionalGroupplanktivore:DO_B                 0.119346   0.117855   1.013  0.31195    
FunctionalGroupalgivore:Turb                   -0.084811   0.077740  -1.091  0.27606    
FunctionalGroupdetrivore:Turb                   0.127197   0.076063   1.672  0.09539 .  
FunctionalGroupinvertivore:Turb                -0.046138   0.070877  -0.651  0.51551    
FunctionalGroupomnivore:Turb                    0.027259   0.070877   0.385  0.70078    
FunctionalGrouppiscivore:Turb                  -0.109743   0.079990  -1.372  0.17098    
FunctionalGroupplanktivore:Turb                 0.077772   0.189431   0.411  0.68165    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5172 on 341 degrees of freedom
Multiple R-squared:  0.8204,	Adjusted R-squared:  0.792 
F-statistic: 28.85 on 54 and 341 DF,  p-value: < 2.2e-16
```

_Functional Group Model (NOT scaled)_

```{r}
Call:
lm(formula = logBiomass ~ -1 + FunctionalGroup + (AvgWaterDepth + 
    ChlA + Sal_B + Avg.PlantCover + Avg.PeriphytonCover + Temp_B + 
    DO_B + Turb):FunctionalGroup, data = Peri_fish_dat_sub1)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.55360 -0.25663 -0.00361  0.27194  2.23732 

Coefficients:
                                                 Estimate Std. Error t value Pr(>|t|)  
FunctionalGroupalgivore                         0.1887696  0.7815435   0.242   0.8093  
FunctionalGroupdetrivore                        0.9577593  0.9464476   1.012   0.3123  
FunctionalGroupinvertivore                      0.0752843  0.7573364   0.099   0.9209  
FunctionalGroupomnivore                        -0.8680538  0.7573364  -1.146   0.2525  
FunctionalGrouppiscivore                        0.2078322  0.8766168   0.237   0.8127  
FunctionalGroupplanktivore                     -2.2567308  1.3913406  -1.622   0.1057  
FunctionalGroupalgivore:AvgWaterDepth          -0.0107539  0.0054320  -1.980   0.0485 *
FunctionalGroupdetrivore:AvgWaterDepth         -0.0076352  0.0063045  -1.211   0.2267  
FunctionalGroupinvertivore:AvgWaterDepth       -0.0086422  0.0050249  -1.720   0.0864 .
FunctionalGroupomnivore:AvgWaterDepth          -0.0050350  0.0050249  -1.002   0.3170  
FunctionalGrouppiscivore:AvgWaterDepth         -0.0002981  0.0058635  -0.051   0.9595  
FunctionalGroupplanktivore:AvgWaterDepth       -0.0019702  0.0088907  -0.222   0.8248  
FunctionalGroupalgivore:ChlA                    0.1141980  0.0461933   2.472   0.0139 *
FunctionalGroupdetrivore:ChlA                  -0.0116199  0.0473970  -0.245   0.8065  
FunctionalGroupinvertivore:ChlA                 0.0495843  0.0451860   1.097   0.2733  
FunctionalGroupomnivore:ChlA                    0.0259927  0.0451860   0.575   0.5655  
FunctionalGrouppiscivore:ChlA                  -0.0196154  0.0487432  -0.402   0.6876  
FunctionalGroupplanktivore:ChlA                 0.0314230  0.1240773   0.253   0.8002  
FunctionalGroupalgivore:Sal_B                  -0.0225269  0.0101958  -2.209   0.0278 *
FunctionalGroupdetrivore:Sal_B                 -0.0264728  0.0109012  -2.428   0.0157 *
FunctionalGroupinvertivore:Sal_B               -0.0192223  0.0094978  -2.024   0.0438 *
FunctionalGroupomnivore:Sal_B                   0.0023592  0.0094978   0.248   0.8040  
FunctionalGrouppiscivore:Sal_B                 -0.0147009  0.0127783  -1.150   0.2508  
FunctionalGroupplanktivore:Sal_B               -0.0102695  0.0173337  -0.592   0.5539  
FunctionalGroupalgivore:Avg.PlantCover          0.0061987  0.0081258   0.763   0.4461  
FunctionalGroupdetrivore:Avg.PlantCover         0.0043812  0.0085197   0.514   0.6074  
FunctionalGroupinvertivore:Avg.PlantCover       0.0103163  0.0078574   1.313   0.1901  
FunctionalGroupomnivore:Avg.PlantCover          0.0088797  0.0078574   1.130   0.2592  
FunctionalGrouppiscivore:Avg.PlantCover         0.0111912  0.0092910   1.205   0.2292  
FunctionalGroupplanktivore:Avg.PlantCover       0.0051322  0.0139143   0.369   0.7125  
FunctionalGroupalgivore:Avg.PeriphytonCover    -0.0044975  0.0036703  -1.225   0.2213  
FunctionalGroupdetrivore:Avg.PeriphytonCover    0.0051712  0.0040645   1.272   0.2041  
FunctionalGroupinvertivore:Avg.PeriphytonCover -0.0004867  0.0034403  -0.141   0.8876  
FunctionalGroupomnivore:Avg.PeriphytonCover     0.0011450  0.0034403   0.333   0.7395  
FunctionalGrouppiscivore:Avg.PeriphytonCover    0.0093753  0.0045002   2.083   0.0380 *
FunctionalGroupplanktivore:Avg.PeriphytonCover  0.0080209  0.0063585   1.261   0.2080  
FunctionalGroupalgivore:Temp_B                 -0.0193214  0.0213654  -0.904   0.3665  
FunctionalGroupdetrivore:Temp_B                -0.0414975  0.0222774  -1.863   0.0634 .
FunctionalGroupinvertivore:Temp_B              -0.0120306  0.0202926  -0.593   0.5537  
FunctionalGroupomnivore:Temp_B                 -0.0274861  0.0202926  -1.354   0.1765  
FunctionalGrouppiscivore:Temp_B                -0.0534373  0.0234496  -2.279   0.0233 *
FunctionalGroupplanktivore:Temp_B              -0.0070899  0.0276954  -0.256   0.7981  
FunctionalGroupalgivore:DO_B                   -0.0382316  0.0608535  -0.628   0.5303  
FunctionalGroupdetrivore:DO_B                  -0.0404741  0.0725432  -0.558   0.5773  
FunctionalGroupinvertivore:DO_B                 0.0972844  0.0587217   1.657   0.0985 .
FunctionalGroupomnivore:DO_B                    0.0696123  0.0587217   1.185   0.2367  
FunctionalGrouppiscivore:DO_B                   0.0133635  0.0615248   0.217   0.8282  
FunctionalGroupplanktivore:DO_B                 0.1001957  0.0989443   1.013   0.3119  
FunctionalGroupalgivore:Turb                   -0.0211746  0.0194090  -1.091   0.2761  
FunctionalGroupdetrivore:Turb                   0.0317571  0.0189904   1.672   0.0954 .
FunctionalGroupinvertivore:Turb                -0.0115192  0.0176957  -0.651   0.5155  
FunctionalGroupomnivore:Turb                    0.0068056  0.0176957   0.385   0.7008  
FunctionalGrouppiscivore:Turb                  -0.0273992  0.0199710  -1.372   0.1710  
FunctionalGroupplanktivore:Turb                 0.0194172  0.0472947   0.411   0.6817  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5172 on 341 degrees of freedom
Multiple R-squared:  0.8204,	Adjusted R-squared:  0.792 
F-statistic: 28.85 on 54 and 341 DF,  p-value: < 2.2e-16
```

_Coefficient estimates upper and lower bounds (NOT scaled)_

```{r}
                                                       2.5 %        97.5 %
FunctionalGroupalgivore                        -1.3484836622  1.726023e+00
FunctionalGroupdetrivore                       -0.9038511849  2.819370e+00
FunctionalGroupinvertivore                     -1.4143549103  1.564924e+00
FunctionalGroupomnivore                        -2.3576929865  6.215854e-01
FunctionalGrouppiscivore                       -1.5164249199  1.932089e+00
FunctionalGroupplanktivore                     -4.9934213799  4.799597e-01
FunctionalGroupalgivore:AvgWaterDepth          -0.0214382926 -6.954004e-05
FunctionalGroupdetrivore:AvgWaterDepth         -0.0200357642  4.765358e-03
FunctionalGroupinvertivore:AvgWaterDepth       -0.0185257848  1.241447e-03
FunctionalGroupomnivore:AvgWaterDepth          -0.0149186330  4.848599e-03
FunctionalGrouppiscivore:AvgWaterDepth         -0.0118312636  1.123498e-02
FunctionalGroupplanktivore:AvgWaterDepth       -0.0194576443  1.551728e-02
FunctionalGroupalgivore:ChlA                    0.0233382053  2.050577e-01
FunctionalGroupdetrivore:ChlA                  -0.1048472427  8.160753e-02
FunctionalGroupinvertivore:ChlA                -0.0392941560  1.384627e-01
FunctionalGroupomnivore:ChlA                   -0.0628857455  1.148712e-01
FunctionalGrouppiscivore:ChlA                  -0.1154906090  7.625975e-02
FunctionalGroupplanktivore:ChlA                -0.2126302823  2.754764e-01
FunctionalGroupalgivore:Sal_B                  -0.0425815170 -2.472209e-03
FunctionalGroupdetrivore:Sal_B                 -0.0479148870 -5.030799e-03
FunctionalGroupinvertivore:Sal_B               -0.0379040134 -5.406672e-04
FunctionalGroupomnivore:Sal_B                  -0.0163224415  2.104090e-02
FunctionalGrouppiscivore:Sal_B                 -0.0398351250  1.043333e-02
FunctionalGroupplanktivore:Sal_B               -0.0443639532  2.382505e-02
FunctionalGroupalgivore:Avg.PlantCover         -0.0097842844  2.218173e-02
FunctionalGroupdetrivore:Avg.PlantCover        -0.0123765787  2.113900e-02
FunctionalGroupinvertivore:Avg.PlantCover      -0.0051388171  2.577149e-02
FunctionalGroupomnivore:Avg.PlantCover         -0.0065754260  2.433488e-02
FunctionalGrouppiscivore:Avg.PlantCover        -0.0070837454  2.946612e-02
FunctionalGroupplanktivore:Avg.PlantCover      -0.0222364176  3.250083e-02
FunctionalGroupalgivore:Avg.PeriphytonCover    -0.0117167060  2.721732e-03
FunctionalGroupdetrivore:Avg.PeriphytonCover   -0.0028234031  1.316588e-02
FunctionalGroupinvertivore:Avg.PeriphytonCover -0.0072536199  6.280166e-03
FunctionalGroupomnivore:Avg.PeriphytonCover    -0.0056218904  7.911896e-03
FunctionalGrouppiscivore:Avg.PeriphytonCover    0.0005235959  1.822694e-02
FunctionalGroupplanktivore:Avg.PeriphytonCover -0.0044858127  2.052764e-02
FunctionalGroupalgivore:Temp_B                 -0.0613459892  2.270312e-02
FunctionalGroupdetrivore:Temp_B                -0.0853159708  2.321041e-03
FunctionalGroupinvertivore:Temp_B              -0.0519449987  2.788380e-02
FunctionalGroupomnivore:Temp_B                 -0.0674004981  1.242830e-02
FunctionalGrouppiscivore:Temp_B                -0.0995613565 -7.313182e-03
FunctionalGroupplanktivore:Temp_B              -0.0615653557  4.738550e-02
FunctionalGroupalgivore:DO_B                   -0.1579270852  8.146392e-02
FunctionalGroupdetrivore:DO_B                  -0.1831625478  1.022143e-01
FunctionalGroupinvertivore:DO_B                -0.0182178436  2.127867e-01
FunctionalGroupomnivore:DO_B                   -0.0458900283  1.851145e-01
FunctionalGrouppiscivore:DO_B                  -0.1076523653  1.343794e-01
FunctionalGroupplanktivore:DO_B                -0.0944221973  2.948137e-01
FunctionalGroupalgivore:Turb                   -0.0593511190  1.700187e-02
FunctionalGroupdetrivore:Turb                  -0.0055959672  6.911012e-02
FunctionalGroupinvertivore:Turb                -0.0463256074  2.328719e-02
FunctionalGroupomnivore:Turb                   -0.0280008064  4.161199e-02
FunctionalGrouppiscivore:Turb                  -0.0666809416  1.188260e-02
FunctionalGroupplanktivore:Turb                -0.0736088141  1.124433e-01
```

_Full Species Richness Model (scaled)_

```{r}
Call:
glm(formula = SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA + 
    Turb + AvgWaterDepth + Avg.PeriphytonCover + Avg.PlantCover, 
    family = poisson, data = Fish_Peri_dat_scale)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-4.2602  -0.5423   0.0472   0.5906   1.4776  

Coefficients:
                     Estimate Std. Error z value Pr(>|z|)    
(Intercept)          2.628315   0.029814  88.157   <2e-16 ***
Temp_B               0.008864   0.036070   0.246   0.8059    
DO_B                 0.028150   0.035607   0.791   0.4292    
Sal_B               -0.073123   0.046140  -1.585   0.1130    
ChlA                -0.033383   0.034793  -0.959   0.3373    
Turb                 0.022834   0.035412   0.645   0.5190    
AvgWaterDepth        0.032897   0.039436   0.834   0.4042    
Avg.PeriphytonCover  0.067038   0.033799   1.983   0.0473 *  
Avg.PlantCover       0.064406   0.032936   1.955   0.0505 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 88.043  on 81  degrees of freedom
Residual deviance: 65.147  on 73  degrees of freedom
AIC: 447

Number of Fisher Scoring iterations: 4
```

_Full Species Richness Model (NOT scaled)_

```{r}
Call:
glm(formula = SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA + 
    Turb + AvgWaterDepth + Avg.PeriphytonCover + +Avg.PlantCover, 
    family = poisson, data = Peri_all_dat_sub1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-4.2602  -0.5423   0.0472   0.5906   1.4776  

Coefficients:
                     Estimate Std. Error z value Pr(>|z|)    
(Intercept)          2.238657   0.394754   5.671 1.42e-08 ***
Temp_B               0.002552   0.010384   0.246   0.8059    
DO_B                 0.023221   0.029372   0.791   0.4292    
Sal_B               -0.007853   0.004956  -1.585   0.1130    
ChlA                -0.021855   0.022778  -0.959   0.3373    
Turb                 0.005669   0.008791   0.645   0.5190    
AvgWaterDepth        0.002212   0.002651   0.834   0.4042    
Avg.PeriphytonCover  0.003548   0.001789   1.983   0.0473 *  
Avg.PlantCover       0.007432   0.003801   1.955   0.0505 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 88.043  on 81  degrees of freedom
Residual deviance: 65.147  on 73  degrees of freedom
AIC: 447

Number of Fisher Scoring iterations: 4
```

_Coefficient estimates upper and lower bounds_

```{r}
                            2.5 %      97.5 %
(Intercept)          1.463505e+00 3.011055006
Temp_B              -1.784614e-02 0.022863761
DO_B                -3.422511e-02 0.080929134
Sal_B               -1.760029e-02 0.001828097
ChlA                -6.731313e-02 0.022064052
Turb                -1.195473e-02 0.022519712
AvgWaterDepth       -2.973978e-03 0.007419798
Avg.PeriphytonCover  3.784548e-05 0.007049896
Avg.PlantCover      -1.002714e-04 0.014798214
```

_ANOVA: Full and sub comparison (species richness)_

```{r}
Analysis of Deviance Table

Model 1: SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA + Turb + AvgWaterDepth + 
    Avg.PeriphytonCover + Avg.PlantCover
Model 2: SpeciesRichness ~ Temp_B + DO_B + Sal_B + ChlA + Turb + AvgWaterDepth
  Resid. Df Resid. Dev Df Deviance
1        73     65.147            
2        75     77.313 -2  -12.166
```