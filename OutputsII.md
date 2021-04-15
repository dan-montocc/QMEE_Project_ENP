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