Lab 10 - Grading the professor, Pt. 2
================
Lindley Slipetz
04/02/2021

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

### Exercise 1

``` r
evals <- evals
```

### Exercise 2

``` r
m_bty <- linear_reg() %>% 
            set_engine('lm') %>% 
            set_mode('regression')
m_bty_fit <- m_bty %>% 
          fit(score ~ bty_avg, data = evals)
m_bty_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  10ms 
    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)      bty_avg  
    ##     3.88034      0.06664

``` r
glance(m_bty_fit$fit)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0350        0.0329 0.535      16.7 0.0000508     1  -366.  738.  751.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

The model is score = 3.88034 + 0.6664bty\_avg. R^2 = 0.035 and adjusted
R^2 = 0.033

### Exercise 3

``` r
m_bty_gen <- linear_reg() %>% 
            set_engine('lm') %>% 
            set_mode('regression')
m_bty_gen_fit <- m_bty_gen %>% 
          fit(score ~ bty_avg + gender, data = evals)
m_bty_gen_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  0ms 
    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg + gender, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)      bty_avg   gendermale  
    ##     3.74734      0.07416      0.17239

``` r
glance(m_bty_gen_fit$fit)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic     p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>       <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0591        0.0550 0.529      14.5 0.000000818     2  -360.  729.  745.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

score = 3.74734 + 0.7416bty\_avg + 0.17239gender. R^2 = 0.059 and
adjusted R^2 = 0.055.

### Exercise 3

For someone with a 0 beauty score who is female, the average score is
3.74734. A one unit increase in score is associated with a 0.7416 point
increase in beauty score, holding gender constant. A one unit increase
in score is more associated with males, holding beauty score constant.

### Exercise 4

5.9% of variability in score is explained by the model.

### Exercise 5

The equation for just males is score = 3.91973 + 0.7416bty\_avg

### Exercise 6

Males tend to have higher scores given the same beauty rating.

### Exercise 7

Males scores change by +0.17239, female scores change by -0.17239.

### Exercise 8

Adjusted R^2 takes into account the number of predictors. Adjusted R^2
for m\_bty was 0.033. Adjusted R^2 for m\_bty\_gen was R^2 = 0.055. This
tells us that the addition of the gender variable increases variability
explained. So, it’s useful.

### Exercise 9

The slope of bty\_avg was 0.6664 for m\_bty. The slope of bty\_avg was
0.7416. Yes, the addition of gender in the model has changed the
parameter estimate for beauty. Beauty now has a greater influence when
we control for gender.

### Exercise 10

Create a new model called m\_bty\_rank with gender removed and rank
added in. Write the equation of the linear model and interpret the
slopes and intercept in context of the dat

``` r
m_bty_rank <- linear_reg() %>% 
            set_engine('lm') %>% 
            set_mode('regression')
m_bty_rank_fit <- m_bty_rank %>% 
          fit(score ~ bty_avg + rank, data = evals)
m_bty_rank_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  0ms 
    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg + rank, data = data)
    ## 
    ## Coefficients:
    ##      (Intercept)           bty_avg  ranktenure track       ranktenured  
    ##          3.98155           0.06783          -0.16070          -0.12623

``` r
glance(m_bty_rank_fit$fit)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0465        0.0403 0.533      7.46 0.0000688     3  -363.  737.  758.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

The equation is score = 3.98155 + 0.06783bty\_avg - 0.16070TT - 0.12623T
with an R^2 of 0.047 and an adjusted R^2 of 0.040. The intercept tells
us that the average score for a teaching with zero beauty score is
3.98155. A one unit change in score is associated with a 0.06783 change
in bty\_avg when rank is held constant. The other two slopes tell us
that higher scores are associated with teaching faculty. The adjusted
R^2 tells us that the addition of rank makes the model better explain
variance.

### Exercise 11

I don’t think \# of professors teaching the course will have as strong
of an effect as the other variables. I was thinking that language in
which a Professor received their degree might not matter, but then I
thought there could be xenophobic implications to some languages. I
chose cls\_profs because I just don’t see the presence of another
professor having an effect. I would think it would have just as much of
an effect as you having other professors teach your other classes. So, I
just don’t think there’s anything special about multiple professors.

### Exercise 12

``` r
m_cls_profs <- linear_reg() %>% 
            set_engine('lm') %>% 
            set_mode('regression')
m_cls_profs_fit <- m_cls_profs %>% 
          fit(score ~ cls_profs, data = evals)
m_cls_profs_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  0ms 
    ## 
    ## Call:
    ## stats::lm(formula = score ~ cls_profs, data = data)
    ## 
    ## Coefficients:
    ##     (Intercept)  cls_profssingle  
    ##         4.18464         -0.02923

``` r
glance(m_cls_profs_fit$fit)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1  0.000649      -0.00152 0.544     0.299   0.585     1  -374.  755.  767.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

The equation is score = 4.18464 - 0.2923cls\_profsingle. The R^2 is
0.000649. So, I think we can say that this model doesn’t really do a
good job of explaining the variance.

### Exercise 13

You should not include cls\_did\_eval. This information is redundant
with the information you get from cls\_perc\_eval and cls\_students.

### Exercise 14

``` r
m_all <- linear_reg() %>% 
            set_engine('lm') %>% 
            set_mode('regression')
m_all_fit <- m_all %>% 
          fit(score ~ rank + ethnicity + gender + 
                language + age + cls_perc_eval +
                cls_students + cls_level + cls_profs +
                cls_credits + bty_avg, data = evals)

summary(m_all_fit$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ rank + ethnicity + gender + language + 
    ##     age + cls_perc_eval + cls_students + cls_level + cls_profs + 
    ##     cls_credits + bty_avg, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

``` r
glance(m_all_fit$fit)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.164         0.141 0.504      7.33 2.41e-12    12  -333.  694.  752.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

### Exercise 15

cls\_prof has issues (i.e., it’s not significant), so we’ll remove that.

``` r
m_all <- linear_reg() %>% 
            set_engine('lm') %>% 
            set_mode('regression')
m_all_fit <- m_all %>% 
          fit(score ~ rank + ethnicity + gender + 
                language + age + cls_perc_eval +
                cls_students + cls_level + 
                cls_credits + bty_avg, data = evals)

summary(m_all_fit$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ rank + ethnicity + gender + language + 
    ##     age + cls_perc_eval + cls_students + cls_level + cls_credits + 
    ##     bty_avg, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.85048 -0.31394  0.08052  0.35956  1.10356 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5286297  0.2402990  14.684  < 2e-16 ***
    ## ranktenure track      -0.1073638  0.0819096  -1.311 0.190606    
    ## ranktenured           -0.0453744  0.0651169  -0.697 0.486278    
    ## ethnicitynot minority  0.1893718  0.0760992   2.488 0.013189 *  
    ## gendermale             0.1780270  0.0513581   3.466 0.000578 ***
    ## languagenon-english   -0.1265737  0.1079088  -1.173 0.241427    
    ## age                   -0.0066619  0.0030788  -2.164 0.031006 *  
    ## cls_perc_eval          0.0056790  0.0015448   3.676 0.000265 ***
    ## cls_students           0.0004493  0.0003573   1.257 0.209319    
    ## cls_levelupper         0.0183743  0.0554870   0.331 0.740687    
    ## cls_creditsone credit  0.5109162  0.1161614   4.398 1.36e-05 ***
    ## bty_avg                0.0611497  0.0166432   3.674 0.000267 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5035 on 451 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1431 
    ## F-statistic: 8.012 on 11 and 451 DF,  p-value: 8.303e-13

``` r
glance(m_all_fit$fit)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.163         0.143 0.503      8.01 8.30e-13    11  -333.  692.  746.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

Next we’ll remove cls\_level, which is also not significant.

``` r
m_all <- linear_reg() %>% 
            set_engine('lm') %>% 
            set_mode('regression')
m_all_fit <- m_all %>% 
          fit(score ~ rank + ethnicity + gender + 
                language + age + cls_perc_eval +
                cls_students +  
                cls_credits + bty_avg, data = evals)

summary(m_all_fit$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ rank + ethnicity + gender + language + 
    ##     age + cls_perc_eval + cls_students + cls_credits + bty_avg, 
    ##     data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84644 -0.31664  0.07694  0.35603  1.10744 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5349247  0.2393099  14.771  < 2e-16 ***
    ## ranktenure track      -0.1057683  0.0816872  -1.295 0.196051    
    ## ranktenured           -0.0433041  0.0647522  -0.669 0.503986    
    ## ethnicitynot minority  0.1927488  0.0753385   2.558 0.010839 *  
    ## gendermale             0.1768728  0.0511891   3.455 0.000602 ***
    ## languagenon-english   -0.1212139  0.1065829  -1.137 0.256026    
    ## age                   -0.0065893  0.0030680  -2.148 0.032264 *  
    ## cls_perc_eval          0.0056879  0.0015431   3.686 0.000255 ***
    ## cls_students           0.0004221  0.0003474   1.215 0.225073    
    ## cls_creditsone credit  0.5003402  0.1115746   4.484 9.28e-06 ***
    ## bty_avg                0.0610867  0.0166257   3.674 0.000267 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.503 on 452 degrees of freedom
    ## Multiple R-squared:  0.1633, Adjusted R-squared:  0.1448 
    ## F-statistic:  8.82 on 10 and 452 DF,  p-value: 2.828e-13

``` r
glance(m_all_fit$fit)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.163         0.145 0.503      8.82 2.83e-13    10  -333.  690.  740.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

Next, we’ll remove rank (also non-significant).

``` r
m_all <- linear_reg() %>% 
            set_engine('lm') %>% 
            set_mode('regression')
m_all_fit <- m_all %>% 
          fit(score ~ ethnicity + gender + 
                language + age + cls_perc_eval +
                cls_students +  
                cls_credits + bty_avg, data = evals)

summary(m_all_fit$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_students + cls_credits + bty_avg, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.89519 -0.31227  0.08596  0.37022  1.09853 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.3863086  0.2094164  16.170  < 2e-16 ***
    ## ethnicitynot minority  0.2044482  0.0746764   2.738 0.006428 ** 
    ## gendermale             0.1768250  0.0503142   3.514 0.000485 ***
    ## languagenon-english   -0.1511723  0.1035293  -1.460 0.144930    
    ## age                   -0.0048725  0.0026073  -1.869 0.062298 .  
    ## cls_perc_eval          0.0057538  0.0015405   3.735 0.000212 ***
    ## cls_students           0.0004073  0.0003428   1.188 0.235355    
    ## cls_creditsone credit  0.5230953  0.1050306   4.980 9.03e-07 ***
    ## bty_avg                0.0618985  0.0165267   3.745 0.000203 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5028 on 454 degrees of freedom
    ## Multiple R-squared:  0.1602, Adjusted R-squared:  0.1454 
    ## F-statistic: 10.82 on 8 and 454 DF,  p-value: 5.463e-14

``` r
glance(m_all_fit$fit)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.160         0.145 0.503      10.8 5.46e-14     8  -334.  688.  730.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

The fit is consistently improving. cls\_students is non-significant so
we’ll remove that.

``` r
m_all <- linear_reg() %>% 
            set_engine('lm') %>% 
            set_mode('regression')
m_all_fit <- m_all %>% 
          fit(score ~ ethnicity + gender + 
                language + age + cls_perc_eval +
                cls_credits + bty_avg, data = evals)

summary(m_all_fit$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_credits + bty_avg, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9067 -0.3103  0.0849  0.3712  1.0611 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.446967   0.203191  16.964  < 2e-16 ***
    ## ethnicitynot minority  0.204710   0.074710   2.740 0.006384 ** 
    ## gendermale             0.184780   0.049889   3.704 0.000238 ***
    ## languagenon-english   -0.161463   0.103213  -1.564 0.118427    
    ## age                   -0.005008   0.002606  -1.922 0.055289 .  
    ## cls_perc_eval          0.005094   0.001438   3.543 0.000436 ***
    ## cls_creditsone credit  0.515065   0.104860   4.912 1.26e-06 ***
    ## bty_avg                0.064996   0.016327   3.981 7.99e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.503 on 455 degrees of freedom
    ## Multiple R-squared:  0.1576, Adjusted R-squared:  0.1446 
    ## F-statistic: 12.16 on 7 and 455 DF,  p-value: 2.879e-14

``` r
glance(m_all_fit$fit)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.158         0.145 0.503      12.2 2.88e-14     7  -335.  688.  725.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

That reduced the fit. So, the best fitting model is score \~ ethnicity +
gender + language + age + cls\_perc\_eval + cls\_students + cls\_credits
+ bty\_avg

### Exercise 16

credits was the most significant categorical variable and cls\_per\_eval
was the most significant numerical variable, so I’ll interpret those.
The slope of credits was 0.5230953. This means, holding all other
predictors constant, a one unit increase in score was associated with a
one credit course instead of a multi-credit course. The slope for
cls\_per\_eval was 0.0057538, meaning, holding everything else constant,
a one unit change in score is a associated with a .58% increase in
percentage of evaluation.

### Exercise 17

A person with a high score would be not a minority, male, in a one
credit class with students who participates in the evaluation, and
beautiful.

### Exercise 18

As I found out from my philosophy portfolio piece, different
universities have different breakdowns of professors. It would be
interesting to see, for instance, if having more women as professors
would normalize having women as professors and decrease the disparity of
men and women’s scores. The same could be said for ethnicity as well.
The other variables, like percent participation, seem plausibly
generalizable.
