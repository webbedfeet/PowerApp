
## Instructions

__Sample size and Statistical Power__

Set a range for the sample size N. This is the sample size *per arm* of the study.  
Set the statistical power to 80% or 90%

__American College of Radiology responses__

The [ACR20 and ACR50](https://eprovide.mapi-trust.org/instruments/american-college-of-rheumatology-20-50-70-criteria) are composite measures that are set to 1 if an individual shows 20% (50%) improvement.

You can set the baseline proportion of individuals meeting ACR20 and ACR50 (say, in your placebo 
or standard of care group). The defaults are 50% and 20% respectively

[__Continuous Disease Activity Measures__](https://www.rheumatology.org/Practice-Quality/Clinical-Support/Quality-Measurement/Disease-Activity-Functional-Status-Assessments)

We need the standard deviations for DAS28 and SDAI activity measures to compute the statistically detectable differences in these measures.
These are set at values estimated from the data in Dasgupta & Ward (2018), but can be entered on 
the left as numeric values based on your data

## Outputs

There are two tabs above -- __Superiority__ and __Non-inferiority__.  Each provides insight into statistical equivalence
of different effect sizes from the different measures.

__Superiority__

The top table gives, for particular sample sizes, statistical power, baseline ACR proportions and continuous measure standard deviations,
the minimal statistically detectable differences. For the ACR measures, _p0_ refers to the proportion with ACR positive in the 
reference group, and _p1_ refers to the proportion ACR positive in the test group that would be required to be statistically different from the reference group. The DAS28 and SDAI columns show the corresponding minimum detectable differences between the two arms for these measures.

The graphs show, for different baseline ACR20 proportions, equivalent differences in ACR20 proportions and continuous measures between the two arms over the specified range of sample sizes and statistical power.

__Non-inferiority__

The table shows, for the specified sample sizes and statistical power, the smallest non-inferiority margin
that will show non-inferiority when there truly is no difference between the arms. 

### About this app

This app, and the companion paper (Dasgupta & Ward, 2018), helps provide insight about changes in 
different rheumatoid arthritis research metrics of improvement that would be equivalent under 
null hypothesis statistical testing, given specified range of  sample sizes,  statistical power, and false positive
rate (Type I error) of 5%. 
