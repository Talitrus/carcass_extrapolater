# PBT Sample Size Calculator

A simple Shiny application for determining sampling requirements for Parentage-Based Tagging (PBT) of salmon carcasses.

## Features
- **Total Adult Run Size Input**: Define the total estimated population of adult salmon.
- **Target Identifiable Offspring Slider**: Set the target percentage of offspring you wish to be able to identify via PBT.
- **Pre-spawn Mortality Slider**: Account for adults that die before spawning and don't contribute to offspring.
- **Genotyping Success Rate Slider**: Account for samples that fail to genotype. The app automatically scales up the required collection effort.

## How to Run
1. Ensure you have the following packages installed:
   ```r
   install.packages(c("shiny", "bslib", "tidyverse", "bsicons"))
   ```

2. Run the app by running this command in R:
```r
shiny::runGitHub("Talitrus/carcass_extrapolater")
```

## Model Logic
The application uses the probability of identifying a juvenile ($P_j$) based on the adult sampling fraction ($p$):

$$P_j = 1 - (1 - p)^2$$

To find the required sampling fraction ($p$) for a target $P_j$, the equation is rearranged to:

$$p = 1 - \sqrt{1 - P_j}$$

The total carcasses to sample is then derived by multiplying $p$ by the total adult run size.