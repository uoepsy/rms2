---
title: "Factor Analysis"
subtitle: "Research Methods and Statistics 2<br><br> "
author: "ALEXANDER WEISS"
institute: "Department of Psychology<br>The University of Edinburgh"
date: "AY 2020-2021"
output:
  xaringan::moon_reader:
    lib_dir: libs
    css: xaringan-themer.css
    nature:
      ratio: '16:9'
      highlightStyle: github
      highlightLines: TRUE
      countIncrementalSlides: FALSE

---





# Today's topics
+ Latent variables
+ Factor analysis
+ Evaluating results
+ Factor scores
+ Sample size

---
# Real friends don't let friends do PCA.

## W. Revelle, 25 October 2020

---

# Factor analysis is suitable regardless of your answers...
+ Why are your variables correlated?
  + Agnostic/don't care
  + Believe there *are* underlying "causes" of these correlations


+ What are your goals?
  + Just reduce the number of variables
  + Reduce your variables and learn about/model their underlying
  (latent) causes
  
# ...and it's a more realistic model

---

# Latent variables
+ One of many features that distinguish factor analysis and principal
  components analysis

+ Key concept of psychometrics (factor analysis is a part)


+ Theorized common cause (e.g., cognitive ability) of responses to a
  set of variables
  + Explain correlations between measured variables
  + Held to be true
  + No direct test of this theory (yet)

---
class:center
![](figures/nazipolice.png)
---

class:center
![](figures/pca_vs_efa.png)

.pull-left[
\begin{equation}
\mathbf{z} = x_{1}w_{1} + x_{2}w_{2} + x_{3}w_{3}
\end{equation}
]

.pull-right[
\begin{equation}
y_{1}=\lambda_{1}\xi+e_{1} \\
y_{2}=\lambda_{2}\xi+e_{2} \\
y_{3}=\lambda_{3}\xi+e_{3} \\
cov(\xi, e_{j})=0
\end{equation}
]

---

# PCA versus EFA
+ PCA
  + The observed measures $(x_{1}, x_{2}, x_{3})$ are independent variables
  + The component $(\mathbf{z})$ is the dependent variable
  + Explains as much variance in the measures $(x_{1}, x_{2}, x_{3})$
  as possible
  + Components are determinate


+ EFA
  + The observed measures $(y_{1}, y_{2}, y_{3})$ are dependent
  variables
  + The factor $(\xi)$, is the independent variable
  + Models the relationships between variables
  $(r_{y_{1},y_{2}},r_{y_{1},y_{3}}, r_{y_{2},y_{3}})$
  + Factors are *in*determinate	

---
# Modeling the data
|        |   Item 1 |   Item 2 |   Item 3 | Item 4 | Item 5 | Item 6 | Item 7 | Item 8 |
|-------:|---------:|---------:|---------:|-------:|-------:|-------:|-------:|-------:|
| Item 1 |     1.00 |          |          |        |        |        |        |        |
| Item 2 | **0.60** |     1.00 |          |        |        |        |        |        |
| Item 3 | **0.55** | **0.61** |     1.00 |        |        |        |        |        |
| Item 4 | **0.45** | **0.48** | **0.71** |   1.00 |        |        |        |        |
| Item 5 |     0.10 |     0.01 |     0.04 |   0.13 |   1.00 |        |        |        |
| Item 6 |     0.05 |     0.00 |     0.08 |   0.20 |   0.52 |   1.00 |        |        |
| Item 7 |     0.14 |     0.02 |     0.11 |   0.14 |   0.76 |   0.51 |   1.00 |        |
| Item 8 |     0.07 |     0.11 |     0.13 |   0.04 |   0.68 |   0.54 |   0.48 |   1.00 |

---
# Modeling the data
|        | Item 1 | Item 2 | Item 3 | Item 4 |   Item 5 |   Item 6 |   Item 7 | Item 8 |
|-------:|-------:|-------:|-------:|-------:|---------:|---------:|---------:|-------:|
| Item 1 |   1.00 |        |        |        |          |          |          |        |
| Item 2 |   0.60 |   1.00 |        |        |          |          |          |        |
| Item 3 |   0.55 |   0.61 |   1.00 |        |          |          |          |        |
| Item 4 |   0.45 |   0.48 |   0.71 |   1.00 |          |          |          |        |
| Item 5 |   0.10 |   0.01 |   0.04 |   0.13 |     1.00 |          |          |        |
| Item 6 |   0.05 |   0.00 |   0.08 |   0.20 | **0.52** |     1.00 |          |        |
| Item 7 |   0.14 |   0.02 |   0.11 |   0.14 | **0.76** | **0.51** |     1.00 |        |
| Item 8 |   0.07 |   0.11 |   0.13 |   0.04 | **0.68** | **0.54** | **0.48** |   1.00 |

---

# Modeling the data
+ EFA tries to explain patterns of correlations
	+ If, for our three items, the model (factor or $\xi$) is good, it
	will explain their interrelationships
	+ Read the dots $(\cdot)$ as "given" or "controlling for"

\begin{equation}
\rho(y_{1},y_{2}\cdot\xi)=corr(e_{1},e_{2})=0 \\
\rho(y_{1},y_{3}\cdot\xi)=corr(e_{1},e_{3})=0 \\
\rho(y_{2},y_{3}\cdot\xi)=corr(e_{2},e_{3})=0 \\
\end{equation}

---
# Modeling the data
+ Factor analysis has to distinguish between the true and
  unique variance
  + True variance
	  + Variance common to an item and at least one other item
	  + Variance specific to an item that is not shared with any other
      items
  + Unique variance
	  + Variance specific to an item that is not shared with any other
      items
	  + Error variance

\begin{equation}
var(total) = var(common) + var(specific) + var(error)
\end{equation}

---
# The general factor model equation
$$\mathbf{\Sigma}=\mathbf{\Lambda}\mathbf{\Phi}\mathbf{\Lambda'}+\mathbf{\Psi}$$

$\mathbf{\Sigma}$: A $p \times p$ observed covariance matrix (from data)

$\mathbf{\Lambda}$: A $p \times m$ matrix of factor loadings (relates
the $m$ factors to the $p$ items)

$\mathbf{\Phi}$: An $m \times m$ matrix of correlations between
factors ("goes away" with orthogonal factors)

$\mathbf{\Psi}$: A diagonal matrix with $p$ elements indicating unique
(error) variance for each item

---
# Assumptions
+ Another way that factor analysis resembles regression
  + The residuals/error terms $(e)$ should be uncorrelated (it's a
    diagonal matrix, remember!)
  + The residuals/errors should not correlate with  factor
  + Relationships between items and factors should be linear, although
  there are models that can account for nonlinear relationships

---
# Communalities
+ The most efficient (i.e., least computationally intensive) way to
  factor analyze data is to start by estimating communalities
  + Communalities are estimates of how much true variance any variable has
  + Indicate how much variance in an item is explained by other
  variables, or factors
  + They appear in the diagonal of your correlation matrix
  + What are the communalities in a PCA? 


+ Estimating communalities is difficult because population
  communalities are unknown
  + Range from 0 (no shared variance) to 1 (all variance is shared)
  + Occasionally estimates will be $\ge 1$ (called a 'Heywood Case')
  + Methods often are iterative and "mechanical" as a result

---

# Principal axis factoring
+ Principal factors with squared multiple correlation (SMC)
  1. Compute initial communalities from SMCs, which are multiple
  correlations of each item regressed on all $p-1$ other variables
  2. Once we have these reasonable lower bounds, we substitute the 1s
  in the diagonal of our correlation matrix with the SMCs derived in
  step 1
  3. Obtain the factor loading matrix using the eigenvalues and
  eigenvectors of the matrix obtained in the step 2


+ Some versions of principal axis factor use an iterative approach in
  which they replace the diagonal with the communalities obtained in
  step 3, and then repeat step 3, and so on, a set number of times

---

# Method of minimum residuals
+ This is an iterative approach and the default of the `fa` procedure
  1. Starts with some other solution, e.g., PCA or principal axes,
  extracting a set number of factors
  2. Adjusts loadings of all factors on each variable so as to
  minimize the residual correlations for that variable


+ MINRES doesn't "try" to estimate communalities


+ If you apply principal axis factoring to the original correlation
  matrix with a diagonal of communalities derived from step 2,
  you get the same factors as in the method of minimum residuals

---

# Maximum likelihood estimation
+ Uses a general iterative procedure for estimating parameters
  + Assume a distribution for your data, e.g., a normal
  distribution
  + Your covariance matrix contains information related to the
  parameters, that is, factor loadings, uniqueness, and correlations
  between factors
  + The procedure works to find values for these parameters that
  maximize the likelihood of obtaining the covariance matrix
  

+ Method offers the advantage of providing numerous "fit" statistics
  that you can use to evaluate how good your model is compared to
  alternative models


+ Assumes a distribution (usually the multivariate normal)
  + The methods I described before do not
  + Which approach would be the most 'robust' to deviations from
    normality in your data?

---

# Many other methods exist

---

# Non-continuous data
+ Don't mistake response options for how the construct is likely to be
  distributed
  + I can easily get some life into a rather dull party? Yes/No
  + I have committed: 1. No crimes; 2. A minor crime; 3. A major crime


+ Most constructs we measure by questionnaire are continuously
  distributed
  + It's thus *usually* okay to treat the data as if they are, too
  + The exception is for maximum likelihood factor analysis
  
  
+ If we are concerned and the construct is normally distributed, we
  can conduct our analysis on a matrix of tetra/polychoric
  correlations
  
  
+ If the construct is not normally distributed, you can conduct a
  factor analysis that allows for these kinds of variables

---

# Factor indeterminacy
+ One tricky thing about factor analysis is that, when there are two
  or more factors, you run into the problem of *factor indeterminacy*,
  sometimes called factor score indeterminacy
  + This occurs because factors are inferred from the pattern of
  correlations
  + Compare that to PCA where components are products of the data
  

+ Factor indeterminacy means that there are an infinite number of
  pairs of factor loadings and factor score matrices which will fit
  the data **equally well**, and are thus **indistinguishable** by any
  numeric criteria
  
  
+ There is no **unique solution** to the factor problem

---
# Factor indeterminacy
+ To demonstrate this, let's create a population correlation matrix
  + In the population, eight items define two factors
	+ Items V1 to V4 define one
	+ Items V5 to V8 define the other

---
To create these data, I'll use `sim.hierarchical` from the psych package


```r
## No general loads on the factors
gload <- matrix(c(0,0))

## Loadings for the population; note each item has a small loading on the other factor
fload <- matrix(c(.7, .8, -.6, .55, .05, .13, .22, -.15, .2, .17, .05, -.15, .65, .80,
                  .50, .75), ncol=2)

## I'll save the population  correlation matrix to an object
simdata <- sim.hierarchical(gload=gload, fload=fload)

## Now I'll extract the factors twice, once with a varimax rotation
## and once with a varimin rotation (aims for the opposite of simple
## structure. The loadings will be saved to two matrices

fa_vmax <- round(matrix(fa(simdata,nfactors=2, rotate="varimax")$loadings,ncol=2),2)

fa_vmin <- round(matrix(fa(simdata,nfactors=2, rotate="varimin")$loadings,ncol=2),2)
```

```
## Loading required namespace: GPArotation
```

---
Here are the three pairs of factor loadings based on the population correlations


|       |     I|    II|   |   |     I|    II|   |   |     I|    II|
|:------|-----:|-----:|--:|--:|-----:|-----:|--:|--:|-----:|-----:|
|item 1 |  0.70|  0.20|   |   |  0.62| -0.39|   |   |  0.23|  0.69|
|item 2 |  0.80|  0.17|   |   |  0.66| -0.48|   |   |  0.20|  0.79|
|item 3 | -0.60|  0.05|   |   | -0.36|  0.48|   |   |  0.03| -0.60|
|item 4 |  0.55| -0.15|   |   |  0.26| -0.51|   |   | -0.13|  0.56|
|item 5 |  0.05|  0.65|   |   |  0.52|  0.40|   |   |  0.65|  0.03|
|item 6 |  0.13|  0.80|   |   |  0.68|  0.44|   |   |  0.80|  0.10|
|item 7 |  0.22|  0.50|   |   |  0.52|  0.17|   |   |  0.51|  0.20|
|item 8 | -0.15|  0.75|   |   |  0.46|  0.61|   |   |  0.74| -0.18|
The first pair shows the population structure. Of the remaining two,
which do you think is varimax rotated and which do you think is
varimin rotated? Why?

---

The correlation between items 1 and 2 (r = 0.594) is recoverable
from the population factor loadings

.pull-left[

|       |     I|    II|
|:------|-----:|-----:|
|item 1 |  0.70|  0.20|
|item 2 |  0.80|  0.17|
|item 3 | -0.60|  0.05|
|item 4 |  0.55| -0.15|
|item 5 |  0.05|  0.65|
|item 6 |  0.13|  0.80|
|item 7 |  0.22|  0.50|
|item 8 | -0.15|  0.75|
]

.pull-right[

```r
(.70*.80) + (.20*.17)
```

```
## [1] 0.594
```

]

---

This also works out for the varimax-rotated factors (bear rounding
errors in mind)

.pull-left[

|       |     I|    II|
|:------|-----:|-----:|
|item 1 |  0.23|  0.69|
|item 2 |  0.20|  0.79|
|item 3 |  0.03| -0.60|
|item 4 | -0.13|  0.56|
|item 5 |  0.65|  0.03|
|item 6 |  0.80|  0.10|
|item 7 |  0.51|  0.20|
|item 8 |  0.74| -0.18|
]

.pull-right[

```r
(.23*.20) + (.69*.79)
```

```
## [1] 0.5911
```
]

---
and for the varimin-rotated factors

.pull-left[

|       |     I|    II|
|:------|-----:|-----:|
|item 1 |  0.62| -0.39|
|item 2 |  0.66| -0.48|
|item 3 | -0.36|  0.48|
|item 4 |  0.26| -0.51|
|item 5 |  0.52|  0.40|
|item 6 |  0.68|  0.44|
|item 7 |  0.52|  0.17|
|item 8 |  0.46|  0.61|
]

.pull-right[

```r
(.62*.66) + (-.39*-.48)
```

```
## [1] 0.5964
```
]

---

# Evaluating results
+ As with PCA, you will probably want to rotate (or transform) your factors


+ The methods for doing so are the same as for principal components analysis


+ Should you use an orthogonal or oblique transformation? (question for
  PCA, too)
	+ Should ideally depend on theory, but that's not always feasible
	+ One view is to *always* use an oblique rotation
		+ If the factors *are* orthogonal, you will get the same
          results
		+ Might lead to fitting noise (my thought)
	+ What I do is rotate both ways and compare the results
		+ If they are similar and the correlations are low, I report the
          orthogonal rotation
		+ If they are different and the correlations are high, I report
          the oblique rotation

---
# Evaluating results
+ Good idea to start by examining how much variance each factor
  accounts for and the total amount of variance
+ Next look at factor loadings; each cell in the loading matrix
  represents the association between an item and a factor
  + Factors 'load on' items (think of the direction of the arrows!)
  + People often say items 'load on' factors, which is okay, but not
  correct technically
+ You interpret the meaning of the factors based on the size and the
  sign of the loadings that you deem to be "salient"
  + What's salient is defined by research question
  + In personality, and most other, research, salient loadings are
  those $\ge |.3| or |.4|$
+ What might these factors be (assume loadings are salient)?
  + Negative loadings on 'disorganized', 'scatterbrained', and 'lazy',
  and positive loadings on 'thoughtful' and 'persistent'
  + *Negative* loadings on grades in mathematics, English, Latin, and
  geography

---
# Check results
+ Need to also look for signs of trouble
  + Heywood cases
	  + If present, something is **wrong**; you should not trust these results
	  + Try different rotation, eliminate item, rethink whether
	  factor analysis is right "tool"
  + Are there items that do not have any salient loadings?
	  + Could signal a problem item, which should be removed
	  + Could signal presence of another factor; can pursue this when
        revising the questionnaire
  + Do some items have multiple salient loadings (cross-loadings)?
	  + If trying to develop a scale, then it's best to drop these
        items
	  + If you just want to understand the items' structure, these may
	  be informative
  + Do any factors load on only two or three items?
	  + May have over-extracted
	  + May be that you're trying to measure too many things with too
        few items

---
# Check results

**Remember**:
If you deleted one or more items, you **must** re-run your factor analysis
starting at trying to determine how many factors you should extract


**Most important is this**: 
If one or more factors don't make sense, then either your items are
bad, your theory is bad, your analysis is bad, or all three are bad!

---
# Replicability
+ After conducting a factor analysis and developing a questionnaire,
  it's a good idea to test whether it replicates
  + One way to do this is to see whether similar factors appear when
  similar data are collected
	  + The Big Five's success, in part, was because its domains
	  showed up *everywhere*
	  + The "positive manifold" of mental abilities that suggests
	  there is a general intelligence factor also shows up everywhere
  + Another way is to test this formally by collecting data on another
    sample
	  + Compute congruence coefficients between the factors
	  + 'Targeted' rotations that try to rotated one set of factors
	  towards another set
	  + Confirmatory factor analysis where you specify what factors
	 load on what items
		
---

# Confirmatory factor analysis
+ In EFA, all factors load on all items
  + These loadings, as you have seen, are purely data driven
  + However, if we have idea about which items should group, we may
    want to test this explicitly


+ In CFA, we specify a model and test how well it fits the data
  + We specify a model by indicating what loadings we believe will be zero
  + We then try to reject this model


+ CFA is powerful and can be used for other purposes, too (ask me
  if you're interested)
  + Test whether people 'use' a test in the same way across samples
  + Control for measurement error
  + Separate our method and trait variance
  
---
.pull-left[
<center>
# Exploratory
</center>
\begin{bmatrix}
\lambda_{11} & \lambda_{12} \\
\lambda_{21} & \lambda_{22} \\
\lambda_{31} & \lambda_{32} \\
\lambda_{41} & \lambda_{42} \\
\lambda_{51} & \lambda_{52} \\
\lambda_{61} & \lambda_{62} \\
\lambda_{71} & \lambda_{72} \\
\lambda_{81} & \lambda_{82}
\end{bmatrix}
]

.pull-right[
<center>
# Confirmatory
</center>
\begin{bmatrix}
0 & \lambda_{12} \\
0 & \lambda_{22} \\
0 & \lambda_{32} \\
0 & \lambda_{42} \\
\lambda_{51} & 0 \\
\lambda_{61} & 0 \\
\lambda_{71} & 0 \\
\lambda_{81} & 0
\end{bmatrix}
]

+ If the correlations derived from the CFA don't match the
  correlations from your data, it *probably* means that parameters set
  to zero should not have been set to zero

---

# Confirmatory factor analysis
+ Requires strong theory


+ Requires that the constructs you're studying exhibit simple
  structure, i.e., *not* personality
  

+ Never, ever, ever conduct a CFA on the same data you used for your
  EFA
  + You end up massively inflating the Type I error rate
  + This is true for other sorts of analyses, too
  + Like many statisticians, it's my strong view, and you can quote
    me, that "double-dipping" in statistics is even worse than
    double-dipping with chips and salsa!

---

# Factor scores
+ Sometimes EFA is an end in and of itself: research question may
  concern the structure of a set of items


+ However, you usually want to "do something" with your factors
  + Test whether your construct is related to other constructs in ways
  that you would predict
  + Test whether your construct is related systematically to other
  variables, including those that you manipulate
 
  
+ This pursuit was key to the resurgence of trait theory


+ To do these things, you need variables that represent what you've measured

---

# Factor scores
+ Unit-weighting
  + Sum raw scores on the observed variables which have primary
  loadings on each factor
  + Which items to sum is a matter of defining what loadings are salient
  + Need to reverse score items with negative loadings
+ Regression-based methods
  + Thurstone or Thompson method
	  + Ordinary least squares approach
	  + Computes scores from observed item correlations and loadings
   + Bartlett method focuses on minimizing sums of squares for unique
   factors
+ How do they fare?
  + Simple, produce estimates that are highly correlated with more
    exact techniques (especially true for unit-weighting)
  + Negative consequence of unit-weighting or regression method is they
  can produce correlated scores for orthogonal factors

---

# Factor scores
+ Anderson-Rubin and Ten Berge methods
  + Negative consequence of unit-weighting or regression method is
  they can produce correlated scores for orthogonal factors
  + The Anderson-Rubin method preserves orthogonality of factors
  + The Ten Berge method generalizes the Anderson-Rubin method to
    preserve correlations (or lack thereof) between factors

---

# Choosing factor scores
+ Simple sum scores (unit-weighting) require strict properties in the
data, but these are rarely tested, and do not often hold


+ If the goal is to try to find higher-order factors, the correlations
  between the scores are important, and so ten Berge scores are preferable
  + Not doing this can lead to really biased results
  + An alternative is to factor the Phi matrix


+ Alternative is to use structural equations modeling, which includes
  a measurement component (a CFA) and a structural component
  (regression)
  + Doesn't require you to compute factor scores
  + Requires good theory of measurement and structure
  + If your constructs don't approximate simple structure, you may
    have to turn to other alternatives
  
---

# Sample size
+ In the past, rules of thumb have guided sample size decisions for
  factor analysis (reviewed by MacCallum et al., 1999)
  + Rules based on minimum number of participants: 100, 200, 250, 500
  + Rules based on the participant-to-item (*N*:*p*) ratio: 3 to 6, 5,
    10


+ MacCallum et al. (1999) tested what was important using simulated
  datasets
  + Tested how well sample sizes of 60, 100, 200, and 400 would
  recover factors from each of nine population correlation 
  + The populations varied in two ways
	  + Items to factor (*p*:*m*) ratio (10:3, 20:7, 20:3)
	  + Communalities: low (.2, .3, .4), wide (.2 to .8 in .1
	  increments), high (.6, .7, .8)

---
# Sample size
+ As MacCallum et al. hypothesized, the crucial determinants of
  minimum sample size was not the number of items, but communalities
  and *p*:*m*
	  + Fewer subjects were needed if communalities were wide or high
	  + Fewer subjects were needed if *p*:*m* was high
	  + Communalities were even more important when *p*:*m* = 20:7 (an
        interaction effect)


+ Subsequent studies support these findings; all reject that the
  *N*:*p* ratio should be used

---

# Sample size
+ Thus, when planning a study you should do the following to determine
  your minimum sample size
  + Think of how many factors you expect and get many items measuring
  each
  + Use pilot data and previous studies to make an "educated guess"
  about what communalities you're going to expect
  
---

# Factor analysis and theory construction
+ People often get hung up on a theory (T. C. Chamberlin discussed
  this in 1890)


+ Although you're not testing hypotheses, factor analysis can be
  used to revise one's theory


+ Also important to test whether factors replicate and that they *are*
  what you *think* they are

---

# GIGO
+ Make sure to check the quality of your data


+ PCA and factor analysis cannot turn bad data into good data


+ 'Garbage in, garbage out'
