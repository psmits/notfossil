Poisson regression where response is number of fossils of order occurring in geologic/macrostrat unit. The zeroes are really important to this problem. Covariates are also interesting here. Because the unit of study is number of occurrences per the geologic/macrostrat unit, we can use fossil group information, geological, and environmental information. At the momment, all fossil occurrences have an order, class, and phylum which gives three group levels making for a fun multi-level problem. If their relative relatedness could somehow be included, all the better. For geologic information I have thickness, duration, spatial movement over duration, tropical at start/stop of unit, if unit changes tropical/temperate over duration, (rough) areal extent, and lithological composition (large matrix). I don't have environmental information yet, though that might be coming from Seth's collaborators.


The simplest model is:
\[
  y &~ Poisson(exp((matrix unit covariates) * (matrix reg coef Beta) + taxonomy_[j[i]]))
\]

I think the most logical extension of this is actually a hurdle model where the unit covariates are included in the zero-generating process, but taxonomy and unit information are included in the count-generating process.

\[
  p(y | \theta, \lambda)  = 
  \begin{cases}
    \theta & \text{if } y = 0, and \\
    (1 - \theta) \frac{Poisson(y, \lambda)}{1 - PoissonCDF(0 | \lambda)} & y > 0.
  \end{cases}
\]


\theta is modeled is a logistic regression where the unit covariates are included (e.g. lithological composition, size, etc.). \lambda is modeled like a Poisson regression where the unit covariates are included along with taxonomic information. This means that the unit covariates have two very different interpretations. For the zero-generating process, the parameter associated with each covariate corresponds to their effect on NOT seeing a fossil. While in the count-generating process, the covariates correspond to the effect on seeing a fossil. It would be important to make that clear when explaining this analysis.


What might be of particular interest for the future, if extremely difficiult to incorporate, is taking into account spatio-temporal structure in the data to better understand how sampling/avaliability is driven where a unit is and when it was "forming."
