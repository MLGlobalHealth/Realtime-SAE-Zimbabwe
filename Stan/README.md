# Stan files
<! --This folder contains three stan files used in the paper. The file `model1.stan` implements our main model, where the structured spatial term ($\phi_s$) follows an Intrinsic Conditional Auto-Regressive (ICAR) prior, and the structured temporal term ($\nu_t$) follows a random walk (RW) prior. The unstructured spatial ($\theta_s$) and temporal ($\xi_t$) effects, along with the spatio-temporal interaction term ($\psi_{rt}$), which models province-level time trend variations, are assigned independent normal priors. All variance parameters are assigned Penalized Complexity (PC) priors with the specification $Pr(\sigma_u > 1) = 0.01$. The file `model2.stan` follows the same model structure as model 1 but replaces the PC priors with half-Cauchy priors, using a scale parameter of 1. The file `model3.stan` is also similar to model 1 but differs in that the spatio-temporal interaction term ($\psi_{st}$) models time trend variations at the district level instead of the province level.-->

This folder contains three Stan files used in the paper, each implementing variations of our spatio-temporal model.

## Overview of models
All models share the following core components:
- structured spatial term ($\phi_s$): follows an Intrinsic Conditional Auto-Regressive (ICAR) prior
- structured temporal term ($\nu_t$): follows a random walk (RW) prior  
- unstructured spatial effects ($\theta_s$): assigned independent normal priors
- unstructured temporal effects ($\xi_t$): assigned independent normal priors
- spatio-temporal interaction terms ($\psi_{rt}$): assigned independent normal priors, models province-level time trend variations


### `model1.stan` - Main Model
Our primary model implementation with the following specifications:
- uses the complete model structure described above
- all variance parameters use Penalized Complexity (PC) priors with specification: $Pr(\sigma_u > 1) = 0.01$

### `model2.stan` - Alternative Prior Specification
Identical model structure to model 1 with one key difference:
- replaces PC priors with half-Cauchy priors using a scale parameter of 1


### `model3.stan` - District-Level Variation
Identical model structure to model 1 but with modified spatial resolution:
- spatio-temporal interaction term ($\psi_{st}$) models time trend variations at the district level instead of the province level.
