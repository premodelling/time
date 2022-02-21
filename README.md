<br>

**CHIC662: Time Series in Epidemiology**


Leading on from 

> joint probability density function = conditional probability density function &nbsp; &nbsp; X <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; marginal probability density function
> 

<br>

The required joint probability density function is

L( &phi;, &mu;(**&beta;**), &sigma; ) = (2 &pi; &sigma;<sup>2</sup>)<sup>-0.5(n - 1)</sup>
  exp[-0.5 &sigma;<sup>-2</sup> &Sigma;<sub>t = 2</sub>{ (Y<sub>t</sub> - &mu;) - &phi;(Y<sub>t - 1</sub> - &mu;) }<sup>2</sup>] 
  &nbsp; &nbsp; X <br>
  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
  (2 &pi; &kappa;)<sup>-0.5</sup>exp[-0.5 &kappa;<sup>-1</sup> (Y<sub>1</sub> - &mu;)<sup>2</sup>]

where

&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &kappa; = &sigma;<sup>2</sup> /(1 - &phi;<sup>2</sup>)

Therefore, the ln(likelihood function) is

l( &phi;, &mu;(**&beta;**), &sigma; ) = - 0.5 n ln(2 &pi; &sigma;<sup>2</sup>) + 0.5 ln(1 - &phi;<sup>2</sup>) <br>
  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
  -&nbsp;0.5 &sigma;<sup>-2</sup>
  exp[-0.5 &sigma;<sup>-2</sup> 
      ( &Sigma;<sub>t = 2</sub>{ (Y<sub>t</sub> - &mu;) - &phi;(Y<sub>t - 1</sub> - &mu;) }<sup>2</sup> + 
          (1 - &phi;<sup>2</sup>)(Y<sub>t</sub> - &mu;)<sup>2</sup> )]

<br>

Hence, [MLEC.R](./R/kericho/likelihood/MLEC.R) called by 
[InterfaceMLEC.R](./R/kericho/likelihood/InterfaceMLEC.R); [in interface.R](./R/kericho/modelling/interface.R)

<br>
<br>

### Development Environment

* Edit the help file skeletons in 'man', possibly combining help files
  for multiple functions.
* Edit the exports in 'NAMESPACE', and add necessary imports.
* Put any C/C++/Fortran code in 'src'.
* If you have compiled code, add a useDynLib() directive to
  'NAMESPACE'.
* Run R CMD build to build the package tarball.
* Run R CMD check to check the package tarball.

Read "Writing R Extensions" for more information.

<br>
<br>

<br>
<br>

<br>
<br>

<br>
<br>
