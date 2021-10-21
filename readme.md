# R course: Introduction to Bayesian GLMs

[Chris Brown](https://experts.griffith.edu.au/7867-chris-brown)

Welcome to our course site for "Introduction to Bayesian GLMs". Below are instructions for getting setup or jump straight to the notes and data.

[Check out the conservation hackers site for upcoming online courses](https://www.conservationhackers.org/courses)

### [Course notes](http://www.seascapemodels.org/intro-bayes-glm/2021-11-22-intro-bayes-glm.html)

### [Data for course](https://github.com/cbrown5/intro-bayes-glm/blob/master/data-raw/data-raw.zip)

## Course Aims

Introduction to Bayesian theory, GLMs and mixed models. How to fit Bayesian models and interpret them. Gain a deeper understanding of how to develop Bayesian models.

## Package requirements

R version 4 or greater
R Packages: rstan, brms and ggplot2

## Agenda  

- Intro to Bayesian stats theory

- Bayesian estimation algorithms (MCMC and HMC)

- Simple model with brms package

- Checking convergence

- Plotting posteriors

- Lunch break ~ 30-45 minutes

- Random effects with brms

- Unusual distributions with brms

- Under the hood - custom stan code

- Playing with priors

## Setup

So that the course runs efficiently, and to save plenty of time for trying fun things in R, we'd ask that you come to the course prepared.

First, please [take this quick quiz](https://docs.google.com/forms/d/e/1FAIpQLScs1h1iogFOM9LZpg1ljtxqBRE1uNbq0ojYhUT-sI0tIDm-qg/viewform?usp=sf_link) so I can tailor the course to the experience level of the class.

This is an advanced level course, so we'll assume you know how to install R and R packages, and that you know how to use GLMs in R already (e.g. with `glm()`).

It goes without saying that you should have a recent version of R (and preferably Rstudio) installed on your computer.

We are using R version >4.0.2 currently for writing this course, so there may be some minor differences if you have a different version. We definitely recommend making sure you have version 4 or greater.

Once you have R [you'll need to install the STAN program, an algorithm for Bayesian inference](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). This can be tricky, so save lots of time to get this working.

One tip, if working on windows, is to make sure you've installed RTools (>V4) and have it properly connected to R.

If anybody has Rtools connection issues, then try these pages:
https://discourse.mc-stan.org/t/error-when-configuring-rstan-c-toolchain/17915/12
https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows#r40.

The main issue with stan on Mac occurs if you're using the new M1 chip. Good luck if you are, I haven't tried it with that.

Once rstan is installed, just install brms like a normal R package.
