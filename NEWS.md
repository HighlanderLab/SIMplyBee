---
editor_options: 
  markdown: 
    wrap: 72
---

# SIMplyBee version 0.4.2

-   20??-??-??

## Major changes
-   TODO

## New features
-   TODO

## Bug fixes
-   editCsdLocus() works now also with just 2 alleles #591

## Backgrond/package/etc. work
-   calcBeeGRMIbs() can now return centred genotype matrix, allele frequencies,
and scale factor used to calculate the GRM #594
-   Improved default for getPooledGeno() (to type="mean"") and added an example
on how to collect pooled workers' genotype accross colonies (but pooling is
done within a colony!) #592
-   We now removed setMisc() and getMisc() because we now use the new AlphaSimR
structure of the misc slot that is much easier to use. #584

# SIMplyBee version 0.4.1

-   2024-09-19

## Bug fixes

-   locations of the colonies in the D_Crossing vignettes were previously
sampled by random. This caused that on some runs some queens were left unmated,
which caused an error. We now read in the locations from a csv file.


# SIMplyBee version 0.4.0

-   2024-08-23

## Major changes

-   createColony() and createMultiColony() do not have the location
    argument anymore; use setLocation() instead; by default, location is
    now c(0, 0) PR#500


## New features
-   In setLocation(MultiColony) we can set one location (numeric) or
    multiple (list or data.frame) PR#500
-   getLocation(MultiColony) got the collapse argument
    Commit#f4e629c3e8920948ad576eae3615a86b26300790

-   We can now sample location of a swarm - see sampleLocation and
    radius arguments in swarm() PR#500 PR#502

-   New function rcircle() to sample a random point within a circle with
    a given radius PR#502

-   Removed the function createRandomCrossPlan() - that option is now
    included in the newly added createCrossPlan() fuction

-   Added the **functionality for spatially-aware mating** of honeybee
    colonies. The new functionality is included in the
    createCrossPlan(), that allows to create the cross plan according to
    the colonies' locations, and in the cross() function, that crosses
    bees according to their location. This did not break the old
    functionality of the cross function.
    
-  Unified setMisc and getMisc behaviour to comply with the new AlphaSimR behaviour

-  Added new C++ function isHeterozygous() to speed up the SIMplyBee function isCsdHeterozygous() 

## Bug fixes

-   Bug fix - get\*Haplo() functions were returning diploid drones when
    input was a Pop-class

# SIMplyBee version 0.3.0

-   2022-12-05 First public/CRAN version of the package

## Major changes

-   \~2 years of work went into this first public version!

## New features

-   Many;) See the vignettes and help pages.

## Bug fixes

-   Many bugs and issues squashed - see
    <https://github.com/HighlanderLab/SIMplyBee/issues>. Please
    contribute to the development of this package.
