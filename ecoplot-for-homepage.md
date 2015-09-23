    #require(devtools); install_github("wstolte/DelwaqR")
    require(DelwaqR)

    ## Loading required package: DelwaqR

    require(plyr)

    ## Loading required package: plyr

    ## Warning: package 'plyr' was built under R version 3.2.1

    modarr <- his2arr("d:\\MODELS\\ED\\eco-scen4-run1-28-y2_2-day.his")

    ## Loading required package: stringr

    limmod = c("Limit e", "Limit nit", "Limit pho", "Limit sil")
    locations = c("WZ590_ZO_Lauwers_oos", "Ra"); name = "randomstations"
    DelwaqEcoplot(arr = modarr, locmod = locations, submod = "fPPtot", limmod = limmod, plottype = 1)

    ## Loading required package: reshape2
    ## The following `from` values were not present in `x`: Limit gro, Limit mor
    ## The following `from` values were not present in `x`: Limit gro, Limit mor
    ## Loading required package: ggplot2
    ## Loading required package: scales

    ## Warning: package 'scales' was built under R version 3.2.1

![](ecoplot-for-homepage_files/figure-markdown_strict/unnamed-chunk-2-1.png)
