# tidytypes

## Overview

Tidytypes is a toolkit to download type strain names of published prokaryotic species in a tidy format from various websites. It can handle single species or entire prokaryotic genera as queries. The package was developed for a research project and is at the moment not being actively maintained. It might be buggy, there is no documentation and the logging could be weird. But all this might improve in the future if I need this functionality again or if you need it badly and ask nicely. 

Tidytypes is probably mainly useful to download data from [LPSN](http://www.bacterio.net/). It can also download from [StrainInfo](http://www.straininfo.net), but this database seems to be no longer available. Finally, tidytypes can also download data from [BacDive](https://bacdive.dsmz.de/), but the data there is probably identical (or very similar) to the data that has recently been updated on [PNU](https://www.dsmz.de/services/online-tools/prokaryotic-nomenclature-up-to-date). 

## Installation

Important remark: tidytypes scrapes various websites with taxonomic information. This is not allowed without permission from the website creators, so ask for it before you make any results public. 

To install tidytypes, run the following R code: 

```R
# install.packages("devtools")
devtools::install_github("SWittouck/tidytypes", ref = "v0.1.0")
```
