# tidytypes

## Overview

Tidytypes is a toolkit to download type strain names of published prokaryotic species in a tidy format from various websites. It can handle single species or entire prokaryotic genera as queries. The package was developed for a research project and is at the moment not being actively maintained. It might be buggy, there is no documentation and the logging could be weird. But all this might improve in the future if I need this functionality again or if you need it badly and ask nicely. 

## Installation

Important remark: tidytypes scrapes various websites with taxonomic information. This is not allowed without permission from the website creators, so ask for it before you make any results public. 

To install tidytypes, run the following R code: 

```R
# install.packages("devtools")
devtools::install_github("SWittouck/tidytypes", ref = "v0.1.0")
```
