# The purpose of today's class code is to walk you through the process of fitting:
## an autoML model on crypto data (with generating features)
# I believe that this is a good introduction to the use of more advanced (sometimes better) models
# for time-series prediction



# Required Packages -------------------------------------------------------

# * Installing h20 --------------------------------------------------------
# Based on http://h2o-release.s3.amazonaws.com/h2o/rel-zipf/2/index.html
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-zipf/2/R")




