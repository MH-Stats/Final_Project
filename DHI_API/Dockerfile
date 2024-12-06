FROM rstudio/plumber

RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev pandoc

RUN R -e "install.packages(c('plumber', 'tidyverse', 'tidymodels', 'ranger', 'parsnip'))"

WORKDIR /app

COPY APIF.R APIF.R 

COPY diabetes_binary_health_indicators_BRFSS2015.csv diabetes_binary_health_indicators_BRFSS2015.csv

EXPOSE 8709

ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('APIF.R'); pr$run(host='0.0.0.0', port=8709)"]