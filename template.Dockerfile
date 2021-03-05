#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies

#include packamon.r-repos

#include packamon.r-dependencies

#include packamon.local-r-dependencies

# extra dependencies of glpgStyle
RUN R -e "remotes::install_version('bookdown', version = '0.21', upgrade = FALSE)" && \
    R -e "remotes::install_version('viridisLite', version = '0.3.0', upgrade = FALSE)"

# glpgStyle
COPY glpgStyle_*.tar.gz /tmp/glpgStyle.tar.gz
RUN R -e "install.packages('/tmp/glpgStyle.tar.gz', repos = NULL, dependencies = FALSE)"

# extra dependencies of glpgUtilityFct
RUN R -e "remotes::install_version('haven', version = '2.3.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('png', version = '0.1-7', upgrade = FALSE)" && \
    R -e "remotes::install_version('htmlwidgets', version = '1.5.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('plotly', version = '4.9.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('officer', version = '0.3.16', upgrade = FALSE)" && \
    R -e "remotes::install_version('DT', version = '0.17', upgrade = FALSE)" && \
    R -e "remotes::install_version('data.table', version = '1.14.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('flextable', version = '0.6.3', upgrade = FALSE)" 

# glpgUtilityFct
COPY glpgUtilityFct_*.tar.gz /tmp/glpgUtilityFct.tar.gz
RUN R -e "install.packages('/tmp/glpgUtilityFct.tar.gz', repos = NULL, dependencies = FALSE)"

#include packamon.runtime-settings
