#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies

#include packamon.r-repos

#include packamon.r-dependencies

#include packamon.local-r-dependencies

# extra dependencies of clinUtils
RUN R -e "remotes::install_version('haven', version = '2.3.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('png', version = '0.1-7', upgrade = FALSE)" && \
    R -e "remotes::install_version('htmlwidgets', version = '1.5.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('plotly', version = '4.9.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('DT', version = '0.17', upgrade = FALSE)" && \
    R -e "remotes::install_version('data.table', version = '1.14.0', upgrade = FALSE)"

# clinUtils
COPY clinUtils_*.tar.gz /tmp/clinUtils.tar.gz
RUN R -e "install.packages('/tmp/clinUtils.tar.gz', repos = NULL, dependencies = FALSE)"

#include packamon.runtime-settings
