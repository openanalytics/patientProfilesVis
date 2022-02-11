#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies

#include packamon.r-repos

#include packamon.r-dependencies

#include packamon.local-r-dependencies

# clinUtils
COPY clinUtils_*.tar.gz /tmp/clinUtils.tar.gz
RUN R -e "install.packages('/tmp/clinUtils.tar.gz', repos = NULL, dependencies = FALSE)"

#include packamon.runtime-settings
