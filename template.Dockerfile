#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies
RUN apt-get update && apt-get install --no-install-recommends -y \
    texinfo \ 
    texlive-latex-base \
    texlive-latex-recommended \  
    texlive-latex-extra \
    texlive-fonts-recommended \
    texlive-fonts-extra
    
RUN pandoc --version

#include packamon.r-repos

#include packamon.r-dependencies

#include packamon.local-r-dependencies

#include packamon.runtime-settings
