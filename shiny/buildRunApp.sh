# build image
cd /home/lcougnaud/git/GLPGPatientProfiles/shiny/
sudo docker build -t openanalytics/patient-profiles-vis .

# launch shiny proxy with:
java -jar shinyproxy-2.0.3.jar
# then connect to http://localhost:8080/ with login: 'jack' and password: 'password'

# if the configuration file has changed, shinyproxy needs to be restarted:
java -jar /opt/shinyproxy/2.0.3/shinyproxy-2.0.3.jar restart

# run image (without shinyproxy)
sudo docker run -p 3838:3838 openanalytics/patient-profiles-vis
# point browser to: http://localhost:3838
# get into the docker container:
sudo docker ps
sudo docker exec -it 5cdcd9d2d88d /bin/bash

## shiny proxy

# kill the process using the 8080 port
sudo netstat -peanut | grep ":8080 "
sudo kill -9  11805
java -jar /opt/shinyproxy/2.0.3/shinyproxy-2.0.3.jar
# check: http://localhost:8080/, user: 'tesla', password: 'password'


