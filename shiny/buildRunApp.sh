# build image
sudo docker build -t openanalytics/patient-profiles-vis .

# run image (without shinyproxy)
sudo docker run -p 3838:3838 openanalytics/patient-profiles-vis R -e "patientProfilesVisShiny::runPatientProfilesVisShiny()"
#point browser to: http://localhost:3838
# get into the docker container:
sudo docker ps
sudo docker exec -it 5cdcd9d2d88d /bin/bash

## shiny proxy

# kill the process using the 8080 port
sudo netstat -peanut | grep ":8080 "
sudo kill -9  11805
java -jar /opt/shinyproxy/2.0.1/shinyproxy-2.0.1.jar 
# check: http://localhost:8080/, user: 'tesla', password: 'password'


