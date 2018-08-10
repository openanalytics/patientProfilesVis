# build image
sudo docker build -t openanalytics/patient-profiles-vis .

# run image (without shinyproxy)
sudo docker run -p 3838:3838 openanalytics/patient-profiles-vis R -e "patientProfilesVisShiny::runPatientProfilesVisShiny()"
#point browser to: http://localhost:3838
