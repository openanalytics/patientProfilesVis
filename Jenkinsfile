pipeline {
    agent any
    options {
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }
    triggers {
        pollSCM('H/15 * * * *')
    }
    environment {
        IMAGE = 'glpgpatientprofiles'
        NS = 'oa'
        REG = '196229073436.dkr.ecr.eu-west-1.amazonaws.com'
        TAG = sh(returnStdout: true, script: "echo $BRANCH_NAME | sed -e 's/[A-Z]/\\L&/g' -e 's/[^a-z0-9._-]/./g'").trim()
        DOCKER_BUILDKIT = '1'
    }
    stages {
        stage('Build Image') {
            agent {
                kubernetes {
                    yaml '''
                    apiVersion: v1
                    kind: Pod
                    spec:
                      containers:
                      - name: dind
                        image: 196229073436.dkr.ecr.eu-west-1.amazonaws.com/oa-infrastructure/dind
                        securityContext:
                          privileged: true'''
                    defaultContainer 'dind'
                }
            }
            steps {
                withOARegistry {
                    sh "docker build --build-arg BUILDKIT_INLINE_CACHE=1 --cache-from ${env.REG}/${env.NS}/${env.IMAGE}:${env.TAG} --cache-from ${env.REG}/${env.NS}/${env.IMAGE}:master -t ${env.NS}/${env.IMAGE}:${env.TAG} -f Dockerfile.build ."
                    copyArtifacts filter: '*.tar.gz', fingerprintArtifacts: true, projectName: 'git/glpgStyle/master', selector: lastSuccessful()
                    copyArtifacts filter: '*.tar.gz', fingerprintArtifacts: true, projectName: 'git/GLPGUtilityFct/master', selector: lastSuccessful()
                }
                ecrPush "${env.REG}", "${env.NS}/${env.IMAGE}", "${env.TAG}", '', 'eu-west-1'
            }
        }
        stage('Packages') {
            agent {
                kubernetes {
                    yaml """
                    apiVersion: v1
                    kind: Pod
                    spec:
                      containers:
                      - name: r
                        image: ${env.REG}/${env.NS}/${env.IMAGE}:${env.TAG}
                        command: 
                        - cat
                        tty: true
                        imagePullPolicy: Always"""
                    defaultContainer 'r'
                }
            }
            stages {
                stage('patientProfilesVis') {
                    stages {
                        stage('Roxygen') {
                            steps {
                                sh 'R -q -e \'roxygen2::roxygenize("package/patientProfilesVis", load = "source")\''
                            }
                        }
                        stage('Build') {
                            steps {
                                sh 'R CMD build package/patientProfilesVis'
                            }
                        }
                        stage('Check') {
                            steps {
                                sh 'ls patientProfilesVis_*.tar.gz && R CMD check patientProfilesVis_*.tar.gz --no-manual'
                            }
                        }
                        stage('Install') {
                            steps {
                                sh 'R -q -e \'install.packages(list.files(".", "patientProfilesVis_.*.tar.gz"), repos = NULL) \''
                            }
                        }
                    }
                }
                stage('Archive artifacts') {
                    steps {
                        archiveArtifacts artifacts: '*.tar.gz, *.pdf, **/00check.log', fingerprint: true
                    }
                }
            }
        }
    }
}

