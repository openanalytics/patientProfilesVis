pipeline {
    agent any
    options {
        copyArtifactPermission('/git/clinDataReview/*');
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }
    environment {
        IMAGE = 'patientprofilesvis'
        NS = 'patientprofilesvis'
        REG = '196229073436.dkr.ecr.eu-west-1.amazonaws.com'
        TAG = sh(returnStdout: true, script: "echo $BRANCH_NAME | sed -e 's/[A-Z]/\\L&/g' -e 's/[^a-z0-9._-]/./g'").trim()
        DOCKER_BUILDKIT = '1'
        NOT_CRAN = 'true'
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
                copyArtifacts filter: '*.tar.gz', fingerprintArtifacts: true, projectName: 'git/clinUtils/master', selector: lastSuccessful()   
                withOARegistry {
                    sh "docker build --build-arg BUILDKIT_INLINE_CACHE=1 --cache-from ${env.REG}/${env.NS}/${env.IMAGE}:${env.TAG} --cache-from ${env.REG}/${env.NS}/${env.IMAGE}:master -t ${env.NS}/${env.IMAGE}:${env.TAG} -f Dockerfile ."
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
                        stage('Rcpp Compile Attributes') {
                            steps {
                                sh 'R -q -e \'Rcpp::compileAttributes("patientProfilesVis")\''
                            }
                        }
                        stage('Roxygen') {
                            steps {
                                sh 'R -q -e \'roxygen2::roxygenize("patientProfilesVis")\''
                            }
                        }
                        stage('Build') {
                            steps {
                                sh 'R CMD build patientProfilesVis'
                            }
                        }
                        stage('Check') {
                            steps {
                                script() {
                                    switch(sh(script: 'ls patientProfilesVis_*.tar.gz && R CMD check patientProfilesVis_*.tar.gz', returnStatus: true)) {
                                        case 0: currentBuild.result = 'SUCCESS'
                                        default: currentBuild.result = 'FAILURE'; error('script exited with failure status')
                                    }
                                }
                            }
                        }
                        stage('Install') {
                            steps {
                                sh 'R -q -e \'install.packages(list.files(".", "patientProfilesVis_.*.tar.gz"), repos = NULL)\''
                            }
                        }
                        stage('Test and coverage') {
                            steps {
                                dir('.') {
                                    sh '''R -q -e \'code <- "testthat::test_package(\\"patientProfilesVis\\", reporter = testthat::MultiReporter$new(list(testthat::ProgressReporter$new(file = file.path(getwd(), \\"results.txt\\")), testthat::JunitReporter$new(file = file.path(getwd(), \\"results.xml\\")))))"
                                    packageCoverage <- covr::package_coverage("patientProfilesVis", type = "none", code = code)
                                    cat(paste(readLines(file.path(getwd(), "results.txt")), collapse="\n"), "\n")
                                    covr::report(x = packageCoverage, file = paste0("testCoverage-", attr(packageCoverage, "package")$package, "-", attr(packageCoverage, "package")$version, ".html"));
                                    covr::to_cobertura(packageCoverage)\''''
                                    sh 'zip -r testCoverage.zip lib/ testCoverage*.html'
                                }
                            }
                            post {
                                always {
                                    dir('.') {
                                        junit 'results.xml'
                                        cobertura autoUpdateHealth: false, autoUpdateStability: false, coberturaReportFile: 'cobertura.xml', conditionalCoverageTargets: '70, 0, 0', failUnhealthy: false, failUnstable: false, lineCoverageTargets: '80, 0, 0', maxNumberOfBuilds: 0, methodCoverageTargets: '80, 0, 0', onlyStable: false, sourceEncoding: 'ASCII', zoomCoverageChart: false
                                    }
                                }
                            }
                        }
                    }
                }
                stage('Archive artifacts') {
                    steps {
                        archiveArtifacts artifacts: '*.tar.gz, *.pdf, **/00check.log, testCoverage.zip', fingerprint: true
                    }
                }
            }
        }
    }
}

