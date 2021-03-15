Author: Adrian Naruszko

# Google Cloud Platform Semester Project

VM & BigQuery

## SHORT DESCRIPTION
Application allows to log in with Google Credentials. 

User can then upload multiple *.csv files that are stored as a single dataset. 

User can run any `SELECT` query within the current dataset - access restriction is based on the results of dry-run, which specifies what kind of query is that and which tables are affected. 

User can browse all uploaded datasets and query any of them.

Datasets are encrypted with CMEK keys.

Application backend is based on flask + jinja + bootstrap. 

Backend containers are defined by instance templates and their instances are organized in an instance group. This group is then used by HTTPS Load Balancer with SSL encryption.

## DEPLOYMENT - bash + Deployment Manager + a bit of manual work
Files: setup_deploy.sh, cloud/templates/*, cloud/cloudbuild_initial.yaml

Run: `./setup_deploy.sh`

The deployment is mostly automated. The manual part is described in `setup_deploy.sh` script.
I was unable to automate those steps.

The rest is done in `setup_deploy.sh`. Script does some initialization that is not easily or cannot be moved to the Deployment Manager. After initialization it calls Cloud Build trigger which builds Docker image. Then the Deployment Manager is called and build up the whole infrastructure. 

To allow ssl encryption script requires a domain as an argument and then to add DNS entry for given IP (echoed at the end of the deployment script). 

## DEVELOPMENT Source Repository + Cloud Build
Files: setup_dev.sh, cloud/cloudbuild.yaml

Run: `./setup_dev.sh`

Development is setup with `setup_dev.sh` script. 

It initializes new repository inside the project and sets remote `google` to it.

Then it adds a Cloud Build trigger so that every push with tag `v.*` will trigger an rolling update on deployment.

## PROBLEMS
1) Application is quite slow on updates and queries, I have no time to make it more responsive.
2) When the docker image is pushed it starts a rolling update, but it still causes the service to become unavailable for a while. Probably need to look at rolling update config.
3) Zone and Location is currently hardcoded to be europe-west-3(-b). This is due to lack of time to set it everywhere and that bigquery is not available everywhere.
4) Logs should be available from Console, unfortunately I have no time for this. They are stored 
5) I don’t user Packer in the solution.
6) `setup_deploy.sh` is not idempotent - double creation of keyrings and ssl results in error




