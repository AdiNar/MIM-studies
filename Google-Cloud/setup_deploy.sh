set -e

LOCATION="europe-west3"
ZONE="europe-west3-b"

if [ $# -ne 3 ]; then
  echo 1>&2 "Usage: $0 OAUTH_CLIENT_ID OAUTH_CLIENT_SECRET DOMAIN"
  echo 1>&2 ""
  echo 1>&2 "Please read before you run the script:"
  echo 1>&2 "Go to Apis & Services -> Oauth Consent Page -> External"
  echo 1>&2 "Pick the name you want and save"
  echo 1>&2 "Go to Apis & Services -> Credentials -> Create Credentials -> Oauth Client ID"
  echo 1>&2 "Create ID for Web Application and add <https:// DOMAIN /auth/google-login/callback> to Authorized redirect URIs"
  echo 1>&2 ""
  echo 1>&2 "After the script is successful please go to Datastore page and create Firestore in Datastore mode in ${LOCATION}"
  exit 3
fi

set -o xtrace
OAUTH_CLIENT_ID=$1
OAUTH_CLIENT_SECRET=$2
DOMAIN=$3
APP_SECRET=$(python3 -c 'import secrets; print(secrets.token_hex(32))')

# Init gcloud
gcloud init
PROJECT_ID=$(gcloud config get-value project)
PROJECT_NUMBER=$(gcloud projects list --filter="${PROJECT_ID}" --format="value(PROJECT_NUMBER)")

# Enable APIs
gcloud services enable "bigquery.googleapis.com"
gcloud services enable "bigquerystorage.googleapis.com"
gcloud services enable "cloudbuild.googleapis.com"
gcloud services enable "cloudkms.googleapis.com"
gcloud services enable "cloudtrace.googleapis.com"
gcloud services enable "compute.googleapis.com"
gcloud services enable "containerregistry.googleapis.com"
gcloud services enable "datastore.googleapis.com"
gcloud services enable "deploymentmanager.googleapis.com"
gcloud services enable "firestore.googleapis.com"
gcloud services enable "logging.googleapis.com"
gcloud services enable "monitoring.googleapis.com"
gcloud services enable "oslogin.googleapis.com"
gcloud services enable "servicemanagement.googleapis.com"
gcloud services enable "serviceusage.googleapis.com"
gcloud services enable "sourcerepo.googleapis.com"
gcloud services enable "storage-api.googleapis.com"
gcloud services enable "storage-component.googleapis.com"

## Create KMS key
#gcloud kms keyrings create app-keyring --location "${LOCATION}"
#gcloud kms keys create bigquery-key --location "${LOCATION}" --purpose "encryption" --keyring app-keyring

# It seems that email address we need becomes present after this command
bq show --encryption_service_account

gcloud kms keys add-iam-policy-binding \
--member "serviceAccount:bq-${PROJECT_NUMBER}@bigquery-encryption.iam.gserviceaccount.com" \
--role roles/cloudkms.cryptoKeyEncrypterDecrypter \
--location="${LOCATION}" \
--keyring=app-keyring \
bigquery-key

# Cloud build permissions
gcloud projects add-iam-policy-binding $PROJECT_ID \
--member "serviceAccount:${PROJECT_NUMBER}@cloudbuild.gserviceaccount.com" \
--role roles/compute.admin
gcloud projects add-iam-policy-binding $PROJECT_ID \
--member "serviceAccount:${PROJECT_NUMBER}@cloudbuild.gserviceaccount.com" \
--role roles/iam.serviceAccountUser

# Oauth, should work but but is not
#gcloud alpha iap oauth-brands create --applicationTitle="${APP_TITLE}" --supportEmail="${SUPPORT_EMAIL}"
#gcloud alpha iap oauth-clients create projects/"${PROJECT_ID}"/brands/BRAND-ID --display_name=client-key

# Ssl
gcloud compute ssl-certificates create 'lb-cert' --domains="${DOMAIN}" --global

# Trigger cloud build, will produce docker image for deployment
gcloud builds submit --config cloudbuild_initial.yaml .

# Deployment Manager
gcloud deployment-manager deployments create configuration \
--template deployment_templates/application.jinja \
 --properties "OAUTH_CLIENT_ID:${OAUTH_CLIENT_ID},OAUTH_CLIENT_SECRET:${OAUTH_CLIENT_SECRET},zone:${ZONE},APP_SECRET:${APP_SECRET}"

IP=$(gcloud compute addresses list | awk '{if ($1 == "configuration-application-ipaddress") {print $2}}')

echo "Please add DNS entry ${DOMAIN} -> ${IP}"
