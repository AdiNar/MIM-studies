set -e
set -o xtrace

# Init Repository and triggers
REPO_NAME=bigquery-project

gcloud init && git config credential.helper gcloud.sh
PROJECT_ID=$(gcloud config get-value project)
gcloud source repos create $REPO_NAME
git remote rm google || true
git remote add google "https://source.developers.google.com/p/${PROJECT_ID}/r/${REPO_NAME}"
git push --all google
git push --tags google

gcloud beta builds triggers create cloud-source-repositories \
  --repo=bigquery-project \
  --tag-pattern="v.*" \
  --build-config=cloudbuild.yaml