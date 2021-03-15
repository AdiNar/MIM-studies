import os
import re
import time
import logging
from flask_bootstrap import Bootstrap
from flask_login import LoginManager, current_user
from google.api_core.exceptions import BadRequest
from google.cloud import datastore, bigquery
from google.cloud.bigquery import EncryptionConfiguration
from google.cloud.exceptions import NotFound
from waitress import serve
from flask import Flask, request, render_template, redirect, url_for, flash, abort

logging.basicConfig(filename='app.log', level=logging.DEBUG)

from auth import bp, User, login_required  # pylint: disable=wrong-import-position  # Must be after logging config

app = Flask(__name__, template_folder='templates/')
Bootstrap(app)

login_manager = LoginManager()
login_manager.init_app(app)


app.register_blueprint(bp)
app.secret_key = os.environ.get('APP_SECRET', os.urandom(32))


@login_manager.user_loader
def load_user(user_id):
    return User(user_id)


def bigquery_client():
    return bigquery.Client()


def datastore_client():
    return datastore.Client()


def main_page():
    return render_template('main.html')


def create_bigquery_dataset(bigquery_dataset_name):
    bigquery_dataset_name = get_bigquery_dataset_name(bigquery_dataset_name)
    dataset_id = f'{bigquery_client().project}.{bigquery_dataset_name}'
    dataset = bigquery.Dataset(dataset_id)
    dataset.location = 'europe-west3'
    try:
        bigquery_client().get_dataset(bigquery_dataset_name)
        flash('Dataset already exist, choose another name', 'error')
        redirect(url_for('main'))
    except NotFound:
        bigquery_client().create_dataset(dataset)
    return bigquery_dataset_name


def create_datastore_dataset(user_id, dataset_id, filenames):
    client = datastore_client()
    dataset_key = client.key('User', user_id, 'Dataset', dataset_id)
    dataset_data = datastore.Entity(key=dataset_key)
    dataset_data['filenames'] = filenames
    dataset_data['timestamp'] = time.time()
    client.put(dataset_data)


def escape(name):
    """
    >>> escape('full/path/filename.csv')
    'full_path_filename_csv'
    """
    return re.sub(r'[ \\/.]', '_', name)


def load_from_csv(dataset_name, table_name, key):
    """Loads single csv file into BigQuery and encrypts table with KMS key"""
    client = bigquery_client()
    table = bigquery.Table(f'{client.project}.{dataset_name}.{table_name}')
    job_config = bigquery.LoadJobConfig()
    job_config.destination_encryption_configuration = EncryptionConfiguration(key)
    job_config.source_format = bigquery.SourceFormat.CSV
    job_config.skip_leading_rows = 1
    job_config.autodetect = True

    with open(f'/tmp/temp_table_{dataset_name}.csv', 'r+b') as temp_file:
        job = client.load_table_from_file(temp_file, table, job_config=job_config)
        os.remove(temp_file.name)
    job.result()

    logging.info("Loaded {} rows into {}:{}.".format(job.output_rows, dataset_name, table_name))


@app.context_processor
def utility_processor():
    def render_information_schema(dataset_name, table_name):
        return get_information_schema(dataset_name, table_name)
    return dict(render_information_schema=render_information_schema)


def get_information_schema(dataset_name, table_name):
    dataset_name = get_bigquery_dataset_name(dataset_name)
    table = bigquery_client().get_table(f'{dataset_name}.{table_name}')

    content = query(dataset_name, f'SELECT * FROM {table_name} LIMIT 5')

    return render_template('query_results.html', table=table_name, count=table.num_rows,
                           rows=content, title=f'Table {table_name} has {table.num_rows} rows')


def verify_accesss(dataset_name, sql):
    dataset_full_name = f'{bigquery_client().project}.{dataset_name}'
    job_config = bigquery.QueryJobConfig(default_dataset=dataset_full_name, dry_run=True)
    query_job = bigquery_client().query(sql, job_config=job_config)
    dataset = bigquery_client().get_dataset(dataset_name)
    for table in query_job.referenced_tables:
        if table.dataset_id != dataset.dataset_id:
            return False
    return True


def query(dataset_name, sql, flash_details=False):
    """Verifies access and runs given sql query in dataset.
    Returns resulting rows limited to some reasonable amount"""
    try:
        if verify_accesss(dataset_name, sql):
            client = bigquery_client()
            job_config = bigquery.QueryJobConfig(default_dataset=f'{client.project}.{dataset_name}')
            query_job = client.query(sql, job_config=job_config)
            query_job.result()
            destination = client.get_table(query_job.destination)

            max_results = 100
            details = f'Job {query_job.job_id} started at {query_job.started}\n' \
                      f'Query contains {destination.num_rows} rows'
            if destination.num_rows > max_results:
                details += ', showing 100 first rows'
            if flash_details:
                flash(details, 'info')
            return client.list_rows(destination, max_results=max_results)
        else:
            flash('Query must be based on current dataset only', 'error')
            return None
    except (BadRequest, NotFound) as err:
        flash('\n'.join([f"{e['reason']}: {e['message']}" for e in err.errors]), 'error')
        return None
    except Exception as err:  # pylint: disable=broad-except  # Any exception must be caught and presented to user
        flash(str(err), 'error')
        return None


def get_uploaded_filenames():
    result = []
    for f in request.files.getlist("dataset_files"):
        result.append(escape(f.filename))
    return result


@app.route('/datasets')
@login_required
def display_datasets():
    """Displays list of all datasets"""
    client = datastore_client()
    user = current_user.email
    query = client.query(kind='Dataset', ancestor=client.key('User', user))
    query.keys_only()
    datasets_entities = list(query.fetch())

    return render_template('dataset_list.html', datasets=datasets_entities, dataset_name=user)


@app.route('/datasets/<dataset_id>', methods=['GET', 'POST'])
@login_required
def display_dataset(dataset_id):
    """Display details of single dataset. Allows to submit an SQL query"""
    client = datastore_client()
    user_id = current_user.email
    dataset_key = client.key('User', user_id, 'Dataset', dataset_id)
    dataset_entity = client.get(dataset_key)
    bigquery_dataset_name = get_bigquery_dataset_name(dataset_id)

    if not dataset_entity:
        raise Exception(f'Dataset {dataset_key} does not exist')

    if request.method == 'POST':
        text = request.form['query']
        query_title = text
        query_result = query(bigquery_dataset_name, text, flash_details=True)
    else:
        query_result, query_title = '', ''

    return render_template('dataset.html',
                           dataset_name=dataset_id,
                           tables=dataset_entity['filenames'],
                           query_result=query_result,
                           query_title=query_title)


def get_bigquery_dataset_name(dataset_name):
    """Every dataset if prefixed with a user uniqe string.
    To avoid escaping problems with emails alphanumeric hash is used instead."""
    return f'{current_user.get_anon_id()}_{dataset_name}'


def handle_upload():
    """Uploads all files send to BigQueery. Metadata are stored in Datastore"""
    project = bigquery_client().project
    location = 'europe-west3'
    keyring = 'app-keyring'
    dataset_name = request.form['name']
    filenames = get_uploaded_filenames()
    bigquery_dataset_name = create_bigquery_dataset(dataset_name)
    create_datastore_dataset(current_user.email, dataset_name, filenames)
    for file in request.files.getlist("dataset_files"):
        file.save(f'/tmp/temp_table_{bigquery_dataset_name}.csv')
        key = 'bigquery-key'
        kms_key_name = f'projects/{project}/locations/{location}/' \
                       f'keyRings/{keyring}/cryptoKeys/{key}'

        load_from_csv(bigquery_dataset_name, escape(file.filename), kms_key_name)

    return redirect(url_for('display_dataset', dataset_id=dataset_name))


@app.route('/', methods=['GET', 'POST'])
@login_required
def main():
    if request.method == 'GET':
        return main_page()
    if request.method == 'POST':
        return handle_upload()
    abort(404)


@app.route('/health')
def health_check():
    return 'Healthy'


if __name__ == '__main__':
    serve(app, host='0.0.0.0', port=8080)
