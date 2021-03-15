#!/usr/bin/env pytest

import os
from unittest import mock
from unittest.mock import Mock

import pytest

from main import app, load_from_csv

bg_client = None
ds_client = None


def make_bg_client():
    return Mock(project='test_project',
                 load_table_from_file=
                 Mock(
                     return_value=Mock(
                         result=Mock(),
                         output_rows=[]
                     )
                 ),
                 get_dataset=Mock(return_value=
                                    Mock(dataset_id='dataset_id')
                                  ),
                 query=Mock(return_value=
                                Mock(referenced_tables=[Mock(dataset_id='dataset_id')],
                                     destination='destination')
                            ),
                 get_table=Mock(return_value=Mock(num_rows=10)),
                 list_rows=Mock(return_value=Mock(
                     schema=['col1', 'col2'],
                     __iter__=Mock(return_value=iter([['col1val1', 'col2val1'],
                                                      ['col1val2', 'col2val2']]))
                 ))
                 )


def make_ds_client():
    return Mock(get=Mock(return_value={'filenames': ['file1.csv', 'file2.csv']}))


@pytest.fixture
def client():
    return app.test_client()


@pytest.fixture
def logged_client(client):
    client.get('/auth/test/login')
    return client


@pytest.fixture(autouse=True)
def bigquery_client_mock():
    global bg_client
    bg_client = make_bg_client()
    with mock.patch('main.bigquery_client', return_value=bg_client):
        yield


@pytest.fixture(autouse=True)
def datastore_client_mock():
    global ds_client
    ds_client = make_ds_client()
    with mock.patch('main.datastore_client', return_value=ds_client):
        yield


def test_not_logged(client):
    protected_endpoints = ['/datasets', 'datasets/test', '/']

    for ep in protected_endpoints:
        assert 'Log in with Google credentials' in str(client.get(ep, follow_redirects=True).data)

    client.post('/')


def test_logged_in(logged_client):
    response = logged_client.get('/')

    assert 'Log out' in str(response.data)


def test_bigquery(logged_client):
    response = logged_client.post('/', content_type='multipart/form-data',
                           data={'dataset_files': [open('test_data/data.csv', 'rb'),
                                                   open('test_data/data2.csv', 'rb')],
                                 'name': 'test-name'},
                           follow_redirects=True)

    assert 'auth/google-login' not in str(response.data)
    assert response.status_code == 200

    assert bg_client.load_table_from_file.call_count == 2


def test_load_from_csv():
    filename = '/tmp/temp_table_dataset.csv'
    open(filename, 'a+').write('file-content')
    load_from_csv('dataset', 'table', 'kms-key')

    assert not os.path.isfile(filename)
    assert bg_client.load_table_from_file.call_count == 1
    assert bg_client.load_table_from_file.call_args[0][0].name == filename
