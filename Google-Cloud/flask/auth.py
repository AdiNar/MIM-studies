"""Handles google authentication in the application. User is recignized by its email address."""

import functools
import json
import base64
import logging
import os

import requests
from flask import redirect, url_for, session, Blueprint, render_template, request
from flask_login import login_user, current_user
from oauthlib.oauth2 import WebApplicationClient

GOOGLE_CLIENT_ID = os.environ.get("OAUTH_CLIENT_ID", None)
GOOGLE_CLIENT_SECRET = os.environ.get("OAUTH_CLIENT_SECRET", None)
GOOGLE_DISCOVERY_URL = (
    "https://accounts.google.com/.well-known/openid-configuration"
)

bp = Blueprint('auth', __name__, url_prefix='/auth')


class User:
    def __init__(self, email):
        self.email = email
        self.is_authenticated = True
        self.is_active = True
        self.is_anonymous = False

    def get_id(self):
        return self.email

    def get_anon_id(self):
        return base64.b32encode(bytes(self.email, encoding='utf-8')).decode().replace('=', '')


def login_required(view):
    @functools.wraps(view)
    def wrapped_view(**kwargs):
        if current_user.is_anonymous:
            return redirect(url_for('auth.login'))

        return view(**kwargs)

    return wrapped_view


@bp.route('/logout')
def logout():
    session.clear()
    return redirect(url_for('main'))


@bp.route('/login')
def login():
    return render_template('login.html')


@bp.route('/test/login')
def test_login():
    if not 'TEST' in os.environ:
        redirect(url_for('main'))
    session['email'] = 'test@test.com'
    login_user(User('test-token'))

    return ''


def get_google_provider_cfg():
    return requests.get(GOOGLE_DISCOVERY_URL).json()


def force_https(url):
    return url.replace('http:', 'https:')


@bp.route("/google-login/callback")
def callback():
    google_provider_cfg = get_google_provider_cfg()
    token_endpoint = google_provider_cfg["token_endpoint"]
    code = request.args.get("code")
    client = WebApplicationClient(GOOGLE_CLIENT_ID)

    token_url, headers, body = client.prepare_token_request(
        token_endpoint,
        authorization_response=force_https(request.url),
        redirect_url=force_https(request.base_url),
        code=code
    )

    token_response = requests.post(
        force_https(token_url),
        headers=headers,
        data=body,
        auth=(GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET),
    )

    client.parse_request_body_response(json.dumps(token_response.json()))

    userinfo_endpoint = google_provider_cfg["userinfo_endpoint"]
    uri, headers, body = client.add_token(userinfo_endpoint)
    userinfo_response = requests.get(uri, headers=headers, data=body)

    logging.info('Logging user')
    login_user(User(userinfo_response.json()["email"]))

    return redirect(url_for('main'))


@bp.route('/google-login')
def google_login():
    client = WebApplicationClient(GOOGLE_CLIENT_ID)
    google_provider_cfg = get_google_provider_cfg()
    authorization_endpoint = google_provider_cfg["authorization_endpoint"]

    request_uri = client.prepare_request_uri(
        authorization_endpoint,
        redirect_uri=force_https(request.base_url) + "/callback",
        scope=["openid", "email", "profile"],
    )
    return redirect(request_uri)
