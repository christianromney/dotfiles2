#!/usr/bin/env bash

set -e

getAuth0Token() {
cat <<EOF \
    | curl -X POST -H 'Content-Type: application/json' -d @- $AUTH0_TOKEN_URL 2>/dev/null \
    | jq -rc '.id_token'
{
     "client_id": "$AUTH0_CLIENT_ID",
     "username": "$AUTH0_ADMIN_USERNAME",
     "password": "$AUTH0_ADMIN_PASSWORD",
     "realm": "$AUTH0_CONNECTION",
     "grant_type": "http://auth0.com/oauth/grant-type/password-realm",
     "scope": "openid"
}
EOF
}

getBengalPayload() {
cat <<EOF
{
     "sub": "$AUTH0_VENDOR_USER_ID",
     "email": "$AUTH0_VENDOR_USER_EMAIL",
     "email_verified": true
}
EOF
}



curl -vX POST \
     -H "Authorization: Bearer $(getAuth0Token)" \
     -H "Content-Type: application/json" \
     -d "$(getBengalPayload)" \
     "$STUDYTEAM_USER_SIGNUP_URL"
