#!/usr/bin/env python

import requests
import jsonschema
import json

validator = jsonschema.Draft7Validator(
    requests.get("https://purl.org/csv-cubed/qube-config/v1").json()
)


def validate_qube_config(json_str):
    json_dict = json.loads(json_str)
    errors = list(validator.iter_errors(json_dict))
    return errors