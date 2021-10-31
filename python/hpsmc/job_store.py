"""Defines a class for creating JSON files with multiple jobs in them."""

import json
import logging

from util import config_logging

logger = config_logging("hpsmc.job_store")

class JobStore:
    """
    Simple JSON based store of job data.
    """

    def __init__(self, path=None):
        self.path = path
        if path:
            logger.info("Initializing job store from: {}".format(self.path))
            self.load(path)

    def load(self, json_store):
        """Load raw JSON data into this job store."""
        with open(json_store, 'r') as f:
            json_data = json.loads(f.read())
        self.data = {}
        for j in json_data:
            self.data[j['job_id']] = j
        logger.info("Loaded %d jobs from job store: %s" % (len(self.data), json_store))

    def get_job(self, job_id):
        """Get a job by its job ID."""
        return self.data[int(job_id)]

    def get_job_data(self):
        """Get the raw dict containing all the job data."""
        return self.data

    def get_job_ids(self):
        """Get a sorted list of job IDs."""
        return sorted(self.data.keys())

    def has_job_id(self, job_id):
        """Return true if the job ID exists in the store."""
        return job_id in list(self.data.keys())
