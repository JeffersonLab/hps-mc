import sqlite3, getpass, json
            
class Database:
    
    def __init__(self, filename):
        self.filename = filename
        self.conn = None
        self.curs = None        
                
    def connect(self):
        self.conn = sqlite3.connect(self.filename)
        self.curs = self.conn.cursor()
        
    def drop(self):
        Jobs(self).drop()
        Productions(self).drop()
        BatchJobs(self).drop()
        
    def create(self):
        Productions(self).create()
        Jobs(self).create()
        BatchJobs(self).create()
                
    def execute(self, query):
        return self.curs.execute(query)
        
    def commit(self):
        self.conn.commit()
    
    def close(self):
        self.conn.close()
        self.conn = None
        self.curs = None                       
            
class Productions:
    
    def __init__(self, db):
        self.db = db
        
    def drop(self):
        self.db.execute("DROP TABLE IF EXISTS productions")
        
    def create(self):
        self.db.execute("CREATE TABLE IF NOT EXISTS productions " 
                        "(prod_id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT UNIQUE, "
                        "params TEXT, created_by TEXT, created_date DATE)")
        
    def insert(self, name, param_file):
        data = open(param_file, 'r').read()
        self.db.execute("INSERT INTO productions (name, params, created_by, created_date) VALUES ('%s', '%s', '%s', CURRENT_TIMESTAMP)" 
                     % (name, data, getpass.getuser()))
        
    def select(self, name):
        return self.db.execute("SELECT * FROM productions WHERE name = '%s'" % name)
    
    def delete(self, name):
        self.db.execute("DELETE FROM productions WHERE name = '%s'" % name)
        
class Jobs:
    
    def __init__(self, db):
        self.db = db
        
    def drop(self):
        self.db.execute("DROP TABLE IF EXISTS jobs")
                
    def create(self):        
        self.db.execute("CREATE TABLE IF NOT EXISTS jobs "
                        "(job_id INTEGER, prod_id INT, params TEXT, "
                        "FOREIGN KEY(prod_id) REFERENCES productions(prod_id))")
        
    def insert(self, job_id, prod_id, param_file):
        data = open('job_test.json', 'r').read()
        self.db.execute("INSERT INTO jobs (job_id, prod_id, params) VALUES (%d, %d, '%s')"
                        % (job_id, prod_id, data))
        
    def select(self, prod_id):
        return self.db.execute("SELECT rowid, * FROM jobs WHERE prod_id = %d" % prod_id)
    
class BatchJobs:
    
    status_codes = {0: 'unknown',
                    1: 'submitted',
                    2: 'running',
                    3: 'complete',
                    4: 'error'}
    
    status_lookup = {'unknown':   0,
                     'submitted': 1,
                     'running':   2,
                     'complete':  3,
                     'error':     4}
    
    systems = ['LSF', 'Auger', 'local']
    
    def __init__(self, db):
        self.db = db
        
    @staticmethod
    def status(status):
        if type(status) == int:
            return BatchJobs.status_codes[status]
        elif type(status) == basestring:
            return BatchJobs.status_lookup[status]
        else:
            raise Exception("The arg '%s' has the wrong type." % status)
        
    def drop(self):
        self.db.execute("DROP TABLE IF EXISTS batch_jobs")
        
    def create(self):
        self.db.execute("CREATE TABLE IF NOT EXISTS batch_jobs "
                        "(batch_id INTEGER, job_id INTEGER, sys TEXT, status INTEGER, "
                        "FOREIGN KEY(job_id) REFERENCES jobs(rowid))")
        
    def insert(self, job_id, batch_id, sys):
        if sys not in BatchJobs.systems:
            raise Exception("The system '%s' is not valid." % sys)
        self.db.execute("INSERT INTO batch_jobs (batch_id, job_id, sys, status) VALUES (%d, %d, '%s', 0)" % (batch_id, job_id, sys))
        
    def select(self, job_id):
        return self.db.execute("SELECT * FROM batch_jobs WHERE job_id = %d" % job_id)
    
