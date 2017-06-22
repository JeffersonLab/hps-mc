import os, sqlite3, getpass, json
            
class Database:
    
    def __init__(self, filename):
        
        self.filename = filename
        self.conn = None
        self.curs = None
        
        self.productions = Productions(self)
        self.jobs = Jobs(self)
        self.batch_jobs = BatchJobs(self)
                    
    def connect(self):
        self.conn = sqlite3.connect(self.filename)
        self.curs = self.conn.cursor()        
        
    def drop(self):
        self.jobs.drop()
        self.productions.drop()
        self.batch_jobs.drop()
        
    def create(self):
        self.productions.create()
        self.jobs.create()
        self.batch_jobs.create()
                
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
                        "(name TEXT UNIQUE, params TEXT, created_by TEXT, created_date DATE)")
        
    def insert(self, name, param_file):
        data = open(param_file, 'r').read()
        self.db.execute("INSERT INTO productions (name, params, created_by, created_date) VALUES ('%s', '%s', '%s', CURRENT_TIMESTAMP)" 
                     % (name, data, getpass.getuser()))
        
    def select(self, name):
        return self.db.execute("SELECT rowid,* FROM productions WHERE name = '%s'" % name).next()
    
    def select_all(self):
        return self.db.execute("SELECT rowid,* FROM productions")
    
    def prod_id(self, name):
        return self.select(name)[0]
    
    def delete(self, name):
        id = self.prod_id(name)
        self.db.execute("DELETE FROM batch_jobs WHERE prod_id = %d" % id)
        self.db.execute("DELETE FROM jobs WHERE prod_id = %d" % id)
        self.db.execute("DELETE FROM productions WHERE name = '%s'" % name)
        
class Jobs:
    
    def __init__(self, db):
        self.db = db
        
    def drop(self):
        self.db.execute("DROP TABLE IF EXISTS jobs")
                
    def create(self):        
        self.db.execute("CREATE TABLE IF NOT EXISTS jobs "
                        "(job_id INTEGER, prod_id INT, params TEXT, name TEXT, "
                        "FOREIGN KEY(prod_id) REFERENCES productions(prod_id))")
        
    def insert(self, job_id, prod_id, params, name):
        self.db.execute("INSERT INTO jobs (job_id, prod_id, params, name) VALUES (%d, %d, \"%s\", '%s')"
                        % (job_id, prod_id, params, name))
                    
    def select_prod(self, prod_id):
        return self.db.execute("SELECT * FROM jobs WHERE prod_id = %d" % prod_id)
    
    def select(self, prod_id, job_id):
        return self.db.execute("SELECT * FROM jobs WHERE prod_id = %d and job_id = %d" % (prod_id, job_id)).next()
    
class BatchJobs:
    
    states = {0: 'UNKNOWN',
              1: 'SUBMIT',
              2: 'PEND',
              3: 'RUN',
              4: 'SUSP',
              5: 'DONE',
              6: 'EXIT'}
    
    states_lookup = {v: k for k, v in states.iteritems()}
    
    systems = ['LSF', 'Auger', 'local']
    
    def __init__(self, db):
        self.db = db
        
    @staticmethod
    def state(s):
        if type(s) == int:
            return BatchJobs.states[s]
        elif type(s) == str:
            return BatchJobs.states_lookup[s]
        else:
            raise Exception("The arg '%s' has the wrong type '%s'." % (str(s), type(s)))
        
    def drop(self):
        self.db.execute("DROP TABLE IF EXISTS batch_jobs")
        
    def create(self):
        self.db.execute("CREATE TABLE IF NOT EXISTS batch_jobs "
                        "(batch_id INTEGER, job_id INTEGER, prod_id INTEGER, sys TEXT, state INTEGER)")
        
    def insert(self, batch_id, job_id, prod_id, sys, state = 0):
        if sys not in BatchJobs.systems:
            raise Exception("The system '%s' is not valid." % sys)
        self.db.execute("INSERT INTO batch_jobs (batch_id, job_id, prod_id, sys, state) VALUES (%d, %d, %d, '%s', %d)" 
                        % (batch_id, job_id, prod_id, sys, state))
        
    def select_job(self, job_id, prod_id):
        return self.db.execute("SELECT * FROM batch_jobs WHERE prod_id = %d and job_id = %d" % (prod_id, job_id))
    
    def select(self, prod_id):
        return self.db.execute("SELECT * FROM batch_jobs WHERE prod_id = %d" % prod_id)
    
    def select_all(self):
        return self.db.execute("SELECT * FROM batch_jobs")
    
    def update(self, job_id, prod_id, batch_id, state):
        if type(state) == str:
            s = BatchJobs.state(state)
        elif type(state) == int:
            s = state
        else:
            raise Exception("The state '%s' has the wrong type '%s'." % (state, type(state)))
        self.db.execute("UPDATE batch_jobs set state = %d where job_id = %d and prod_id = %d and batch_id = %d" % (s, job_id, prod_id, batch_id))
