import sqlite3
import glob
from itertools import islice
import csv

class Dbop(object):
	def __init__(self, db):
		self.cursor = db.cursor()

	def put(self, tableName, docLoc):
		self.cursor.execute('DROP TABLE IF EXISTS ' + tableName)
		attlist, i, SQL = [], 0, ''
		with open(docLoc) as f:			
			for line in f.readlines():
				att = line
				attlist = att.split()
				break
			for i in range(len(attlist)):
				if i == 0:
					SQL = 'CREATE TABLE %s(%s VARCHAR (30))' % (tableName, attlist[i])
				else:
					SQL = 'ALTER TABLE %s ADD COLUMN %s VARCHAR (30)' % (tableName, attlist[i])
				self.cursor.execute(SQL)

		with open(docLoc) as f:
			for line in islice(f, 1, None):
				
				vals = line.rstrip('\r\n').split('\t')
				SQL = 'INSERT INTO %s VALUES %r' % (tableName, tuple(vals))
				self.cursor.execute(SQL)

dw = Dbop(sqlite3.connect('Czech.db'))
path = 'Value/*.txt'
files = glob.glob(path)


for file in files:
    tableName = file.title().lstrip('Value').lstrip('/').rstrip('Txt').rstrip('.')
    # print(tableName)s
    dw.put(tableName, file.title())






