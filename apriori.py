import csv
import sys

def read(fname):

	with open(fname, 'rb') as f:
		# print readfile
		res = []
		for row in f.readlines():
			row = row.rstrip('\r\n').split('\t')
			res.append(row)
		# print len(res)
		
		dic = {}

		for i in range(1, len(res)):
			dic['sample' + str(i)] = [(res[0][j] + '_' + res[i][j]) for j in range(2, len(res[0])) if res[i][j] != '' or res[i][j] != 'NA']
		# print dic
	return dic

t = read('All_scale_never_4.txt')
# print read('cho.txt')

def getItemTransactionList(dic):
	transactionList = []
	itemSet = set()
	for k,v in dic.items():
		transaction = frozenset(v)
		transactionList.append(transaction)
		for item in transaction:
			itemSet.add(frozenset([item]))
	# print len(transactionList)
	return itemSet, transactionList


# print getItemTransactionList(t)

def getCurrentSets(itemSet,transactionList,minSupport):

	dic1 = {}
	freqSet = {}
	set1 = set()

	# itemSet, transactionList = getItemTransactionList(dic)
	
	for trans in transactionList:
		for item in itemSet:
			if item.issubset(trans):
				if dic1.has_key(item):
					dic1[item] += 1
				else:
					dic1[item] = 1
	# print len(dic1)

	for item,count in dic1.items():
		support = float(count)/len(transactionList) # SupportValue
		if support >= minSupport:
			set1.add(item)
	# print len(set1)

	freqSet = { k:v for k,v in dic1.items() if float(v)/len(transactionList) >= minSupport}
	# print freqSet

	return set1, freqSet

def joinSet(itemset, length):
	# itemset is a frozenSet, there is no duplicate in each set.
	return set([i.union(j) for i in itemset for j in itemset if len(i.union(j)) == length])

def getSupport(item,dic, transactionList):
		print dic[item], ' ', len(transactionList)
		return float(dic[item])/len(transactionList)

def Apriori(dic,minSupport): 

	itemSet, transactionList = getItemTransactionList(dic)
	
	current, dictn = getCurrentSets(itemSet,transactionList,minSupport) #output currentItemSet && Map<itemset, count>
	# print dic, len(transactionList)	
	largest = {}

	k = 2
	res = []
	while current != set([]):
		largest[k-1] = current # current is previous frequentItemSet
		current = joinSet(current,k)
		
		# print current
		set2, dictn = getCurrentSets(current,transactionList,minSupport)
		
		current = set2

		k = k + 1 

		if current != set([]):
			res = current

	print 'Current', res
		# print len(largest)

	# for k,v in largest.items():
	# 	for item in k:
	# 		if item in freqSet:
	# 			freqSet[item].apend()

	return current

s = Apriori(t,0.5)