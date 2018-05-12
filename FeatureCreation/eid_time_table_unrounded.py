import time
import os
import sys
#import numpy
import csv

NE = 44514
NT = 22126031
# rootdir = 'E:\\public\\ICDM11\\wikichallenge_data_all\\'
# resdir = 'D:\\ICDM11\\Honourable Mention\\AllFeatureSets\\'
rootdir = sys.argv[1]
resdir = sys.argv[2]
input_file = rootdir + 'training.tsv'

toff = time.mktime(time.strptime('2001-01-01 00:00:00','%Y-%m-%d %H:%M:%S'))

csvname = rootdir + 'wikichallenge_example_entry.csv'
reader = csv.reader(open(csvname,'rb'))
reader.next()
eids = list();
for line in reader:
    eids.append(int(line[0]))

del reader, csvname

Emap = dict([(eids[x], x) for x in range(len(eids))])

f = open(input_file,'r')
f.next()
edits = [[] for i in range(NE)]

k = 0
for line in f:
    sline = line.split('\t')
    eid = int(sline[0])
    try: 
        ix = Emap[eid]
    except:
        print "iwarnedu: Editor ID not present in list: ", eid
        continue

    #t = int(time.mktime(time.strptime(sline[4],'%Y-%m-%d %H:%M:%S')) - toff)/86400
    t = (time.mktime(time.strptime(sline[4],'%Y-%m-%d %H:%M:%S')) - toff)/86400
    edits[ix].append(t);
    
    k = k+1
    if (k % 100000 == 0):
        print 'Parsed', k, 'lines...'

assert(k == NT)

print 'Finished parsing...writing out csv file now'
writer = csv.writer(open(resdir+'edit_times_unrounded.csv','wb'),delimiter=',')
eider = open(resdir+'editor_list.txt','wb')
for irow in range(len(edits)):    
    writer.writerow(edits[irow])
    eider.write(repr(eids[irow])+'\n')

eider.close()
print 'done.'	


    
