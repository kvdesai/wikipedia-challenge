import time
import os
from numpy import *
import csv
import sys

NE = 44514
NT = 22126031
# rootdir = 'E:\\public\\ICDM11\\wikichallenge_data_all\\'
# resdir = 'D:\\ICDM11\\Honourable Mention\\AllFeatureSets\\'
rootdir = sys.argv[1]
resdir = sys.argv[2]
input_file = rootdir + 'training.tsv'

toff = time.mktime(time.strptime('2001-01-01 00:00:00','%Y-%m-%d %H:%M:%S'))
tetrain = (time.mktime(time.strptime('2010-04-01 00:00:00','%Y-%m-%d %H:%M:%S')) - toff)/86400
telead = (time.mktime(time.strptime('2010-09-01 00:00:00','%Y-%m-%d %H:%M:%S')) - toff)/86400

Mlen = telead/116.0
L5len = Mlen*5

csvname = rootdir + 'wikichallenge_example_entry.csv'
reader = csv.reader(open(csvname,'rb'))
reader.next()
eids = list();
for line in reader:
    eids.append(int(line[0]))

del reader, csvname

Emap = dict([(eids[x], x) for x in range(len(eids))])
revmat = zeros((len(eids), 19), 'int32')
revmat[:,0] = eids

f = open(input_file,'r')
f.next() # bypass header

k = 0
it_tot_sgr = 1; it_tot_smr = 2; it_tot_sar = 3
it_l5m_sgr = 4; it_l5m_smr = 5; it_l5m_sar = 6
it_l1m_sgr = 7; it_l1m_smr = 8; it_l1m_sar = 9

il_tot_sgr = 10; il_tot_smr = 11; il_tot_sar = 12
il_l5m_sgr = 13; il_l5m_smr = 14; il_l5m_sar = 15
il_l1m_sgr = 16; il_l1m_smr = 17; il_l1m_sar = 18

for line in f:
    sline = line.split('\t')
    eid = int(sline[0])
    try:
        ix = Emap[eid]
    except:
        raise SystemExit('Uid doesnot exit in the list: ' + sline[0])
    
    t = (time.mktime(time.strptime(sline[4],'%Y-%m-%d %H:%M:%S')) - toff)/86400
    
    if sline[6]=='1':
        uidrev = int(sline[7])
        try:
            ixrev = Emap[uidrev]
            if ixrev == ix:
                autorevert = True
            else:
                autorevert = False
        except:
            ixrev = []
            autorevert = False
            
        ######### Columns for training dataset ##############
        if t < tetrain:
            revmat[ix,it_tot_sgr] += 1
            if ixrev != []:
                revmat[ixrev,it_tot_smr] += 1
                if autorevert == True:
                    revmat[ix,it_tot_sar] += 1
                    
            if t < tetrain - L5len:
                revmat[ix,it_l5m_sgr] += 1
                if ixrev != []:
                    revmat[ixrev,it_l5m_smr] += 1
                    if autorevert == True:
                        revmat[ix,it_l5m_sar] += 1
                        
            if t < tetrain - Mlen:
                revmat[ix,it_l1m_sgr] += 1
                if ixrev != []:
                    revmat[ixrev,it_l1m_smr] += 1
                    if autorevert == True:
                        revmat[ix,it_l1m_sar] += 1
                        
        ######### Columns for leaderboard dataset ##############
        if t < telead:
            revmat[ix,il_tot_sgr] += 1
            if ixrev != []:
                revmat[ixrev,il_tot_smr] += 1
                if autorevert == True:
                    revmat[ix,il_tot_sar] += 1
                    
            if t < telead - L5len:
                revmat[ix,il_l5m_sgr] += 1
                if ixrev != []:
                    revmat[ixrev,il_l5m_smr] += 1
                    if autorevert == True:
                        revmat[ix,il_l5m_sar] += 1
                        
            if t < telead - Mlen:
                revmat[ix,il_l1m_sgr] += 1
                if ixrev != []:
                    revmat[ixrev,il_l1m_smr] += 1
                    if autorevert == True:
                        revmat[ix,il_l1m_sar] += 1             
                
        k = k+1        
    elif sline[6] == '0':
        k = k+1
        continue
    else:
        raise SystemExit('Nonbinary revert flag: ' + str(sline[6]))
        
    
    if (k % 10000 == 0):
        print 'Parsed', k, 'lines...'

assert(k == NT)

print 'Finished parsing...writing out pickle file now'
revmat.dump(open('reverts_related_features_training_and_LB.pickled','wb'));

print 'Finished pickling...writing out csv file now'
header = ','.join(['user_id','train_tot_got','train_tot_made', 'train_tot_auto', \
                   'train_l5m_got','train_l5m_made', 'train_l5m_auto', \
                   'train_l1m_got','train_l1m_made', 'train_l1m_auto', \
                   'lead_tot_got','lead_tot_made', 'lead_tot_auto', \
                   'lead_l5m_got','lead_l5m_made','lead_l5m_auto', \
                   'lead_l1m_got','lead_l1m_made','lead_l1m_auto']) + '\n'

outfile = resdir + 'reverts_related_features_training_and_LB.csv'
f = open(outfile,'wb')
f.write(header)
savetxt(f,revmat,delimiter=',',newline='\n',fmt='%d')
f.close()

print 'done.'	


    
