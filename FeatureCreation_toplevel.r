# The TopLevel Script that will create all featuresets and intermediate files
rm(list=ls())
MAIN_DIR = 'D:/ICDM11/Honourable Mention/'
setwd(paste(MAIN_DIR,'FeatureCreation',sep=''))

DATADIR = 'E:/public/ICDM11/wikichallenge_data_all/' # Pointer to the raw data directory - 
				#must contain following three files as available from Kaggle WikiChallege portal: 
				#	training.tsv, regdates.tsv, wikichallenge_example_entry.csv
				
RESDIR = paste(MAIN_DIR,'AllFeatureSets/',sep='') # Pointer to the output directory where all featuresets will be stored

cat('Creating List of Edit Times (rounded to day) for each editor...','\n')

system(paste('python -m ',"eid_time_table",' ','"',DATADIR,'"',' ','"',RESDIR,'"',sep=''))
	#OUTPUTS: edit_times.csv, editor_list.txt
	
cat('Creating List of Edit Times (un rounded) for each editor...','\n')
system(paste('python -m ',"eid_time_table_unrounded",' ','"',DATADIR,'"',' ','"',RESDIR,'"',sep=''))
	#OUTPUTS: edit_times_unrounded.csv, editor_list.txt
	
cat('Creating Reverts related features...','\n')
system(paste('python -m ',"revert_features",' ','"',DATADIR,'"',' ','"',RESDIR,'"',sep=''))
	#OUTPUTS: reverts_related_features_training_and_LB.csv
	
	
cat('Finished Running Python Scripts...','\n')
# Now call R scripts to create all feature sets


# create EditorID | First Edit Time | Registration Date table for all editors
source('eid_fsdt_regdate_table_creation.r') 
	#OUTPUTS: eid_fsdt_regdate_table.csv

source('editors_mnthly_edits.r')
	#OUTPUTS: editor_monthly_edits.csv, editor_monthly_edits_unique_days.csv
	#		  editor_regdate.csv
	
source('editors_mnthly_edits_unrounded.r')
	#OUTPUTS: editor_monthly_edits_unrounded.csv, editor_monthly_edits_unique_sessions_unrounded.csv
	
# source('create_raw_parsed_dump.r')
	# #OUTPUTS: Parsed_RawDump_Full.csv, Parsed_RawDump_Full.RDATA
	
source('orig_features_subset1.r')
	# OUTPUTS: OrigSubset1_train.csv, OrigSubset1_lead.csv
	
source('create_orig_features.r')
	# OUTPUTS: OrigSet_xinp1p2_yinp3.csv, OrigSet_xinp1p2p3.csv
	
source('OrigFeatures_to_Feature113.r')
	# OUTPUTS: Features113_XinP1P2_YinP3.csv, Features113_XinP1P2P3.csv
	
source('Data Generation.r')	
	# OUTPUTS: leaderboard_features.csv, training_features.csv
	
source('OrigFeatures_to_Featurekd_wrev.r')
	# OUTPUTS: Featureskd_XinP1P2P3.csv, Featureskd_XinP1P2_YinP3.csv
	#		Featureskd_wrevs_XinP1P2P3.csv, Featureskd_wrevs_XinP1P2_YinP3.csv
