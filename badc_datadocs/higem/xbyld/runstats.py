#export PATH=/usr/local/badc/linux/suse10/cdat/bin:$PATH

import shelf

#read run statistics from a file
#----------------------------------
shelf=shelve.open('/home/badc/workspace/charlotte/xbyld_stats.shelf')
dataset=shelf['dataset']
runid=shelf['runid']
nyears=shelf['nyears']
nmonths=shelf['nmonths']
plusmonths=shelf['plusmonths']


