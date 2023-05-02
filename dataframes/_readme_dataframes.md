### readme dataframes
csv files with data in long format
separator: ,
string indicator: ""



### dfHilbertMeans.txt ###
dataframe with mean Hilbert amplitudes for all participants and conditions

# Variables
partID
- participant identifier

att
- content: whether tagged pictures were attended or ignored
- type: within-subject factor
- levels: att, ign

col
- content: whether tagged pictures were in color or in grayscale
- type: within-subject factor
- levels: col, gray

freq
- content: driving frequency of tagged pictures (8.57 or 15 Hz)
- type: within-subject factor
- levels: 857, 15

amplitude
- content: mean Hilbert amplitude (% change relative to baseline) in predefined electrodes and time window (see script 01_createDataframes.R)
- type: continuous



### dfHilbertTimecourse.txt ###
dataframe contains Hilbert amplitude time courses averaged across participants for plotting

# Variables
partID
- participant identifier

att
- content: whether tagged pictures were attended or ignored
- type: within-subject factor
- levels: att, ign

col
- content: whether tagged pictures were in color or in grayscale
- type: within-subject factor
- levels: col, gray

freq
- content: driving frequency of tagged pictures (8.57 or 15 Hz)
- type: within-subject factor
- levels: 857, 15

time
- content: within-trial latency in milliseconds relative to attention cue onset
- type: continuous

amplitude
- content: Hilbert amplitude (% change relative to baseline)
- type: continuous



### dfHilbertTopos.txt ###
dataframe with topographies for each condition

# Variables
partID
- participant identifier

att
- content: whether tagged pictures were attended or ignored
- type: within-subject factor
- levels: att, ign

col
- content: whether tagged pictures were in color or in grayscale
- type: within-subject factor
- levels: col, gray

freq
- content: driving frequency of tagged pictures (8.57 or 15 Hz)
- type: within-subject factor
- levels: 857, 15

time
- content: within-trial latency in milliseconds relative to attention cue onset
- type: continuous

amplitude
- content: Hilbert amplitude (% change relative to baseline)
- type: continuous

lab
- content: lab where data was collected
- type: between-subject factor
- levels: Florida, Leipzig

freq
- content: driving frequency
- type: within-subject factor
- levels: 6Hz, 8.57Hz, 15Hz

electrode
- content: electrode labels

x
- electrode x coordinates in 2D representation for plotting using eegUtils package
- type: continuous

y
- electrode y coordinates in 2D representation for plotting using eegUtils package
- type: continuous

amplitude
- content: mean Hilbert amplitude (% change relative to baseline) in predefined time window (see script 01_createDataframes.R)
- type: continuous