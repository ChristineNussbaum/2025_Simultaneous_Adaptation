form McAleer2014Personality

	comment for all SELECTED sounds  compute: F0, F0SD, F1-F4 (Hz)

	comment PITCH ANALYSIS
	comment measurment interval (frame duration), in seconds, 0 --> 0.75/(pitch floor), 
	real     timeStep 0
        comment males Voices 60-250 Hz, females: 120-400 Hz
	positive pitchFloor 60
	positive pitchCeiling 250


	comment FORMANT ANALYSIS, i.e. formant (burg)

	comment time between the centres of analysis frames, 0.0 --> 25% of window length
	real timeStepF 0.0
	
	comment maximum number of formants [0.5, 1, 1.5, ...  6]
	positive maxNum 5

	comment maximum formant (Hz), e.g. male: 5000, female: 5500, child: 8000
	positive maxFormantHz 5000

	comment window length (s), effective duration of the analysis window in seconds
	positive winLength 0.025

	comment pre-emphasis from (Hz), i.e. the +3dB point for an inverted low-pass filter with a slope of +6dB/ocatve
	positive preEmphHz 50

endform

clearinfo

n = numberOfSelected ("Sound")

for i to n
    sound'i' = selected ("Sound", i)   
endfor


printline procedures: To Pitch... and Formant (burg cc) 
printline pitch parameters:   timeStep:  'timeStep:3'   pitchFloor: 'pitchFloor:1' pitchCeiling: 'pitchCeiling:1'
printline formant parameters: timeStepF: 'timeStepF:3'  maxNum: 'maxNum:0' maxFormantHz: 'maxFormantHz:1' winLength: 'winLength:3' preEmphHz:'preEmphHz:1'
printline
printline 'n' sounds analysed
printline filename,duration,intDB,f0,f0SD,f0min,f0max,f0intonation,f0Start,f0End,f0Glide,f1,f2,f3,f4,formDisp,lowFreqEnergy,highFreqEnergy,alphaRatio,hnr,jitRap,shimAPQ3,logF0,logformDisp

# Median pitches of all selected sounds:
for i to n
    select sound'i'
    name$ = selected$("Sound")

    duration  = Get total duration
    intDB     = Get intensity (dB)
    duration = duration*1000


    select sound'i'
    To Pitch... timeStep pitchFloor pitchCeiling
      	f0    = Get mean... 0 0 Hertz
	f0SD  = Get standard deviation: 0, 0, "Hertz"
        logF0 = log10(f0)

	f0min = Get minimum: 0, 0, "Hertz", "Parabolic"
	f0max = Get maximum: 0, 0, "Hertz", "Parabolic"
	f0intonation = f0max - f0min

	startTime = Get start time
        endTime   = Get end time

	f0Last  = -1
        first   = 0
        last    = 0
 	f0First = -1
	
	numberOfTimeSteps = (endTime - startTime) / 0.05
	for step to numberOfTimeSteps
    		tmin = startTime + (step - 1) * 0.05
    		tmax = tmin + 0.05
    		mean = Get mean: tmin, tmax, "Hertz"
    		#minimum = Get minimum: tmin, tmax, "Hertz", "Parabolic"
    		#maximum = Get maximum: tmin, tmax, "Hertz", "Parabolic"
    		#stdev = Get standard deviation: tmin, tmax, "Hertz"
    		#appendInfoLine: fixed$ (tmin, 6), " ", fixed$ (tmax, 6), " ", fixed$ (mean, 2),
    		#... " ", fixed$ (minimum, 2), " ", fixed$ (maximum, 2), " ", fixed$ (stdev, 2)

		if mean > 0
 			if first == 0
                        	f0First = mean
                                first = 1
			endif
                endif

		tmax2 = endTime - (step-1) * 0.05
		tmin2 = tmax2-0.05
		mean2 = Get mean: tmin2, tmax2, "Hertz"
		if mean2 > 0
 			if last == 0
                        	f0Last = mean2
                                last = 1
			endif
                endif
	endfor

 	f0Glide = f0Last - f0First
    Remove
 
    #compute some jitter measures 
    select sound'i'	
    To PointProcess (periodic, cc): pitchFloor, pitchCeiling
    jitRap = Get jitter (rap): 0, 0, 0.0001, 0.02, 1.3
    #jitLoc = Get jitter (local): 0, 0, 0.0001, 0.02, 1.3
    #jitLocAbs = Get jitter (local, absolute): 0, 0, 0.0001, 0.02, 1.3
    
    #compute a shimmer measures, to compute it, a selection of both the sound and the pointprocess is necessary
    #as the point process is still selected, it is possible to add the sound to the selection by calling:
    plus sound'i'
    #shimLocDB = Get shimmer (local_dB): 0, 0, 0.0001, 0.02, 1.3, 1.6
    shimAPQ3   = Get shimmer (apq3): 0, 0, 0.0001, 0.02, 1.3, 1.6

    select sound'i'
    To Formant (burg)... timeStepF  maxNum maxFormantHz winLength preEmphHz
	f1 = Get mean: 1, 0, 0, "Hertz"
	f2 = Get mean: 2, 0, 0, "Hertz"
	f3 = Get mean: 3, 0, 0, "Hertz"
	f4 = Get mean: 4, 0, 0, "Hertz"
    Remove
  
    #compute formant dispersion by [(f2-f1) + (f3-f2) + (f4-f3)]/3
    formDisp = ((f2-f1) + (f3-f2) + (f4-f3))/3
    logformDisp = log10(formDisp)
			     
    #Converting the sound to a long term average spectrum. Then queries
    #the maximum amplitude within a frequency region specified by the frequency
    #boundaries around H1, H2, F1, F2, and F3. The difference between these maxima
    #is a measure of spectral tilt which is then written to the data file.

    select sound'i'
    To Ltas... 50
    ltasID = selected("Ltas")

    #energy in low frequency regions (0-1kHz)	
    lowFreqEnergy =  Get mean: 0, 1000, "energy"
      
    #energy in high frequency regions (1-5kHz)
    highFreqEnergy =  Get mean: 1000, 5000, "energy"

    alphaRatio = lowFreqEnergy/highFreqEnergy

    Remove

    select sound'i'

    To Harmonicity (cc): 0.01, 75, 0.1, 1
	    hnr = Get mean: 0, 0 
    Remove


		
    printline 'name$'.wav,'duration:1','intDB:1','f0:1','f0SD:2','f0min:1','f0max:1','f0intonation:1','f0First:1','f0Last:1','f0Glide:1','f1:1','f2:1','f3:1','f4:1','formDisp:3','lowFreqEnergy:1','highFreqEnergy:1','alphaRatio:1','hnr:1','jitRap:5','shimAPQ3:5','logF0:2','logformDisp:2'
endfor







