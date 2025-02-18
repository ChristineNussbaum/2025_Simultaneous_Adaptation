#This script can cut fixed time windows at the beginning and the end of sound files. It can be used to remove e.g. 500 ms of silence at the beginning of the stimuli

form Trim Sound Files in Directory
    sentence inputDir C:\Users\Christine Nussbaum\Documents\Arbeit\Forschungsprojekte\2024_Simultaneus_Adaptation\Stimuli1_JAVMEPS\test\
    sentence outputDir C:\Users\Christine Nussbaum\Documents\Arbeit\Forschungsprojekte\2024_Simultaneus_Adaptation\Stimuli1_JAVMEPS\test_cut\
    real cutBegin 0.5
    real cutEnd 0.45
    comment Filename appendix
    word append _cut
endform

# Get list of all sound files in the directory
Create Strings as file list... soundFiles 'inputDir$'/*.wav
numberOfFiles = Get number of strings


# loop through all files in the directory
for i to numberOfFiles
    # always start by selecting the object with the file list
    selectObject: "Strings soundFiles"
    inputFile$ = Get string... i
    
    # load the current sound file
    Read from file: inputDir$  + inputFile$
 
    # Get the total duration of the sound
    soundDuration = Get total duration
    
    # Define trimming times 
    startTrim = cutBegin  
    endTrim = soundDuration - cutEnd  
    
    # Extract the middle portion
    Extract part... startTrim endTrim rectangular 1 no
    
    # Define new filename (with the specified appendix)
    filenameshort$ = inputFile$ - ".wav"
    outputFile$ = filenameshort$ + append$ + ".wav"
    
    # Save the trimmed sound file
    Save as WAV file: outputDir$  + outputFile$
    #writeInfoLine: inputFile$
 
    
    # Clean up
    Remove
endfor

printline "All sound files trimmed and saved in 'outputDir$'"