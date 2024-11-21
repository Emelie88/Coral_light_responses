
/////////////////////////////////////////////////////////
// ImageJ macro to extract the tracks and the          //
// distribution of the larvae from the raw videos.     //
/////////////////////////////////////////////////////////

// in this version, videos must already have background subtracted and
// be thresholded manually before running this macro
// Make sure your file directories have no spaces in the names or it won't work


// Asks the user where their video files are and then 
// into which directory the output files should go.     
    
macro "Extract Tracks"
{
    if(getBoolean("Choose an input and output directory otherwise give a text file containing a list of input and output directories"))
    {
         inputDir = getDirectory("Choose your video input directory");
        outputDir = getDirectory("Choose an output directory for results files");
        extractTracks(inputDir, outputDir);
    }
    else
    {
        fileList = File.openDialog("Open a text file containing a list of input and output directories");
        lines = split(File.openAsString(fileList), "\n");
        for(i = 0; i < lines.length; i++)
        {
            dirs = split(lines[i], " ");
            extractTracks(dirs[0], dirs[1]);
        }
    }
}

// Extracts the tracks from your videos in the folder inputDir via mTrack2. 
// All files in the folder inputDir must be video files that ImageJ can read,
// otherwise this macro aborts with an error and will not analyse more videos.        //

function extractTracks(inputDir, outputDir)
{
    print (inputDir);
    print (outputDir);

    // Get all files in the input directory
    list=getFileList(inputDir);
    Array.sort(list);
    print(list.length);
    
    // Track the number of files that have been read
    files = 0;
    setBatchMode(true);
    for (k=0; k<list.length; k++)
    {
        print(list[k]);
        open(inputDir + list[k]);
        imageTitle = getTitle();
        imageTitle = replace(imageTitle, " ", "_"); // Replace spaces by underscores to avoid problems with file writing
        print(imageTitle);

// ADD INFO HERE!! 
        numFramesToProcess = 20 ;  //  number of frames to process (e.g. 20 is 1 sec at 20 fps)
        
        startFrame = 1;
        print(startFrame);
        rename("video");
        selectWindow("video");
        setBatchMode(true);
        
// Cut the video into smaller clips and give each part an index.
// Start with m=100 for the namimg system to avoid problems with file sorting
        m=100;   
        while (nSlices > 1)
        {
// Process the first n frames of the video so that different points in time can be checked.
     nSlices
     run("Duplicate...", "title=stack duplicate range=" + startFrame + "-" + numFramesToProcess);

// tell it to run the tracking    
       trackParticles2(m);
       close();
            
// Delete the first n frames of the video so that the next n frames can be processed.
     selectWindow("video");
            	if(nSlices > numFramesToProcess)
            {
             run("Slice Remover", "first=1 last=" + numFramesToProcess + " increment=1");
            }
            	else
            {
             run("Slice Remover", "first=1 last=" + (nSlices-1) + " increment=1");
            }
     selectWindow("video");
         m++;
        }
        close();
    }
}

// Tracks the larvae with mTrack2, and writes the Results to   
// file, with .csv extension that is immediately readable by R or Excel

function trackParticles2(laneNumber)
{
    setBatchMode("show"); // IMPORTANT! windows will pop up so that it can find the Results to save properly
    run("Clear Results");
    outputFilename = imageTitle + "_clip_" + laneNumber + ".csv";
    fullPathResults = outputDir + outputFilename;
    
// check and amend these parameters manually before you batch process
// min & max sized objects to track, max no. of pixels larva can go between frames before filtering out, min frames it is trackable
    run("MTrack2 ", "minimum=40 maximum=1500 maximum_=8 minimum_=20 display"); 
    selectWindow("Results");
	saveAs("Measurements", fullPathResults);
    run("Clear Results");
        
// OPTIONAL - save individual video clips if you want to check it worked
     //  selectWindow("stack");
   //    videoName = imageTitle + "_clip_" + laneNumber;
   //    videoPath = outputDir + videoName;
   //    saveAs("avi",videoPath);
}
