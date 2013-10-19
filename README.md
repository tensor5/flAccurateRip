Introduction
------------

*flAccurateRip* is a command line tool to verify the accuracy of FLAC files
ripped form CD, using the information contained in the AccurateRip™ database
about other rips of the same CD.

Usage
-----

Suppose you ripped a CD into `track01.flac`, `track02.flac`, ...,
`trackNN.flac`. Open a shell in the directory containing the FLAC files, and
run:

    $ flaccuraterip track01.flac track02.flac ... trackNN.flac

If a pressing of this CD is present in the AccurateRip™ database,
`flaccuraterip` will output the ripping accuracy for each track. If the CD was
ripped without setting the drive offset, you should use the flag
`--sample-offset=N`, where N is the offset indicated
[here](http://accuraterip.com/driveoffsets.htm
"http://accuraterip.com/driveoffsets.htm") for your drive.
