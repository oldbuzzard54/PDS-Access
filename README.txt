PDS-Access is a subprogram for Hercules/Hyperion programmers to 
access PDS (Partioned Data Set) directory and members from COBOL
and PL/1 programs.  Three functional programs are included as
samples of using the subroutines.  Two of them convert a PDS to
IEBUPDTE input.  The third scans a PDS for one or more strings
and produces a report listing the records in the PDS that match
the string(s).

INSTALLING PDS-Access
=====================
The JCL to install can be found in the file Receive.jcl.  This JCL uses
the userid of HERC01 and create several files on DASD volume PUB000.
Adjust the user id and volume if desired and submit.  All steps should
end with CC=0000.

EXECUTING PDS-Access
=====================
The JCL for run the sample programs can be found int TEST.jcl.

