# Multifit

## Multifit/Polydefix

Multifit/Polydefix is an open-source IDL software package for an efficient 
processing of diffraction data obtained in deformation apparatuses at 
synchrotron beamlines. Multifit allows users to decompose two-dimensional 
diffraction images into azimuthal slices, fit peak positions, half-widths, 
and intensities, and propagate the results to other azimuth and images. 
Polydefix is for analyzes of deformation experiments. Starting from 
output files created in Multifit or other packages, it will extract elastic 
lattice strains, evaluate sample pressure and differential stress,
and prepare input files for further texture analysis.

Multifit/Polydefix homepage is at http://merkel.zoneo.net/Multifit-Polydefix/

## About Multifit

Multifit itself is dedicated to the fitting of two-dimensional (2-D) 
diffraction data. It will extract d-spacings, intensities, and half-widths 
for peaks of a given material, for multiple orientations and over multiple 
diffraction images. Data analysis procedure is in place for 2-D data collected 
at the D-DIA apparatus available at the GeoSoilEnviroCARS sector of the 
Advanced Photon Source (APS), Argonne National Laboratory, USA. It is currently 
being adapted to data collected at the D-DIA apparatus available at the 

Multifit/Polydefix is written in the Interactive Data Language (IDL) provided 
by Exelis Visual Information Solutions (Boulder, Colorado). It is an 
open-source software package, licensed under GNU General Public License that 
runs within the IDL Virtual Machine, as provided at no-cost by Exelis Visual 
Information. It will run on any platform with an IDL Virtual Machine,
Windows, OS X, Linux, and Solaris. The IDL Virtual Machine does not require a 
license to run. An IDL license is required, however, for developing, compiling, 
and adding new features.

## Development with IDL

IDL comes with a complex eclipse-based development environment in a graphical user 
interface (gui) for editing projects. Over the years the gui became more and more 
complex. It is a complete overkill for what we are doing here. Moreover, each upgrade 
of IDL comes with technical issues and, recently, it simply stopped working on my 
computers. Therefore, I decided to completely quit using the IDL gui for developing 
applications and develop multifit/polydefix in text mode only. You are welcome to try 
using the gui if you want, but I will not help you with it. 

## Compiling, editing, and playing with the code

Once you downloaded the latest version of the code, you can simply open the 
files in your favorite text editor. I use kate, one of the KDE text editor. 
It does have a specific mode for coloring IDL codes (select Tools -> Highlighting 
-> Sources -> RSI IDL).

To compile and run the code, you should move into the folder with your source, 
start IDL in the command line, and type `@build`:
```
  cd ~/IDL/Multifit
  idl
  @build
```
You can then test your changes by typing
```
  multifit
```
in the command line. It will start the latest version you compiled.

Once you're done, if you want to quit IDL, simply type `exit` in the IDL command line.

I do not recommend the IDL development environment. On the other hand, the online help 
is extremely helpful. To start the help from the command line, simply type idlhelp. 
