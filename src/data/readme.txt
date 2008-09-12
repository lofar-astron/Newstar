Updated December 4  1995
Ger de Bruyn

This directory contains MDL files for the primary and secondary WSRT
flux density and position calibrators at the wavelengths of 6cm (4874
MHZ), 21cm (1412 MHz), 49cm (608.8 MHz) and 92cm (325.125 MHz).

The WSRT flux density scale is based on the Baars et al.  (Astron.
Astrophys.  61, 104, 1977) scale values for the source 3C286.  3C286
is not known to be variable to more than 1%.  The secondary
calibrators 3C48 and 3C147 are, however, known to be variable, by
about a few %, at wavelengths of 6 and 21 cm, and possibly also at
longer wavelengths.  The values in the models may therefore have to be
changed somewhat.  At 92cm we also have included models for 3C295 and
3C345.  3C295 is a double radio source (4" separation) that CANNOT
vary on human timescales (the source is tens of kpc in diameter) and
will be used in the future to check the variability of all other
calibrators.  At 6cm and 21cm 3C295 is strongly resolved across the
array and is therefore less suitable as a calibrator.  The source
3C345 is polarized (about 3% at 92cm) and can be used to check any
phase-difference between the X and Y dipoles. In the future we hope to
make regular observations at 92cm of either this source or other
polarized calibrators (e.g. 3C303).

At frequencies different from the normal ones the values will have to
be changed according to the spectral index of the source.  Especially
for 21cm line observations at large heliocentric velocities the
correction may be several % and should not be neglected.  The values
for the spectral index (spectral index = dlog(S)/dlog(freq) are:
 at 6cm: 3C48 -0.95, 3C147 -0.91, 3C286 -0.62
 at 21cm: 3C48 -0.84, 3C147 -0.70, 3C286 -0.45
 at 92cm: 3C48 -0.68, 3C147 -0.62, 3C286 -0.34, 3C295 -0.60

At 49 cm the band is so narrow that you will never have to worry about
the spectral index.

Especially the low-frequency models contain a varying numbers of
background sources. The fluxdensities of these background sources
obviously need to be adjusted for the frequency dependent primary
beamwidth. You can do this using NMODEL, option FEDIT. This option
will then ask you for a reference epoch (use B1950) and a SCN-file
from which it gets the pointing centre of the calibrator observation.
It then scales the background sources using a COS(cfr)**6 function
(where c=0.0629, r is the radius in degrees and f is the frequency in
MHz) which assumes that the primary beams scale with frequency. This
is a good first order approximation.

(The spectral indices at 92cm are only approximate. Most of the WSRT
calibrators are socalled SSC sources which show a spectral turnover
around 100-200 MHz).


When doing bandpass calibration in line observations it is strongly
advised to use a frequency INDEPENDENT value of the flux density. The
models in this directory do not have a spectral index, so this will be
the default when you use these models for calibration. The reference
frequency in the header of the model is therefore not used.  (Note
that if you would use a spectral index then each source in the
synthesized field will adopt the spectral index of the calibrator and
when you subtract images from two sides of the band then part of the
source, and all its side/grating lobes, will remain in the image).

However, it is possible to give the calibrator a spectral index using
the EDIT command in NMODEL (first go to the MODIFY subgroup of
keywords). (If you want to give all sources in the model the same
spectral index you can use the FEDIT option).  This could be useful
when you have data with the broadband 92cm DCB (which permits
observations from 300 -390 MHz) and you want to determine real
spectral indices for sources in your field. But if you wish to
conduct bandwidth synthesis with the broadband 92cm system then again
it is best to only change the models for the different primary beams
but not for the intrinsic spectral index.


If you have any questions about using the calibrator models I refer to
NEWSTAR recipe #13 (to be written), or contact me (Ger de Bruyn,
0521-595257, ger@astron.nl)).






