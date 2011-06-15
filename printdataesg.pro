; *******************************************************************
; Multfit efficient processing of 2D diffraction images
; Copyright (C) 2000-2011 S. Merkel, Universite Lille 1
; http://merkel.zoneo.net/Multifit/
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;
; *******************************************************************

; *******************************************************************
;
; printdataesg
;
; Export data for all orientations in esg format
; Send
; - lun: unit number of file to write in
; - distance: the sample to detector distance, in cm
;
; Created 03/2005, S. Merkel
;
; *******************************************************************
pro printdataesgall, lun, distance
common rawdata, nalpha, ntheta, alpha, twotheta, data
printf, lun, "_pd_block_id noTitle|#0"
printf, lun, ""
printf, lun, "_diffrn_detector Image Plate"
printf, lun, "_diffrn_detector_type ?"
printf, lun, "_pd_meas_step_count_time ?"
printf, lun, "_diffrn_measurement_method ?"
printf, lun, "_diffrn_measurement_distance_unit cm"
printf, lun, "_pd_instr_dist_spec/detc " + STRTRIM(STRING(distance, /PRINT),2)
printf, lun, "_diffrn_radiation_wavelength ?"
printf, lun, "_diffrn_source_target ?"
printf, lun, "_diffrn_source_power ?"
printf, lun, "_diffrn_source_current ?"
printf, lun, "_pd_meas_angle_omega 90.0"
printf, lun, "_pd_meas_angle_chi 90.0"
printf, lun, "_pd_meas_angle_phi 0.0"
printf, lun, "_riet_par_spec_displac_x 0"
printf, lun, "_riet_par_spec_displac_y 0"
printf, lun, "_riet_par_spec_displac_z 0"
printf, lun, "_riet_meas_datafile_calibrated false"
;string = "Adding data for azimuth = " + STRING(alpha(0), /PRINT)
;logit, log, string
printdataesg, lun, 0
for i=1,nalpha-1 do begin
    ;string = "Adding data for azimuth = " + STRING(alpha(i), /PRINT)
    ;logit, log, string
    printf, lun, "_pd_block_id noTitle|#" + STRTRIM(STRING(i, /PRINT),2)
    printf, lun, ""
    printdataesg, lun, i
endfor
end


; *******************************************************************
;
; printdataesg
;
; Export data for one orientation in esg format
; Send
; - lun: unit number of file to write in
; - i: index for azimuth
;
; Created 03/2005, S. Merkel
;
; *******************************************************************

pro printdataesg, lun, index
common rawdata, nalpha, ntheta, alpha, twotheta, data
printf, lun, "_pd_meas_angle_eta " + STRTRIM(STRING(alpha(index),/PRINT),2)
printf, lun, ""
printf, lun, "loop_"
printf, lun, "_pd_proc_2theta_corrected"
printf, lun, "_pd_calc_intensity_total"

; This is not necessary. I thought data might have to start from 2theta=0
; but it's not true. Similarly, interval between data points is not critical.
; you can choose whatever you like
;if (twotheta(0) gt 0) then begin
;    interval = twotheta(1)-twotheta(0)
;    angle = 0.0
;    while (angle lt twotheta(0)) do begin
;       printf, lun, " " + STRTRIM(STRING(angle,/PRINT),2) + " " + STRTRIM(STRING(0.0,/PRINT),2)
;       angle = angle + interval
;    endwhile
;endif
for i=0, ntheta-1 do begin
    printf, lun, " " + STRTRIM(STRING(twotheta(i),/PRINT),2) + " " + STRTRIM(STRING(data(index,i),/PRINT),2)
endfor
printf, lun, ""
end
