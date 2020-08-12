#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
December 2019
Gridded Cloud Water Balance Model Version 1.5: Retroactively removing Senay et al. NDVI method of calculating AET BUT retaining Jennings et al. 2018 T50 coefficients for estimating snow.
This version also removes the IGRID veg layer precip correction. The relevant lines have just been commented out.
--Modified from the "season test" code of November 201sdf9.
This version reads the reprojected tif files from the MACA GCMs instead of daymet netCDF files.
"""sdfdf
import netCDF4,datetime,os,multiprocessing, gdal
import numpy as np
import time,sys
np.seterr(invalid = 'ignore') #Don't let nans in rasters raise a warning.
#np.seterr(over = 'ignore') #These have been checked and screen valid exceptions.
np.seterr(divide = 'ignore') #could comment out for debugging if wish.


def open_tif(get_filename):
    global multiplier_mask
    src_ds = gdal.Open(input_data_path + get_filename)
    #numbands = src_ds.RasterCount
    #numrows = src_ds.RasterYSize
    #numcols = src_ds.RasterXSize
    src_band = src_ds.GetRasterBand(1) # Indexing starts with one rather than zero.
    #blocksize = src_band.GetBlockSize()
    #dtype = src_band.DataType
    array = src_band.ReadAsArraycxv
    final_array = array * multiplier_mask
    final_array = np.where(final_array > 9000, np.nan, final_array)
    final_array = np.where(final_array < -9000, np.nan,final_array)
    src_ds = None
    return final_array

def est_snow():
    swe_m = melt_one_day()
    out_swe = accum_one_day(swe_m)
    return out_swe

def melt_one_day(): # Melt factor used is an average of table 1 in Hock 2003. Journal of Hydrology 282:104-115. Math based on equation 2 of this publication.
    global melt_factor, low_temperature_differences, accumswe # AKD - melt factor = DDF (Hock 2003) = mm melt/day/degrees C
    melt_delta = low_temperature_differences * melt_factor
    melt_delta = np.where(melt_delta < 0,0,melt_delta) # When Tavg < s, melt_delta < 0, therefore this takes care of condition Td <= T0 in Hock 2003.
    end_swe = accumswe - melt_delta
    end_swe = np.where(end_swe < 0,0,end_swe)
    return end_swe

def accum_one_day(start_swe): # Dingman et al. 2002., Lutz et al. 2010.
    global precip_fraction, tmean, low_temperature_differences, low_thresh_temperatures, high_thresh_temperatures, precip
    rain = precip_fraction * low_temperature_differences
    rain = np.where(tmean <= low_thresh_temperatures, 0,rain) # AKD - 0% change rain if tmean < low threshold
    rain = np.where(tmean > high_thresh_temperatures, 1.0, rain) # AKD - 100% chance rain if temp > high threshold
    snow_increment = (1.0 - rain) * precip
    #snow_increment = np.where(snow_increment < 0,0,snow_increment)
    end_swe = start_swe + snow_increment
    return end_swe

def namefixlen(v):
    s = str(v)
    while len(s) < 3:
        s = '0' + s
    return s

def get_param_one_day(year, param, day_index): # Read Daymet netcdf files, get one day for specified param.
    infile = netCDF4.Dataset(input_data_path + '{y}_{p}_na.nc4'.format(y = year, p = param))
    data = np.ma.filled(infile.variables[param][day_index].astype(np.float32),np.nan)
    #yearday = infile.variables['yearday'][:]
    #if len(yearday) != 365: #Daymet files do not have leap year extra days, will always be 365.
    #    print(year, param)
    #    raise Exception('Terminating Program: Wrong number of days (not 365) in netcdf file!')
    infile.close()
    return data

def get_latitude_radians(year,seed_param): # Returns 2D array of radian latitudes for every grid cell in current daymet file.
    infile = netCDF4.Dataset(input_data_path + '{y}_{p}_na.nc4'.format(y = year, p = seed_param))
    lat_degrees = infile.variables['lat'][:].astype(np.float32)
    infile.close()
    return (np.pi/180) * lat_degrees

def get_tmean(tmin_name,tmax_name):
    print(tmin_name, tmax_name)
    tmax = open_tif(tmax_name)
    tmin = open_tif(tmin_name)
    tmax = tmax - 273.15 # MACA GCM data provide temperatures in degrees Kelvin
    tmin = tmin - 273.15
    tmean = (tmax + tmin) /2
    return tmean,tmax,tmin

def leapyearlist():
    #Generate list of leap years
    leapyearlist=list(range(1804,2300,4))
    leapyearlist.remove(1900);leapyearlist.remove(2100);leapyearlist.remove(2200)
    return leapyearlist

def fix_len(s):
    s = str(s)
    if len(s) == 1: s = '0' + s
    return s

def convert_yday_to_date(year,yday): # Daymet always has 365 days in a year. On leap years, December 31 is discarded.
    one_day = datetime.timedelta(days = 1)
    year = int(year)
    first_day_of_year = datetime.datetime(year = year,month = 1,day = 1)
    this_day = first_day_of_year + (yday) * one_day
    year = this_day.year
    month = this_day.month
    day = this_day.day
    today = datetime.datetime(month = month,day = day, year = year)
    return today

def onlyabsneg(arr):
    ret = np.where(arr > 0,0,arr)
    ret = np.absolute(ret)
    return ret

def nonneg(arr):
    return np.where(arr <0,0,arr)

def hamon_evapotrans(): #daytime_length in hours, temperatures in degrees C.
    global daylength, tmean
    es = calc_Es() # Lutz et al 2010
    bottom = tmean+273.3
    bracket = es/bottom
    pet = 29.8*daylength * bracket # Days = 1 because daily timestep so not included.
    return pet #mm / day

def calc_inverse_rel_distance(doy): #CHECKS OK
    doy = float(doy)
    bracket_term = ((2*np.pi)/365) * doy
    retval = (.033 * np.cos(bracket_term)) + 1
    return retval #radians

def calc_solar_declination(doy): #CHECKS OK
    doy = float(doy)
    bracket_term = (((2*np.pi)/365) * doy) - 1.39
    retval = .409 * np.sin(bracket_term)
    return retval # radians

def calc_sunset_hour_angle(latitude,solar_declination): # CHECKS OK
    #Both inputs need to be in radians
    bracket_term = np.tan(latitude) * -1 * np.tan(solar_declination) # All trig function use radians
    bracket_term = np.where(bracket_term < -1,-1,bracket_term)
    bracket_term = np.where(bracket_term > 1, 1, bracket_term) #arccos is undefined outside the range [-1,1] and high latitudes in the dataset lead to too large values of np.tan(latitude)
    retval = np.arccos(bracket_term)
    return retval #radians

def calc_Rso(Ra): #CHECKS OK
    global elevation #meters
    # Equation 37 in ch3 of FAO doc.
    # Ra as calc from equation 21.
    # Elevation in meters
    correction_term = (.00002 * elevation) + .75
    Rso = Ra * correction_term
    return Rso


def calculate_Ra(inverse_rel_distance,sunset_hour_angle,latitude,solar_declination): #CHECKS OK
    #Ra = extra-terrestrial radiation in MJ/m2/day. Equation 21 of Ch3 FAO doc. Radiation hitting top of Earth's atmosphere.
    # Latitude (j) in radians. Positive for northern hemisphere and negative for southern hemisphere.
    # Sunset hour angle (Ws) in radians. Solar declination (d) in radians.
    Gsc = .0820 # Solar constant : MJ/m^2/day
    first_term = (float((24*60))/np.pi) * Gsc #37.586
    first_half_bracket_term = (np.sin(latitude) * np.sin(solar_declination)) * sunset_hour_angle
    second_half_bracket_term = (np.cos(latitude) * np.cos(solar_declination) * np.sin(sunset_hour_angle))
    full_bracket_term = first_half_bracket_term + second_half_bracket_term
    Ra = first_term * full_bracket_term * inverse_rel_distance
    return Ra

def calc_Rnl(Ea,Rs,Rso): #CHECKS OK
    #Rnl = net long-wave radiation in MJ/m2/day. Equation 39 Ch3 FAO doc.
    #Ea = actual vapor pressure (Kpa). Rs = Solar radiation in MJ/m2/day as calc by equation 35.
    #Rso = clear sky radiation in MJ/m2/day as calculated by equation 37.
    global tmax, tmin
    relative_shortwave_radiation = Rs/Rso
    #if relative_shortwave_radiation > 1 : relative_shortwave_radiation = 1
    relative_shortwave_radiation = np.where(relative_shortwave_radiation > 1, 1, relative_shortwave_radiation)
    tmax_k = tmax + 273.16;tmin_k = tmin + 273.16 # Convert to degrees Kelvin
    tmax_k = tmax_k**4;tmin_k=tmin_k**4
    left_bracket_term = (tmax_k+tmin_k)/2;left_bracket_term = left_bracket_term * .000000004903 # Stefan-Boltzmann constant
    middle_term = 0.34 - (.14 * np.sqrt(Ea))
    right_term = (1.35*relative_shortwave_radiation) - 0.35
    Rnl = left_bracket_term * middle_term * right_term
    return Rnl

def calc_Rn(Rnl,Rs): # CHECKS OK
    # Equation 40 of ch3 FAO doc.
    # Using estimate of Rns from equation 38 :> Rns = (1-a)*Rs. a is default set to 0.25 unless a regression has been done.
    Rns = 0.75* Rs
    Rn = Rns - Rnl
    return Rn


def calc_todays_PET(Et_type):
    #print('Calculating PET')
    global tmax,tmin,tmean,precip,accumswe,year,day_index,daylength,latitude
    if Et_type == 'oudin':
        inverse_rel_distance = calc_inverse_rel_distance(day_index + 1) # day_of_year = day_index + 1
        solar_declination = calc_solar_declination(day_index + 1)
        sunset_hour_angle = calc_sunset_hour_angle(latitude,solar_declination)
        Ra = calculate_Ra(inverse_rel_distance,sunset_hour_angle,latitude,solar_declination)
        Et = calc_oudin_pet(Ra)
    if Et_type == 'hamon': Et = hamon_evapotrans()
    elif Et_type == 'penman_montieth':
        inverse_rel_distance = calc_inverse_rel_distance(day_index + 1) # day_of_year = day_index + 1
        solar_declination = calc_solar_declination(day_index + 1)
        sunset_hour_angle = calc_sunset_hour_angle(latitude,solar_declination)
        Ra = calculate_Ra(inverse_rel_distance,sunset_hour_angle,latitude,solar_declination) # AKD - need to convert Watts to mJ
        srad = get_param_one_day(year, 'srad',day_index) # Incidient short wave radiation flux density, taken as an average over the daylight period of the day in MJ/m2/day.
        Rs= (srad * daylength) / 1000000 # Convert to daily total radiation in MJ/m2/day. See ref here: https://daac.ornl.gov/DAYMET/guides/Daymet_V3_CFMosaics.html
        # Rs is now equivalent to Rs in FAO procedures for calculating Penman Montieth: http://www.fao.org/docrep/X0490E/x0490e07.htm#meteorological%20factors%20determining%20et
        G = 0 # Soil heat flux is assumed to be zero for daily calculations
        Rso = calc_Rso(Ra) # Clear sky solar radiation

        Es_minus_Ea,Ea = calc_Es_minus_Ea()
        Rnl = calc_Rnl(Ea,Rs,Rso) #The ratio of Rs/Rso constrained to 1.

        D = calc_D()
        Rn = calc_Rn(Rnl,Rs)
        Et = calc_Penman_Montieth_Eto(Rn,G,Es_minus_Ea,D)
    Et = np.where(accumswe > 0,0,Et) # No PET can happen when snow on ground.
    return Et

def calc_gamma():#CHECKS OK
    # Gamma is the psychrometric constant used for Penman_Montieth Eto
    global atmospheric_pressure
    gamma = 0.665 * .001 * atmospheric_pressure
    return gamma

def calc_atmospheric_pressure(): # CHECKS OK
    global elevation
    tr = .0065*elevation
    top = 293-tr
    inside = top/293
    right = inside**5.26
    atmos_pressure = 101.3*right
    return atmos_pressure

def calc_D():
    #Slope of the vapor pressure curve
    # t is AVERAGE air temperature in degrees C
    # Equation 13 in FAO doc
    global tmean
    tk = tmean + 237.3
    bracket_term = (17.27*tmean)/(tk)
    right_term = 0.6108 * np.exp(bracket_term)
    top_term = 4098*right_term
    bottom_term = (tk)**2
    D = top_term/bottom_term
    return D


def calc_Es(): #CHECKS OK
    #Equation 12 in FAO doc
    # This is just saturation vapor pressure for Tmean
    global tmax, tmin
    e_tmax = calc_saturation_vapor_pressure(tmax)
    e_tmin = calc_saturation_vapor_pressure(tmin)
    Es = (e_tmax + e_tmin)/2
    return Es #kPa

def calc_Es_minus_Ea():
    # Will use humidity data or estimate Ea by assuming that dewpoint is two degrees below daily Tmin.
    global tmax, tmin
    Es = calc_Es()
    tmin_adjusted = tmin - 2 # Suggested for arid regions by FAO. In the absence of humidity data, assume dewpoint is 2 degrees below Tmin.
    Ea = calc_saturation_vapor_pressure(tmin_adjusted)
    Es_minus_Ea = Es - Ea
    return Es_minus_Ea,Ea

def calc_saturation_vapor_pressure(t): #CHECKS OK
    # This is Es for a given t
    # t = degrees C, equation 11 in FAO doc
    bracket_term = (17.27*t)/(t+237.3)
    right_term = np.exp(bracket_term)
    saturation_vapor_pressure = 0.6108 * right_term
    return saturation_vapor_pressure #kPa


def calc_Penman_Montieth_Eto(Rn,G,Es_minus_Ea,D,u2 = 2): #CHECKS OK WITH EXAMPLE 17 CHAPTER 4.
    #Rn = net radiation (MJ/m2/day), G = soil heat flux density (MJ/m2/day),Tavg in degrees C
    #Es_minus_Ea = vapor pressure deficit in KPa, D = slope of vapor pressure curve
    #gamma = psychrometric constant (Kpa / degrees C)
    #u2 = wind speed at 2m height, set to 2 m/s by default
    global gamma,tmean
    topleft_term = .408*D*(Rn-G)
    corrected_tavg = tmean + 273
    topmiddle_term = (900/corrected_tavg)*gamma
    topright_term = u2*Es_minus_Ea
    overall_top_term = (topmiddle_term*topright_term) + topleft_term
    wind_correction = 0.34*u2
    bottomright_term = (1 + wind_correction) * gamma
    overall_bottom_term = D + bottomright_term
    Eto = overall_top_term / overall_bottom_term
    Eto = np.where(Eto < 0,0,Eto)
    return Eto

def calc_oudin_pet(Ra): # Equation 3 in Oudin et al. 2010
    global tmean
    # Since this is daily timestep, mdays = 1 and not included here.
    top = Ra * (tmean + 5) * .408
    bottom = 100
    PET = top/bottom
    PET = np.where(tmean > -5, PET, 0)
    return PET # mm / d

'''
#These two funcs were used to make the heat load layer. Not needed for model runs.
def fold_aspect(aspect): # Used to make the folded aspect array for heat load. Can be deleted or saved for reference.
    inner = np.absolute(aspect - 225)
    ret = np.absolute(180 - inner)
    ret = ret * (np.pi/180) # convert to radians
    return ret

def heat_load(lat,slope,aspect): # S1 appendix to Lutz et al. 2010 # Used to make the heat load layer. Saved for reference.
    first = 0.339 + (0.808 * np.cos(lat) * np.cos(slope))
    print('first',np.nanmin(first),np.nanmax(first))
    second = 0.196 * (np.sin(lat) * np.sin(slope))
    print('second',np.nanmin(second),np.nanmax(second))
    third = 0.482 * (np.cos(aspect) * np.sin(slope)) # Folded aspect used, converted to radians for np funcs
    print('third',np.nanmin(third),np.nanmax(third))
    HL = first - second - third
    print('HL',np.nanmin(HL),np.nanmax(HL))
    return HL
'''

def heat_load_adjust_pet():
    global heat_load, PET
    PET_HL = PET * heat_load
    return PET_HL

def calc_remove_fraction():
    # When pet = w, exponent = 0, therefore multiplier = 0, therefore amount removed from soil = 0
    # When w = 0, multiplier becomes 1 - (1/e^(pet/soil_whc))
    # When w < pet, multiplier becomes  1 - (1/e^(reduced_pet/soil_whc)), where reduced pet = pet - w
    # When pet = soil_whc, still can remove only a frac = 1 - (1/e)
    global soil_whc, soil_water, PET_adjusted, w
    exponent = (nonneg((PET_adjusted - w)) / soil_whc) * -1 #B2
    exponent = np.where(np.isinf(exponent) == True, 0, exponent) # Some soil_whc cells indicate zero holding capacity. Division by zero in prev line creates inf, must be screened.
    multiplier = (1.0 - np.e**exponent) #B3
    decrements = soil_water * multiplier #B4
    return decrements

def calc_aet_and_runoff_and_soilw(): # soil_water and associated vars are 2D arrays. Does not use NDVI correction.
    # w = water input to soil = rain + melt
    global soil_water, PET_adjusted, w,year,day_index
    soilw_inputs = np.where(w > PET_adjusted, w - PET_adjusted, 0) # If PET >= w, assume no water input to soil. If w > PET then water input to soil = "excess", where excess = w-aet, but aet = pet when w > PET, so excess = w - pet. See equation 12 in Lutz et al. appendix
    remove_fraction = calc_remove_fraction() #Delta soil in Lutz appendix eq 13.
    soilw_remove_amounts = np.where(w <= PET_adjusted, remove_fraction, 0) #If w > PET then no water removed from soil. If w <= PET, then Lutz eq 13 (calc_remove_fraction) used to determined amount removed.
    soil_water = soil_water + soilw_inputs #Since we are working with 2D arrays, some locations will have positive inputs, some will be zero.
    #if np.nanmin(soilw_remove_amounts) < 0 : raise Exception('{y}  {d} Negative soil water remove amounts detected. Terminating program'.format(y = year, d = day_index))
    #if np.nanmin(soilw_inputs) < 0 : raise Exception('{y}  {d} Negative soil water remove amounts detected. Terminating program'.format(y = year, d = day_index))
    soil_water = soil_water - soilw_remove_amounts #Since we are working with 2D arrays, some locations will have positive remove amounts, some will be zero.
    soil_water = nonneg(soil_water)
    runoff = nonneg(soil_water - soil_whc) # The nonneg function modifier ensures that only locations with positive runoff values pass through.
    soil_water = np.where(soil_water > soil_whc, soil_whc,soil_water) #Bring all locations with soil_water > whc back to whc. Previous line has gotten rid of that excess as runoff.
    total_evap_avail = w + remove_fraction #B4
    aet = np.where(PET_adjusted >= total_evap_avail,total_evap_avail,PET_adjusted) #From Lutz appendix: "AET equals the smaller of PET or (delta_soil + w)"; total_evap_avail = delta_soil + w
    aet = np.where(np.isinf(aet) == True, 0, aet)
    aet = np.where(np.isnan(PET_adjusted) == True, np.nan,aet)
    soil_water = np.where(np.isnan(PET_adjusted) == True, np.nan, soil_water)
    return aet,runoff

'''
# This is the version2 function which in corporates Senay et al. NDVI correction.
def calc_aet_and_runoff_and_soilw(): # soil_water and associated vars are 2D arrays
    # w = water input to soil = rain + melt
    global soil_water, PET_adjusted, w,year,day_index,rain, melt, soil_whc
    #------ Code below is from older version of model that used Lutz et al. methods--------------
    # remove_fraction = calc_remove_fraction() #Delta soil in Lutz appendix eq 13.
    # soilw_remove_amounts = np.where(w <= PET_adjusted, remove_fraction, 0) #If w > PET then no water removed from soil. If w <= PET, then Lutz eq 13 (calc_remove_fraction) used to determined amount removed.
    # soilw_inputs = np.where(w > PET_adjusted, w - PET_adjusted, 0) # If PET >= w, assume no water input to soil. If w > PET then water input to soil = "excess", where excess = w-aet, but aet = pet when w > PET, so excess = w - pet. See equation 12 in Lutz et al. appendix
    # soil_water = soil_water + soilw_inputs #Since we are working with 2D arrays, some locations will have positive inputs, some will be zero.
    # soil_water = soil_water - soilw_remove_amounts #Since we are working with 2D arrays, some locations will have positive remove amounts, some will be zero.
    # ----End old model code -----
    # --------------------------------------------------------------------------------------------------------------------------
    # Lines below are from Senay et al. Use NDVI to calculate AET
    SWi = soil_water + rain + melt # This is different from Senay et al. They use SWi = previous_soil_water + effective_precipitation. This is previous soil water + fraction of precip falling as rain + melt from snow.
    # Notes we are no longer using the calc_remove_fraction() to apply the exponential decay of soil water removal.
    varA = 1.25
    varB = 0.2
    ndvi_name = 'ndvi' + namefixlen(day_index+1) + '.npy.npz'
    ndvi_array = np.load(ndvi_name)
    ndviF = ndvi_array['ndvi']
    etasw1A = (varA * ndviF + varB) * PET_adjusted
    etasw1B = (varA * ndviF) * PET_adjusted
    etasw1 = np.where(ndviF > 0.4, etasw1A, etasw1B)
    etasw2 = (SWi / (0.5 * soil_whc)) * etasw1
    etasw3 = np.where(SWi > (0.5 *soil_whc), etasw1, etasw2)
    etasw4 = np.where(etasw3 > SWi, SWi, etasw3)
    aet = np.where(etasw4 > soil_whc, soil_whc, etasw4)
    etasw1A = 0 # reset to save memory
    etasw1b = 0
    etasw1 = 0
    etasw2 = 0
    etasw3 = 0
    etasw4 = 0
    # End Senay et al. NDVI Code
    # -----------------------------------------------------
    aet = np.where(np.isinf(aet) == True, 0, aet) # Fix unrealistic values
    aet = np.where(np.isnan(PET_adjusted) == True, np.nan,aet) # Fix unrealistic values
    soil_water_temp = SWi - aet # per Senay et al. pers comm.
    soil_water = np.where(SWi > soil_whc, soil_whc - aet, soil_water_temp)
    soil_water = nonneg(soil_water)
    runoff = nonneg(SWi - soil_whc) # This has changed from previous version of model. Was runoff = soil_water - soil_whc
    soil_water = np.where(soil_water > soil_whc, soil_whc,soil_water) #Bring all locations with soil_water > whc back to whc. Previous lines have gotten rid of that excess as runoff.
    #total_evap_avail = w + remove_fraction #B4 # From old model with Lutz equations.
    #aet = np.where(PET_adjusted >= total_evap_avail,total_evap_avail,PET_adjusted) #From Lutz appendix: "AET equals the smaller of PET or (delta_soil + w)"; total_evap_avail = delta_soil + w # From old model
    soil_water = np.where(np.isnan(PET_adjusted) == True, np.nan, soil_water)
    return aet,runoff
'''

def write_var_to_netCDF(outfile,varname,arr,dayindex):
    #arr = arr * output_mult_factor
    outfile.variables[varname][dayindex] = np.ma.masked_invalid(arr)


def init_output_netCDF(filename,year,seed_param,new_param, signif_digits = 2, intswitch = False): # Resets spatial extent to match subsetted data.
    print('Initializing output file {f} for {y}'.format(f=filename, y = year))
    global output_data_path,output_units, new_lats, new_lons, new_x, new_y, model, scenario
    fill_dict = {'lat': new_lats, 'lon': new_lons, 'x': new_x, 'y': new_y}
    fill_dims = {'x': 4700, 'y': 3300, 'time': 366}
    try:
        os.remove(output_data_path + filename)
    except:
        pass
    dsout = netCDF4.Dataset(output_data_path + 'V_1_5_' + str(year) + '_' + model + '_' + scenario + '_' + filename,'w',clobber = True, format = 'NETCDF4')
    dsin = netCDF4.Dataset(input_data_path + '1980_dayl_na.nc4') # Seed file is a leap year, so time dimension = 366 days.
    #Copy dimensions from seed file
    for item in dsin.dimensions:
        dname = dsin.dimensions[item].name
        if dname in fill_dims:
            dlen = fill_dims[dname]
        else:
            dlen = dsin.dimensions[item].size
        dsout.createDimension(dname, dlen)
    # Copy variables from old file, but replace seed param with new param.
    for item in dsin.variables:
        dtype = dsin.variables[item].dtype

        vname = dsin.variables[item].name
        '''
        if vname.strip() == seed_param :
            vname = new_param
        dims = dsin.variables[item].dimensions #Dimensions for the new output variable are defined as ('time','y','x') here with values from fill_dict used for y and x.
       '''
        if vname.strip() == seed_param :
            vname = new_param
        elif vname.strip() == 'yearday':
            vname = 'timeunit'
        #print(item,dsin.variables[item].dimensions)
        if vname != 'timeunit': dims = dsin.variables[item].dimensions
        else: dims = dsin.variables['time'].dimensions
        # Set variable attributes

        #print(item,vname,intswitch)
        if vname != new_param :
            print('old')
            outVar = dsout.createVariable(vname, dtype, dims, zlib = True, complevel = 3) #Set complevel 1 - 9 to change compression level. 9 is most compression
            outVar.setncatts({k: dsin.variables[item].getncattr(k) for k in dsin.variables[item].ncattrs()})
        elif intswitch == False:
            outVar = dsout.createVariable(vname, dtype, dims, zlib = True, complevel = 3, least_significant_digit = signif_digits) #Set complevel 1 - 9 to change compression level. 9 is most compression
            outVar.setncatts({k: dsin.variables[item].getncattr(k) for k in dsin.variables[item].ncattrs()})
        else:
            outVar = dsout.createVariable(varname = vname, datatype = 'i2',dimensions =  dims, zlib = True, complevel = 3)
            attdict = {}
            for k in dsin.variables[item].ncattrs():
                if k == '_FillValue':
                    continue
                elif k == 'missing_value':
                    continue
                else: attdict[k] = dsin.variables[item].getncattr(k)
            outVar.setncatts(attdict)
        outVar.long_name = vname
        try:
            outVar.units = output_units[vname]
        except:
            pass
    fill_params = ['x','y','lat','lon','time','timeunit','time_bnds','time_bnds']
    newtimes = np.arange(366)
    newtimeunits = [int(x) for x in newtimes+1]
    newtimes = newtimes + 0.5
    newtimebnds = np.vstack((newtimes,newtimes))
    for param in fill_params:
        if param == 'time':
            dsout.variables[param][:] = newtimes
        elif param == 'timeunit':
            dsout.variables[param][:] = newtimeunits
        elif param == 'time_bnds':
            dsout.variables[param][:] = newtimebnds
        elif param in fill_dict:
            outdata = fill_dict[param]
            dsout.variables[param][:] = outdata
        else:
            outdata = dsin.variables[param][:]
            dsout.variables[param][:] = outdata

    dsin.close()
    print('Done init ', new_param)
    return dsout

def chunks(l, n):
    #Yield successive n-sized chunks from l.
    for i in range(0, len(l), n):
        yield l[i:i + n]

def mp_write_daily(): # Subsets to just CONUS and multiplies the output_mult_factor if applicable
    global file_handles,day_index,var_dict, output_params, npz_cores,jobs, subsetting_indices, output_mult_factor
    chunk_size = npz_cores
    param_chunks = chunks(output_params,chunk_size)
    var_dict = {'PET':PET_adjusted,'AET':AET,'runoff':runoff,'Deficit':deficit,'rain':rain,'water_input_to_soil':w,'melt':melt,
               'days_snow':daily_snow,'agdd':agdd,'accum_precip':accum_precip,'accumswe':accumswe,'soil_water':soil_water}
    for chunk in param_chunks:
        jobs = []
        for f in chunk:
            output_chunk = var_dict[f][subsetting_indices[0][0]:subsetting_indices[0][1],subsetting_indices[1][0]:subsetting_indices[1][1]] # Subsetting spatially here to just CONUS
            p = multiprocessing.Process(target = write_daily_to_npz,args=(day_index,year,f,output_chunk,output_mult_factor))
            p.start()
            jobs.append(p)
        for j in jobs:
            j.join()

def write_daily_to_npz(day,year,param,data, output_mult_factor):
    filename = output_data_path + year + '_' + str(day) + '_' + param + '.npz'
    data = data * output_mult_factor
    if output_mult_factor > 1:
        data = np.round(data,0)
        data = np.where(np.isnan(data) == True, -9999, data)
    np.savez_compressed(filename, param = data)

def close_annual_files(file_handles,year):
    for fname in file_handles:
        print('Closing file: {f} for {y}'.format(f = fname, y = year))
        file_handles[fname].close()

def collate_into_netcdf(year,output_params, signif_digits, intswitch = False):
    #output_params is not global here
    file_handles = {}
    for fname in output_params:
        file_handles[fname] = init_output_netCDF(fname + '.nc4',year,'dayl',fname, signif_digits = signif_digits, intswitch = intswitch)
    days = list(range(0,366)) # 366 days here but last will be skipped in non-leapyears because of try-except clause below

    for param in output_params:
        for day in days:
            infilename = output_data_path + str(year) + '_' + str(day) + '_' + param + '.npz'
            try:
                loaded = np.load(infilename)
                arr  = loaded['param']
                arr = np.around(arr, decimals = signif_digits)
                if intswitch:
                    arr = arr.astype('i2')
            except: # Will skip day 366 in non-leapyears
                continue

            print('collating : ',param,year,day)
            try:
                write_var_to_netCDF(file_handles[param],param,arr,day)
            except:
                continue
            os.remove(infilename)
    close_annual_files(file_handles,year)
    return time.time()

def get_soil_whc():
    soil_whc = np.load(input_data_path + 'aligned_soil_whc_array.npy') # File is in cm.
    soil_whc = soil_whc * 10 # convert to mm
    ret = np.where(np.isnan(soil_whc) == True,100, soil_whc)
    return ret

def find_next_collate_year():
    global output_data_path,year_list,next_collate_year,years_done
    fl = os.listdir(output_data_path)
    fl = [x for x in fl if x.split('.')[-1] == 'npz']
    fl = [x for x in fl if output_params[0] in x]
    next_found = False
    for check_year in year_list:
        for npz_file in fl:
            if check_year in npz_file:
                next_collate_year = check_year
                next_found = True
                break
        if next_found == True: break
    return next_collate_year

def launch_new_collation():
    global next_collate_year, current_collate_year, years_done, collate_cores,end_times,output_params, pool, result_list, all_ints
    int_params = ['agdd']
    double_precision = ['runoff']
    if (next_collate_year != current_collate_year) and (next_collate_year not in years_done):
        end_times = []
        print('Launching collation of ', next_collate_year)
        years_done.append(next_collate_year)
        if pool != 'null':
            pool.join()
            print('Results: ',current_collate_year,result_list)
            result_list = []
        current_collate_year = next_collate_year
        pool = multiprocessing.Pool(processes = collate_cores)
        signif_digits = 1
        for param in output_params:
            if all_ints == True:
                intswitch = True
            elif param in int_params:
                intswitch = True
            elif param in double_precision:
                signif_digits = 2
                intswitch = False
            else:
                intswitch = False
                signif_digits = 1
            end_times.append(pool.apply_async(collate_into_netcdf,args = (next_collate_year,[param], signif_digits, intswitch), callback = log_result))
        pool.close()
    return pool

def log_result(result):
    global result_list
    result_list.append(result)

'''
def Igrid_adjust_precip(p): # From Senay et al. pers. comm.
    global Igrid
    out_p = p * (1 - (Igrid/100))
    return out_p
'''

def read_text_list(textfilename):
    out = []
    infile = open(textfilename, 'r')
    for l in infile:
        line = l.split(' ')
        line = [x for x in line if x != '']
        f = line[3]
        out.append(f)
    infile.close()
    return out

def strip_zip(s):
    if s.split('.')[-1].strip() == 'zip': out = s[:-5]
    elif s.split('.')[-1] == 'zip': out = s[:-4]
    else: out = s
    return out

def set_filename_number(s, setval):
    setval = str(setval)
    sf = s.split('_')
    sf[12] = 'extent{sv}'.format(sv = setval)
    out = '_'.join(sf)
    return out

def set_param(s, setval):
    sf = s.split('_')
    sf[2] = setval
    out = '_'.join(sf)
    return out

def read_dir():
    global scenario, input_data_path
    #print('***', input_data_path)
    fl = os.listdir(input_data_path)
    out = [x for x in fl if x.split('.')[-1] == 'tif']
    return out

def find_file_chunks(search_first_year, search_last_year, textfilename = 'null'): # For parsing filenames of GCM data
    global web, scenario
    chunk_list = []
    sfy = int(search_first_year)
    sly = int(search_last_year)
    print('sfy = ', sfy, 'sly = ', sly)
    if web == False: fl = read_text_list(textfilename)
    else: fl = read_dir()
    fl = [x for x in fl if x.split('_')[2] == 'pr'] # look at only one param, pr, for this screening
    fl = [x for x in fl if x.split('_')[5] == scenario]
    fl.sort()

    first_file_chunk = 'null'
    last_file_chunk = 'null'
    current_first_year = 9999
    current_last_year = -9999
    for f in fl:
        sf = f.split('_')
        first_year = sf[6]
        second_year = sf[7]
        if int(first_year) >= sfy:
            if int(first_year) < current_first_year:
                first_file_chunk = f
                current_first_year = int(first_year)
        if int(second_year) >= sly:
            if int(last_year) > current_last_year:
                last_file_chunk = f
                current_last_year = int(second_year)

    real_start_year = first_file_chunk.split('_')[6]
    real_end_year = last_file_chunk.split('_')[7]
    first_chunk_index = int(first_file_chunk.split('_')[0])
    last_chunk_index = int(last_file_chunk.split('_')[0])
    this_chunk_index = first_chunk_index
    while this_chunk_index <= last_chunk_index:
        for f in fl:
            if int(f.split('_')[0]) == this_chunk_index:
                new_file = strip_zip(f)
                chunk_list.append(new_file)
                break
        this_chunk_index +=1
    print('Real start = ', real_start_year, 'Real end = ', real_end_year)
    return chunk_list, real_start_year, real_end_year

def check_files_and_get_breaks(filename): #Numbers in returned year_breaks list indicate file number of first day in years, but 1 is not included.
    global web, textfilenamelist #So for 2006_2010 chunk, 2008 is the only leap and year breaks are [366,731,1097,1462], which means start of 2007 = file 366,2008 = 731, 2009 = 1097, 2010 = 1462
    if web == False: fl = read_text_list(textfilenamelist)
    else: fl = read_dir()
    sf = filename.split('_')
    first_year = sf[6]
    second_year = sf[7]
    leapyears = leapyearlist()
    years_in_file = list(range(int(first_year), int(second_year) + 1))
    leaps_here = [x for x in years_in_file if x in leapyears]
    number_of_leaps = len(leaps_here)
    number_of_years = len(years_in_file)
    number_of_files_in_this_chunk = (number_of_years * 365) + number_of_leaps
    year_breaks = []
    last_break = 1
    for year in years_in_file:
        if year in leapyears: leap_add = 1
        else: leap_add = 0
        next_break = last_break + 365 + leap_add
        year_breaks.append(next_break)
        last_break = next_break
    year_breaks = year_breaks[:-1]
    return number_of_files_in_this_chunk, year_breaks

def create_tif_file_list(first_year, last_year):
    #print('>>> ', first_year, last_year)
    global maca_format_string, textfilenamelist
    out_year_breaks = []
    out_file_list = []
    chunk_list, real_start_year, real_end_year = find_file_chunks(first_year, last_year, textfilename = textfilenamelist)
    for chunk in chunk_list:
        num_files, year_breaks = check_files_and_get_breaks(chunk)
        out_year_breaks.append(year_breaks)
        for extentnum in range(1, num_files + 1):
            this_filename = set_filename_number(chunk, extentnum)
            out_file_list.append(this_filename)
    fl = os.listdir(input_data_path)
    missing_files = [x for x in out_file_list if x not in fl]
    if len(missing_files) > 0 and web == True: print('Missing these files: ', missing_files)
    return out_file_list, out_year_breaks, real_start_year, real_end_year




if __name__ == '__main__':
    # If not using daymet swe, then add accumswe to output_params list. Otherwise, same output can be obtained from daymet source files.
    #multiprocessing.log_to_stderr(logging.INFO)

    #MACA File Format = {YEAR_CHUNK}_macav2metdata_{PARAM}_{MODEL_PART1}_{MODEL_PART2}_{SCENARIO}_{FIRST_YEAR_OF_CHUNK}_{LAST_YEAR_OF_CHUNK}_CONUS_dail_reprojected_with_extent{DAYNUMBER}_resampled.tif
    # _ Splits =               0           1           2         3           4              5              6                      7            8    9        10      11           12            13

    maca_format_string = '{year_chunk}_macav2metdata_{param}_{model_name1}_{model_name2}_{scenario}_{year1}_{year2}_CONUS_dail_reprojected_with_extent{daynumber}_resampled.tif.zip'
    subsetting_indices = [[3600,6900],[2500,7200]] #Used to clip daymet spatial extent to just CONUS for outputs since GCM data is smaller extent.
    #If change subsetting indices, then must re-create the lat, lon,x,y grids loaded below

    scenario = 'rcp85' # ******MUST SET THIS TO CORRECT VALUE BEFORE RUN *****
    model = 'GFDL-ESM2G'
    melt_factor = 4.0
    precip_fraction = 0.167
    web = True
    if web == False: textfilenamelist = 'canesm2_list.txt'
    else: textfilenamelist = 'null'
    start_time = time.time()
    PET_type = 'oudin'
    agdd_base = 10 # Degrees C
    end_times = []
    result_list = []
    first_year = 1950
    last_year = 2099
    next_collate_year = 'null'
    output_mult_factor = 10.0 # See def mp_write_daily(). For smaller file sizes, this multiplies all output values by 10 and stores them as 16 bit integer.
    all_ints = True # If True, all output netcdfs will be int16. If False, then categories in def launch_new_collation() are used.
    bad_list = []

    if web == False:
        #years = list(range(int(real_start_year), int(real_end_year) + 1))
        input_data_path =  '/media/mt/0CED00122A266FA8/daymet_wb/downloaded_tifs/'
        output_data_path = '/media/mt/0CED00122A266FA8/daymet_wb/new_outputs/'
        npz_cores  = 2
        collate_cores = 5
        first_day = 0
        last_day = 5
    else:
        #years = list(range(int(real_start_year), int(real_end_year) + 1))
        input_data_path = '/home/ubuntu/data/'
        output_data_path = '/home/ubuntu/data/results/'
        npz_cores  = 2
        collate_cores = 4 # This can be raised once the model loops finish.
        first_day = 0
        last_day = 366
    new_lats = np.load(input_data_path + 'new_lats.npz')['lat']
    new_lons = np.load(input_data_path + 'new_lons.npz')['lon']
    new_x = np.load(input_data_path + 'new_x.npz')['x']
    new_y = np.load(input_data_path + 'new_y.npz')['y']

    tif_list, year_breaks, real_start_year, real_end_year = create_tif_file_list(first_year, last_year)
    #print(tif_list[0:10], tif_list[-10:])
    with open('listfile.txt', 'w') as filehandle:
        filehandle.writelines("%s\n" % t for t in tif_list)
    years = list(range(int(real_start_year), int(real_end_year) + 1))
    year_list = [str(x) for x in years]
    #print(year_list)
    years_done = []
    pool = 'null'
    leapyears = leapyearlist()
    output_params = ['soil_water','PET','AET','Deficit','runoff','agdd','accumswe', 'rain']
    output_units = {'PET':'mm','AET':'mm','Deficit':'mm','accumswe':'mm','melt':'mm','days_snow':'mm','rain':'mm','water_input_to_soil':'mm','runoff':'mm','agdd':'C','accum_precip':'mm'}
    accum_precip = np.zeros((8075,7814)).astype(np.float32)
    agdd = np.zeros((8075,7814)).astype(np.float32)
    last_accumswe = np.zeros((8075,7814)).astype(np.float32)
    latitude = get_latitude_radians('1980','dayl') #Radians
    if (np.nanmax(latitude) > 1.5) or (np.nanmin(latitude) < 0.1) : raise Exception('Latitude is not in radians or wrong file has been used. Terminating.')
    #Global vars used here to save duplication in memory associated with passing to funcs
    elevation = np.load(input_data_path + 'etopo1_aligned_array.npy') # In meters
    multiplier_mask = np.load(input_data_path + 'multiplier_mask.npz')['arr_0']
    if (np.nanmax(elevation) > 5700) or (np.nanmin(elevation) < 0): raise Exception('Do you have the wrong elevation file? Terminating.')
    heat_load = np.load(input_data_path + 'heat_load_based_on_etopo1.npy')
    soil_whc = get_soil_whc()
    soil_water = np.copy(soil_whc) # Initialize soil values at full.
    #intercept_file = np.load('intercept1_from_senay.npz') # Vegetation intercept layer from Gabriel Senay et al. pers. comm.
    #Igrid = intercept_file['intercept']
    snow_thresh_file = np.load(input_data_path + 'jennings_t50_coefficients.npz') # Jennings, K. et al. 2018. Spatial variation of the rain-snow temperature threshold across the northern hemisphere. Nature Communications 9: 1148. DOI: 10.1038/s41467-018-03629-7
    snow_thresh_temperatures = snow_thresh_file['t50']
    low_thresh_temperatures = snow_thresh_temperatures - 3.0 # The Jennings coefficients are T50, i.e. where precip is half snow, half rain
    high_thresh_temperatures = snow_thresh_temperatures + 3.0 # This sets up a 6 degree span, which corresponds to the 1/6 = 0.167 precip fraction.

    if PET_type == 'penman_montieth':
        #slope = np.load('aligned_slope_in_radians.npy') # radians needed for numpy trig functions
        #aspect = np.load('aligned_folded_aspect_in_radians.npy')
        atmospheric_pressure = calc_atmospheric_pressure() # Used to calculate gamma.
        gamma = calc_gamma()
    current_collate_year = 'null'
    jobs = []

    #-----------------------------------------------------
    if web == False: testing_offset = 363
    else: testing_offset = 0
    file_index = 0 + testing_offset # The number of the file in tif_list
    year_index = 0 # The number of the year in year_list
    year = year_list[0]
    extent_index = 1 + testing_offset# The number that appears after "extent" in the filenames
    chunk_index = tif_list[0].split(T'_')[0] # The number appearing at the start of each tif filename
    year_break_index = 0 # The set of year breaks, corresponding to file chunks, that we are currently using
    day_index = 0 + testing_offset
    log_file = open('logfile.csv','w')
    for tif in tif_list:
        current_pr_file = tif_list[file_index]
        current_tmax_file = set_param(current_pr_file,'tasmax')
        current_tmin_file = set_param(current_pr_file,'tasmin')
        new_chunk_index = current_pr_file.split('_')[0] # The number appearing at the start of each tif filename
        if new_chunk_index != chunk_index:
            extent_index = 1
            year_break_index +=1
            chunk_index = new_chunk_index
            year_index +=1
            year = year_list[year_index]
            day_index = 0

        these_year_breaks = year_breaks[year_break_index]
        if extent_index in these_year_breaks:
            year_index +=1
            year = year_list[year_index]
            day_index = 0 + testing_offset
            extent_index = extent_index + testing_offset

        var_dict = {}
        if year in leapyears: leap_offset = 1
        else: leap_offset = 0
        print('Calculating: ',year, 'day = ', day_index)
        outline = year + ',' + str(day_index) + ',' + str(extent_index) + ',' + current_pr_file + '\n'
        log_file.write(outline)

        try:
            tmean,tmax,tmin = get_tmean(current_tmin_file, current_tmax_file)
        except:
            print('Bad temperature file')
            msg = 'bad temperature ' + current_tmin_file
            bad_list.append(msg)

        low_temperature_differences = tmean - low_thresh_temperatures

        if day_index == 273 + leap_offset: # October 1 is the 274th day of the year and 275th in leap years. Indexes start at 0 so subtract one = 273.
            try:
                precip = open_tif(current_pr_file) # MACA precip is in mm.
            except:
                print('Bad pr file')
                msg = 'bad pr ' + current_pr_file
                bad_list.append(msg)
            #precip = Igrid_adjust_precip(precip) # from v2 of model
            accum_precip = precip
            agdd = nonneg(tmean - agdd_base)
        else:
            try:
                print('pr: ',current_pr_file)
                precip = open_tif(current_pr_file)
            except:
                print('Bad pr file')
                msg = 'bad pr ' + current_pr_file
                bad_list.append(msg)

            #precip = Igrid_adjust_precip(precip) # from v2 of model
            accum_precip = accum_precip + precip
            gdd = nonneg(tmean - agdd_base)
            agdd = agdd + gdd


        if day_index == 0 + testing_offset and year == year_list[0]: accumswe = np.zeros((8075,7814)) # Set snow to zero everywhere for start of run. Note first year "Spin up" will not have accurate snow values so must consider only data beginning in start_year + 1.
        else: accumswe = est_snow()
        #Daylength is read from a daymet file. Every year of daymet dayl has the same data, so we have the year set permanently to 1980 here.
        if PET_type != 'oudin': daylength = get_param_one_day(1980, 'dayl', day_index)/3600.0 # Duration of daylight period. File is in seconds but converted to hours here.
        PET = calc_todays_PET(PET_type)
        PET_adjusted = heat_load_adjust_pet()
        snow_diff = accumswe - last_accumswe
        melt = onlyabsneg(snow_diff)
        daily_snow = nonneg(snow_diff)
        rain = nonneg(precip - daily_snow)
        w = rain + melt

        AET,runoff = calc_aet_and_runoff_and_soilw()
        deficit = nonneg(PET_adjusted - AET)
        last_accumswe = accumswe

        mp_write_daily()
        if int(year) > int(year_list[0]) :
            if day_index == 2 or day_index == 182:
                print(bad_list)
                next_collate_year = find_next_collate_year() #Will not let collation start on a new year until prev is finished.
                if year != next_collate_year:
                    pool = launch_new_collation()

        day_index += 1
        extent_index += 1
        file_index += 1

log_file.close()

if web == True: collate_cores = 7 # Once model loops are over, don't need the npz cores any more.
#Finish collating remaining npz into netCDF files
cleanup_count = 0
print('Years collated so far: ',years_done)
while (len(years_done) < len(year_list)) and (cleanup_count <= len(year_list)) :
    next_collate_year = find_next_collate_year()
    pool = launch_new_collation()

print('Done Launching Collations')
pool.join()
print('Results: ',current_collate_year,result_list)
print(bad_list)
print('Done!!')
