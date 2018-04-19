import math as m
import numpy as np
import matplotlib.pyplot as plt
import scipy as sc
from scipy import stats



#param:
	#student's t test
	#F test
#assumes distribution of pop
#this distrib is a funct of parameters (mean, std dev)
#estimation & testing of these params is main prob

#param Bayesian
	#model known
	#data gathering & uncertainty well understood
	
#param Classical
	#model known
	#underlying data distrib known
	#larger sample size
	#data on ordinal or interval scales
		#ordinal: ordered scale (ex: army ranks)
		#interval: uniformly calibrated scale (ex: time)
		
#student's t test (comparison of means)

#F test (comparison of variances)

#non-param:
	#chi-square test (single & 2-sample)
	#KS test (single & 2-sample)
	#runs of randomness for single sample
	#Wilcoxon-Mann-Whitney U test for 2 samples
#doesn't assume nature of pop distrib
#uses order/ranks of data instead of mean & variance
#insensitive to outliers

#non-param Bayesian: N/A (Bayesian methods involve a *known* distrib)

#non-param Classical
	#unknown model
	#unknown underlying distribs or errors
	#small sample size (as low as n=3)
	#data on nominal or categorical scales
		#nominal/categorical: binned scales (ex: gender, races)
		
#chi-square test (goodness of fit of data to model)

#KS test (single) (compare 1 sample w/ model in non-binned data)

#KS test (2-sample) (compare 2 samples in non-binned data)

#runs test of randomness (checks randomness of sample)

#Wilcoxon-Mann-Whitney U test

#if parametric assumptions valid, parametric method is more powerful
	
#null hypothesis:

#lurking variable: warm stars?
#compare to R mixture models?
#compare to results for HRFU14 sample (too small?)

#hrfu sample includes lots TO stars
def DC_sort(label,CEMP_no,CEMP_s):

	print"Carollo sorting for {}:\n".format(label)

	def classify(CEMP_list):
	
		IH=0.
		OH=0.
		TZ=0.
		
		for i in range(0,len(CEMP_list)):
			if((CEMP_list[i]['STAECKEL_R_APO']>15.) and (-9.999<CEMP_list[i]['FEH']<=-1.3) and (CEMP_list[i]['STAECKEL_BOUND']==True)):
				if(CEMP_list[i]['STAECKEL_ENERGY']<(-1.1*10**5)):
					IH+=1.
				elif(CEMP_list[i]['STAECKEL_ENERGY']>(-0.8*10**5)):
					OH+=1.
				else:
					TZ+=1.
					
		return IH, OH, TZ
		
	no_IH, no_OH, no_TZ = classify(CEMP_no)
	s_IH, s_OH, s_TZ = classify(CEMP_s)
	
	print "CEMP-no: {} IH, {} OH, {} TZ".format(no_IH, no_OH, no_TZ)
	print "CEMP-s: {} IH, {} OH, {} TZ\n".format(s_IH, s_OH, s_TZ)
	
	IH_tot = no_IH + s_IH
	no_IH=100.*no_IH/IH_tot
	s_IH=100.*s_IH/IH_tot
	
	OH_tot = no_OH + s_OH
	no_OH=100.*no_OH/OH_tot
	s_OH=100.*s_OH/OH_tot
	
	print "IH percentages: {} perc CEMP-no, {} perc CEMP-s".format(no_IH, s_IH)
	print "OH percentages: {} perc CEMP-no, {} perc CEMP-s\n".format(no_OH, s_OH)
	
#chi-square 2-sample test?
def chi_sq(label,CEMP_no,CEMP_s,param,min,max,interval):
	#H_0: these 2 samples are drawn from same parent distrib
	#bin each sample in r bins (k * r contingency table)
	
	#have k bins
	#num pts in each bin: O[i]
	#expected num pts in each bin, according to model: E[i]

	#col 0: CEMP-no
	#col 1: CEMP-s
	contingency_table = [[0 for x in range(max-min)] for y in range(2)] 
	
	for i in range(min,max,interval):
		bound1=i
		bound2=i+interval
		for j1 in range(0,len(CEMP_no)):
			if(bound1<CEMP_no[j1][param]<=bound2):
				contingency_table[0][i-min]+=1.
		for j2 in range(0,len(CEMP_s)):
			if(bound1<CEMP_s[j2][param]<=bound2):
				contingency_table[1][i-min]+=1.
	
	chi2, p, dof, expected = sc.stats.chi2_contingency(contingency_table)
	
	print "chi-squared test for label={}, param={}, min={}, max={}, interval={}".format(label,param,min,max,interval)
	print'chi2 = {}, p = {}, dof = {}, expected = {}\n'.format(chi2, p, dof, expected)
	
	
def WMW_U_test(label,CEMP_no,CEMP_s,param):

	stat, pval = sc.stats.mannwhitneyu(CEMP_no[param],CEMP_s[param])
	print "WMW-U test (scipy), label = {}, param = {}:\n".format(label,param)
	print "stat, pval = {}, {}\n".format(stat, pval)
	
	stat, pval = sc.stats.mannwhitneyu(CEMP_no[param],CEMP_s[param],alternative='two-sided')
	print "2-sided stat, pval = {}, {}\n".format(stat, pval)
	
	stat, pval = sc.stats.mannwhitneyu(CEMP_no[param],CEMP_s[param],alternative='less')
	print "less stat, pval = {}, {}\n".format(stat, pval)
	
	stat, pval = sc.stats.mannwhitneyu(CEMP_no[param],CEMP_s[param],alternative='greater')
	print "greater stat, pval = {}, {}\n".format(stat, pval)
	
	#create combined sample, preserving identity:
	tot_length = len(CEMP_no[param]) + len(CEMP_s[param])
	#create empty array of correct size
	combined_sample = [[None for i in range(2)] for j in range(tot_length)]
	
	#add CEMP-no to combined sample
	for i in range(0,len(CEMP_no[param])):
		combined_sample[i][0]=CEMP_no[i][param]
		combined_sample[i][1]="no"
	#add CEMP-s to combined sample
	for i in range(len(CEMP_no[param]),tot_length):
		combined_sample[i][0]=CEMP_s[(i-len(CEMP_no[param]))][param]
		combined_sample[i][1]="s"
	
	#rank combined sample	
	combined_sample=sorted(combined_sample,key=lambda x: x[0])
	
	#calculate U_no & U_s
	U_no=0.
	U_s=0.
	for i in range(0,tot_length):
		if(combined_sample[i][1]=="no"):
			U_no+=i
		if(combined_sample[i][1]=="s"):
			U_s+=i
	
	print "-no length = {}, s-length = {}".format(len(CEMP_no[param]),len(CEMP_s[param]))
	N=float(len(CEMP_no[param])+len(CEMP_s[param]))
	print "N(N+1)/2 = {}".format(N*(N+1.)/2.)
	print "U_no = {}, U_s = {}".format(U_no,U_s)
	print "U_no + U_s = {}\n".format(U_no+U_s)

def KS_test(label,CEMP_no,CEMP_s,param):
	print "KS test (scipy) for {}, parameter {}:\n".format(label,param)
	#H_0: these 2 samples are drawn from same parent distrib
	#H_1: they differ (2-tailed) in a specific direction (1-tailed)
	
	#cumulative freq for sample 1
	
	#cumulative freq for sample 2
	
	#D=max|Sm(x)-Sn(y)|
	
	#can we reject H_0?
	#at what level of significance?
	def EDF_pt(data, x):

		n=len(data)
	
		if(x<data[0]):
			return 0.
		elif(data[n-1]<=x):
			return 1.
		else:
			for r in range(0,n):
				if(data[r]<=x<data[r+1]):
					return 1.*r/n

	def EDF_all(data):

		EDF_x=[]
		EDF_y=[]
	
		for x in data:
			EDF_x.append(x)
			EDF_y.append(EDF_pt(data,x))
		
		return EDF_x,EDF_y

	no_param=np.sort(CEMP_no[param])
	s_param=np.sort(CEMP_s[param])	
	
	no_x, no_y = EDF_all(no_param)
	s_x, s_y = EDF_all(s_param)

	#CHECK LATER
	stat, pval = sc.stats.ks_2samp(no_x,s_x)
	
	print("stat, p_val = {}, {}\n".format(stat,pval))

	plt.plot(no_x, no_y, color="blue", alpha=0.7, label="CEMP-no ({})".format(len(no_param)))
	plt.plot(s_x, s_y, color="red", alpha=0.7, label="CEMP-s ({})".format(len(s_param)))

	plt.xlabel('{} (kpc)'.format(param))
	plt.ylabel('Cumulative Fraction')
	plt.legend(loc="lower right")
	plt.title("{}, pval = {}".format(label,round(pval,5)))

	a = plt.axes([.475, .325, .4, .4])
	plt.plot(no_x, no_y, color="blue", alpha=0.7,)
	plt.plot(s_x, s_y, color="red", alpha=0.7,)
	plt.title("(zoomed)")
	plt.xlim(0,40)

	plt.savefig("{}_{}_scipyCDF.pdf".format(label,param))
	plt.savefig("{}_{}_scipyCDF.png".format(label,param), dpi=300)
	plt.close()
	
def KS_2(label,CEMP_no,CEMP_s,param,min,max,interval):

	print"KS test (self) for {}, parameter {}:\n".format(label,param)

	def EDF(data,min,max,interval):
		#arrange data in increasing order
		data.sort()
		
		#construct EDF
		Fx=[]
		x_range=np.arange(min,max,interval)
		n=len(data)
		for x in x_range:
			if(x < data[0]):
				Fx.append(0.)
			elif(x >= data[n-1]):
				Fx.append(1.)
			else:				
				for r in range(0,n):
					if(data[r] <= x < data[r+1]):
						Fx.append(float(r)/float(n))
						
		return Fx, x_range
		
	CEMP_no_F, CEMP_no_x = EDF(CEMP_no[param],min,max,interval)
	CEMP_s_F, CEMP_s_x = EDF(CEMP_s[param],min,max,interval)
	
	d= np.max(np.absolute(np.array(CEMP_no_F) - np.array(CEMP_s_F)))
	
	stat = m.sqrt(len(CEMP_no_F)*len(CEMP_s_F)/(len(CEMP_no_F) + len(CEMP_s_F)))*d

	num=float(len(CEMP_no_F))
	factor = m.sqrt((num+num)/(num*num))

	#print m.sqrt(len(CEMP_no_F)*len(CEMP_s_F)/(len(CEMP_no_F) + len(CEMP_s_F)))
	print "test: d = {}, stat = {}\n".format(d,stat)
	
def AEGIS_stars():

	#for input, choose latest run of kinematics file
	a1 = np.genfromtxt('AEG_condor_all_data.csv', autostrip=True, delimiter=",", names=True, case_sensitive="upper", dtype=None)

	#indices of a1 rows w/ problematic kinematics removed
	#kin_case = np.where((np.isnan(a1["KIN_IN_S"])==False) & (a1["KIN_IN_S"]==1))[0]
	kin_case = np.where((a1['PMRA']!=-9.999) & (a1['PMDEC']!=-9.999) & (a1['DIST']>0.) & (a1['RV']!=-9.999) & (a1['KIN_IN_S']==1))[0]

	#FIX LATER
	orb_case = np.where((a1["STAECKEL_BOUND"]==True) & (np.isnan(a1["STAECKEL_ECC"])==False))[0]

	#indices of a1 rows with problematic metallicities removed
	#ADD LATER: (a1["SN_AVG"]>=10.)
	feh_case = np.where((np.isnan(a1["FEH"])==False) & (a1["FEH"]!=-9.999))[0]

	#indices of a1 rows with problematic carbon abundances removed
	#ADD LATER: (a1["SN_AVG"]>=10.) & ((a1["CAR_DET"]=="D") & (a1["CC_CAR"]>=0.7) 
	carb_case = np.where((np.isnan(a1["CFE"])==False) & (a1["CFE"]!=-9.999))[0]

	#indices of feh_case that satisfy this inequality
	cemp_temp1=np.where(a1["FEH"][feh_case] < -1.)[0]
	#indices of a1 that satisfy the conditions of feh_case & cemp_temp1
	cemp_case1=feh_case[cemp_temp1]

	#indices of carb_case that satisfy this inequality
	cemp_temp2=np.where(a1["CFE"][carb_case] > 0.7)[0]
	#indices of a1 that satisfy the conditions of carb_case & cemp_temp
	cemp_case2=carb_case[cemp_temp2]

	#(a1 indices of) cemp stars
	cemp_case=np.intersect1d(cemp_case1,cemp_case2)

	giant_case1 = np.where((a1["CAR_DET"]=="D") & (a1["CC_CAR"]>=0.7) & (a1["FEH_C"]!=-9.999) & (np.isnan(a1["FEH_C"])==False) & (a1["SN_AVG"]>=10.))[0]
	giant_case2 = np.where((a1["LUM_CLASS"]=="G") | (a1["LUM_CLASS"]=="SG/G"))[0]

	#(a1 indices of) giant stars
	giant_case = np.intersect1d(giant_case1,giant_case2)

	dto_case1 = np.where((a1["CAR_DET"]=="D") & (a1["CC_CAR"]>=0.7) & (a1["FEH_C"]!=-9.999) & (np.isnan(a1["FEH_C"])==False) & (a1["SN_AVG"]>=10.))[0]
	dto_case2 = np.where((a1["LUM_CLASS"]=="D") | (a1["LUM_CLASS"]=="TO"))[0]

	#(a1 indices of) dwarf & turnoff stars
	dto_case = np.intersect1d(dto_case1,dto_case2)

	#-->"good" values for giants (gives a1 indices)
	giant_case=reduce(np.intersect1d,[kin_case,orb_case,giant_case])

	#-->"good" value for CEMP giants (gives a1 indices)
	giant_cemp_case=reduce(np.intersect1d,[feh_case,kin_case,orb_case,carb_case,cemp_case,giant_case])

	#gives indices of giant_cemp_case (gives giant_cemp_case indices)
	no_temp=np.where(a1["AC"][giant_cemp_case] <= 7.1)[0]

	#-->"good" values for CEMP-no stars (gives a1 indices)
	no_giant_case=giant_cemp_case[no_temp]

	#gives indices of giant_cemp_case (gives giant_cemp_case indices)
	s_temp=np.where(a1["AC"][giant_cemp_case] > 7.1)[0]

	#-->"good" values for CEMP-s stars (gives a1 indices)
	s_giant_case=giant_cemp_case[s_temp]

	#(a1 indices) of cemp stars w/ good kin
	valid_cemp_case=reduce(np.intersect1d,[kin_case,orb_case,cemp_case])

	#gives indices of valid_cemp_case
	no_temp=np.where(a1["AC"][valid_cemp_case] <= 7.1)[0]
	#gives a1 indices
	no_all=valid_cemp_case[no_temp]

	#gives indices of valid_cemp_case
	s_temp=np.where(a1["AC"][valid_cemp_case] > 7.1)[0]
	#gives a1 indices
	s_all=valid_cemp_case[s_temp]

	print'{} AEGIS CEMP stars'.format(len(valid_cemp_case))
	print'   {} AEGIS CEMP-no, {} CEMP-s\n'.format(len(no_all),len(s_all))

	print("{} AEGIS giants, {} CEMP giants".format(len(giant_case),len(giant_cemp_case)))
	print("   {} AEGIS CEMP-no giants, {} CEMP-s giants\n".format(len(no_giant_case),len(s_giant_case)))
	
	DC_sort('aeg_all',a1[no_all],a1[s_all])
	KS_2('aeg_all',a1[no_all],a1[s_all],'R_GAL',0,15,0.1)
	KS_test('aeg_all',a1[no_all],a1[s_all],'R_GAL')
	WMW_U_test('aeg_all',a1[no_all],a1[s_all],'R_GAL')
	KS_2('aeg_all',a1[no_all],a1[s_all],'STAECKEL_R_APO',0,15,0.1)
	KS_test('aeg_all',a1[no_all],a1[s_all],'STAECKEL_R_APO')
	WMW_U_test('aeg_all',a1[no_all],a1[s_all],'STAECKEL_R_APO')
	KS_2('aeg_all',a1[no_all],a1[s_all],'STAECKEL_Z_MAX',0,15,0.1)
	KS_test('aeg_all',a1[no_all],a1[s_all],'STAECKEL_Z_MAX')
	WMW_U_test('aeg_all',a1[no_all],a1[s_all],'STAECKEL_Z_MAX')
	KS_test('aeg_all',a1[no_all],a1[s_all],'VTG')
	
	DC_sort('aeg_SGG',a1[no_giant_case],a1[s_giant_case])
	KS_2('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'R_GAL',0,15,0.1)
	KS_test('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'R_GAL')
	WMW_U_test('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'R_GAL')
	KS_2('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'STAECKEL_R_APO',0,15,0.1)
	KS_test('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'STAECKEL_R_APO')
	WMW_U_test('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'STAECKEL_R_APO')
	KS_2('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'STAECKEL_Z_MAX',0,15,0.1)
	KS_test('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'STAECKEL_Z_MAX')
	WMW_U_test('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'STAECKEL_Z_MAX')
	KS_test('aeg_SGG',a1[no_giant_case],a1[s_giant_case],'VTG')

def hrfu_stars():
	a2=np.genfromtxt('hrfu_proj_kin/hrfu_proj_all_data.csv', autostrip=True, delimiter=",", names=True, case_sensitive="upper", dtype=None)

	#indices of a2 rows w/ problematic kinematics removed
	kin_case = np.where((a2['PMRA']!=-9.999) & (a2['PMDEC']!=-9.999) & (a2['DIST']>0.) & (a2['RV']!=-9.999) & (a2['KIN_IN_S']==1))[0]

	#FIX LATER
	orb_case = np.where((a2["STAECKEL_BOUND"]==True) & (np.isnan(a2["STAECKEL_ECC"])==False))[0]

	cemp_case = np.where(a2['CEMP']=="CEMP")[0]

	cemp_no_case = np.where(a2['COMBINED_CLASS']=="CEMP-no")[0]
	cemp_s_case = np.where(a2['COMBINED_CLASS']=="CEMP-s")[0]

	cimp_no_case = np.where(a2['COMBINED_CLASS']=="CIMP-no")[0]
	cimp_s_case = np.where(a2['COMBINED_CLASS']=="CIMP-s")[0]

	giant_case = np.where((a2["TYPE"]=="G") | (a2["TYPE"]=="SG/G"))[0]

	dto_case = np.where((a2["TYPE"]=="D") | (a2["TYPE"]=="TO"))[0]

	#-->"good" value for CEMP giants (gives a2 indices)
	giant_cemp_case=reduce(np.intersect1d,[kin_case,orb_case,cemp_case,giant_case])

	#-->"good" values for CEMP-no stars (gives a2 indices)
	no_giant_case=np.intersect1d(cemp_no_case,giant_cemp_case)

	#-->"good" values for CEMP-s stars (gives a2 indices)
	s_giant_case=np.intersect1d(cemp_s_case,giant_cemp_case)

	#(a2 indices) of cemp stars w/ good kin
	valid_cemp_case=reduce(np.intersect1d,[kin_case,orb_case,cemp_case])

	#gives a2 indices
	no_all=np.intersect1d(cemp_no_case,valid_cemp_case)
	
	no_all_cimp=reduce(np.intersect1d,[kin_case,orb_case,cimp_no_case])
	no_giant_cimp=reduce(np.intersect1d,[kin_case,orb_case,cimp_no_case,giant_case])

	#gives a2 indices
	s_all=np.intersect1d(cemp_s_case,valid_cemp_case)
	
	s_all_cimp=reduce(np.intersect1d,[kin_case,orb_case,cimp_s_case])
	s_giant_cimp=reduce(np.intersect1d,[kin_case,orb_case,cimp_s_case,giant_case])

	print'{} hrfu CEMP stars'.format(len(valid_cemp_case))
	print'   {} hrfu CEMP-no, {} CEMP-s\n'.format(len(no_all),len(s_all))

	print("{} hrfu giants, {} CEMP giants".format(len(giant_case),len(giant_cemp_case)))
	print("   {} hrfu CEMP-no giants, {} CEMP-s giants\n".format(len(no_giant_case),len(s_giant_case)))
	
	DC_sort('hrfu_all',a2[no_all],a2[s_all])
	DC_sort('hrfu_all_cimp',a2[no_all_cimp],a2[s_all_cimp])
	KS_2('hrfu_all',a2[no_all],a2[s_all],'R_GAL',0,15,0.1)
	KS_test('hrfu_all',a2[no_all],a2[s_all],'R_GAL')
	WMW_U_test('hrfu_all',a2[no_all],a2[s_all],'R_GAL')
	KS_2('hrfu_all',a2[no_all],a2[s_all],'STAECKEL_R_APO',0,15,0.1)
	KS_test('hrfu_all',a2[no_all],a2[s_all],'STAECKEL_R_APO')
	WMW_U_test('hrfu_all',a2[no_all],a2[s_all],'STAECKEL_R_APO')
	KS_2('hrfu_all',a2[no_all],a2[s_all],'STAECKEL_Z_MAX',0,15,0.1)
	KS_test('hrfu_all',a2[no_all],a2[s_all],'STAECKEL_Z_MAX')
	WMW_U_test('hrfu_all',a2[no_all],a2[s_all],'STAECKEL_Z_MAX')
	KS_test('hrfu_all',a2[no_all],a2[s_all],'VTG')
	
	DC_sort('hrfu_SGG',a2[no_giant_case],a2[s_giant_case])
	DC_sort('hrfu_SGG_cimp',a2[no_giant_cimp],a2[s_giant_cimp])
	KS_2('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'R_GAL',0,15,0.1)
	KS_test('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'R_GAL')
	WMW_U_test('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'R_GAL')
	KS_2('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'STAECKEL_R_APO',0,15,0.1)
	KS_test('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'STAECKEL_R_APO')
	WMW_U_test('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'STAECKEL_R_APO')
	KS_2('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'STAECKEL_Z_MAX',0,15,0.1)
	KS_test('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'STAECKEL_Z_MAX')
	WMW_U_test('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'STAECKEL_Z_MAX')
	KS_test('hrfu_SGG',a2[no_giant_case],a2[s_giant_case],'VTG')

print "-------AEGIS STARS -------\n"
AEGIS_stars()
print "-------HRFU STARS -------\n"
hrfu_stars()