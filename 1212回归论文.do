cls
graph set window fontface "Times New Roman"
import delimited "df_patent_information.csv", case(preserve) clear
encode appln_auth, generate(appln_auth_id)

gen complementarity_sum_w=complementarity_sum/(len_No_AI*len_AI)
gen substitution_sum_w=substitution_sum/(len_No_AI*len_AI)
gen complementarity_mean_w=complementarity_sum/len_AI_4
gen substitution_mean_w=substitution_sum/len_AI_4
norm complementarity_sum substitution_sum, method(mmx)
gen ln_citation_count=log(1+citation_count)
gen ln_citation_count_3=log(1+citation_count_3)
gen ln_sum_claims=log(1+sum_claims)
gen ln_avg_claims=log(1+avg_claims)
gen ln_docdb_family_size=log(1+docdb_family_size)
gen ln_cpc_class_symbol_AI_cum=log(1+cpc_class_symbol_AI_cum)
gen ln_patent_age=log(1+patent_age)

gen ln_nb_inventors =log(1+nb_inventors)
gen ln_nb_applicants =log(1+nb_applicants)
gen ln_nb_citing_docdb_fam=log(1+nb_citing_docdb_fam)
egen std_cpc_class_symbol_AI_cum=std(cpc_class_symbol_AI_cum)
gen ln_cpc_class_symbol_len=log(1+cpc_class_symbol_len)

gen ln_title_abs_string_len=log(1+title_abstract_len)
gen ln_title_abs_lexicon_count=log(1+title_abstract_lexicon_count)

global control_fe "ln_nb_inventors ln_nb_applicants ln_nb_citing_docdb_fam cpc_class_symbol_entropy ln_title_abs_string_len title_abstract_smog_index"

// keep if lnage>0
// keep if ln_Employee>0
// keep if ln_Fixed_Assets>0
keep if year >= 2010 & year <=2022
// drop if missing(lnlag_tobin_A)
// keep if year < 2021
// keep if paper_count > 0
// keep if patent_count > 0

// 描述性统计
logout,save(描述性统计结果2)word replace:tabstat citation_normal ln_docdb_family_size mmx_complementarity_sum mmx_substitution_sum ln_cpc_class_symbol_AI_cum $control_fe,s(N mean sd min max) f(%12.3f) c(s)

wmtcorr citation_normal ln_docdb_family_size mmx_complementarity_sum mmx_substitution_sum ln_cpc_class_symbol_AI_cum $control_fe using "Myfile.rtf", replace

*============================ 五年被引 =================================

**************************共线性************************
collin complementarity_sum substitution_sum ln_cpc_class_symbol_AI_cum $control_fe

**************************************回归分析1**************************************
// 主回归 
reghdfe citation_normal mmx_complementarity_sum $control_fe, absorb(id_org year) keepsingletons 
est store H1_model1
reghdfe citation_normal mmx_substitution_sum $control_fe, absorb(id_org year) keepsingletons 
est store H1_model2
reghdfe ln_docdb_family_size  mmx_complementarity_sum $control_fe, absorb(id_org year) keepsingletons 
est store H2_model1
reghdfe ln_docdb_family_size  mmx_substitution_sum $control_fe, absorb(id_org year) keepsingletons 
est store H2_model2

reghdfe citation_normal mmx_complementarity_sum $control_fe, absorb(id_org year appln_auth_id) keepsingletons 
est store H1_model3
reghdfe citation_normal mmx_substitution_sum $control_fe, absorb(id_org year appln_auth_id) keepsingletons 
est store H1_model4
reghdfe ln_docdb_family_size  mmx_complementarity_sum $control_fe, absorb(id_org year appln_auth_id) keepsingletons 
est store H2_model3
reghdfe ln_docdb_family_size  mmx_substitution_sum $control_fe, absorb(id_org year appln_auth_id) keepsingletons 
est store H2_model4

reghdfe citation_normal mmx_complementarity_sum $control_fe, keepsingletons 
est store H1_model5
reghdfe citation_normal mmx_substitution_sum $control_fe, keepsingletons 
est store H1_model6
reghdfe ln_docdb_family_size  mmx_complementarity_sum $control_fe, keepsingletons 
est store H2_model5
reghdfe ln_docdb_family_size  mmx_substitution_sum $control_fe, keepsingletons 
est store H2_model6
esttab H1_model1 H1_model2 H2_model1 H2_model2 H1_model3 H1_model4 H2_model3 H2_model4 H1_model5 H1_model6  H2_model5 H2_model6 using "主回归结果.rtf",se r2 b(%7.4f) replace starlevels (+ 0.1 * 0.05 ** 0.01 *** 0.001) order(mmx_complementarity_sum mmx_substitution_sum $control_fe) 

// //////////////////  *=======调节==========*  ////////
** 调节效应 - 技术影响 **
reghdfe citation_normal c.mmx_complementarity_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons 
est store H3_model1
reghdfe citation_normal c.mmx_complementarity_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons 
est store H3_model2
reghdfe citation_normal c.mmx_substitution_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons 
est store H4_model1
reghdfe citation_normal c.mmx_substitution_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons 
est store H4_model2

* 画图*
* ln_cpc_class_symbol_AI_cum
su ln_cpc_class_symbol_AI_cum if e(sample)
local mean_ln_cpc_class_symbol_AI_cum = r(mean)
local sd_ln_cpc_class_symbol_AI_cum = r(sd)

local low_ln_cpc_class_symbol_AI_cum = `mean_ln_cpc_class_symbol_AI_cum' - `sd_ln_cpc_class_symbol_AI_cum'
local high_ln_cpc_class_symbol_AI_cum = `mean_ln_cpc_class_symbol_AI_cum' + `sd_ln_cpc_class_symbol_AI_cum'

* mmx_complementarity_sum
su mmx_complementarity_sum if e(sample)
local low_mmx_complementarity_sum = r(mean) - r(sd)
local high_mmx_complementarity_sum = r(mean) + r(sd)

* 
est restore H3_model2
* 
margins, at(mmx_complementarity_sum=(`low_mmx_complementarity_sum' `high_mmx_complementarity_sum') ln_cpc_class_symbol_AI_cum=(`low_ln_cpc_class_symbol_AI_cum' `high_ln_cpc_class_symbol_AI_cum'))
marginsplot, xlabel(-0.005 "Low Complementarity" 0.015 "High Complementarity") ///
            xtitle("Complementarity") ///
            ytitle("Predicted Answer Score") ///
            ylabel(, angle(horizontal) nogrid) ///
            legend(position(3) col(1) stack) ////
            title("Moderating Effect of Question Diversity to Answer Score") noci
graph export "技术影响 moderate mmx_complementarity_sum.jpg", replace


* mmx_substitution_sum
su mmx_substitution_sum if e(sample)
local low_mmx_substitution_sum = r(mean) - r(sd)
local high_mmx_substitution_sum = r(mean) + r(sd)
* 
est restore H4_model2


* 
margins, at(mmx_substitution_sum=(`low_mmx_substitution_sum' `high_mmx_substitution_sum') ln_cpc_class_symbol_AI_cum=(`low_ln_cpc_class_symbol_AI_cum' `high_ln_cpc_class_symbol_AI_cum'))
marginsplot, xlabel(-0.005 "Low Substitution" 0.012 "High Substitution") ///
            xtitle("Substitution") ///
            ytitle("Predicted Answer Score") ///
            ylabel(, angle(horizontal) nogrid) ///
            legend(position(3) col(1) stack) ////
            title("Moderating Effect of Question Diversity to Substitution") noci
graph export "技术影响 moderate mmx_substitution_sum.jpg", replace

** 调节效应 - 市场价值 **
reghdfe ln_docdb_family_size c.mmx_complementarity_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons 
est store H5_model1 
reghdfe ln_docdb_family_size c.mmx_complementarity_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons 
est store H5_model2
reghdfe ln_docdb_family_size c.mmx_substitution_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons 
est store H6_model1 
reghdfe ln_docdb_family_size c.mmx_substitution_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons 
est store H6_model2
esttab H3_model1 H3_model2 H4_model1 H4_model2 H5_model1 H5_model2 H6_model1 H6_model2 using "调节效应结果.rtf",se r2 replace b(%7.4f) starlevels (+ 0.1 * 0.05 ** 0.01 *** 0.001) order(ln_cpc_class_symbol_AI_cum mmx_complementarity_sum mmx_substitution_sum $control_fe) 

* 画图*
* ln_cpc_class_symbol_AI_cum
su ln_cpc_class_symbol_AI_cum if e(sample)
local mean_ln_cpc_class_symbol_AI_cum = r(mean)
local sd_ln_cpc_class_symbol_AI_cum = r(sd)

local low_ln_cpc_class_symbol_AI_cum = `mean_ln_cpc_class_symbol_AI_cum' - `sd_ln_cpc_class_symbol_AI_cum'
local high_ln_cpc_class_symbol_AI_cum = `mean_ln_cpc_class_symbol_AI_cum' + `sd_ln_cpc_class_symbol_AI_cum'

* mmx_complementarity_sum
su mmx_complementarity_sum if e(sample)
local low_mmx_complementarity_sum = r(mean) - r(sd)
local high_mmx_complementarity_sum = r(mean) + r(sd)

* 
est restore H5_model2
* 
margins, at(mmx_complementarity_sum=(`low_mmx_complementarity_sum' `high_mmx_complementarity_sum') ln_cpc_class_symbol_AI_cum=(`low_ln_cpc_class_symbol_AI_cum' `high_ln_cpc_class_symbol_AI_cum'))
marginsplot, xlabel(-0.005 "Low Complementarity" 0.015 "High Complementarity") ///
            xtitle("Complementarity") ///
            ytitle("Predicted Answer Score") ///
            ylabel(, angle(horizontal) nogrid) ///
            legend(position(3) col(1) stack) ////
            title("Moderating Effect of Question Diversity to Answer Score") noci
graph export "市场价值 moderate mmx_complementarity_sum.jpg", replace


* mmx_substitution_sum
su mmx_substitution_sum if e(sample)
local low_mmx_substitution_sum = r(mean) - r(sd)
local high_mmx_substitution_sum = r(mean) + r(sd)
* 
est restore H6_model2
* 
margins, at(mmx_substitution_sum=(`low_mmx_substitution_sum' `high_mmx_substitution_sum') ln_cpc_class_symbol_AI_cum=(`low_ln_cpc_class_symbol_AI_cum' `high_ln_cpc_class_symbol_AI_cum'))
marginsplot, xlabel(-0.005 "Low Substitution" 0.012 "High Substitution") ///
            xtitle("Substitution") ///
            ytitle("Predicted Answer Score") ///
            ylabel(, angle(horizontal) nogrid) ///
            legend(position(3) col(1) stack) ////
            title("Moderating Effect of Question Diversity to Substitution") noci
graph export "市场价值 moderate mmx_substitution_sum.jpg", replace

*********************稳健性1 替换变量*********************
// 主效应
reghdfe ln_citation_count_3  mmx_complementarity_sum $control_fe, absorb(id_org year) keepsingletons, if year<2020
est store H1_t1
reghdfe ln_citation_count_3  mmx_substitution_sum $control_fe, absorb(id_org year) keepsingletons, if year<2020
est store H1_t2
reghdfe ln_avg_claims  mmx_complementarity_sum $control_fe, absorb(id_org year) keepsingletons, if year<2020
est store H2_t1
reghdfe ln_avg_claims  mmx_substitution_sum $control_fe, absorb(id_org year) keepsingletons, if year<2020
est store H2_t2

// 调节效应
reghdfe ln_citation_count_3 c.mmx_complementarity_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if year<2020
est store H3_t1
reghdfe ln_citation_count_3 c.mmx_complementarity_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if year<2020
est store H3_t2
reghdfe ln_citation_count_3 c.mmx_substitution_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if year<2020
est store H4_t1
reghdfe ln_citation_count_3 c.mmx_substitution_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if year<2020
est store H4_t2

reghdfe ln_avg_claims c.mmx_complementarity_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons, if year<2020
est store H5_t1 
reghdfe ln_avg_claims c.mmx_complementarity_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if year<2020
est store H5_t2
reghdfe ln_avg_claims c.mmx_substitution_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if year<2020
est store H6_t1 
reghdfe ln_avg_claims c.mmx_substitution_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if year<2020
est store H6_t2
esttab H1_t1 H1_t2 H2_t1 H2_t2 H3_t1 H3_t2 H4_t1 H4_t2 H5_t1 H5_t2 H6_t1 H6_t2 using "稳健性替换因变量.rtf",se r2 b(%7.4f) replace starlevels (+ 0.1 * 0.05 ** 0.01 *** 0.001) order( mmx_complementarity_sum mmx_substitution_sum ln_cpc_class_symbol_AI_cum $control_fe) 

*********************稳健性1 替换A type************** 
// 主效应
reghdfe citation_normal  mmx_complementarity_sum $control_fe, absorb(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H1_t5
reghdfe citation_normal  mmx_substitution_sum $control_fe, absorb(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H1_t6
reghdfe ln_docdb_family_size  mmx_complementarity_sum $control_fe, absorb(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H2_t5
reghdfe ln_docdb_family_size  mmx_substitution_sum $control_fe, absorb(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H2_t6

// 调节效应
reghdfe citation_normal c.mmx_complementarity_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H3_t5
reghdfe citation_normal c.mmx_complementarity_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H3_t6
reghdfe citation_normal c.mmx_substitution_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H4_t5
reghdfe citation_normal c.mmx_substitution_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H4_t6

reghdfe ln_docdb_family_size c.mmx_complementarity_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year)  keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H5_t5
reghdfe ln_docdb_family_size c.mmx_complementarity_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H5_t6
reghdfe ln_docdb_family_size c.mmx_substitution_sum c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H6_t5
reghdfe ln_docdb_family_size c.mmx_substitution_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, a(id_org year) keepsingletons, if appln_kind=="A " & appln_auth=="CN"
est store H6_t6
esttab H1_t5 H1_t6 H2_t5 H2_t6 H3_t5 H3_t6 H4_t5 H4_t6 H5_t5 H5_t6 H6_t5 H6_t6 using "稳健性替换type A.rtf",se r2 b(%7.4f) replace starlevels (+ 0.1 * 0.05 ** 0.01 *** 0.001) order( mmx_complementarity_sum mmx_substitution_sum ln_cpc_class_symbol_AI_cum $control_fe) 


*******************替换模型************
nbreg citation_count mmx_complementarity_sum $control_fe, nolog, if year<2020
est store H1_t3
nbreg citation_count  mmx_substitution_sum $control_fe, nolog, if year<2020
est store H1_t4
nbreg docdb_family_size  mmx_complementarity_sum $control_fe, nolog, if year<2020
est store H2_t3
nbreg docdb_family_size  mmx_substitution_sum $control_fe, nolog, if year<2020
est store H2_t4

// 调节效应
nbreg citation_count c.mmx_complementarity_sum c.ln_cpc_class_symbol_AI_cum $control_fe, nolog, if year<2020
est store H3_t3
nbreg citation_count c.mmx_complementarity_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, nolog, if year<2020
est store H3_t4
nbreg citation_count c.mmx_substitution_sum c.ln_cpc_class_symbol_AI_cum $control_fe, nolog, if year<2020
est store H4_t3
nbreg citation_count c.mmx_substitution_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, nolog, if year<2020
est store H4_t4

nbreg docdb_family_size c.mmx_complementarity_sum c.ln_cpc_class_symbol_AI_cum $control_fe, nolog, if year<2020
est store H5_t3 
nbreg docdb_family_size c.mmx_complementarity_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, nolog, if year<2020
est store H5_t4
nbreg docdb_family_size c.mmx_substitution_sum c.ln_cpc_class_symbol_AI_cum $control_fe, nolog, if year<2020
est store H6_t3 
nbreg docdb_family_size c.mmx_substitution_sum##c.ln_cpc_class_symbol_AI_cum $control_fe, nolog, if year<2020
est store H6_t4
esttab H1_t3 H1_t4 H2_t3 H2_t4 H3_t3 H3_t4 H4_t3 H4_t4 H5_t3 H5_t4 H6_t3 H6_t4 using "稳健性替换模型.rtf",se b(%7.4f) replace starlevels (+ 0.1 * 0.05 ** 0.01 *** 0.001) order(mmx_complementarity_sum mmx_substitution_sum ln_cpc_class_symbol_AI_cum $control_fe) 

