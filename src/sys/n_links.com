$    !01 N_LINKS.COM created on robin at 940601
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]BLDPPD_2_DEF." BLDPPD_2_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]CLI_1_DEF." CLI_1_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]CPL_2_DEF." CPL_2_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]DWARF_4_DEF." DWARF_4_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]DWC_DEF." DWC_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FBC_E_DEF." FBC_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FBC_O_DEF." FBC_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FBC_T_DEF." FBC_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FCA_E_DEF." FCA_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FCA_O_DEF." FCA_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FCA_T_DEF." FCA_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FCQ_DEF." FCQ_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FEL_E_DEF." FEL_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FEL_O_DEF." FEL_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]FEL_T_DEF." FEL_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]MCA_E_DEF." MCA_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]MCA_O_DEF." MCA_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]MCA_T_DEF." MCA_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]NSTAR_DSF." NSTAR_DSF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]PARM_6_DEF." PARM_6_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]PPDREC_4_DEF." PPDREC_4_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]PPDSTAT_2_DEF." PPDSTAT_2_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]SSH_DSF." SSH_DSF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]WNC_DEF." WNC_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]WND_DEF." WND_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]WNG_DEF." WNG_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]WNT_DEF." WNT_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]WNT_E_DEF." WNT_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]WNT_O_DEF." WNT_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]WNT_T_DEF." WNT_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]WXH_DEF." WXH_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]bmd.def" BMD_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]bmd.inc" bmd_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]cbits.def" CBITS_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]cbits.inc" cbits_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]dwe.def" DWE_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]dwe.inc" dwe_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fbc_e_inc." fbc_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fbc_o_inc." fbc_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fbc_t_inc." fbc_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fca_e_inc." fca_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fca_o_inc." fca_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fca_t_inc." fca_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fcq_inc." fcq_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdw_e.def" FDW_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdw_e.inc" fdw_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdw_o.def" FDW_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdw_o.inc" fdw_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdw_t.def" FDW_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdw_t.inc" fdw_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdx_e.def" FDX_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdx_e.inc" fdx_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdx_o.def" FDX_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdx_o.inc" fdx_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdx_t.def" FDX_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fdx_t.inc" fdx_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fel_e_inc." fel_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fel_o_inc." fel_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]fel_t_inc." fel_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flf_e.def" FLF_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flf_e.inc" flf_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flf_o.def" FLF_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flf_o.inc" flf_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flf_t.def" FLF_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flf_t.inc" flf_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flh_e.def" FLH_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flh_e.inc" flh_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flh_o.def" FLH_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flh_o.inc" flh_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flh_t.def" FLH_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]flh_t.inc" flh_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]gfh_e.def" GFH_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]gfh_e.inc" gfh_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]gfh_o.def" GFH_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]gfh_o.inc" gfh_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]gfh_t.def" GFH_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]gfh_t.inc" gfh_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ifh_e.def" IFH_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ifh_e.inc" ifh_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ifh_o.def" IFH_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ifh_o.inc" ifh_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ifh_t.def" IFH_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ifh_t.inc" ifh_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ihw_e.def" IHW_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ihw_e.inc" ihw_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ihw_o.def" IHW_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ihw_o.inc" ihw_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ihw_t.def" IHW_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ihw_t.inc" ihw_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mca_e_inc." mca_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mca_o_inc." mca_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mca_t_inc." mca_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdh_e.def" MDH_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdh_e.inc" mdh_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdh_o.def" MDH_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdh_o.inc" mdh_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdh_t.def" MDH_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdh_t.inc" mdh_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdl_e.def" MDL_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdl_e.inc" mdl_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdl_o.def" MDL_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdl_o.inc" mdl_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdl_t.def" MDL_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mdl_t.inc" mdl_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mph_e.def" MPH_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mph_e.inc" mph_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mph_o.def" MPH_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mph_o.inc" mph_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mph_t.def" MPH_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]mph_t.inc" mph_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nat.def" NAT_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nat.inc" nat_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nca.def" NCA_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nca.inc" nca_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ncl.def" NCL_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ncl.inc" ncl_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nco.def" NCO_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nco.inc" nco_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nfi.def" NFI_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nfi.inc" nfi_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nfl.def" NFL_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nfl.inc" nfl_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngc.def" NGC_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngc.inc" ngc_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngf_e.def" NGF_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngf_e.inc" ngf_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngf_o.def" NGF_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngf_o.inc" ngf_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngf_t.def" NGF_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngf_t.inc" ngf_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngi.def" NGI_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ngi.inc" ngi_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nma.def" NMA_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nma.inc" nma_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nmo.def" NMO_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nmo.inc" nmo_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]npl.def" NPL_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]npl.inc" npl_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nsc.def" NSC_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]nsc.inc" nsc_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ohw_e.def" OHW_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ohw_e.inc" ohw_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ohw_o.def" OHW_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ohw_o.inc" ohw_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ohw_t.def" OHW_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ohw_t.inc" ohw_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]qub.def" QUB_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]qub.inc" qub_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]qub_e.def" QUB_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]qub_e.inc" qub_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]qub_o.def" QUB_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]qub_o.inc" qub_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]qub_t.def" QUB_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]qub_t.inc" qub_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]rpf.def" RPF_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]rpf.inc" rpf_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sch_e.def" SCH_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sch_e.inc" sch_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sch_o.def" SCH_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sch_o.inc" sch_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sch_t.def" SCH_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sch_t.inc" sch_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]scn.def" SCN_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]scn.inc" scn_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]scw_e.def" SCW_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]scw_e.inc" scw_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]scw_o.def" SCW_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]scw_o.inc" scw_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]scw_t.def" SCW_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]scw_t.inc" scw_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sgh_e.def" SGH_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sgh_e.inc" sgh_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sgh_o.def" SGH_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sgh_o.inc" sgh_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sgh_t.def" SGH_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sgh_t.inc" sgh_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]shw_e.def" SHW_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]shw_e.inc" shw_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]shw_o.def" SHW_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]shw_o.inc" shw_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]shw_t.def" SHW_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]shw_t.inc" shw_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]smp_e.def" SMP_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]smp_e.inc" smp_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]smp_o.def" SMP_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]smp_o.inc" smp_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]smp_t.def" SMP_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]smp_t.inc" smp_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ssh_e.def" SSH_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ssh_e.inc" ssh_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ssh_o.def" SSH_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ssh_o.inc" ssh_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ssh_t.def" SSH_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]ssh_t.inc" ssh_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sth_e.def" STH_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sth_e.inc" sth_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sth_o.def" STH_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sth_o.inc" sth_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sth_t.def" STH_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]sth_t.inc" sth_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wnc_inc." wnc_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wnd_inc." wnd_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wng_inc." wng_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wnt_e_inc." wnt_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wnt_inc." wnt_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wnt_o_inc." wnt_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wnt_t_inc." wnt_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wpg_xlogo64_inc." wpg_xlogo64_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_eal.def" WQ_EAL_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_eal.inc" wq_eal_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_eap.def" WQ_EAP_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_eap.inc" wq_eap_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_epp.def" WQ_EPP_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_epp.inc" wq_epp_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_eps.def" WQ_EPS_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_eps.inc" wq_eps_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_fna.def" WQ_FNA_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_fna.inc" wq_fna_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_fnb.def" WQ_FNB_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_fnb.inc" wq_fnb_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_pal.def" WQ_PAL_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_pal.inc" wq_pal_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_pap.def" WQ_PAP_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_pap.inc" wq_pap_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_psl.def" WQ_PSL_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_psl.inc" wq_psl_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_psp.def" WQ_PSP_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_psp.inc" wq_psp_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_qmp.def" WQ_QMP_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_qmp.inc" wq_qmp_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_qms.def" WQ_QMS_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_qms.inc" wq_qms_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_reg.def" WQ_REG_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_reg.inc" wq_reg_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_xwi.def" WQ_XWI_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wq_xwi.inc" wq_xwi_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqd_e.def" WQD_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqd_e.inc" wqd_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqd_o.def" WQD_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqd_o.inc" wqd_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqd_t.def" WQD_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqd_t.inc" wqd_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqf_e.def" WQF_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqf_e.inc" wqf_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqf_o.def" WQF_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqf_o.inc" wqf_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqf_t.def" WQF_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqf_t.inc" wqf_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqg.def" WQG_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqg.inc" wqg_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqi_e.def" WQI_E_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqi_e.inc" wqi_e_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqi_o.def" WQI_O_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqi_o.inc" wqi_o_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqi_t.def" WQI_T_DEF ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wqi_t.inc" wqi_t_inc ! 940622
$  ASSIGN/NOLOG "N_ROOT:[LIB.INC]wxh_inc." wxh_inc ! 940622
