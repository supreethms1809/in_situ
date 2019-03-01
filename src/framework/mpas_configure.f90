












! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!-----------------------------------------------------------------------
!  mpas_configure
!
!> \brief MPAS Configuration routines.
!> \author Michael Duda, Doug Jacobsen
!> \date   03/26/13
!> \details
!>  This module will contain all namelist parameter definitions, as well as the routine which reads them from the namelist file.
!
!-----------------------------------------------------------------------
module mpas_configure

   use mpas_dmpar
   use mpas_io_units

   logical :: config_do_restart
   character (len=StrKIND) :: config_start_time
   character (len=StrKIND) :: config_stop_time
   character (len=StrKIND) :: config_run_duration
   character (len=StrKIND) :: config_calendar_type
   character (len=StrKIND) :: config_input_name
   character (len=StrKIND) :: config_output_name
   character (len=StrKIND) :: config_restart_name
   character (len=StrKIND) :: config_restart_timestamp_name
   character (len=StrKIND) :: config_restart_interval
   character (len=StrKIND) :: config_output_interval
   character (len=StrKIND) :: config_stats_interval
   logical :: config_write_stats_on_startup
   logical :: config_write_output_on_startup
   integer :: config_frames_per_outfile
   integer :: config_pio_num_iotasks
   integer :: config_pio_stride
   real (KIND=RKIND) :: config_dt
   character (len=StrKIND) :: config_time_integrator
   character (len=StrKIND) :: config_vert_coord_movement
   logical :: config_use_min_max_thickness
   real (KIND=RKIND) :: config_min_thickness
   real (KIND=RKIND) :: config_max_thickness_factor
   logical :: config_set_restingThickness_to_IC
   logical :: config_dzdk_positive
   logical :: config_use_freq_filtered_thickness
   real (KIND=RKIND) :: config_thickness_filter_timescale
   logical :: config_use_highFreqThick_restore
   real (KIND=RKIND) :: config_highFreqThick_restore_time
   logical :: config_use_highFreqThick_del2
   real (KIND=RKIND) :: config_highFreqThick_del2
   logical :: config_alter_ICs_for_pbcs
   character (len=StrKIND) :: config_pbc_alteration_type
   real (KIND=RKIND) :: config_min_pbc_fraction
   logical :: config_check_ssh_consistency
   integer :: config_num_halos
   character (len=StrKIND) :: config_block_decomp_file_prefix
   integer :: config_number_of_blocks
   logical :: config_explicit_proc_decomp
   character (len=StrKIND) :: config_proc_decomp_file_prefix
   logical :: config_hmix_ScaleWithMesh
   real (KIND=RKIND) :: config_maxMeshDensity
   real (KIND=RKIND) :: config_apvm_scale_factor
   logical :: config_use_mom_del2
   logical :: config_use_tracer_del2
   real (KIND=RKIND) :: config_mom_del2
   real (KIND=RKIND) :: config_tracer_del2
   logical :: config_use_mom_del4
   logical :: config_use_tracer_del4
   real (KIND=RKIND) :: config_mom_del4
   real (KIND=RKIND) :: config_tracer_del4
   logical :: config_use_Leith_del2
   real (KIND=RKIND) :: config_Leith_parameter
   real (KIND=RKIND) :: config_Leith_dx
   real (KIND=RKIND) :: config_Leith_visc2_max
   real (KIND=RKIND) :: config_h_kappa
   real (KIND=RKIND) :: config_h_kappa_q
   logical :: config_use_mom_del2_tensor
   real (KIND=RKIND) :: config_mom_del2_tensor
   logical :: config_use_mom_del4_tensor
   real (KIND=RKIND) :: config_mom_del4_tensor
   logical :: config_Rayleigh_friction
   real (KIND=RKIND) :: config_Rayleigh_damping_coeff
   real (KIND=RKIND) :: config_convective_visc
   real (KIND=RKIND) :: config_convective_diff
   logical :: config_use_const_visc
   logical :: config_use_const_diff
   real (KIND=RKIND) :: config_vert_visc
   real (KIND=RKIND) :: config_vert_diff
   logical :: config_use_rich_visc
   logical :: config_use_rich_diff
   real (KIND=RKIND) :: config_bkrd_vert_visc
   real (KIND=RKIND) :: config_bkrd_vert_diff
   real (KIND=RKIND) :: config_rich_mix
   logical :: config_use_tanh_visc
   logical :: config_use_tanh_diff
   real (KIND=RKIND) :: config_max_visc_tanh
   real (KIND=RKIND) :: config_min_visc_tanh
   real (KIND=RKIND) :: config_max_diff_tanh
   real (KIND=RKIND) :: config_min_diff_tanh
   real (KIND=RKIND) :: config_zMid_tanh
   real (KIND=RKIND) :: config_zWidth_tanh
   logical :: config_use_cvmix
   real (KIND=RKIND) :: config_cvmix_prandtl_number
   logical :: config_use_cvmix_background
   real (KIND=RKIND) :: config_cvmix_background_diffusion
   real (KIND=RKIND) :: config_cvmix_background_viscosity
   logical :: config_use_cvmix_convection
   real (KIND=RKIND) :: config_cvmix_convective_diffusion
   real (KIND=RKIND) :: config_cvmix_convective_viscosity
   logical :: config_use_cvmix_kpp
   real (KIND=RKIND) :: config_cvmix_kpp_criticalBulkRichardsonNumber
   character (len=StrKIND) :: config_cvmix_kpp_interpolationOMLType
   character (len=StrKIND) :: config_forcing_type
   real (KIND=RKIND) :: config_restoreT_timescale
   real (KIND=RKIND) :: config_restoreS_timescale
   real (KIND=RKIND) :: config_restoreT_lengthscale
   real (KIND=RKIND) :: config_restoreS_lengthscale
   real (KIND=RKIND) :: config_flux_attenuation_coefficient
   logical :: config_frazil_ice_formation
   character (len=StrKIND) :: config_sw_absorption_type
   integer :: config_jerlov_water_type
   logical :: config_fixed_jerlov_weights
   character (len=StrKIND) :: config_vert_tracer_adv
   integer :: config_vert_tracer_adv_order
   integer :: config_horiz_tracer_adv_order
   real (KIND=RKIND) :: config_coef_3rd_order
   logical :: config_monotonic
   real (KIND=RKIND) :: config_bottom_drag_coeff
   character (len=StrKIND) :: config_pressure_gradient_type
   real (KIND=RKIND) :: config_density0
   character (len=StrKIND) :: config_eos_type
   real (KIND=RKIND) :: config_eos_linear_alpha
   real (KIND=RKIND) :: config_eos_linear_beta
   real (KIND=RKIND) :: config_eos_linear_Tref
   real (KIND=RKIND) :: config_eos_linear_Sref
   real (KIND=RKIND) :: config_eos_linear_densityref
   integer :: config_n_ts_iter
   integer :: config_n_bcl_iter_beg
   integer :: config_n_bcl_iter_mid
   integer :: config_n_bcl_iter_end
   integer :: config_n_btr_subcycles
   integer :: config_n_btr_cor_iter
   logical :: config_vel_correction
   integer :: config_btr_subcycle_loop_factor
   real (KIND=RKIND) :: config_btr_gam1_velWt1
   real (KIND=RKIND) :: config_btr_gam2_SSHWt1
   real (KIND=RKIND) :: config_btr_gam3_velWt2
   logical :: config_btr_solve_SSH2
   logical :: config_conduct_tests
   logical :: config_test_tensors
   character (len=StrKIND) :: config_tensor_test_function
   logical :: config_check_zlevel_consistency
   logical :: config_filter_btr_mode
   logical :: config_prescribe_velocity
   logical :: config_prescribe_thickness
   logical :: config_include_KE_vertex
   logical :: config_check_tracer_monotonicity
   logical :: config_disable_thick_all_tend
   logical :: config_disable_thick_hadv
   logical :: config_disable_thick_vadv
   logical :: config_disable_thick_sflux
   logical :: config_disable_vel_all_tend
   logical :: config_disable_vel_coriolis
   logical :: config_disable_vel_pgrad
   logical :: config_disable_vel_hmix
   logical :: config_disable_vel_windstress
   logical :: config_disable_vel_vmix
   logical :: config_disable_vel_vadv
   logical :: config_disable_tr_all_tend
   logical :: config_disable_tr_adv
   logical :: config_disable_tr_hmix
   logical :: config_disable_tr_vmix
   logical :: config_disable_tr_sflux

   contains

!-----------------------------------------------------------------------
!  routine mpas_read_namelist
!
!> \brief MPAS read namelist routine
!> \author Michael Duda, Doug Jacobsen
!> \date   03/26/13
!> \details
!>  This routine reads and broadcasts the namelist file. 
!
!-----------------------------------------------------------------------
   subroutine mpas_read_namelist(dminfo, nml_filename)

      implicit none

      type (dm_info), intent(in) :: dminfo !< Input: Domain information
      character (len=*), optional :: nml_filename !< Input - Optional: Namelist filename. Defaults to namelist.input

      integer :: funit, ierr

      namelist /time_management/ config_do_restart, &
                    config_start_time, &
                    config_stop_time, &
                    config_run_duration, &
                    config_calendar_type
      namelist /io/ config_input_name, &
                    config_output_name, &
                    config_restart_name, &
                    config_restart_timestamp_name, &
                    config_restart_interval, &
                    config_output_interval, &
                    config_stats_interval, &
                    config_write_stats_on_startup, &
                    config_write_output_on_startup, &
                    config_frames_per_outfile, &
                    config_pio_num_iotasks, &
                    config_pio_stride
      namelist /time_integration/ config_dt, &
                    config_time_integrator
      namelist /ALE_vertical_grid/ config_vert_coord_movement, &
                    config_use_min_max_thickness, &
                    config_min_thickness, &
                    config_max_thickness_factor, &
                    config_set_restingThickness_to_IC, &
                    config_dzdk_positive
      namelist /ALE_frequency_filtered_thickness/ config_use_freq_filtered_thickness, &
                    config_thickness_filter_timescale, &
                    config_use_highFreqThick_restore, &
                    config_highFreqThick_restore_time, &
                    config_use_highFreqThick_del2, &
                    config_highFreqThick_del2
      namelist /partial_bottom_cells/ config_alter_ICs_for_pbcs, &
                    config_pbc_alteration_type, &
                    config_min_pbc_fraction, &
                    config_check_ssh_consistency
      namelist /decomposition/ config_num_halos, &
                    config_block_decomp_file_prefix, &
                    config_number_of_blocks, &
                    config_explicit_proc_decomp, &
                    config_proc_decomp_file_prefix
      namelist /hmix/ config_hmix_ScaleWithMesh, &
                    config_maxMeshDensity, &
                    config_apvm_scale_factor
      namelist /hmix_del2/ config_use_mom_del2, &
                    config_use_tracer_del2, &
                    config_mom_del2, &
                    config_tracer_del2
      namelist /hmix_del4/ config_use_mom_del4, &
                    config_use_tracer_del4, &
                    config_mom_del4, &
                    config_tracer_del4
      namelist /hmix_Leith/ config_use_Leith_del2, &
                    config_Leith_parameter, &
                    config_Leith_dx, &
                    config_Leith_visc2_max
      namelist /standard_GM/ config_h_kappa, &
                    config_h_kappa_q
      namelist /hmix_del2_tensor/ config_use_mom_del2_tensor, &
                    config_mom_del2_tensor
      namelist /hmix_del4_tensor/ config_use_mom_del4_tensor, &
                    config_mom_del4_tensor
      namelist /Rayleigh_damping/ config_Rayleigh_friction, &
                    config_Rayleigh_damping_coeff
      namelist /vmix/ config_convective_visc, &
                    config_convective_diff
      namelist /vmix_const/ config_use_const_visc, &
                    config_use_const_diff, &
                    config_vert_visc, &
                    config_vert_diff
      namelist /vmix_rich/ config_use_rich_visc, &
                    config_use_rich_diff, &
                    config_bkrd_vert_visc, &
                    config_bkrd_vert_diff, &
                    config_rich_mix
      namelist /vmix_tanh/ config_use_tanh_visc, &
                    config_use_tanh_diff, &
                    config_max_visc_tanh, &
                    config_min_visc_tanh, &
                    config_max_diff_tanh, &
                    config_min_diff_tanh, &
                    config_zMid_tanh, &
                    config_zWidth_tanh
      namelist /cvmix/ config_use_cvmix, &
                    config_cvmix_prandtl_number, &
                    config_use_cvmix_background, &
                    config_cvmix_background_diffusion, &
                    config_cvmix_background_viscosity, &
                    config_use_cvmix_convection, &
                    config_cvmix_convective_diffusion, &
                    config_cvmix_convective_viscosity, &
                    config_use_cvmix_kpp, &
                    config_cvmix_kpp_criticalBulkRichardsonNumber, &
                    config_cvmix_kpp_interpolationOMLType
      namelist /forcing/ config_forcing_type, &
                    config_restoreT_timescale, &
                    config_restoreS_timescale, &
                    config_restoreT_lengthscale, &
                    config_restoreS_lengthscale, &
                    config_flux_attenuation_coefficient, &
                    config_frazil_ice_formation, &
                    config_sw_absorption_type, &
                    config_jerlov_water_type, &
                    config_fixed_jerlov_weights
      namelist /advection/ config_vert_tracer_adv, &
                    config_vert_tracer_adv_order, &
                    config_horiz_tracer_adv_order, &
                    config_coef_3rd_order, &
                    config_monotonic
      namelist /bottom_drag/ config_bottom_drag_coeff
      namelist /pressure_gradient/ config_pressure_gradient_type, &
                    config_density0
      namelist /eos/ config_eos_type
      namelist /eos_linear/ config_eos_linear_alpha, &
                    config_eos_linear_beta, &
                    config_eos_linear_Tref, &
                    config_eos_linear_Sref, &
                    config_eos_linear_densityref
      namelist /split_explicit_ts/ config_n_ts_iter, &
                    config_n_bcl_iter_beg, &
                    config_n_bcl_iter_mid, &
                    config_n_bcl_iter_end, &
                    config_n_btr_subcycles, &
                    config_n_btr_cor_iter, &
                    config_vel_correction, &
                    config_btr_subcycle_loop_factor, &
                    config_btr_gam1_velWt1, &
                    config_btr_gam2_SSHWt1, &
                    config_btr_gam3_velWt2, &
                    config_btr_solve_SSH2
      namelist /testing/ config_conduct_tests, &
                    config_test_tensors, &
                    config_tensor_test_function
      namelist /debug/ config_check_zlevel_consistency, &
                    config_filter_btr_mode, &
                    config_prescribe_velocity, &
                    config_prescribe_thickness, &
                    config_include_KE_vertex, &
                    config_check_tracer_monotonicity, &
                    config_disable_thick_all_tend, &
                    config_disable_thick_hadv, &
                    config_disable_thick_vadv, &
                    config_disable_thick_sflux, &
                    config_disable_vel_all_tend, &
                    config_disable_vel_coriolis, &
                    config_disable_vel_pgrad, &
                    config_disable_vel_hmix, &
                    config_disable_vel_windstress, &
                    config_disable_vel_vmix, &
                    config_disable_vel_vadv, &
                    config_disable_tr_all_tend, &
                    config_disable_tr_adv, &
                    config_disable_tr_hmix, &
                    config_disable_tr_vmix, &
                    config_disable_tr_sflux

      funit = 21

      ! Set default values for namelist options
      config_do_restart = .false.
      config_start_time = "0000-01-01_00:00:00"
      config_stop_time = "none"
      config_run_duration = "0001_00:00:00"
      config_calendar_type = "360day"
      config_input_name = "grid.nc"
      config_output_name = "output.nc"
      config_restart_name = "restart.nc"
      config_restart_timestamp_name = "restart_timestamp"
      config_restart_interval = "0001_00:00:00"
      config_output_interval = "0001_00:00:00"
      config_stats_interval = "0000_01:00:00"
      config_write_stats_on_startup = .false.
      config_write_output_on_startup = .false.
      config_frames_per_outfile = 1000
      config_pio_num_iotasks = 0
      config_pio_stride = 1
      config_dt = 300.000000
      config_time_integrator = "split_explicit"
      config_vert_coord_movement = "uniform_stretching"
      config_use_min_max_thickness = .false.
      config_min_thickness = 1.000000
      config_max_thickness_factor = 6.000000
      config_set_restingThickness_to_IC = .false.
      config_dzdk_positive = .false.
      config_use_freq_filtered_thickness = .false.
      config_thickness_filter_timescale = 5.000000
      config_use_highFreqThick_restore = .false.
      config_highFreqThick_restore_time = 30.000000
      config_use_highFreqThick_del2 = .false.
      config_highFreqThick_del2 = 100.000000
      config_alter_ICs_for_pbcs = .false.
      config_pbc_alteration_type = "full_cell"
      config_min_pbc_fraction = 0.100000
      config_check_ssh_consistency = .false.
      config_num_halos = 3
      config_block_decomp_file_prefix = "graph.info.part."
      config_number_of_blocks = 0
      config_explicit_proc_decomp = .false.
      config_proc_decomp_file_prefix = "graph.info.part."
      config_hmix_ScaleWithMesh = .false.
      config_maxMeshDensity = -1.000000
      config_apvm_scale_factor = 0.000000
      config_use_mom_del2 = .false.
      config_use_tracer_del2 = .false.
      config_mom_del2 = 10.000000
      config_tracer_del2 = 10.000000
      config_use_mom_del4 = .false.
      config_use_tracer_del4 = .false.
      config_mom_del4 = 50000000188416.000000
      config_tracer_del4 = 0.000000
      config_use_Leith_del2 = .false.
      config_Leith_parameter = 1.000000
      config_Leith_dx = 15000.000000
      config_Leith_visc2_max = 2500.000000
      config_h_kappa = 0.000000
      config_h_kappa_q = 0.000000
      config_use_mom_del2_tensor = .false.
      config_mom_del2_tensor = 10.000000
      config_use_mom_del4_tensor = .false.
      config_mom_del4_tensor = 50000000188416.000000
      config_Rayleigh_friction = .false.
      config_Rayleigh_damping_coeff = 0.000000
      config_convective_visc = 1.000000
      config_convective_diff = 1.000000
      config_use_const_visc = .false.
      config_use_const_diff = .false.
      config_vert_visc = 0.000100
      config_vert_diff = 0.000100
      config_use_rich_visc = .false.
      config_use_rich_diff = .false.
      config_bkrd_vert_visc = 0.000100
      config_bkrd_vert_diff = 0.000010
      config_rich_mix = 0.005000
      config_use_tanh_visc = .false.
      config_use_tanh_diff = .false.
      config_max_visc_tanh = 0.250000
      config_min_visc_tanh = 0.000100
      config_max_diff_tanh = 0.025000
      config_min_diff_tanh = 0.000010
      config_zMid_tanh = -100.000000
      config_zWidth_tanh = 100.000000
      config_use_cvmix = .false.
      config_cvmix_prandtl_number = 1.000000
      config_use_cvmix_background = .false.
      config_cvmix_background_diffusion = 0.000010
      config_cvmix_background_viscosity = 0.000100
      config_use_cvmix_convection = .false.
      config_cvmix_convective_diffusion = 1.000000
      config_cvmix_convective_viscosity = 1.000000
      config_use_cvmix_kpp = .false.
      config_cvmix_kpp_criticalBulkRichardsonNumber = 0.250000
      config_cvmix_kpp_interpolationOMLType = "quadratic"
      config_forcing_type = "off"
      config_restoreT_timescale = 90.000000
      config_restoreS_timescale = 90.000000
      config_restoreT_lengthscale = 50.000000
      config_restoreS_lengthscale = 50.000000
      config_flux_attenuation_coefficient = 0.001000
      config_frazil_ice_formation = .false.
      config_sw_absorption_type = "jerlov"
      config_jerlov_water_type = 3
      config_fixed_jerlov_weights = .false.
      config_vert_tracer_adv = "stencil"
      config_vert_tracer_adv_order = 3
      config_horiz_tracer_adv_order = 3
      config_coef_3rd_order = 0.250000
      config_monotonic = .false.
      config_bottom_drag_coeff = 0.010000
      config_pressure_gradient_type = "pressure_and_zmid"
      config_density0 = 1014.650024
      config_eos_type = "linear"
      config_eos_linear_alpha = 0.255000
      config_eos_linear_beta = 0.764000
      config_eos_linear_Tref = 19.000000
      config_eos_linear_Sref = 35.000000
      config_eos_linear_densityref = 1025.021973
      config_n_ts_iter = 2
      config_n_bcl_iter_beg = 1
      config_n_bcl_iter_mid = 2
      config_n_bcl_iter_end = 2
      config_n_btr_subcycles = 20
      config_n_btr_cor_iter = 2
      config_vel_correction = .false.
      config_btr_subcycle_loop_factor = 2
      config_btr_gam1_velWt1 = 0.500000
      config_btr_gam2_SSHWt1 = 1.000000
      config_btr_gam3_velWt2 = 1.000000
      config_btr_solve_SSH2 = .false.
      config_conduct_tests = .false.
      config_test_tensors = .false.
      config_tensor_test_function = "sph_uCosCos"
      config_check_zlevel_consistency = .false.
      config_filter_btr_mode = .false.
      config_prescribe_velocity = .false.
      config_prescribe_thickness = .false.
      config_include_KE_vertex = .false.
      config_check_tracer_monotonicity = .false.
      config_disable_thick_all_tend = .false.
      config_disable_thick_hadv = .false.
      config_disable_thick_vadv = .false.
      config_disable_thick_sflux = .false.
      config_disable_vel_all_tend = .false.
      config_disable_vel_coriolis = .false.
      config_disable_vel_pgrad = .false.
      config_disable_vel_hmix = .false.
      config_disable_vel_windstress = .false.
      config_disable_vel_vmix = .false.
      config_disable_vel_vadv = .false.
      config_disable_tr_all_tend = .false.
      config_disable_tr_adv = .false.
      config_disable_tr_hmix = .false.
      config_disable_tr_vmix = .false.
      config_disable_tr_sflux = .false.


      if (dminfo % my_proc_id == IO_NODE) then
         if (present(nml_filename)) then
            write(stderrUnit,*) 'Reading ', trim(nml_filename)
            open(funit,file=trim(nml_filename),status='old',form='formatted')
         else
            write(stderrUnit,*) 'Reading namelist.input'
            open(funit,file='namelist.input',status='old',form='formatted')
         end if

         read(funit,time_management,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &time_management'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &time_management not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,io,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &io'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &io not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,time_integration,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &time_integration'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &time_integration not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,ALE_vertical_grid,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &ALE_vertical_grid'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &ALE_vertical_grid not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,ALE_frequency_filtered_thickness,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &ALE_frequency_filtered_thickness'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &ALE_frequency_filtered_thickness not found; using default values for this namelist''s &
variables'
         end if
         rewind(funit)
         read(funit,partial_bottom_cells,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &partial_bottom_cells'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &partial_bottom_cells not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,decomposition,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &decomposition'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &decomposition not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,hmix,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &hmix'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &hmix not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,hmix_del2,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &hmix_del2'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &hmix_del2 not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,hmix_del4,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &hmix_del4'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &hmix_del4 not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,hmix_Leith,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &hmix_Leith'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &hmix_Leith not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,standard_GM,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &standard_GM'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &standard_GM not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,hmix_del2_tensor,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &hmix_del2_tensor'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &hmix_del2_tensor not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,hmix_del4_tensor,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &hmix_del4_tensor'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &hmix_del4_tensor not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,Rayleigh_damping,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &Rayleigh_damping'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &Rayleigh_damping not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,vmix,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &vmix'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &vmix not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,vmix_const,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &vmix_const'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &vmix_const not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,vmix_rich,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &vmix_rich'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &vmix_rich not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,vmix_tanh,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &vmix_tanh'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &vmix_tanh not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,cvmix,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &cvmix'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &cvmix not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,forcing,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &forcing'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &forcing not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,advection,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &advection'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &advection not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,bottom_drag,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &bottom_drag'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &bottom_drag not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,pressure_gradient,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &pressure_gradient'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &pressure_gradient not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,eos,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &eos'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &eos not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,eos_linear,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &eos_linear'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &eos_linear not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,split_explicit_ts,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &split_explicit_ts'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &split_explicit_ts not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,testing,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &testing'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &testing not found; using default values for this namelist''s variables'
         end if
         rewind(funit)
         read(funit,debug,iostat=ierr)
         if (ierr > 0) then
            write(0,*) 'Error while reading namelist record &debug'
            call mpas_dmpar_abort(dminfo)
         else if (ierr < 0) then
            write(0,*) 'Namelist record &debug not found; using default values for this namelist''s variables'
         end if
         rewind(funit)

         close(funit)
         write(stderrUnit,*) ' '
      end if

      call mpas_dmpar_bcast_logical(dminfo, config_do_restart)
      call mpas_dmpar_bcast_char(dminfo, config_start_time)
      call mpas_dmpar_bcast_char(dminfo, config_stop_time)
      call mpas_dmpar_bcast_char(dminfo, config_run_duration)
      call mpas_dmpar_bcast_char(dminfo, config_calendar_type)
      call mpas_dmpar_bcast_char(dminfo, config_input_name)
      call mpas_dmpar_bcast_char(dminfo, config_output_name)
      call mpas_dmpar_bcast_char(dminfo, config_restart_name)
      call mpas_dmpar_bcast_char(dminfo, config_restart_timestamp_name)
      call mpas_dmpar_bcast_char(dminfo, config_restart_interval)
      call mpas_dmpar_bcast_char(dminfo, config_output_interval)
      call mpas_dmpar_bcast_char(dminfo, config_stats_interval)
      call mpas_dmpar_bcast_logical(dminfo, config_write_stats_on_startup)
      call mpas_dmpar_bcast_logical(dminfo, config_write_output_on_startup)
      call mpas_dmpar_bcast_int(dminfo, config_frames_per_outfile)
      call mpas_dmpar_bcast_int(dminfo, config_pio_num_iotasks)
      call mpas_dmpar_bcast_int(dminfo, config_pio_stride)
      call mpas_dmpar_bcast_real(dminfo, config_dt)
      call mpas_dmpar_bcast_char(dminfo, config_time_integrator)
      call mpas_dmpar_bcast_char(dminfo, config_vert_coord_movement)
      call mpas_dmpar_bcast_logical(dminfo, config_use_min_max_thickness)
      call mpas_dmpar_bcast_real(dminfo, config_min_thickness)
      call mpas_dmpar_bcast_real(dminfo, config_max_thickness_factor)
      call mpas_dmpar_bcast_logical(dminfo, config_set_restingThickness_to_IC)
      call mpas_dmpar_bcast_logical(dminfo, config_dzdk_positive)
      call mpas_dmpar_bcast_logical(dminfo, config_use_freq_filtered_thickness)
      call mpas_dmpar_bcast_real(dminfo, config_thickness_filter_timescale)
      call mpas_dmpar_bcast_logical(dminfo, config_use_highFreqThick_restore)
      call mpas_dmpar_bcast_real(dminfo, config_highFreqThick_restore_time)
      call mpas_dmpar_bcast_logical(dminfo, config_use_highFreqThick_del2)
      call mpas_dmpar_bcast_real(dminfo, config_highFreqThick_del2)
      call mpas_dmpar_bcast_logical(dminfo, config_alter_ICs_for_pbcs)
      call mpas_dmpar_bcast_char(dminfo, config_pbc_alteration_type)
      call mpas_dmpar_bcast_real(dminfo, config_min_pbc_fraction)
      call mpas_dmpar_bcast_logical(dminfo, config_check_ssh_consistency)
      call mpas_dmpar_bcast_int(dminfo, config_num_halos)
      call mpas_dmpar_bcast_char(dminfo, config_block_decomp_file_prefix)
      call mpas_dmpar_bcast_int(dminfo, config_number_of_blocks)
      call mpas_dmpar_bcast_logical(dminfo, config_explicit_proc_decomp)
      call mpas_dmpar_bcast_char(dminfo, config_proc_decomp_file_prefix)
      call mpas_dmpar_bcast_logical(dminfo, config_hmix_ScaleWithMesh)
      call mpas_dmpar_bcast_real(dminfo, config_maxMeshDensity)
      call mpas_dmpar_bcast_real(dminfo, config_apvm_scale_factor)
      call mpas_dmpar_bcast_logical(dminfo, config_use_mom_del2)
      call mpas_dmpar_bcast_logical(dminfo, config_use_tracer_del2)
      call mpas_dmpar_bcast_real(dminfo, config_mom_del2)
      call mpas_dmpar_bcast_real(dminfo, config_tracer_del2)
      call mpas_dmpar_bcast_logical(dminfo, config_use_mom_del4)
      call mpas_dmpar_bcast_logical(dminfo, config_use_tracer_del4)
      call mpas_dmpar_bcast_real(dminfo, config_mom_del4)
      call mpas_dmpar_bcast_real(dminfo, config_tracer_del4)
      call mpas_dmpar_bcast_logical(dminfo, config_use_Leith_del2)
      call mpas_dmpar_bcast_real(dminfo, config_Leith_parameter)
      call mpas_dmpar_bcast_real(dminfo, config_Leith_dx)
      call mpas_dmpar_bcast_real(dminfo, config_Leith_visc2_max)
      call mpas_dmpar_bcast_real(dminfo, config_h_kappa)
      call mpas_dmpar_bcast_real(dminfo, config_h_kappa_q)
      call mpas_dmpar_bcast_logical(dminfo, config_use_mom_del2_tensor)
      call mpas_dmpar_bcast_real(dminfo, config_mom_del2_tensor)
      call mpas_dmpar_bcast_logical(dminfo, config_use_mom_del4_tensor)
      call mpas_dmpar_bcast_real(dminfo, config_mom_del4_tensor)
      call mpas_dmpar_bcast_logical(dminfo, config_Rayleigh_friction)
      call mpas_dmpar_bcast_real(dminfo, config_Rayleigh_damping_coeff)
      call mpas_dmpar_bcast_real(dminfo, config_convective_visc)
      call mpas_dmpar_bcast_real(dminfo, config_convective_diff)
      call mpas_dmpar_bcast_logical(dminfo, config_use_const_visc)
      call mpas_dmpar_bcast_logical(dminfo, config_use_const_diff)
      call mpas_dmpar_bcast_real(dminfo, config_vert_visc)
      call mpas_dmpar_bcast_real(dminfo, config_vert_diff)
      call mpas_dmpar_bcast_logical(dminfo, config_use_rich_visc)
      call mpas_dmpar_bcast_logical(dminfo, config_use_rich_diff)
      call mpas_dmpar_bcast_real(dminfo, config_bkrd_vert_visc)
      call mpas_dmpar_bcast_real(dminfo, config_bkrd_vert_diff)
      call mpas_dmpar_bcast_real(dminfo, config_rich_mix)
      call mpas_dmpar_bcast_logical(dminfo, config_use_tanh_visc)
      call mpas_dmpar_bcast_logical(dminfo, config_use_tanh_diff)
      call mpas_dmpar_bcast_real(dminfo, config_max_visc_tanh)
      call mpas_dmpar_bcast_real(dminfo, config_min_visc_tanh)
      call mpas_dmpar_bcast_real(dminfo, config_max_diff_tanh)
      call mpas_dmpar_bcast_real(dminfo, config_min_diff_tanh)
      call mpas_dmpar_bcast_real(dminfo, config_zMid_tanh)
      call mpas_dmpar_bcast_real(dminfo, config_zWidth_tanh)
      call mpas_dmpar_bcast_logical(dminfo, config_use_cvmix)
      call mpas_dmpar_bcast_real(dminfo, config_cvmix_prandtl_number)
      call mpas_dmpar_bcast_logical(dminfo, config_use_cvmix_background)
      call mpas_dmpar_bcast_real(dminfo, config_cvmix_background_diffusion)
      call mpas_dmpar_bcast_real(dminfo, config_cvmix_background_viscosity)
      call mpas_dmpar_bcast_logical(dminfo, config_use_cvmix_convection)
      call mpas_dmpar_bcast_real(dminfo, config_cvmix_convective_diffusion)
      call mpas_dmpar_bcast_real(dminfo, config_cvmix_convective_viscosity)
      call mpas_dmpar_bcast_logical(dminfo, config_use_cvmix_kpp)
      call mpas_dmpar_bcast_real(dminfo, config_cvmix_kpp_criticalBulkRichardsonNumber)
      call mpas_dmpar_bcast_char(dminfo, config_cvmix_kpp_interpolationOMLType)
      call mpas_dmpar_bcast_char(dminfo, config_forcing_type)
      call mpas_dmpar_bcast_real(dminfo, config_restoreT_timescale)
      call mpas_dmpar_bcast_real(dminfo, config_restoreS_timescale)
      call mpas_dmpar_bcast_real(dminfo, config_restoreT_lengthscale)
      call mpas_dmpar_bcast_real(dminfo, config_restoreS_lengthscale)
      call mpas_dmpar_bcast_real(dminfo, config_flux_attenuation_coefficient)
      call mpas_dmpar_bcast_logical(dminfo, config_frazil_ice_formation)
      call mpas_dmpar_bcast_char(dminfo, config_sw_absorption_type)
      call mpas_dmpar_bcast_int(dminfo, config_jerlov_water_type)
      call mpas_dmpar_bcast_logical(dminfo, config_fixed_jerlov_weights)
      call mpas_dmpar_bcast_char(dminfo, config_vert_tracer_adv)
      call mpas_dmpar_bcast_int(dminfo, config_vert_tracer_adv_order)
      call mpas_dmpar_bcast_int(dminfo, config_horiz_tracer_adv_order)
      call mpas_dmpar_bcast_real(dminfo, config_coef_3rd_order)
      call mpas_dmpar_bcast_logical(dminfo, config_monotonic)
      call mpas_dmpar_bcast_real(dminfo, config_bottom_drag_coeff)
      call mpas_dmpar_bcast_char(dminfo, config_pressure_gradient_type)
      call mpas_dmpar_bcast_real(dminfo, config_density0)
      call mpas_dmpar_bcast_char(dminfo, config_eos_type)
      call mpas_dmpar_bcast_real(dminfo, config_eos_linear_alpha)
      call mpas_dmpar_bcast_real(dminfo, config_eos_linear_beta)
      call mpas_dmpar_bcast_real(dminfo, config_eos_linear_Tref)
      call mpas_dmpar_bcast_real(dminfo, config_eos_linear_Sref)
      call mpas_dmpar_bcast_real(dminfo, config_eos_linear_densityref)
      call mpas_dmpar_bcast_int(dminfo, config_n_ts_iter)
      call mpas_dmpar_bcast_int(dminfo, config_n_bcl_iter_beg)
      call mpas_dmpar_bcast_int(dminfo, config_n_bcl_iter_mid)
      call mpas_dmpar_bcast_int(dminfo, config_n_bcl_iter_end)
      call mpas_dmpar_bcast_int(dminfo, config_n_btr_subcycles)
      call mpas_dmpar_bcast_int(dminfo, config_n_btr_cor_iter)
      call mpas_dmpar_bcast_logical(dminfo, config_vel_correction)
      call mpas_dmpar_bcast_int(dminfo, config_btr_subcycle_loop_factor)
      call mpas_dmpar_bcast_real(dminfo, config_btr_gam1_velWt1)
      call mpas_dmpar_bcast_real(dminfo, config_btr_gam2_SSHWt1)
      call mpas_dmpar_bcast_real(dminfo, config_btr_gam3_velWt2)
      call mpas_dmpar_bcast_logical(dminfo, config_btr_solve_SSH2)
      call mpas_dmpar_bcast_logical(dminfo, config_conduct_tests)
      call mpas_dmpar_bcast_logical(dminfo, config_test_tensors)
      call mpas_dmpar_bcast_char(dminfo, config_tensor_test_function)
      call mpas_dmpar_bcast_logical(dminfo, config_check_zlevel_consistency)
      call mpas_dmpar_bcast_logical(dminfo, config_filter_btr_mode)
      call mpas_dmpar_bcast_logical(dminfo, config_prescribe_velocity)
      call mpas_dmpar_bcast_logical(dminfo, config_prescribe_thickness)
      call mpas_dmpar_bcast_logical(dminfo, config_include_KE_vertex)
      call mpas_dmpar_bcast_logical(dminfo, config_check_tracer_monotonicity)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_thick_all_tend)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_thick_hadv)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_thick_vadv)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_thick_sflux)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_vel_all_tend)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_vel_coriolis)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_vel_pgrad)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_vel_hmix)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_vel_windstress)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_vel_vmix)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_vel_vadv)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_tr_all_tend)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_tr_adv)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_tr_hmix)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_tr_vmix)
      call mpas_dmpar_bcast_logical(dminfo, config_disable_tr_sflux)


   end subroutine mpas_read_namelist

end module mpas_configure
