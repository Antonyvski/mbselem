! Copyright (c) 2016 Dassault Systemes Simulia Corp.
!
!***********************************************************************
!
!> \file  Simpack User Force/Control Element Type 40
!
!***********************************************************************



!***********************************************************************
!
!> \brief  Type-specific settings of Force/Control element type 40
!>
!> \remarks
!>  \li  Define type name (type_name)
!>  \li  Define force class fclass_force or fclass_control (spck_df_FClass)
!>  \li  Define static dimensions (spck_df_Force*Dim)
!>  \li  Optionally enable setup task -1 for par-dependent dimensions (spck_df_ForceSetupDim)
!>  \li  Optionally enable setup task 5 for default parameters (spck_df_ForceSetupDefault)
!
!***********************************************************************

      subroutine uforce40_type( str_dim    !< [in ] name string length
     +                        , type_name  !< [out] force type name
     +                        , ierr       !< [out] error code
     +                        )

#if defined(WINDOWS)
      !DEC$ ATTRIBUTES DLLEXPORT::uforce40_type
#endif

C ----------------------------------------------------------------------
C Declaration of Global Variables
C ----------------------------------------------------------------------

      use simpack      
      implicit none
	  
C ----------------------------------------------------------------------
C Declaration of Interface Parameters
C ----------------------------------------------------------------------

      integer, intent(in ) :: str_dim
      integer, intent(out) :: ierr

      character(len=*), intent(out) :: type_name

C ----------------------------------------------------------------------
C Declaration of Local Variables
C ----------------------------------------------------------------------

      integer :: jerr

      character(len=MESSAGE_DIM) :: message

C ----------------------------------------------------------------------
C Initialisation
C ----------------------------------------------------------------------

      ierr = 0

C ----------------------------------------------------------------------
C Execution
C ----------------------------------------------------------------------

      ! name      '123456789012345678901234567890'
      type_name = 'User Force/Control Type 40    '

      ! class
      call spck_df_FClass( fclass_force, ierr )  ! force element
      if ( ierr .ne. 0 ) goto 9001

      ! static dimensions
      call spck_df_ForceParDim(    500, ierr )     ! parameters
      if ( ierr .ne. 0 ) goto 9002

      call spck_df_ForceStDynDim(  0, ierr )     ! dynamic states
      if ( ierr .ne. 0 ) goto 9002

      call spck_df_ForceOvDim(     500, ierr )     ! output values
      if ( ierr .ne. 0 ) goto 9002

      ! enable setup task -1 for par-dependent dimensions
      call spck_df_ForceSetupDim( 1, ierr )
      if ( ierr .ne. 0 ) goto 9003

      ! enable setup task 5 for default parameters
cc    call spck_df_ForceSetupDefault( 1, ierr )
cc    if ( ierr .ne. 0 ) goto 9004

C ----------------------------------------------------------------------
C Error Handling and Return
C ----------------------------------------------------------------------

      return

 9001 write(message,*)
     +'FATAL ERROR in user Force/Control Element type 40:',lf,
     +'Cannot define force class, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 1
      return

 9002 write(message,*)
     +'FATAL ERROR in user Force/Control Element type 40:',lf,
     +'Cannot define dimensions, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 2
      return

 9003 write(message,*)
     +'FATAL ERROR in user Force/Control Element type 40:',lf,
     +'Cannot enable dimensions setup call, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 3
      return

 9004 write(message,*)
     +'FATAL ERROR in user Force/Control Element type 40:',lf,
     +'Cannot enable default parameter setup call, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 4
      return

      end subroutine



!***********************************************************************
!
!> \brief  Element-specific settings of Force/Control element type 40
!>
!> \remarks
!>  \li  Task -1: Optionally define par-dependent dimensions
!>                - Parameters:     spck_df_ForceParDim
!>                - Dynamic states: spck_df_ForceStDynDim
!>                - Output values:  spck_df_ForceOVDim
!>                - This task has to be enabled by spck_df_ForceSetupDim
!>                  in type subroutine.
!>                - Note that most Access Functions cannot be used in
!>                  this task.
!>
!>  
!>
!>  \li  Task  0: Define names and types of parameters, states and output values
!>                - Parameter names \a par_name
!>                - Parameter types \a par_type
!>                - Parameter unit types \a par_unit
!>                - Dynamic state names \a stdyn_name
!>                - Dynamic state unit types \a stdyn_unit
!>                - Algebraic state names and types:   spck_df_ForceStAlg
!>                - Root state names and types:        spck_df_ForceStRoot
!>                - Descriptive state names and types: spck_df_ForceStDesc
!>                - Output value names \a ov_name
!>                - Output value unit types \a ov_unit
!>                - Note that most Access Functions cannot be used in
!>                  this task.
!>
!>  \li  Task  1: Define element-specific infos
!>                - Result type \a res_type
!>                - State reset type \a strst_type
!>                - Continuation run capability: spck_df_ForceContinueRun
!>                - Multi-threading capability: spck_df_ThreadSafe
!>                - Set reference system for pre-defined measurements and force/torque outputs: spck_df_ForceRefSys
!>                - Optionally pre-defined measurements: spck_df_ForceMeas
!>                - Optionally force at Marker: spck_df_ForceAtMarkerIni
!>                - Note that parameter checks and expensive
!>                  pre-processing should be done in task 2.
!>
!>  \li  Task  2: Check parameters and perform pre-processing
!>                - Check parameters \a par
!>                - Open files
!>                - Pre-calculate internal parameters
!>                - Set internal parameters: spck_ds_ForceAddDbl, spck_ds_ForceAddInt
!>
!>  \li  Task  4: Optionally deallocate memory and close files
!>
!***********************************************************************

      subroutine uforce40_setup( task        !< [in    ] |-1 | 5 | 0 | 1 | 2 | 4 | task flag
     +                         , par_dim     !< [in    ] | i | i | i | i | i | i | number of parameters
     +                         , stdyn_dim   !< [in    ] | i | i | i | i | i | i | number of dynamic states
     +                         , ov_dim      !< [in    ] | i | i | i | i | i | i | number of output values
     +                         , str_dim     !< [in    ] | i | i | i | - | - | - | max. length of names
     +                         , id          !< [in    ] | i | i | i | i | i | i | element id
     +                         , mk_from     !< [in    ] | i | i | i | i | i | i | from-marker id
     +                         , mk_to       !< [in    ] | i | i | i | i | i | i | to-marker id
     +                         , par         !< [in,out] | i |i/o| i |i/o|i/o| i | parameters
     +                         , par_type    !< [   out] | - | - | o | - | - | - | parameter types
     +                         , par_name    !< [   out] | - | - | o | - | - | - | parameter names
     +                         , par_unit    !< [   out] | - | - | o | - | - | - | parameter unit types
     +                         , stdyn_name  !< [   out] | - | - | o | - | - | - | dynamic state names
     +                         , stdyn_unit  !< [   out] | - | - | o | - | - | - | dynamic state unit types
     +                         , ov_name     !< [   out] | - | - | o | - | - | - | output value names
     +                         , ov_unit     !< [   out] | - | - | o | - | - | - | output value unit types
     +                         , res_type    !< [   out] | - | - | - | o | - | - | output flag
     +                         , stdyn_nr    !< [      ] | - | - | - | - | - | - | (obsolete)
     +                         , stroot_nr   !< [      ] | - | - | - | - | - | - | (obsolete)
     +                         , strst_type  !< [   out] | - | - | - | o | - | - | state reset flag
     +                         , ierr        !< [   out] | o | o | o | o | o | o | error code
     +                         )

#if defined(WINDOWS)
      !DEC$ ATTRIBUTES DLLEXPORT::uforce40_setup
#endif

C ----------------------------------------------------------------------
C Declaration of Global Variables
C ----------------------------------------------------------------------

      use simpack
	  
      implicit none
	  
C ----------------------------------------------------------------------
C Declaration of Interface Parameters
C ----------------------------------------------------------------------

      integer, intent(in ) :: task
      integer, intent(in ) :: par_dim
      integer, intent(in ) :: stdyn_dim
      integer, intent(in ) :: ov_dim
      integer, intent(in ) :: str_dim
      integer, intent(in ) :: id
      integer, intent(in ) :: mk_from
      integer, intent(in ) :: mk_to
      integer, intent(out) :: par_type(par_dim)
      integer, intent(out) :: par_unit(par_dim)
      integer, intent(out) :: stdyn_unit(stdyn_dim)
      integer, intent(out) :: ov_unit(ov_dim)
      integer, intent(out) :: res_type
      integer              :: stdyn_nr
      integer              :: stroot_nr
      integer, intent(out) :: strst_type
      integer, intent(out) :: ierr

      double precision, intent(inout) :: par(par_dim)

      character(len=*), intent(out) :: par_name(par_dim)
      character(len=*), intent(out) :: stdyn_name(stdyn_dim)
      character(len=*), intent(out) :: ov_name(ov_dim)

C ----------------------------------------------------------------------
C Declaration of Local Variables
C ----------------------------------------------------------------------

      integer :: nov
      integer :: jerr
      integer :: npar
      integer :: lname
      integer :: nstdyn

      character(len=ELEMNAME_DIM) :: name
      character(len=MESSAGE_DIM)  :: message

C ----------------------------------------------------------------------
C Initialisation
C ----------------------------------------------------------------------

      ierr = 0

C ----------------------------------------------------------------------
C Execution
C ----------------------------------------------------------------------

      ! dimensions
      npar   = 500
      nstdyn = 0
      nov    = 500

C ----------------------------------------------------------------------
C task = -1 : Parameter-dependent Dimensions
C ----------------------------------------------------------------------

      if ( task .eq. -1 ) then

         ! parameters
cc       call spck_df_ForceParDim( npar, ierr )
cc       if ( ierr .ne. 0 ) goto 9001

         ! dynamic states
cc       call spck_df_ForceStDynDim( nstdyn, ierr )
cc       if ( ierr .ne. 0 ) goto 9001

         ! output values
cc       call spck_df_ForceOVDim( nov, ierr )
cc       if ( ierr .ne. 0 ) goto 9001 

         return

C ----------------------------------------------------------------------
C task = 0 : Names and Types
C ----------------------------------------------------------------------


      else if ( task .eq. 0 ) then

         ! initialise outputs
         par_name(1:par_dim) = ' '
         par_type(1:par_dim) = knodef
         par_unit(1:par_dim) = knodef
         stdyn_name(1:stdyn_dim) = ' '
         stdyn_unit(1:stdyn_dim) = knodef
         ov_name(1:ov_dim) = ' '
         ov_unit(1:ov_dim) = knodef

         ! check array dimensions
         if ( par_dim   .ne. npar   ) goto 9002
         if ( stdyn_dim .ne. nstdyn ) goto 9002
         if ( ov_dim    .ne. nov    ) goto 9002

C ----------------------------------------------------------------------
C ---------------------Parameter Lists
C ----------------------------------------------------------------------

         ! parameters
         !         name '123456789012345678901234567890'  |  type                          |  unit type
		 ! ##### MARKER #####
         par_name( 1) = 'OuterRace_BRF                 '  ;  par_type( 1) = knr_marker     ;  par_unit( 1) = knodef
         par_name( 2) = 'InnerRace_BRF                 '  ;  par_type( 2) = knr_marker     ;  par_unit( 2) = knodef
		 par_name( 3) = 'WK_Centre	                   '  ;  par_type( 3) = knr_marker     ;  par_unit( 3) = knodef
		 par_name( 4) = 'WK_Aussen	                   '  ;  par_type( 4) = knr_marker     ;  par_unit( 4) = knodef
		 par_name( 5) = 'Grund_centre                  '  ;  par_type( 5) = knr_marker     ;  par_unit( 5) = knodef
		 		 
		 ! ##### Parameter #####
		 par_name( 6) = 'Kontaktlocation               '  ;  par_type( 6) = knr_integer		;  par_unit( 8) = knodef
		 		 
		 ! ##### Geometrie Parameter (Außenring/Innenring) #####
		 par_name( 7) = 'AR Profilradius               '  ;  par_type( 7) = knr_double     	;  par_unit( 7) = knodef
		 par_name( 8) = 'AR Laufbahnradius             '  ;  par_type( 8) = knr_double     	;  par_unit( 8) = knodef
         par_name( 9) = 'AR Breite                     '  ;  par_type( 9) = knr_double     	;  par_unit( 9) = knodef
		 par_name(10) = 'AR Dicke                      '  ;  par_type(10) = knr_double     	;  par_unit(10) = knodef
		 par_name(11) = 'IR Profilradius               '  ;  par_type(11) = knr_double     	;  par_unit(11) = knodef
		 par_name(12) = 'IR Laufbahnradius             '  ;  par_type(12) = knr_double     	;  par_unit(12) = knodef
		 par_name(13) = 'IR Breite                     '  ;  par_type(13) = knr_double     	;  par_unit(13) = knodef
		 
		 ! ##### Geometrie Parameter Wälzkörper ##### 
		 
		  !#Aus WK_LB_Gfosub: Index der mittleren Scheibe#!
		 par_name(14) = 'WK Länge                      '  ;  par_type(14) = knr_double     	;  par_unit(14) = knodef
		 par_name(15) = 'WK Radius                     '  ;  par_type(15) = knr_double     	;  par_unit(15) = knodef 
		 
		 !WK Profilierung
		 par_name(16) = 'Profil Type                   '  ;  par_type(16) = knr_double     	;  par_unit(16) = knodef
		 par_name(17) = 'WK-Profil-Radius              '  ;  par_type(17) = knr_double     	;  par_unit(17) = knodef
		 par_name(18) = 'WKpro-ap                      '  ;  par_type(18) = knr_double     	;  par_unit(18) = knodef
		 par_name(19) = 'WKpro-cp                      '  ;  par_type(19) = knr_double     	;  par_unit(19) = knodef
		 par_name(20) = 'WKpro-dp                      '  ;  par_type(20) = knr_double     	;  par_unit(20) = knodef
		 par_name(21) = 'WKpro-kp                      '  ;  par_type(21) = knr_double     	;  par_unit(21) = knodef
		 par_name(22) = 'WKpro-rk Kantenradius         '  ;  par_type(22) = knr_double     	;  par_unit(22) = knodef

		
		 ! ##### Vektor mit Materialdaten #####
		 par_name(23) = 'IR_E                          '  ;  par_type(23) = knr_double     	;  par_unit(23) = knodef
		 par_name(24) = 'IR_v                          '  ;  par_type(24) = knr_double     	;  par_unit(24) = knodef
		 par_name(25) = 'AR_E                          '  ;  par_type(25) = knr_double     	;  par_unit(25) = knodef
		 par_name(26) = 'AR_v                          '  ;  par_type(26) = knr_double     	;  par_unit(26) = knodef
		 par_name(27) = 'WK_E                          '  ;  par_type(27) = knr_double     	;  par_unit(27) = knodef
		 par_name(28) = 'WK_v                          '  ;  par_type(28) = knr_double     	;  par_unit(28) = knodef
		 
		 
		 ! ##### Reibwert- und Schmierstoffdaten #####
		 par_name(29) = 'Festkörperreibmodell          '  ;  par_type(29) = knr_double     	;  par_unit(29) = knodef
		 par_name(30) = 'EHD-Reibung   0-aus 1-an      '  ;  par_type(30) = knr_double     	;  par_unit(30) = knodef
		 !par_name() = 'Schmierstoff - race_lub_mod   '  ;  par_type() = knr_double     	;  par_unit() = knodef
		 !par_name() = 'Schubspannung - tau_mod       '  ;  par_type() = knr_double     	;  par_unit() = knodef
		 par_name(31) = 'Ther_Korr-faktor - filmT_mod  '  ;  par_type(31) = knr_double     	;  par_unit(31) = knodef
		 par_name(32) = 'Erwärmung - comprT_mod        '  ;  par_type(32) = knr_double     	;  par_unit(32) = knodef
		 par_name(33) = 'vel_s                         '  ;  par_type(33) = knr_double     	;  par_unit(33) = knodef
		 par_name(34) = 'vel_d                         '  ;  par_type(34) = knr_double     	;  par_unit(34) = knodef
		 par_name(35) = 'mu_s                          '  ;  par_type(35) = knr_double     	;  par_unit(35) = knodef
		 par_name(36) = 'mu_d                          '  ;  par_type(36) = knr_double     	;  par_unit(36) = knodef
		 par_name(37) = 'lubtmp                        '  ;  par_type(37) = knr_double     	;  par_unit(37) = knodef
		 par_name(38) = 'etaZero                       '  ;  par_type(38) = knr_double     	;  par_unit(38) = knodef
		 par_name(39) = 'alphaT                        '  ;  par_type(39) = knr_double     	;  par_unit(39) = knodef
		 par_name(40) = 'alphaP                        '  ;  par_type(40) = knr_double     	;  par_unit(40) = knodef
		 par_name(41) = 'lambdaZero                    '  ;  par_type(41) = knr_double     	;  par_unit(41) = knodef
		 par_name(42) = 'alphaLambda                   '  ;  par_type(42) = knr_double     	;  par_unit(42) = knodef
		 par_name(43) = 'C1                            '  ;  par_type(43) = knr_double     	;  par_unit(43) = knodef
		 par_name(44) = 'C2                            '  ;  par_type(44) = knr_double     	;  par_unit(44) = knodef
		 par_name(45) = 'AV                            '  ;  par_type(45) = knr_double     	;  par_unit(45) = knodef
		 par_name(46) = 'Bv                            '  ;  par_type(46) = knr_double     	;  par_unit(46) = knodef
		 par_name(47) = 'CV                            '  ;  par_type(47) = knr_double     	;  par_unit(47) = knodef
		 par_name(48) = 'DR                            '  ;  par_type(48) = knr_double     	;  par_unit(48) = knodef
		 par_name(49) = 'ER                            '  ;  par_type(49) = knr_double     	;  par_unit(49) = knodef
		 par_name(50) = 'pR                            '  ;  par_type(50) = knr_double     	;  par_unit(50) = knodef
		 par_name(51) = 'B                             '  ;  par_type(51) = knr_double     	;  par_unit(51) = knodef
		 par_name(52) = 'C                             '  ;  par_type(52) = knr_double     	;  par_unit(52) = knodef
		 par_name(53) = 'K                             '  ;  par_type(53) = knr_double     	;  par_unit(53) = knodef
		 par_name(54) = 'a1                            '  ;  par_type(54) = knr_double     	;  par_unit(54) = knodef
		 par_name(55) = 'a2                            '  ;  par_type(55) = knr_double     	;  par_unit(55) = knodef
		 par_name(56) = 'b1                            '  ;  par_type(56) = knr_double     	;  par_unit(56) = knodef
		 par_name(57) = 'b2                            '  ;  par_type(57) = knr_double     	;  par_unit(57) = knodef
		 par_name(58) = 'rhoZero                       '  ;  par_type(58) = knr_double     	;  par_unit(58) = knodef
		 par_name(59) = 'tauRheo                       '  ;  par_type(59) = knr_double     	;  par_unit(59) = knodef
		 par_name(60) = 'sigma                         '  ;  par_type(60) = knr_double     	;  par_unit(60) = knodef
		 par_name(61) = 'BZH                           '  ;  par_type(61) = knr_double     	;  par_unit(61) = knodef
		 par_name(62) = 'CZH                           '  ;  par_type(62) = knr_double     	;  par_unit(62) = knodef
		 par_name(63) = 'alphaHys                      '  ;  par_type(63) = knr_double     	;  par_unit(63) = knodef
		 par_name(64) = 'scaleHys                      '  ;  par_type(64) = knr_double     	;  par_unit(64) = knodef
		 par_name(65) = 'cr                            '  ;  par_type(65) = knr_double     	;  par_unit(65) = knodef
		 par_name(66) = 'rexp                          '  ;  par_type(66) = knr_double     	;  par_unit(66) = knodef

		 ! ##### Dämpfungs parameter #####
		 par_name(67) = 'd_mod                         '  ;  par_type(67) = knr_double     	;  par_unit(67) = knodef
		 par_name(68) = 'p_t                           '  ;  par_type(68) = knr_double     	;  par_unit(68) = knodef
		 par_name(69) = 'd_max                         '  ;  par_type(69) = knr_double     	;  par_unit(69) = knodef
		 par_name(70) = 'K0                            '  ;  par_type(70) = knr_double     	;  par_unit(70) = knodef
		 par_name(71) = 'KR                            '  ;  par_type(71) = knr_double     	;  par_unit(71) = knodef
		 par_name(72) = 'KL                            '  ;  par_type(72) = knr_double     	;  par_unit(72) = knodef
		 par_name(73) = 'KE                            '  ;  par_type(73) = knr_double     	;  par_unit(73) = knodef
		 par_name(74) = 'Keta                          '  ;  par_type(74) = knr_double     	;  par_unit(74) = knodef
		 par_name(75) = 'Kalpha                        '  ;  par_type(75) = knr_double     	;  par_unit(75) = knodef
		 par_name(76) = 'Kq                            '  ;  par_type(76) = knr_double     	;  par_unit(76) = knodef
		 par_name(77) = 'Ku                            '  ;  par_type(77) = knr_double     	;  par_unit(77) = knodef
		 par_name(78) = 'Kf                            '  ;  par_type(78) = knr_double     	;  par_unit(78) = knodef
		 par_name(79) = 'fe                            '  ;  par_type(79) = knr_double     	;  par_unit(79) = knodef
		 
		 ! ###### Parametern für Scheibenmodell, Gauss-Integration, Curvefit #####
		 
		 par_name(81) = 'ctctype_LB                    '  ;  par_type(81) = knr_double     	;  par_unit(81) = knodef
		 par_name(82) = 'no_gauss_LB                   '  ;  par_type(82) = knr_double     	;  par_unit(82) = knodef
		 par_name(83) = 'cf_pnts_LB                    '  ;  par_type(83) = knr_double     	;  par_unit(83) = knodef
		 par_name(84) = 'cf_p_max_LB                   '  ;  par_type(84) = knr_double      ;  par_unit(84) = knodef
		 par_name(85) = 'no_slce_LB                    '  ;  par_type(85) = knr_double      ;  par_unit(84) = knodef
         par_name(86) = 'mid_slce                      '  ;  par_type(86) = knr_double      ;  par_unit(86) = knodef
		 

         ! dynamic states
         ! --------------
         !           name '123456789012345678901234567890'  |  unit type
cc       stdyn_name( 1) = 'Dynamic state 1               '  ;  stdyn_unit( 1) = knodef
cc       stdyn_name( 2) = 'Dynamic state 2               '  ;  stdyn_unit( 2) = knodef
cc       stdyn_name( 3) = 'Dynamic state 3               '  ;  stdyn_unit( 3) = knodef

         ! algebraic states
         ! ----------------
         !                   name '123456789012345678901234567890' | type   | unit type
cc       call spck_df_ForceStAlg( 'Algebraic state 1             ' , knodef , knodef, id , 1 , ierr )
cc       if ( ierr .ne. 0 ) goto 9004
cc       call spck_df_ForceStAlg( 'Algebraic state 2             ' , knodef , knodef, id , 2 , ierr )
cc       if ( ierr .ne. 0 ) goto 9004
cc       call spck_df_ForceStAlg( 'Algebraic state 3             ' , knodef , knodef, id , 3 , ierr )
cc       if ( ierr .ne. 0 ) goto 9004

         ! root states
         ! -----------
         !                   name '123456789012345678901234567890' | type
cc       call spck_df_ForceStRoot( 'Root state 1                 ' , knodef , id , 1 , ierr )
cc       if ( ierr .ne. 0 ) goto 9005
cc       call spck_df_ForceStRoot( 'Root state 2                 ' , knodef , id , 2 , ierr )
cc       if ( ierr .ne. 0 ) goto 9005
cc       call spck_df_ForceStRoot( 'Root state 3                 ' , knodef , id , 3 , ierr )
cc       if ( ierr .ne. 0 ) goto 9005

         ! descriptive states
         ! ------------------
         !                    name '123456789012345678901234567890' | type              | unit type
cc       call spck_df_ForceStDesc( 'Descriptive state 1           ' , kstdesc_undefined , knodef, id , 1 , ierr )
cc       if ( ierr .ne. 0 ) goto 9006
cc       call spck_df_ForceStDesc( 'Descriptive state 2           ' , kstdesc_undefined , knodef, id , 2 , ierr )
cc       if ( ierr .ne. 0 ) goto 9006
cc       call spck_df_ForceStDesc( 'Descriptive state 3           ' , kstdesc_undefined , knodef, id , 3 , ierr )
cc       if ( ierr .ne. 0 ) goto 9006



C ---------------------------------------------------------------------
C --------------------- Ausgabedaten ----------------------------------
C ---------------------------------------------------------------------

		 ! output values
			! -------------
		!	!      name  'Laufbahn'   |  unit type
	    	ov_name( 1) = 'Normalkraft X                '   ;  ov_unit( 1) = kp_force
	    	ov_name( 2) = 'Normalkraft Y                '   ;  ov_unit( 2) = kp_force
	    	ov_name( 3) = 'Normalkraft Z                '   ;  ov_unit( 3) = kp_force
	    	ov_name( 4) = 'Normalkraft ABS              '   ;  ov_unit( 4) = kp_force
	    	ov_name( 5) = 'Dämpfungskraft X             '   ;  ov_unit( 5) = kp_force
	    	ov_name( 6) = 'Dämpfungskraft Y             '   ;  ov_unit( 6) = kp_force
	    	ov_name( 7) = 'Dämpfungskraft Z             '   ;  ov_unit( 7) = kp_force
	    	ov_name( 8) = 'Dämpfungskraft ABS           '   ;  ov_unit( 8) = kp_force
	    	ov_name( 9) = 'Reibkraft X                  '   ;  ov_unit( 9) = kp_force
	    	ov_name(10) = 'Reibkraft Y                  '   ;  ov_unit(10) = kp_force
	    	ov_name(11) = 'Reibkraft Z                  '   ;  ov_unit(11) = kp_force
	    	ov_name(12) = 'Reibkraft ABS                '   ;  ov_unit(12) = kp_force
	    	ov_name(13) = 'Reibmoment X                 '   ;  ov_unit(13) = kp_torque
	    	ov_name(14) = 'Reibmoment Y                 '   ;  ov_unit(14) = kp_torque
	    	ov_name(15) = 'Reibmoment Z                 '   ;  ov_unit(15) = kp_torque
	    	ov_name(16) = 'Reibmoment ABS               '   ;  ov_unit(16) = kp_torque                                            
			ov_name(17) = 'Schmierfilmhoehe [mum]       '   ;  ov_unit(17) = knodef
			ov_name(18) = 'Durchdringung [mm]           '   ;  ov_unit(18) = knodef
			ov_name(19) = 'Pressung auf der Laufbahn [MPa]' ;  ov_unit(19) = knodef
			
			! PWI Test Borddämpfung
	    	!ov_name(20) = 'DForce_tvel_brd X            '   ;  ov_unit(20) = kp_force
	    	!ov_name(21) = 'DForce_tvel_brd Y            '   ;  ov_unit(21) = kp_force
	    	!ov_name(22) = 'DForce_tvel_brd Z            '   ;  ov_unit(22) = kp_force
	    	!ov_name(23) = 'DForce_tvel_brd ABS          '   ;  ov_unit(23) = kp_force
			!ov_name(24) = 'delta X            '   ;  ov_unit(24) = kp_force
	    	!ov_name(25) = 'delta Y            '   ;  ov_unit(25) = kp_force
	    	!ov_name(26) = 'delta Z            '   ;  ov_unit(26) = kp_force
	    	!ov_name(27) = 'DForce_rvel_brd ABS          '   ;  ov_unit(27) = kp_force
			!ov_name(28) = 'vnorm_tvel_brd X [mm/s]      '   ;  ov_unit(28) = knodef
			!ov_name(29) = 'vnorm_tvel_brd Y [mm/s]      '   ;  ov_unit(29) = knodef	
			!ov_name(30) = 'vnorm_tvel_brd Z [mm/s]      '   ;  ov_unit(30) = knodef			
			!ov_name(31) = 'vnorm_rvel_brd X [mm/s]      '   ;  ov_unit(31) = knodef			
			!ov_name(32) = 'vnorm_rvel_brd Y [mm/s]      '   ;  ov_unit(32) = knodef			
			!ov_name(33) = 'vnorm_rvel_brd Z [mm/s]      '   ;  ov_unit(33) = knodef			
	    	!ov_name(34) = 'result_tfk X     			' 	;  ov_unit(34) = kp_force
	    	!ov_name(35) = 'result_tfk Y     			' 	;  ov_unit(35) = kp_force
	    	!ov_name(36) = 'result_tfk Z     			' 	;  ov_unit(36) = kp_force
	    	!ov_name(37) = 'result_tfk X     			' 	;  ov_unit(37) = kp_torque
	    	!ov_name(38) = 'result_tfk Y     			' 	;  ov_unit(38) = kp_torque
	    	!ov_name(39) = 'result_tfk Z     			' 	;  ov_unit(39) = kp_torque
	    	!ov_name(40) = 'tdisp_122 X      			'   ;  ov_unit(40) = knodef	
	    	!ov_name(41) = 'tdisp_122 Y      			'   ;  ov_unit(41) = knodef	
	    	!ov_name(42) = 'tdisp_122 Z      			'   ;  ov_unit(42) = knodef	
	    	!ov_name(43) = 'tvel_122 X       			'   ;  ov_unit(43) = knodef	
	    	!ov_name(44) = 'tvel_122 Y       			'   ;  ov_unit(44) = knodef	
	    	!ov_name(45) = 'tvel_122 Z       			'   ;  ov_unit(45) = knodef	
			
			
C ----------------------------------------------------------------------
C task = 1 : Element-specific Infos
C ----------------------------------------------------------------------

      else if ( task .eq. 1 ) then

         ! result type
         ! kforce_res_none         = no force, no torque
         ! kforce_res_force        = force only
         ! kforce_res_torque       = torque only
         ! kforce_res_force_torque = force and torque
         res_type = kforce_res_force_torque

         ! state reset type (see also spck_slv_StRstInit)
         ! kforce_strst_none     = state reset disabled
         ! kforce_strst_continue = state reset enabled, do not calculate consistent states
         ! kforce_strst_assemble = state reset enabled, calculate consistent states
         strst_type = kforce_strst_none

         ! continuation run capability
cc       call spck_df_ForceContinueRun( 0, id, ierr )
cc       if ( ierr .ne. 0 ) goto 9007

         ! multi-threading capability
cc       call spck_df_ThreadSafe( 0, ierr )
cc       if ( ierr .ne. 0 ) goto 9008

         ! reference system for pre-defined measurements and force/torque outputs
         call spck_df_ForceRefSys( kref_marker, ierr )
         if ( ierr .ne. 0 ) goto 9009

C ----------------------------------------------------------------------
C task = 2 : Check Parameters and Pre-Processing
C ----------------------------------------------------------------------

      else if ( task .eq. 2 ) then

         continue

C ----------------------------------------------------------------------
C task = 4 : Final Call
C ----------------------------------------------------------------------

      else if ( task .eq. 4 ) then

         continue

C ----------------------------------------------------------------------
C task = 5 : Default Parameters
C ----------------------------------------------------------------------

      else if ( task .eq. 5 ) then

         continue

      end if

C ----------------------------------------------------------------------
C Error Handling and Return
C ----------------------------------------------------------------------

      return

 9001 call spck_an_ForceID2Name( name, lname, id, jerr )
      write(message,*)
     +'FATAL ERROR in pre-processing of user Force/Control Element',lf,
     + trim(name),':',lf,
     +'Cannot define dimensions, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 1
      return

 9002 call spck_an_ForceID2Name( name, lname, id, jerr )
      write(message,*)
     +'FATAL ERROR in pre-processing of user Force/Control Element',lf,
     + trim(name),':',lf,
     +'Array dimensions are not sufficient.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 2
      return

 9003 call spck_an_ForceID2Name( name, lname, id, jerr )
      write(message,*)
     +'FATAL ERROR in pre-processing of user Force/Control Element',lf,
     + trim(name),':',lf,
     +'Cannot define selection menu, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 3
      return

 9004 call spck_an_ForceID2Name( name, lname, id, jerr )
      write(message,*)
     +'FATAL ERROR in pre-processing of user Force/Control Element',lf,
     + trim(name),':',lf,
     +'Cannot define algebraic state, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 4
      return

 9005 call spck_an_ForceID2Name( name, lname, id, jerr )
      write(message,*)
     +'FATAL ERROR in pre-processing of user Force/Control Element',lf,
     + trim(name),':',lf,
     +'Cannot define root state, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 5
      return

 9006 call spck_an_ForceID2Name( name, lname, id, jerr )
      write(message,*)
     +'FATAL ERROR in pre-processing of user Force/Control Element',lf,
     + trim(name),':',lf,
     +'Cannot define descriptive state, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 6
      return

 9007 call spck_an_ForceID2Name( name, lname, id, jerr )
      write(message,*)
     +'FATAL ERROR in pre-processing of user Force/Control Element',lf,
     + trim(name),':',lf,
     +'Cannot define continuation run capability, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 7
      return

 9008 call spck_an_ForceID2Name( name, lname, id, jerr )
      write(message,*)
     +'FATAL ERROR in pre-processing of user Force/Control Element',lf,
     + trim(name),':',lf,
     +'Cannot define multi-threading capability, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 8
      return

 9009 call spck_an_ForceID2Name( name, lname, id, jerr )
      write(message,*)
     +'FATAL ERROR in pre-processing of user Force/Control Element',lf,
     + trim(name),':',lf,
     +'Cannot define Force reference system, ierr = ',ierr,'.'
      call spck_uf_message( MSG_FATAL, message )
      ierr = 9
      return

      end subroutine



!***********************************************************************
!
!> \brief  Evaluation of Force/Control element type 40
!>
!> \remarks
!>  \li  Task  0: Determine force, torque and output values
!>  \li  Task  1: Evaluate root functions
!>  \li  Task  2: Perform state reset after root state switch
!>  \li  Task  3: Determine algebraic state residuals
!>  \li  Task  4: Initialise states after calculation of consistent states
!>  \li  Task  5: Initialisation (first call)
!>  \li  Task  6: Finalisation (last call)
!>  \li  \a force and \a torque have to be returned in coordinates of
!>       the from-body reference frame Ri.
!>  \li  \a force and \a torque are applied at the from-marker Mi
!>       and with reversed sign at the to-marker Mj. In addition a
!>       reaction torque r_MiMj x \a force is applied at Mi by the
!>       solver.
!
!***********************************************************************

      subroutine uforce40( task        !< [in    ] | 0 | 1 | 2 | 3 | 4 | 5 | 6 | task flag
     +                   , par_dim     !< [in    ] | i | i | i | i | i | i | i | number of parameters
     +                   , uin_dim     !< [in    ] | i | i | i | i | i | i | i | number of u-vector components
     +                   , stdyn_dim   !< [in    ] | i | i | i | i | i | i | i | number of dynamic states
     +                   , stroot_dim  !< [in    ] | i | i | i | i | i | i | i | number of root states
     +                   , ov_dim      !< [in    ] | i | i | i | i | i | i | i | number of output values
     +                   , id          !< [in    ] | i | i | i | i | i | i | i | element id
     +                   , par         !< [in    ] | i | i | i | i | i | i | i | parameters
     +                   , mk_from     !< [in    ] | i | i | i | i | i | i | i | from-marker id
     +                   , mk_to       !< [in    ] | i | i | i | i | i | i | i | to-marker id
     +                   , time        !< [in    ] | i | i | i | i | i | i | i | time
     +                   , uin         !< [in    ] | i | i | i | i | i | i | i | u-vector (deprecated, use access function spck_as_UEle)
     +                   , stdyn       !< [in,out] | i | i | i | i |i/o|i/o| i | dynamic states
     +                   , stroot      !< [in,out] |i/o| i | i | i |i/o|i/o| i | root states
     +                   , stdynd      !< [   out] | o | - | - | - | - | - | - | dynamic state derivatives
     +                   , force       !< [   out] | o | - | - | - | - | - | - | force vector acting at from-marker w.r.t. from-brf
     +                   , torque      !< [   out] | o | - | - | - | - | - | - | torque vector acting at from-marker w.r.t. from-brf
     +                   , ov          !< [   out] | o | - | - | - | - | - | - | output values
     +                   , valroot     !< [   out] | - | o | - | - | - | - | - | root function values
     +                   , ierr        !< [   out] | o | o | o | o | o | o | o | error code
     +                   )

#if defined(WINDOWS)
      !DEC$ ATTRIBUTES DLLEXPORT::uforce40
#endif

C ----------------------------------------------------------------------
C Declaration of Global Variables
C ----------------------------------------------------------------------

      use simpack
  
	  implicit none
	  

C ----------------------------------------------------------------------
C Declaration of Interface Parameters
C ----------------------------------------------------------------------

      integer, intent(in   ) :: task
      integer, intent(in   ) :: par_dim
      integer, intent(in   ) :: uin_dim
      integer, intent(in   ) :: stdyn_dim
      integer, intent(in   ) :: stroot_dim
      integer, intent(in   ) :: ov_dim
      integer, intent(in   ) :: id
      integer, intent(in   ) :: mk_from
      integer, intent(in   ) :: mk_to
      integer, intent(inout) :: stroot(stroot_dim)
      integer, intent(  out) :: ierr

      double precision, intent(in   ) :: par(par_dim)
      double precision, intent(in   ) :: time
      double precision, intent(in   ) :: uin(uin_dim)
      double precision, intent(inout) :: stdyn(stdyn_dim)
      double precision, intent(  out) :: stdynd(stdyn_dim)
      double precision, intent(  out) :: force(3)
      double precision, intent(  out) :: torque(3)
      double precision, intent(  out) :: ov(ov_dim)
      double precision, intent(  out) :: valroot(stroot_dim)

C ----------------------------------------------------------------------
C Declaration of Local Variables
C ----------------------------------------------------------------------
	
	  !Aufbau der Vektoren und Felder wie sie in ADAMS verwendet werden
	  real, dimension (10) 				  :: par_Kon		           !benötigte Parameter aus übergabeliste für Kontaktmodul
	  integer                             :: i
	  integer                             :: bear_nr, wk_nr, ctloc, ct_mod 
	  integer 							  :: error

	  integer                             :: ct_id
	  real(8) :: temp(2,31,3)
	  
	  !-------------------------------------
	  !Variablen aus dem Module BearingData:
	  !-------------------------------------
	  
	  !----------------------------------------------------------------------------------
	  ! Allgemeine Daten
	  real(8),parameter           :: ZERO = 0.0d0, ONE = 1.0d0, PI = acos(-1.0d0), TWOPI = 2.0d0*PI, MPREC = epsilon(1.0d0), TOL = 1.0d-12, PREC = 1.0d-8
	  integer,parameter           :: n_min=15                 ! Min. Scheibenanzahl (für curve-fit)
	  integer,parameter           :: n_max=201                ! Max. Scheibenanzahl
	  integer,parameter           :: no_gauss_max=25          ! Max. Anzahl an Stützpunkten für
															  ! Gauss-Legendre-Quadratur
	  integer,parameter           :: wk_max=101               ! Max. WK-Anzahl

	  integer                     :: beartype, no_bear, no_wk, no_slce, no_gauss, mid_slce, mid_gauss
	  integer                     :: no_cellx, no_celly
	  integer                     :: contactmod
	  integer                     :: ctctype_LB

	  real(kind=8)                :: v_e_mod(4), INV_M_TRANS_DIR_mod(4,4)

	  character(len=100)          :: msg_vault(100)

	  integer                     :: cf_pnts
	  real(kind=8)                :: cf_p_max
	  logical					  :: dflag=0
	  real(kind=8)				  :: rot(3,3)
	  real(kind=8)				  :: rot2(3,3)
	  
	  !PWI
	  ! WK-LB-Kontakt !
	  real(kind=8)			      :: rot3(3,3)
	  real(8), dimension(3)       :: tdisp_wk_ar_ar
	  real(8), dimension(3)       :: tdisp_wk_ar_gr
	  real(8), dimension(3)       :: tdisp_wk_wka_ar
	  real(8), dimension(3)       :: tdisp_wk_ir_ir
	  real(8), dimension(3)       :: tdisp_wk_ir_gr
	  real(8), dimension(3)       :: tdisp_wk_wka_gr
	  real(kind=8)			      :: angles_gr_ar(3)
	  real(8), dimension(3)       :: angles_wk_ar
	  real(8), dimension(3)       :: angles_gr_ir
	  real(8), dimension(3)       :: angles_wk_ir
	  real(8), dimension(3)       :: angles_gr_wk
	  real(8), dimension(9)       :: matTr_gr_ar, matTr_wk_ar, matTr_gr_ir
	  real(8), dimension(9)       :: matTr_wk_ir, matTr_gr_wk
	  real(8)					  :: temptdisp
	  
	  !PWI
	  ! LB-Welligkeit: Anzahl und Amplitude der Oberflächenwelligkeit von AR und IR der Ordnung 1-5
	  
	  real(8), dimension(3)       :: angles_ir_gr_gr
	  real(kind=8)				  :: IRwave_anz(5), IRwave_amp(5)	
	  real(kind=8)				  :: ARwave_anz(5), ARwave_amp(5)	
	  
	  ! WK-LB-Geschwindigkeiten 
	  real(kind=8)				  :: tvel_wk_ar_gr(3), tvel_ar_gr_gr(3), tvel_wk_ir_gr(3), tvel_ir_gr_gr(3), tdisp_wk_wka_ir(3), tvel_wk_gr_gr(3)
	  real(kind=8)				  :: rvel_wk_ar_dr(3), rvel_ar_gr_gr(3), rvel_wk_ir_gr(3), rvel_ir_gr_gr(3), rvel_wk_gr_gr(3)
	  real(kind=8)	              :: vtrans_LB_gr(3), vtrans_WK_LB(3), vtrans_WK_gr(3)
	  !real(kind=8)	              :: vtrans_WK_gr(no_slce,3)
	   
	  real(kind=8)                :: v_abs, v_ptp, w_abs
	  ! WK-LW-Dämpfung
	  real(kind=8)				  :: WK_achse(3)
	  
	  !PWI
	  ! WK-KF-Kontakt
	  DOUBLE PRECISION			  :: tdisp_wk_kfT_kfT(3)
	  DOUBLE PRECISION			  :: angles_wk_kfT(3)
	  DOUBLE PRECISION			  :: temp1_kf
	  DOUBLE PRECISION			  :: rvel_wk_kfT_kfT(3), tvel_wk_kfT_kfT(3)
	  
	  !----------------------------------------------------------------------------------
	  ! Geometrie Pedelrollenlager

	  real(kind=8)                :: slce_wdth

      ! Geometrie des Außen- und Innenrings
	  real(kind=8)                :: rad_AR, prorad_AR, AR_breite, AR_dicke
	  real(kind=8)                :: rad_IR, prorad_IR, IR_breite
      !!real(kind=8)                :: AR_MANTEL_DRM
	  ! Geometrie des Bordes (benötigt für Bordkontakte // sind im Angebot nicht enthalten)

	  ! Geometrie des Wälzkörpers
	  integer                     :: wk_protype
	  real(kind=8)                :: wk_rad
	  real(kind=8)                :: wk_l
	  real(kind=8)                :: wk_pro_rad
	  real(kind=8)                :: wk_pro_ap, wk_pro_cp, wk_pro_dp, wk_pro_kp, wk_pro_rk
	  real(kind=8), allocatable   :: wk_prorad(:), wk_dpro_deta(:)
	  real(kind=8)                :: wk_winkel, lb_winkel, tk_rad, wk_Pos(3)
	  real(kind=8)                :: distnce(25)                                                        !!!###später muss man ändern .nicht immer 25###!!!
	  ! Abgeleitete Größen
	  real(kind=8), allocatable   :: sum_rho_WKAR(:), sum_rho_WKIR(:)
	  real(kind=8), allocatable   :: R_dash_WKAR(:), R_dash_WKIR(:)
	  
	  !----------------------------------------------------------------------------------
	  ! Materialeigenschaften

	  real(kind=8)                :: ar_youngs, ar_poisson, ir_youngs, ir_poisson, wk_youngs, wk_poisson, kf_youngs, kf_poisson
	  real(kind=8)                :: mat_c_WKAR, mat_c_WKIR
	  real(kind=8)                :: E_dash_WKAR,E_dash_WKIR

      ! Werkstoffdaten des Außen- und Innenrings und des Wälzkörpers
      real(kind=8)                :: IR_E, IR_v, AR_E, AR_v, WK_E, WK_v
	  !----------------------------------------------------------------------------------
	  ! Schmierstoffeigenschaften
	  ! Reibungsdaten
	  integer                     :: frctn_mod, frctnEHD_mod, lub_mod, tau_mod
	  real(kind=8)                :: vel_s, vel_d, mu_s, mu_d
	  integer                     :: filmT_mod, comprT_mod

	  ! Schmierstoffdaten
	  real(kind=8)                :: etaZero_in, alphaT_in, alphaP_in
	  real(kind=8)                :: etaZero, alphaT, alphaP
	  real(kind=8)                :: LubTemp, lambdaZero, alphaLambda, rhoZero, lambda
	  real(kind=8)                :: AV, BV, CV, DR, ER, pR, tauRheo
	  real(kind=8)                :: C1, C2
	  real(kind=8)                :: sigma, BZH, CZH
	  real(kind=8)                :: alphaHys, scaleHys, cr, rexp
	  real(kind=8)                :: B, C, K, a1, a2, b1, b2

	  !----------------------------------------------------------------------------------
	  ! Dämpfungseigenschaften
	  integer                     :: d_mod
	  real(kind=8)                :: d_max, p_t
	  integer                     :: d_mod_kf
	  real(kind=8)                :: d_max_kf, p_t_kf
	  real(kind=8)				  :: K0, KR, KL, KE, Keta, Kalpha, Kq, Ku, Kf, fe

	  !------------------------
	  !ENDE Module BearingData:
	  !------------------------
	  
	  !Variablen zur Erstellung der Ausgabe
	  real(kind=8)				:: temp33(3,3)
	  real(kind=8)				:: res(6)
	  real(kind=8) 				:: result(6)
	  
	  !----------------------------------------------------------------------------------
	  !Bordkräfte
	  ! ##### Marker IDs #####
      INTEGER				:: ID_Kaefig 

	  
	  ! ##### Kontaktvariablen #####
      INTEGER				:: num_contacts
	  real(kind=8)			:: penetrtn(2), ctnorm(2,3), ctpoint(2,3)
	  
	  ! Geometrien des Außen- und Innenrings
      real(kind=8) 		:: AR_LFB_breite, Bord_dicke
      real(kind=8) 		:: AR_Bord_D, AR_Bord_AD, AR_Bord_ID
     	  
      real(kind=8) 		:: IR_LFB_breite, Bord_dicke_IR
      real(kind=8) 		:: IR_Bord_D, IR_Bord_AD, IR_Bord_ID

      real(kind=8) 		:: bord_w_ar, bord_w_ir,bord_oeffnungswinkel_AR,bord_oeffnungswinkel_IR
      real(kind=8) 		:: rad_teilkreis
      
	 
	  ! ##### Vektor mit Parametern für Scheibenmodell, Gauss-Integration #####
      INTEGER, PARAMETER	:: no_slce_Bord = 2
	  INTEGER, PARAMETER    :: no_slce_LB   = 25    !new 
	  ! ##### Variablen Reibung #####
      real(kind=8) 		:: vtang_rel_brd(no_slce_Bord,3), vtang_sum_brd(no_slce_Bord,3)
      INTEGER			:: race_f_mod_brd
      real(kind=8) 		:: vel_s_brd, vel_d_brd, mu_s_brd, mu_d_brd
	  
	  ! ##### Variablen Dämpfung #####     
      real(kind=8) 		:: d_max_brd, p_t_brd
	  
	  real(kind=8) 		:: delta(no_slce_Bord)
	  
	  ! ##### Sonstige Variablen #####	 
      real(kind=8) 		:: y, h, r, chord_l_a,d,chord_l_i
      real(kind=8) 		:: temp1_brd, temp1a_brd
      real(kind=8) 		:: temp33_brd(3,3)
	  real(kind=8)      :: mat_WK, mat_AR, mat_IR
	  real(kind=8)      :: rho1x_WKAR(n_max), rho2x_WKAR, rho1y_WKAR, rho2y_WKAR
	  real(kind=8)      :: rhox_WKAR(n_max), rhoy_WKAR
      real(kind=8)      :: rho1x_WKIR(n_max), rho2x_WKIR, rho1y_WKIR, rho2y_WKIR
      real(kind=8)      :: rhox_WKIR(n_max), rhoy_WKIR   

	  ! ##### Auslesefelder aus Simpack #####
      real(kind=8) 		:: temptdisp_brd
      real(kind=8) 		:: tdisp_wk_wka_wk_brd(3), tdisp_ar_ar a_ar_brd(3)
	  real(kind=8) 		:: tdisp_wk_ar_kf_brd(3), tdisp_wk_ir_kf_brd(3)
      real(kind=8) 		:: angles_ar_gr_brd(3), angles_wk_ar_brd(3),angles_wk_ir_brd(3)
      real(kind=8) 		:: tdisp_ir_ira_ir_brd(3), tdisp_wk_Bord_gr_brd(3)     
      real(kind=8) 		:: tdisp_wk_ar_gr_brd(3),tdisp_wk_wka_gr_brd(3)
      real(kind=8) 		:: tdisp_ar_ara_gr_brd(3), tdisp_ar_gr_gr_brd(3) 	
      real(kind=8) 		:: tdisp_wk_ir_gr_brd(3), tdisp_ir_ira_gr_brd(3)
      real(kind=8) 		:: tdisp_ir_gr_gr_brd(3), tdisp_wk_gr_gr_brd(3)    
      real(kind=8) 		:: angles_ir_gr_brd(3)
      real(kind=8) 		:: tdisp_wk_kf_wk_brd(3)
      real(kind=8) 		:: gamma_kipp_brd
	  
      real(kind=8) 		:: rvel_wk_ar_gr_brd(3),tvel_wk_ar_gr_brd(3)
      real(kind=8) 		:: rvel_Bord_gr_gr_brd(3), rvel_wk_gr_gr_brd(3)
      real(kind=8) 		:: tvel_wk_Bord_gr_brd(3), tvel_wk_gr_gr_brd(3)
      real(kind=8) 		:: tvel_Bord_gr_gr_brd(3),rvel_wk_Bord_gr_brd(3)
      real(kind=8) 		:: rvel_wk_ir_gr_brd(3), tvel_wk_ir_gr_brd(3)
	  
	  ! ##### Variablen Summenkräfte #####	  
      real(kind=8)  	:: NForce_brd(2,3), NMoment_brd(2,3)
      real(kind=8)  	:: DForce_brd(2,3), DMoment_brd(2,3)
      real(kind=8)  	:: TForce_brd(2,3), TMoment_brd(2,3)
      real(kind=8) 		:: sumNForce_brd(3), sumNMoment_brd(3)
      real(kind=8) 		:: sumDForce_brd(3), sumDMoment_brd(3)
      real(kind=8) 		:: sumTForce_brd(3), sumTMoment_brd(3)
      real(kind=8) 		:: result_brd(6)	  
	  ! PWI Test Borddaempfung
	  real(kind=8)  	:: DForce_tvel_brd(2,3), DForce_rvel_brd(2,3)
	  real(kind=8) 		:: sumDForce_tvel_brd(3), sumDForce_rvel_brd(3)
	  real(kind=8) 		:: vnorm_tvel_brd(3), vnorm_rvel_brd(3)
	  !----------------------------------------------------------------------------------
	  ! Taschenfederkäfig
	  integer					:: ID_Tasche
	  real(kind=8)				:: taschenspiel_tang, taschenspiel_rad, taschenspiel_ax
	  real(kind=8) 				:: result_tfk(6), result_tfk_kf(3)
	  real(kind=8) 				:: tdisp_122(3), tvel_122(3)
		  
	  !----------------------------------------------------------------------------------
	  !Kontakt Structure
		integer             :: id_ar
        integer             :: id_lbcone
        integer             :: id_ir
        integer             :: id_lb
        integer             :: id_wk
        integer             :: id_wka
        integer             :: id_gr
        integer             :: id_wko

        real(kind=8)        :: E_dash
        real(kind=8)        :: wk_mat
        real(kind=8)        :: ar_mat
        real(kind=8)        :: ir_mat
        real(kind=8)        :: k1_tripp
        real(kind=8)        :: k2_tripp

        !real(kind=8)        :: tdisp_wk_lb_gr(3)

        real(kind=8)        :: NForceSum(3)
        real(kind=8)        :: NMomentSum(3)
        real(kind=8)        :: NForceSum_mag
        real(kind=8)        :: DForceSum(3)
        real(kind=8)        :: DMomentSum(3)
        real(kind=8)        :: NDForceSum(3)
        real(kind=8)        :: NDForceSum_mag
        real(kind=8)        :: TForceSum(3)
        real(kind=8)        :: TMomentSum(3)
		
		!real(kind=8)                :: rad_AR, rad_IR
		real(kind=8),allocatable    :: p_slce(:), b_slce(:), h0(:)
		real(kind=8)                :: ctpoint_glob(int(no_slce_LB),3)                !new from ADAMS
	 !------------------------------------------------------------------
	 !notwendige Variablen zur automatisierten Ausgabe
	 !------------------------------------------------------------------
	 
	  logical 							:: ordnerabfrage
	  integer, save 					:: ausgabeindex, ausgabe
	  
      integer							:: err
	  integer 							:: meas_flg

	  logical                           :: iflag 	            ! Logische Variable: beim 1. Aufrufen der GForce ist
										                        ! iflag = 1, danach iflag = 0

	  integer, save 					:: hilfs_iflag 			! Die Erstellung der hilfs_iflag Variable ist notwendig um einmalige Aktionen auszuführen
	  data hilfs_iflag/1/ 					    				! Ein data-statement wird nur einmalig ausgeführt
		
	  integer, save 					:: lager_iflag, lagernr
	  data lager_iflag/1/
		 
	  integer 							:: i_wk
	  integer 							:: date_time(8)
	  integer, save 					:: anz_parallelSim_int
	  integer, save 					:: force_element_name_length, marker_element_name_length
				
	  integer, save 					:: dateinr, modelltyp, l_ausgabepfad		!
						
	  integer 							:: ID_Aussen, ID_Innen, ID_Waelz, ID_Waelz_A, ID_Ground 
      double precision					:: temptdisp_ascii
	  
	  
	 ! ##### Auslesefelder aus Simpack #####
	  double precision 					:: verschiebung211(3),verkippung211(3),kraft255(3),moment255(3)
	  double precision 					:: kraft155(3),moment155(3),kraft455(3),moment455(3)
	  
	  double precision 					:: tdisp155(3), tdisp255(3), tdisp455(3), tdisp311_ausgabe(3)		! Verlagerung bzgl. GR von AR,IR,KF
	  double precision 					:: rdisp355(3), rdisp455(3), rdisp155(3), rdisp255(3)				! Rotationsgeschw. bzgl. GR von AR,IR,KF
	  character(len=100),save			:: lager_vektor(10)  
	                                     ! max. 10 Lager pro System (Annahme)
		 
	 ! ##### Ausgabevariablen #####
	  character(len=10) 				:: date_time_b(3)
	  character(len=300),save			:: ausgabepfad, ausgabepfad_anfang, ausgabepfad_ende
	  character(len=10) ,save			:: datum, uhrzeit
	  character(len=300),save			:: path, folder
	  character(len=100),save			:: anz_parallelSim_char
	  character(len=300),save			:: makedirectory
	  character(len=100),save			:: force_element_name, marker_element_name
	 
	 ! Start TFR 19.04.13
		
	  character(len=100),save			:: jointARname, jointIRname
	  integer 							:: ID_JointAR, ID_JointIR
	  double precision 					:: CFJointAR(8), CFJointIR(8)
	  double precision 					:: nAR, nIR
	  double precision 					:: nKF_ideal, nWK_ideal
		 
	 ! ##### Flexibler Aussenring #####
	  integer							:: FlexARmod, id_Flex
	  double precision					:: NForce_IR_GR(3), NForce_IR_GR_mag
	  double precision					:: FE_NForceFlex, psiFlex, gammaFlex
	
	  !New from WK_LB_Gfosub
	  integer, parameter::        no_nodes = 40
	  real(8)::                   theta(3*no_nodes), rad(3*no_nodes), delta_ax(3*no_nodes), theta_wk
	  integer::                   id_array(3*no_nodes)
	  real(8)::                   b_rad(3*no_nodes), c_rad(3*no_nodes), d_rad(3*no_nodes)
	  real(8)::                   b_ax(3*no_nodes), c_ax(3*no_nodes), d_ax(3*no_nodes)
	  real(8)::                   ispline
	  real(8)::                   tdisp(no_nodes,3), tdisp_wk(3)
	  real(8)::                   AR_LFB_delta_rad_flex, AR_LFB_delta_ax_flex
C ----------------------------------------------------------------------
C Initialization
C ----------------------------------------------------------------------

      ierr = 0
	  
	  par_Kon(1)	= par(1) 
	  par_Kon(2)	= par(2)
	  par_Kon(3)	= par(3)
	  par_Kon(4)	= par(4)
	  par_Kon(5)	= par(5)
	  par_Kon(6)	= 0
	  par_Kon(8)	= 0
	  par_Kon(9)	= 0
	  
	  
	  !Belegen der globalen Parameter
	  beartype=40				!!!40 - Penderollenlager
	  no_bear=1
	  no_wk=1						
	  
	  
	  ctloc	  = int(par(6))						
	! par(6) Kontaktlocation    !ctloc = 1 Kontakt Außenring // 
								!ctloc = 2 Kontakt Innenring // 
								!ctloc = 4 Kontakt Bord //
							
		
	  !Geometriedaten------------------------------------------------------
	  !##Außen-Ring Innen-Ring##
	  prorad_AR = (par(7))			
	  rad_AR = (par(8))                    !rad-AR heißt eigentlich "lfb_rad_AR" 
	  AR_breite = (par(9))
	  AR_dicke = (par(10)) 

	  prorad_IR = (par(11)) 
	  rad_IR = (par(12))                   !rad-IR heißt eigentlich "lfb_rad_IR" 
	  IR_breite= (par(13)) 

	  !AR_MANTEL_DRM=(par(85))
	 
	  !für Modul calc_wkpro-----------------------------------------------

	  	
	  wk_l=			(par(14))
	  wk_rad=		(par(15))		
	  				  
	  !lb_winkel=	(par())			
	 					
	  wk_protype=	int(par(16))
	  wk_pro_rad=	(par(17))				
	  wk_pro_ap=	(par(18))			
	  wk_pro_cp=	(par(19))			
	  wk_pro_dp=	(par(20))			
	  wk_pro_kp=	(par(21))			
	  wk_pro_rk=	(par(22))			
	  
	  !!! rad_IR=(par(14))/2.0d0-((par(15))/2.0d0)*cosd(par(17))	???sind diese 2 Definitionen richtig???
	  !!! rad_AR=(par(14))/2.0d0+((par(15))/2.0d0)*cosd(par(17))	

	  !für Modul calc_data-----------------------------------------------	  
	  ir_youngs=	(par(23))   !IR_E
	  ir_poisson=	(par(24))   !IR_V
	  
	  ar_youngs=	(par(25))   !AR_E
	  ar_poisson=	(par(26))   !AR_V
  
	  wk_youngs=	(par(27))   !WK_E
	  wk_poisson=	(par(28))   !WK_V
	  

	  
	  wk_nr =mod(id,1000)
	  bear_nr = id/10000
	  
	  !Schmierstoffdaten und Reibungsmodelle
	  frctn_mod		= int(par(29))     		! Festkörperreibmodell:	
											! 1 - linear  
											! 2 - kubisch
											! 3 - arctan
	  frctnEHD_mod 	= int(par(30))     		! EHD-Reibung:  
											! 0 - aus
											! 1 - an (mit Festkörperreibung aus race_f_mod und Hysteresereibung
	  !lub_mod 		= int(par())   		! Schmierstoffbeschreibung:	
											! 1 - FVA 400
											! 2 - Teutsch
	  !tau_mod 		= int(par()) 			! Modell zur Berechnung der Schubspannung im Schmierstoff
											! 1 - Ree-Eyring-Modell (maximale Schubspannung unbegrenzt)
											! 2 - Bair-Winer-Modell (Schubspannung begrenzt)
											! 3 - Modell nach Fassbender (ohne Gleitanteil)
	  filmT_mod 	= int(par(31)) 			! Modell für thermischen Korrekturfaktor
											! 1 - Modell nach Murch und Wilson (bei geringem Gleitanteil)
											! 2 - Modell nach Zhu und Cheng (mit/ohne Gleitanteil)
	  comprT_mod	= int(par(32))    		! Modell zur Berücksichtigung der Schmierstofferw?rmung aufgrund von Kompression
											! 1 - Modell nach Gold
											! 2 - Modell nach Dicke				
	  vel_s   		= (par(33)) 			! Schlupfgeschwindigkeit bei mu_s
	  vel_d			= (par(34)) 			! Schlupfgeschwindigkeit bei mu_d
	  mu_s			= (par(35)) 			! mu statisch
	  mu_d			= (par(36)) 			! mu dynamisch
	  LubTemp		= (par(37)) 			! Temperatur des verwendeten Öles [癈]
	  etaZero   	= (par(38)) 			! Dynamische Viskosit?t bei lubTmp [MPas]
	  alphaT		= (par(39)) 			! Temperaturviskosit?tskoeffizient [1/K]
	  alphaP		= (par(40)) 			! Variable nicht definiert und Verwendung ist unklar ! Druckviskosit?tskoeffizient [1/MPa]
	  lambdaZero 	= (par(41)) 			! Wärmeleitfähigkeit [W/mK]
	  alphaLambda 	= (par(42)) 			! Wärmeleitfähigkeitskoeffizient [W/mK^2]		
	  C1          	= (par(43)) 			! Konstante zur Bestimmung der Temperatur im Kontaktbereich [bar/K^2]
	  C2			= (par(44)) 			! Konstante zur Bestimmung der Temperatur im Kontaktbereich [bar/K^2]
	  AV			= (par(45)) 			! Rodermund T-p-Viskositätsparameter [mPas]
	  BV			= (par(46)) 			! Rodermund T-p-Viskositätsparameter [癈]
	  CV		 	= (par(47)) 			! Rodermund T-p-Viskositätsparameter [癈]
	  DR			= (par(48)) 			! Rodermund T-p-Viskositätsparameter [-]
	  ER 			= (par(49)) 			! Rodermund T-p-Viskositätsparameter [-]
	  pR			= (par(50)) 			! Rodermund T-p-Viskositätsparameter [bar]
	  B				= (par(51))				! [癈] Parameter B, C, K zur Berechnung der Viskosit?t bei _lubT und Umgebungsdruck
	  C				= (par(52))				! [癈]
	  K				= (par(53))				! [mPas]
	  a1			= (par(54))				! [bar] Parameter a1, a2, b1, b2 zur Berechnung der Viskosit?t bei _lubT und Kontaktdruck und des Druck-Viskosit?tskoeffizienten
	  a2			= (par(55))				! [bar/°C]
	  b1			= (par(56))				! [-]
	  b2			= (par(57))				! [1/°C]
	  rhoZero		= (par(58)) 			! [g/ml] Dichte bei 15°C
	  tauRheo		= (par(59)) 			! [MPa] Spannung im Ree-Eyring- bzw. Bair-Winer-Modell
	  sigma			= (par(60))				! Kombinierte Standardabweichung der Rauheiten beider Kontaktk?rper [mm]
	  BZH			= (par(61))				! Konstante zur Mischreibungsberechnung nach Zhou und Hoeprich (Lasttraganteil) [-]
	  CZH			= (par(62))				! Konstante zur Mischreibungsberechnung nach Zhou und Hoeprich (Lasttraganteil) [-]
	  alphaHys		= (par(63)) 			! Hysterese-Verlustfaktor (bei trockener W?lzreibung) - wird nur in Kombination mit EHD berücksichtigt
	  scaleHys		= (par(64)) 			! Skalierungsfaktor für Skalierungsfunktion bei Hysteresereibung
	  cr			= (par(65)) 			! Rollwiderstandskoeffizient (Reynolds-Verlustmonent)
	  rexp			= (par(66)) 			! Rollwiderstandsexponent (Reynolds-Verlustmonent)			
	 
	 !für Modul WK_LB_Dämpfung----------------------------------------------	
	 d_mod 		    = int(par(67))    		! Dämpfungsmodell
											! 1 - Modell nach ---
											! 2 - Modell nach Dietl												
	 p_t			= (par(68))		 		! Durchdringungstiefe max. für Materiald?mpfung
	 d_max			= (par(69)) 			! Maximale Dämpfung
	 K0				= (par(70))				! Dämpfungsparameter nach Dietl [(kg**-0,04)*(m**0,06)*(s**0,0115)]
	 KR				= (par(71))				! Dämpfungsparameter nach Dietl [-]
	 KL				= (par(72))				! Dämpfungsparameter nach Dietl [-]
	 KE				= (par(73))				! Dämpfungsparameter nach Dietl [-]
	 Keta			= (par(74))				! Dämpfungsparameter nach Dietl [-]
	 Kalpha			= (par(75))				! Dämpfungsparameter nach Dietl [-]
	 Kq				= (par(76))				! Dämpfungsparameter nach Dietl [-]
	 Ku				= (par(77))				! Dämpfungsparameter nach Dietl [-]
	 Kf				= (par(78))				! Dämpfungsparameter nach Dietl [-]
	 fe				= (par(79))				! Dämpfungsparameter nach Dietl [-]	 

	 !num. Parameter für Kontakt---------------------------------------------	
	 ct_mod   		= int(par(81))		!Kontaktberechnungsmethode  ##Kontakte Type Laufbahn##
										!Kontaktmodell für WK-LB-Kontakt
										!1: Scheibenmodell AST basierend auf Trippschen Formeln (gegenseitige Beeinflussung der Scheiben, h鯿hste Genauigkeit)
										!2: Scheibenmodell Tripp approximiert (ohne gegenseitige Beeinflussung der Scheiben)
										!3: Scheibenmodell nach DIN ISO 281 (ohne gegenseitige Beeinflussung der Scheiben)
	 
	 no_gauss		= int(par(82))		! Gauss-Integration
	 cf_pnts		= int(par(83))
	 cf_p_max		= (par(84)) 
	 
	
	!PWI !eventuell später nochmals anpassen
	 !ar_dicke		=	(AR_MANTEL_DRM/2.0d0-(par(14))/2.0d0-(par(15))/2.0d0)*10.0d0	
	 !Faktor 10 für numerische Stabilit?t bei Losrollen der WK
	 
	 !durch call werden die MARKER IDS im Feld par_Kon als Pointer übergeben und damit der INTEGER des Pointers gespeichert
	 ct_id = ctloc + 2*(wk_nr-1) + 2*no_wk*(bear_nr-1)

	 ! ##### Marker ID's #####
	 id_ar   		= int(par(1))    		! Marker ID Aussenring Mitte
	 id_ir    		= int(par(2))	 		! Marker ID Innenring Mitte
	 id_wk    		= int(par(3))	 		! Marker ID Waelzkoerper Mitte
	 id_wka  		= int(par(4))    		! Marker ID Waelzkoerper-aussen
	 id_gr   		= int(par(5))    		! Marker ID Ground
	 
		if (ctloc==1) then
			id_lb          = id_ar
		elseif (ctloc==2) then
			id_lb          = id_ir
		!elseif (ctloc == 3 .OR. ctloc == 4)	then
		!	id_lb			= int(par(90))
		endif

		iflag = 1
	 
	 
! ----------------------------------------------------------------------
!                           Execution
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-------------------------Zustandsgrößen------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!Kontakt WK-Außenring:

	! Position RB.Centre bzgl. OuterRace.Centre im KOS OuterRace.Centre
	call SPCK_AV_DXYZ( temptdisp, tdisp_wk_ar_ar, id_wk, id_ar, id_ar, error)
	
	! Position, translatorische und rotatorische Geschwingigkeit von RB.Centre bzgl. OuterRace.Centre im KOS ground.Centre
	call SPCK_AV_DXYZ( temptdisp, tdisp_wk_ar_gr, id_wk, id_ar, id_gr, error)
	call SPCK_AV_VXYZ( v_abs, v_ptp, tvel_wk_ar_gr, id_wk, id_ar, id_gr, id_gr, ierr)
	call SPCK_AV_WXYZ( w_abs, rvel_wk_ar_dr, id_wk, id_ar, id_gr, ierr)
	
	! Rotatorische und tranlatorische Geschwingigkeit von OuterRace.Centre bzgl. ground.Centre im KOS ground.Centre
	call SPCK_AV_WXYZ( w_abs, rvel_ar_gr_gr, id_ar, id_gr, id_gr, ierr)
	call SPCK_AV_VXYZ( v_abs, v_ptp, tvel_ar_gr_gr, id_ar, id_gr, id_gr, id_gr, ierr)
	
	! Position von RB.Centre bzgl. RB.Aussen im KOS RB.Centre
	call SPCK_AV_DXYZ( temptdisp, tdisp_wk_wka_ar, id_wk, id_wka, id_ar, error)
	
	! Richtungskosinus zw. ground.Centre und OuterRace.Centre
	call SPCK_AV_ANGLE( angles_gr_ar, id_gr, id_ar, 3, error)
	! Erstellen der Rotationsmatrizen
	call SPCK_UF_Angle2TrMat( matTr_gr_ar, 3, angles_gr_ar, error)
	
	! Richtungskosinus zw. RB.Centre und OuterRace.Centre
	call SPCK_AV_ANGLE( angles_wk_ar, id_wk, id_ar, 3, error)
	! Erstellen der Rotationsmatrizen
	call SPCK_UF_Angle2TrMat( matTr_wk_ar, 3, angles_wk_ar, error)

!!!Ausgabe Werte Überprüfen!!!	
	open(1020,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tdisp_wk_ar_ar.out')
	write(1020,*) tdisp_wk_ar_ar

	open(1021,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tdisp_wk_ar_gr.out')
	write(1021,*) tdisp_wk_ar_gr

	open(1022,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tvel_wk_ar_gr.out')
	write(1022,*) tvel_wk_ar_gr

	open(1023,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\rvel_wk_ar_dr.out')
	write(1023,*) rvel_wk_ar_dr

	open(1024,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\rvel_ar_gr_gr.out')
	write(1024,*) rvel_ar_gr_gr 

	open(1025,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tvel_ar_gr_gr.out')
	write(1025,*) tvel_ar_gr_gr

	open(1026,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tdisp_wk_wka_ar.out')
	write(1026,*) tdisp_wk_wka_ar

	open(1027,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\angles_gr_ar.out')
	write(1027,*) angles_gr_ar

	open(1028,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\matTr_gr_ar.out')
	write(1028,*) matTr_gr_ar

	open(1029,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\angles_wk_ar.out')
	write(1029,*) angles_wk_ar
	
	open(1030,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\matTr_wk_ar.out')
	write(1030,*) matTr_wk_ar

	
! Kontakt WK-Innenring:

	! Position RB.Centre bzgl. OuterRace.Centre im KOS InnerRace.Centre
	call SPCK_AV_DXYZ( temptdisp, tdisp_wk_ir_ir, id_wk, id_ir, id_ir, error)                  !tdisp322
	
	! Position, translatorische und rotatorische Geschwingigkeit von RB.Centre bzgl. InnerRace.Centre im KOS ground.Centre
	call SPCK_AV_DXYZ( temptdisp, tdisp_wk_ir_gr, id_wk, id_ir, id_gr, error)                  !tdisp325
	call SPCK_AV_VXYZ( v_abs, v_ptp, tvel_wk_ir_gr, id_wk, id_ir, id_gr, id_gr, ierr)          !tvel325
	call SPCK_AV_WXYZ( w_abs, rvel_wk_ir_gr, id_wk, id_ir, id_gr, ierr)                        !revl325

	! Rotatorische und tranlatorische Geschwingigkeit von InnerRace.Centre bzgl. ground.Centre im KOS ground.Centre
	call SPCK_AV_WXYZ( w_abs, rvel_ir_gr_gr, id_ir, id_gr, id_gr, ierr)                        
	call SPCK_AV_VXYZ( v_abs, v_ptp, tvel_ir_gr_gr, id_ir, id_gr, id_gr, id_gr, ierr)
	
	! Position von RB.Centre bzgl. RB.Aussen im KOS LB.Centre
	call SPCK_AV_DXYZ( temptdisp, tdisp_wk_wka_ir, id_wk, id_wka, id_ir, error)
	
	! Richtungskosinus zw. ground.Centre und InnerRace.Centre
	call SPCK_AV_ANGLE( angles_gr_ir, id_gr, id_ir, 3, error)
	
	! Erstellen der Rotationsmatrizen
	call SPCK_UF_Angle2TrMat( matTr_gr_ir, 3, angles_gr_ir, error)
    
    ! Richtungskosinus zw. RB.Centre und InnerRace.Centre
	call SPCK_AV_ANGLE( angles_wk_ir, id_wk, id_ir, 3, error)
	! Erstellen der Rotationsmatrizen
	call SPCK_UF_Angle2TrMat( matTr_wk_ir, 3, angles_wk_ir, error)
	! Position von RB.Centre bzgl. RB.Aussen im KOS ground.Centre
	call SPCK_AV_DXYZ( temptdisp, tdisp_wk_wka_gr, id_wk, id_wka, id_gr,error)
	! Rotatorische Geschwingigkeit von RB.Centre bzgl. ground.Centre im KOS ground.Centre
	call SPCK_AV_WXYZ( w_abs, rvel_wk_gr_gr, id_wk, id_gr, id_gr, ierr)
	! Translatorische Geschwingigkeit von RB.Centre bzgl. ground.Centre im KOS ground.Centre
	call SPCK_AV_VXYZ( v_abs, v_ptp, tvel_wk_gr_gr, id_wk, id_gr, id_gr, id_gr, ierr)
	! Richtungskosinus zw. RB.Centre und InnerRace.Centre
	call SPCK_AV_ANGLE( angles_gr_wk, id_gr, id_wk, 3, error)
	! Erstellen der Rotationsmatrizen
	call SPCK_UF_Angle2TrMat( matTr_gr_wk, 3, angles_gr_wk, error) 

!!!Ausgabe Werte Überprüfen!!!		
	open(1031,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tdisp_wk_ir_ir.out')
	write(1031,*) tdisp_wk_ir_ir

	open(1032,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tdisp_wk_ir_gr.out')
	write(1032,*) tdisp_wk_ir_gr

	open(1033,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tvel_wk_ir_gr.out')
	write(1033,*) tvel_wk_ir_gr

	open(1034,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\rvel_wk_ir_gr.out')
	write(1034,*) rvel_wk_ir_gr

	open(1035,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\rvel_ir_gr_gr.out')
	write(1035,*) rvel_ir_gr_gr

	open(1036,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tvel_ir_gr_gr.out')
	write(1036,*) tvel_ir_gr_gr

	open(1037,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tdisp_wk_wka_ir.out')
	write(1037,*) tdisp_wk_wka_ir

	open(1038,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\angles_gr_ir.out')
	write(1038,*) angles_gr_ir

	open(1039,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\matTr_gr_ir.out')
	write(1039,*) matTr_gr_ir

	open(1040,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\angles_wk_ir.out')
	write(1040,*) angles_wk_ir

	open(1041,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\matTr_wk_ir.out')
	write(1041,*) matTr_wk_ir

	open(1042,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tdisp_wk_wka_gr.out')
	write(1042,*) tdisp_wk_wka_gr

	open(1043,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\rvel_wk_gr_gr.out')
	write(1043,*) rvel_wk_gr_gr

	open(1044,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\tvel_wk_gr_gr.out')
	write(1044,*) tvel_wk_gr_gr

	open(1045,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\angles_gr_wk.out')
	write(1045,*) angles_gr_wk

	open(1046,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\matTr_gr_wk.out')
	write(1046,*) matTr_gr_wk

!---------------------------------------------------------------------------------------------
!-------------------------------------- Wälzkörperprofil -------------------------------------
!---------------------------------------------------------------------------------------------

	if(iflag) then
	
		! Breite einer Scheibe
		slce_wdth = wk_l/no_slce_LB

		open(1049,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\slce_wdth.out')
		write(1049,*)slce_wdth

		! Abstand der Scheibe i vom Wälzkörpermittelpunkt
		do i=1,no_slce_LB
			distnce(i)=slce_wdth*(i-0.5)-wk_l/2
		enddo

		open(1049,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\distnce_ufroce.out')
		write(1049,*)distnce(:)

		

		! Wälzkörperprofil: wk_prorad(i) und wk_dpro_deta(i)
		call mod_ProfileDetect_mp_ProfileDetect(iflag, wk_protype, no_slce_LB,wk_rad,wk_l, wk_pro_rad,
     &                          distnce(1:no_slce_LB), wk_prorad(1:no_slce_LB),      
     &							wk_pro_ap, wk_pro_cp, wk_pro_dp, wk_pro_kp, wk_pro_rk,           
     &							wk_dpro_deta(1:no_slce_LB))
	
	endif

    !##check point 
	!!!Test, ob Ausgabe von der Funktion richtig gerechnet wurden. 
	open(1047,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\wk_prorad.out')
	write(1047,*) wk_prorad(:)                                                                    !!!soll ähnlich so groß wie rad_wk

	open(1048,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\wk_dpro_deta.out')
	write(1048,*) wk_dpro_deta(:)        
	 

!---------------------------------------------------------------------------------------------
!------------------- Reduzierter Elastizitätsmodul und reduzierter Radius --------------------
!---------------------------------------------------------------------------------------------

	if(iflag) then
	
		! WK-AR-Kontakt
		if(ctloc == 1) then
	
			! Reduzierter Elastizitätsmodul
			mat_WK      = (1.0d0-WK_v**2.0d0)/WK_E
			mat_AR      = (1.0d0-AR_v**2.0d0)/AR_E
			mat_c_WKAR  = (mat_WK+mat_AR)/2.0d0
			E_dash_WKAR = 1.0d0/mat_c_WKAR
	
			! Reduzierter Radius
			rho1x_WKAR(1:no_slce_LB) = 1.0d0/ wk_prorad(1:no_slce_LB)
			rho2x_WKAR               = -1.0d0/rad_AR
	
			! Für Hertz-Kontaktmodell in Kombination mit Wälzkörperkreisprofil
			if(ctctype_LB==4 .and. wk_protype==2) then
				rho1y_WKAR = 1.0d0/wk_pro_rad
				rho2y_WKAR = 1.0d0/prorad_AR
			! Näherung: Linienkontakt mit nicht profilierten Kontaktpartnern
			else
				rho1y_WKAR = 0.0d0
				rho2y_WKAR = 0.0d0
			endif
	
			rhox_WKAR(1:no_slce_LB) = rho1x_WKAR(1:no_slce_LB)+rho2x_WKAR
			rhoy_WKAR               = rho1y_WKAR+rho2y_WKAR
	
			if(abs(rad_AR)>0.0d0) then
				sum_rho_WKAR(1:no_slce_LB) = rhox_WKAR(1:no_slce_LB)+rhoy_WKAR
			else
				sum_rho_WKAR(1:no_slce_LB) = rho1x_WKAR(1:no_slce_LB)           ! Wälzkörper-Ebene Kontakt
			endif
			R_dash_WKAR(1:no_slce_LB) = 1.0d0/sum_rho_WKAR(1:no_slce_LB)
    
	!##check point##!
	open(1049,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\R_dash_WKAR.out')
	write(1049,*) R_dash_WKAR(:)  
	
		! WK-IR-Kontakt
		elseif(ctloc == 2) then
	
			! Reduzierter Elastizitätsmodul
			mat_WK      = (1.0d0-WK_v**2.0d0)/WK_E
			mat_IR      = (1.0d0-AR_v**2.0d0)/IR_E
			mat_c_WKIR  = (mat_WK+mat_IR)/2.0d0
			E_dash_WKIR = 1.0d0/mat_c_WKIR
	
			! Reduzierter Radius (Näherung: Linienkontakt mit nicht profilierten Kontaktpartnern)
			rho1x_WKIR(1:no_slce_LB) = 1.0d0/ wk_prorad(1:no_slce_LB)
			rho2x_WKIR               = 1.0d0/rad_IR
	
			! Für Hertz-Kontaktmodell in Kombination mit Wälzkörperkreisprofil
			if(ctctype_LB==4 .and. wk_protype==2) then
				rho1y_WKIR  = 1.0d0/wk_pro_rad
				rho2y_WKIR  = 1.0d0/prorad_IR
			else
				rho1y_WKIR  = 0.0d0
				rho2y_WKIR  = 0.0d0
			endif
	
			rhox_WKIR(1:no_slce_LB) = rho1x_WKIR(1:no_slce_LB)+rho2x_WKIR
			rhoy_WKIR               = rho1y_WKIR+rho2y_WKIR
	
			if(abs(rad_IR)>0.0d0) then
				sum_rho_WKIR(1:no_slce_LB) = rhox_WKIR(1:no_slce_LB)+rhoy_WKIR
			else
				sum_rho_WKIR(1:no_slce_LB) = rho1x_WKIR(1:no_slce_LB)           ! Wälzkörper-Ebene Kontakt
			endif
			R_dash_WKIR(1:no_slce_LB) = 1.0d0/sum_rho_WKIR(1:no_slce_LB)
	
		endif
	endif

    !##check point
	open(1050,file='C:\Users\Zewang\Documents\BA\CODE\Routine_PeRoLa\AusgabePRL\R_dash_WKIR.out')
	write(1050,*) R_dash_WKIR(:)  
	
		
    
	
!###new from WK_LB_Gfosub###

! Winkelstellung des  Wälzkörpers
    ! theta_wk = atan2d(tdisp_wk(1),tdisp_wk(2))
    ! Umrechnen auf Bereich 0-360�?�
    ! theta_wk = mod(theta_wk+720.0,360.0)

    ! Berechnen der Laufbahnradius�?�nderung bei der Winkelposition
    ! des  Wälzkörpers (theta_wk)
	!AR_LFB_delta_rad_flex = ispline(theta_wk, theta, rad, b_rad, c_rad, d_rad, 3*no_nodes) - 499.4959125

	! Berechnen der Laufbahnverformung in axiale Richtung
	! bei der Winkelposition des  Wälzkörpers (theta_wk)
	!AR_LFB_delta_ax_flex = ispline(theta_wk, theta, delta_ax, b_ax, c_ax, d_ax, 3*no_nodes)

	  !write(1,*) id, 'XXX'
	  !write(1,'(1001f18.8)') theta
	  !write(1,'(1001f18.8)') AR_LFB_delta_rad_flex
      !write(1,'(1001f18.8)') AR_LFB_delta_ax_flex
    
      AR_LFB_delta_rad_flex = 0.0d0
      AR_LFB_delta_ax_flex  = 0.0d0
 

!---------------------------------------------------------------------------------------------
!-------------------------------------- Kontakterkennung -------------------------------------
!---------------------------------------------------------------------------------------------

! Durchdringung, Kontaktnormale und Kontaktpunkt
! Wälzkörper - Außenring
      if(ctloc == 1) then
		! Berechung des Kipp-/Schräglaufwinkels des WK zur LB
		!call KippSchr(time, id, iflag, int(par(3)),int(par(1)),int(par(5)),tdisp_wk_ar_gr ,			
        ! &							beta_schr, gamma_kipp)
	
	      call mod_WK_LB_Kontakt_AR_mp_WK_LB_Kontakt_AR(id, time, ctloc, no_slce_LB,							 
     &					tdisp_wk_ar_gr, tdisp_wk_wka_gr, distnce(1:no_slce_LB),					 
     &						prorad_AR, wk_prorad(1:no_slce_LB), wk_pro_rad, AR_breite,	 
     &						    AR_LFB_delta_rad_flex, AR_LFB_delta_ax_flex,        
     &							penetrtn, ctnorm, ctpoint, ctpoint_glob)             
	
	! Wälzkörper - Innenring
	  elseif(ctloc == 2) then
		! Berechung des Kipp-/Schräglaufwinkels des WK zur LBKon
		!call KippSchr(time, id, iflag, int(par(3)),int(par(2)),int(par(5)),tdisp_wk_ir_gr,			
        !&					beta_schr, gamma_kipp)
	
	      call mod_WK_LB_Kontakt_IR_mp_WK_LB_Kontakt_IR(id, iflag, dflag,time,
     &	           ctloc, no_slce_LB, angles_wk_ir, angles_gr_wk, tdisp_wk_ir_ir,	
     &					tdisp_wk_ir_gr, tdisp_wk_wka_gr, tdisp_wk_wka_ir, distnce(1:no_slce_LB), rad_IR,                        
     &						prorad_IR,  wk_prorad(1:no_slce_LB), wk_pro_rad, wk_dpro_deta(1:no_slce_LB),          
     &							penetrtn, ctnorm, ctpoint, ctpoint_glob)
      endif


C ----------------------------------------------------------------------
C task = 0 : Determine force, torque and output values
C ----------------------------------------------------------------------

      if ( task .eq. 0 ) then
	  	 ! ---------------------------------------------------------------------------------------------
		 ! Gesamtkraft
		 !Output Parameter definieren 

         ! initialise outputs
         force(1:3)          = 0.0d0
         torque(1:3)         = 0.0d0
         ov(1:ov_dim)        = 0.0d0
         stdynd(1:stdyn_dim) = 0.0d0
		 
		 !W?lk?rper / Laufbahn - Ausgabe
		 if (ctloc == 1 .OR. ctloc == 2) then 		!Wälzkörper / Laufbahn Ausgabe
		 
			 force(1) = res(1)
			 force(2) = res(2)
			 force(3) = res(3)
			 torque(1)= res(4)/1.0d3
			 torque(2)= res(5)/1.0d3
			 torque(3)= res(6)/1.0d3 
		 
			 ov(1)=NForceSum(1)
			 ov(2)=NForceSum(2)
			 ov(3)=NForceSum(3)
			 ov(4)=sqrt(NForceSum(1)**2+NForceSum(2)**2+NForceSum(3)**2)
			 ov(5)=DForceSum(1)
			 ov(6)=DForceSum(2)
			 ov(7)=DForceSum(3)
			 ov(8)=sqrt(DForceSum(1)**2+DForceSum(2)**2+DForceSum(3)**2 )
			 ov(9)=TForceSum(1)
			 ov(10)=TForceSum(2)
			 ov(11)=TForceSum(3)
			 ov(12)=sqrt(TForceSum(1)**2+TForceSum(2)**2+TForceSum(3)**2 )
			 ov(13)=TMomentSum(1)/1.0d3
			 ov(14)=TMomentSum(2)/1.0d3
			 ov(15)=TMomentSum(3)/1.0d3
			 ov(16)=sqrt(TMomentSum(1)**2+TMomentSum(2)**2+TMomentSum(3)**2)/1.0d3
			 !ov(17)=h0_th(mid_slce)*1.0d3 	!Schmierfilmhoehe [mum]
			 !ov(18)=maxval(pene)				!Durchdringung [mm]
			 !ov(19)=maxval(p_slce)			!Pressung [MPa]
			 
			 res(1:3) = matmul(res(1:3), temp33)	
			 res(1:3) = matmul(res(1:3), rot2)
			
			 !ov(34)=res(1)
			 !ov(35)=res(2)
			 !ov(36)=res(3)
			 
			 stdynd(1:stdyn_dim) = 0.0d0
			 	 
		 endif

C ----------------------------------------------------------------------
C task = 1 : Evaluate root functions
C ----------------------------------------------------------------------

      else if ( task .eq. 1 ) then

         ! initialise outputs
         valroot(1:stroot_dim)  = 0.0d0

C ----------------------------------------------------------------------
C task = 2 : Perform state reset after root state switch
C ----------------------------------------------------------------------

      else if ( task .eq. 2 ) then

         continue

C ----------------------------------------------------------------------
C task = 3 : Determine algebraic state residuals
C ----------------------------------------------------------------------

      else if ( task .eq. 3 ) then

         continue

C ----------------------------------------------------------------------
C task = 4 : Initialise states after calculation of consistent states
C ----------------------------------------------------------------------

      else if ( task .eq. 4 ) then

         continue

C ----------------------------------------------------------------------
C task = 5 : Initialisation (first call)
C ----------------------------------------------------------------------

      else if ( task .eq. 5 ) then
			
         continue

C ----------------------------------------------------------------------
C task = 6 : Finalisation (last call)
C ----------------------------------------------------------------------

      else if ( task .eq. 6 ) then

         continue

      end if

C ----------------------------------------------------------------------
C Error Handling and Return
C ----------------------------------------------------------------------


C ----------------------------------------------------------------------
C automatisierte Ausgabe
C ----------------------------------------------------------------------
	 !ausgabe 	0: keine Ausgabe - 1: Einzellager - 2: Lagersystem
	 
       if (ctloc == 1 .or. ctloc == 2) then
	     if (ausgabe == 1 .or. ausgabe == 2) then	

		 	i_wk = 0.0d0

			! Grundl. Parameter zur Erst. von Lastkollektiven für Workbench - Lager2
			! --> Lebensdauerberechnung
			!	Variante	|			Vorgabe			|	Berechnung				|
			!		1		| Verschiebung, Verkippung	| Kraft, Moment				|
			!		2		| Verkippung, Kraft			| Moment, Verschiebung		|
			!		3		| Verschiebung, Moment		| Kraft, Verkippung			|
			!		4		| Kraft, Moment				| Verschiebung, Verkippung	|
			!		5		| DZ, Verkippung, FX, FY	| FZ, Moment, DX, DY		|
			
			verschiebung211 = 0.0d0
			verkippung211	= 0.0d0
			kraft255		= 0.0d0
			moment255		= 0.0d0
			kraft155		= 0.0d0
			moment155		= 0.0d0
			kraft455		= 0.0d0
			moment455		= 0.0d0

		! Verschiebung/Verkippung IR zu AR
		call SPCK_AV_DXYZ( temptdisp_ascii, verschiebung211, ID_Innen, ID_Aussen, ID_Aussen, err)
		call SPCK_AV_ANGLE( verkippung211, ID_Innen, ID_Aussen, ID_Aussen, err)
		! Kraft/Moment auf IR
		call SPCK_AV_FXYZ( temptdisp_ascii, kraft255, ID_Innen, ID_Ground, ID_Ground, err)
		call SPCK_AV_TXYZ( temptdisp_ascii, moment255, ID_Innen, ID_Ground, ID_Ground, err)
		! Kraft/Moment auf AR
		call SPCK_AV_FXYZ( temptdisp_ascii, kraft155, ID_Aussen, ID_Ground, ID_Ground, err)
		call SPCK_AV_TXYZ( temptdisp_ascii, moment155, ID_Aussen, ID_Ground, ID_Ground, err)

		
		! Bestimmen der Verlagerung/Rotationsgeschw. von AR,IR,KF bzgl. GR
		tdisp155 = 0.0d0
		tdisp255 = 0.0d0
		tdisp455 = 0.0d0
		rdisp355 = 0.0d0
		rdisp455 = 0.0d0
		rdisp155 = 0.0d0
		rdisp255 = 0.0d0
		tdisp311_ausgabe = 0.0d0
	
		call SPCK_AV_DXYZ( temptdisp_ascii, tdisp155, ID_Aussen, ID_Ground, ID_Ground, err)
		call SPCK_AV_DXYZ( temptdisp_ascii, tdisp255, ID_Innen, ID_Ground, ID_Ground, err)		    
		call SPCK_AV_WXYZ( temptdisp_ascii, rdisp355, ID_Waelz, ID_Ground, ID_Ground, err)
		call SPCK_AV_WXYZ( temptdisp_ascii, rdisp155, ID_Aussen, ID_Ground, ID_Ground, err)
		call SPCK_AV_WXYZ( temptdisp_ascii, rdisp255, ID_Innen, ID_Ground, ID_Ground, err)
		call SPCK_AV_DXYZ( temptdisp_ascii, tdisp311_ausgabe, ID_Waelz, ID_Aussen, ID_Aussen, err)
		
		call SPCK_SLV_MEASFLAG( meas_flg , ierr )
			! Erklaerung zur Integer-Variable meas_flg:
			!   Equations of motion are evaluated for
			!     meas_flg = 0 : non-time domain analyses (pre-processing, linearisation, etc.)
			!     meas_flg = 1 : time domain analyses (time integration, static equilibrium by time integration, etc.)
			!     meas_flg = 2 : measurements
			! hilfs_iflag = 1
			
					if (meas_flg == 0) then
					if (hilfs_iflag == 1) then
						call date_and_time(date_time_b(1), date_time_b(2), date_time_b(3), date_time)
							! Erklaerung zu den Variablen der Zeiterfassung:
							! integer - Variablen:  
							! 	date_time(1)   : Jahr
							!	date_time(2)   : Monat
							!	date_time(3)   : Tag
							!	date_time(4)   : Zeitdifferenz in min
							!	date_time(5)   : Stunde
							!   date_time(6)   : Minute
							!   date_time(7)   : Sekunde
							!   date_time(8)   : Millisekunde
							! char - Variablen:  
							!   date_time_b(1) : Datum
							!   date_time_b(2) : Zeit
							!   date_time_b(3) : Zeitzone
						datum = date_time_b(1)
						uhrzeit = date_time_b(2)


						anz_parallelSim_int = 1.0     ! Number of inovocations of Routine
                        write(anz_parallelSim_char,'(I5.1)') anz_parallelSim_int
						anz_parallelSim_char = adjustl(anz_parallelSim_char)
			
			
					! Festlegen/Erstellen des Ausgabeordners

						

! *****************************************************************
! *** Automatische PFADANGABE
! *****************************************************************
                    call SPCK_UF_OutputPath(ausgabepfad, l_ausgabepfad, err)
					
9887				ausgabeindex = index(ausgabepfad,'/')
					if (ausgabeindex /= 0) then	 
						ausgabepfad_ende = ausgabepfad((ausgabeindex+1):len(ausgabepfad))
						ausgabepfad_anfang = ausgabepfad(1:ausgabeindex-1)
						ausgabepfad = trim(ausgabepfad_anfang)//'\\'//trim(ausgabepfad_ende)
						goto 9887
					endif
					!ausgabepfad = ausgabepfad
					path = trim(ausgabepfad)
					
					folder = trim(datum) //'_'// trim(uhrzeit(1:4))
					makedirectory = 'mkdir -p ' // trim(path) // trim(folder) 
					call system(makedirectory)
					
					! Erhöhung der Simulationsnr. um eins, falls es parallel laufende Simulationen gibt (gleicher hh:mm-Ordner)
9901				inquire(file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)// 
     &   '__Simulationsbeginn.txt', EXIST=ordnerabfrage)	
				    if(ordnerabfrage==.true.) then
						anz_parallelSim_int = anz_parallelSim_int + 1.0
						write(anz_parallelSim_char,'(I5.1)') anz_parallelSim_int
						anz_parallelSim_char = adjustl(anz_parallelSim_char)
						goto 9901
				    endif
					
					open(1030,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)// '__Simulationsbeginn.txt')
				    write(1030,*) 'Die zugehörige Simulation der in diesem Ordner hinterlegten Ergebnisdateien'
				    write(1030,*) 'wurde am ',datum(7:8),'.',datum(5:6),'.',datum(1:4),' um ',uhrzeit(1:2),':',
     &	uhrzeit(3:4),':',uhrzeit(5:6),' Uhr gestartet.'
				    write(1030,*) ' '
					
				endif		

				hilfs_iflag = hilfs_iflag + 1
			endif
			
		    if (meas_flg==2) then 
							
			! Über den Ground-Marker (ID_Ground) wird auf das zugehörige Lager geschlossen.
			! Hierfür wird anhand der ID der Markername in der Form $LAGERNAME.$M_GROUND/$M_Isys ausgelesen.
			! Anschließend wird der Name auf den LAGERNAMEN gekürzt.
			    call SPCK_AN_MARKERID2NAME(marker_element_name, marker_element_name_length, ID_Ground, err) 
			
			    if (ausgabe == 1) then		! Einzellager
					marker_element_name = trim(marker_element_name)
					marker_element_name_length = len_trim(marker_element_name)
					marker_element_name = marker_element_name(2:(marker_element_name_length))
				
			    else if (ausgabe == 2) then	! Lagersystem
					marker_element_name = trim(marker_element_name)
					marker_element_name_length = len_trim(marker_element_name)
				    if (ctloc == 1 .or. ctloc == 2) then
					    marker_element_name = marker_element_name(2:(marker_element_name_length-8)) ! "Abzug von .$M_Isys" - daher 8
				    elseif (ctloc == 3 .or. ctloc == 4) then
					    marker_element_name = marker_element_name(2:(marker_element_name_length-10)) ! "Abzug von .$M_GROUND" - daher 10
				    endif
					marker_element_name_length = len_trim(marker_element_name)
			    endif
			
				lagernr = 1
			    if (ausgabe == 2) then
				    if(lager_iflag == 1) then
						lager_vektor(:) = ''
						lager_vektor(lagernr) = trim(marker_element_name)
						lager_iflag = lager_iflag + 1
				    endif
					! Erkl.: lager_vektor(1) = Aussen ID von Lager 1
					
9982				if (trim(marker_element_name) /= trim(lager_vektor(lagernr))) then
						lagernr = lagernr + 1
						if (trim(lager_vektor(lagernr)) == '') then
							lager_vektor(lagernr) = trim(marker_element_name)
						endif
						goto 9982
				    endif	
			    endif		
			
			    open(1235468,file= trim(path) // 'tmp.txt')
			    write(1235468,*) time , ' ', ID_Ground, ID_Aussen, lagernr, ' ', ctloc, lager_vektor
			
			    call SPCK_AN_FORCEID2NAME(force_element_name, force_element_name_length, id, err)
			    if (ausgabe == 1) then	 	! Einzellager		
					force_element_name = trim(force_element_name)
					force_element_name_length = len_trim(force_element_name)
					force_element_name = force_element_name(2:(force_element_name_length))
			    else if (ausgabe == 2) then	! Lagersystem
					force_element_name = trim(force_element_name)
					force_element_name_length = len_trim(force_element_name)
					force_element_name = force_element_name((marker_element_name_length+4):(force_element_name_length))
			    endif
			
			    if (ausgabe == 1) then	 	! Einzellager		
					jointARname = '$J_Aussenring'
					jointIRname = '$J_Innenring'
			    else if (ausgabe == 2) then	! Lagersystem
					jointARname = '$'//trim(marker_element_name)//'.$J_Aussenring'
					jointIRname = '$'//trim(marker_element_name)//'.$J_Innenring'
			    endif
			
			    if (ctloc == 1) then
				    call SPCK_AN_JOINTNAME2ID(ID_JointAR, jointARname, err)
				! Constraint Forces of Joint - AR
				    call SPCK_AV_JOINTCF(CFJointAR, ID_JointAR, 0, 0, ID_Ground, err)
			    else if (ctloc == 2) then
				    call SPCK_AN_JOINTNAME2ID(ID_JointIR, jointIRname, err)
				! Constraint Forces of Joint - IR
				    call SPCK_AV_JOINTCF(CFJointIR, ID_JointIR, 0, 0, ID_Ground, err)
			    endif
		
		

			    open(7781,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_info.txt')
			    write(7781,*) time,' ',lagernr,' ',ctloc ,' ',trim(marker_element_name),' ',trim(force_element_name)
			
				dateinr = 2000.0d0 + lagernr*100.0d0
			
			    if (ctloc == 1) then
			
				    open(dateinr+1,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_displ_AR_GR_GR.txt')
				    write(dateinr+1,'(1001e18.6)') tdisp155
				    open(dateinr+8,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_Force_AR.txt')
				    write(dateinr+8,'(1001e18.6)') kraft155
				    open(dateinr+9,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_Torque_AR.txt')
				    write(dateinr+9,'(1001e18.6)') moment155
				
				    open(dateinr+15,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_CFJointAR.txt')
				    write(dateinr+15,'(1001e18.6)') CFJointAR
				! SPCK_AV_JOINTCF -> (1) Fmag, (2-4) Fx,Fy,Fz, (5) Tmag, (6-8) Tx,Ty,Tz
				
			    else if (ctloc == 2) then
			
				    open(dateinr+2,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_displ_IR_GR_GR.txt')
				    write(dateinr+2,'(1001e18.6)') tdisp255
				    open(dateinr+3,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_rotgeschw_IR_GR_GR.txt')
				    write(dateinr+3,'(1001e18.6)') rdisp255
				    open(dateinr+4,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_displ_IR_AR_AR.txt')
				    write(dateinr+4,'(1001e18.6)') verschiebung211
				    open(dateinr+5,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_kipp_IR_AR_AR.txt')
				    write(dateinr+5,'(1001e18.6)') verkippung211
				    open(dateinr+6,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_Force_IR.txt')
				    write(dateinr+6,'(1001e18.6)') kraft255
				    open(dateinr+7,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_Torque_IR.txt')
				    write(dateinr+7,'(1001e18.6)') moment255
				
				    open(dateinr+14,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)//'_RESULT_CFJointIR.txt')
				    write(dateinr+14,'(1001e18.6)') CFJointIR
				! SPCK_AV_JOINTCF -> (1) Fmag, (2-4) Fx,Fy,Fz, (5) Tmag, (6-8) Tx,Ty,Tz
					
			    endif
				
				nAR = rdisp155(3)
				nIR = rdisp255(3)
				nKF_ideal = (1.0d0-(2.0d0*wk_rad/(rad_AR+rad_IR)))*(nIR/2.0d0)+												
     &                      (1.0d0+(2.0d0*wk_rad/(rad_AR+rad_IR)))*(nAR/2.0d0)												! Für KeRoLa anpassen !PWI
				nWK_ideal = (((rad_AR+rad_IR)**2.0d0-4.0d0*wk_rad**2.0d0)/((rad_AR+rad_IR)*2.0d0*wk_rad))*((nAR-nIR)/2.0d0) ! Für KeRoLa anpassen !PWI
				
				dateinr = lagernr*100000 + ctloc*10000 + ID_Waelz*100
			! Erklärung: "einzigartige" Dateinummer über Zusammensetzung aus ctloc und ID_Waelz
	
			! Ausgabe der Verlagerung von AR,IR,KF bzgl. GR
			! Achtung: Die Datei wird für jeden WK neu beschrieben!
			    open(1040,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_RESULT_time.txt')
			    write(1040,'(1001e18.6)') time
							
			    open(dateinr+21,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)// '_RESULT_nWK_ideal.txt')
			    write(dateinr+21,'(1001e18.6)') nWK_ideal				
			
			    open(dateinr+22,file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'//
     &											trim(marker_element_name)// '_RESULT_nKF_ideal.txt')
			    write(dateinr+22,'(1001e18.6)') nKF_ideal
			
			    if (ctloc == 1 .OR. ctloc == 2) then
			
				    open((dateinr+1),file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'
     &											//trim(marker_element_name)//'_RESULT_pH_'//trim(force_element_name)//'.txt')
				    write((dateinr+1),'(1001e18.6)') p_slce
				
				    open((dateinr+2),file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'
     &											//trim(marker_element_name)//'_RESULT_bH_'//trim(force_element_name)//'.txt')
				    write((dateinr+2),'(1001e18.6)') b_slce
				    open((dateinr+3),file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'
     &											//trim(marker_element_name)//'_RESULT_distnce_'//trim(force_element_name)//'.txt')
				    write((dateinr+3),'(1001e18.6)') distnce
				    open((dateinr+4),file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'
     &											//trim(marker_element_name)//'_RESULT_h0_'//trim(force_element_name)//'.txt')
				    write((dateinr+4),'(1001e18.6)') h0(1:no_slce)
					
					
	!			    open((dateinr+8),file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'
    ! &											//trim(marker_element_name)//'_RESULT_pm_'//trim(force_element_name)//'.txt')
	!			    write((dateinr+8),'(1001e18.6)') ov(32)
				    open((dateinr+9),file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'
     &											//trim(marker_element_name)//'_RESULT_h0m_'//trim(force_element_name)//'.txt')
				    write((dateinr+9),'(1001e18.6)') ov(17)
				    open((dateinr+10),file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'
     &											//trim(marker_element_name)//'_RESULT_FN_abs_'//trim(force_element_name)//'.txt')
				    write((dateinr+10),'(1001e18.6)') ov(4)
			    else
				
				! Platzhalter für WK-Bord-Kontakt
				
			    endif
				
			    if (ctloc == 1) then
				    open((dateinr+6),file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'
     &											//trim(marker_element_name)//'_RESULT_rotgeschw_WK_GR_GR_'//trim(force_element_name)//'.txt')
				    write((dateinr+6),'(1001e18.6)') rdisp355
				    open((dateinr+7),file= trim(path) // trim(folder) // '\\_Sim' //trim(anz_parallelSim_char)//'_'
     &											//trim(marker_element_name)//'_RESULT_displ_WK_AR_AR_'//trim(force_element_name)//'.txt')
				    write((dateinr+7),'(1001e18.6)') tdisp311_ausgabe

			    endif
		    endif
		 endif
	   endif


! ----------------------------------------------------------------------
! Ende: autom. Ausgabe
! ----------------------------------------------------------------------	

      return

      end subroutine
