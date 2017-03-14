!***********************************************************************
!
! Function:  LAIRDEP_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:38 1999
!
! Description:
!  Lookup function for matching AIRDEP code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAIRDEP_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LAIRDEP_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LAIRDEP_DESC = 'DEPLOYED PROPERLY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LAIRDEP_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ND' ) THEN
        LAIRDEP_DESC = 'NOT DEPLOYED PROPERLY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LAIRDEP_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LAIRDEP_DESC()


!***********************************************************************
!
! Function:  LAIRDEP_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:38 1999
!
! Description:
!  Lookup function for matching AIRDEP description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAIRDEP_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LAIRDEP_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DEPLOYED PROPERLY' ) THEN
        LAIRDEP_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LAIRDEP_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT DEPLOYED PROPERLY' ) THEN
        LAIRDEP_CODE = 'ND'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LAIRDEP_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LAIRDEP_CODE()


!***********************************************************************
!
! Function:  LAIRMNT_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:38 1999
!
! Description:
!  Lookup function for matching AIRMNT code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAIRMNT_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LAIRMNT_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AP' ) THEN
        LAIRMNT_DESC = 'A PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AR' ) THEN
        LAIRMNT_DESC = 'ARM REST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BP' ) THEN
        LAIRMNT_DESC = 'B PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CH' ) THEN
        LAIRMNT_DESC = 'CHILD SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DM' ) THEN
        LAIRMNT_DESC = 'DASHPANEL - MID'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LAIRMNT_DESC = 'DASHPANEL - UNSPECIFIED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR' ) THEN
        LAIRMNT_DESC = 'DOOR PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DT' ) THEN
        LAIRMNT_DESC = 'DASHPANEL - TOP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HR' ) THEN
        LAIRMNT_DESC = 'HEAD REST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HS' ) THEN
        LAIRMNT_DESC = 'HEADER - SIDE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HW' ) THEN
        LAIRMNT_DESC = 'HEADER - WINDSHIELD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LAIRMNT_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LAIRMNT_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SB' ) THEN
        LAIRMNT_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SP' ) THEN
        LAIRMNT_DESC = 'SIDE PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SS' ) THEN
        LAIRMNT_DESC = 'SIDE WINDOW SILL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW' ) THEN
        LAIRMNT_DESC = 'STEERING WHEEL'
        RETURN
      END IF

      RETURN

      END

! end of LAIRMNT_DESC()


!***********************************************************************
!
! Function:  LAIRMNT_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:38 1999
!
! Description:
!  Lookup function for matching AIRMNT description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAIRMNT_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LAIRMNT_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'A PILLAR' ) THEN
        LAIRMNT_CODE = 'AP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ARM REST' ) THEN
        LAIRMNT_CODE = 'AR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B PILLAR' ) THEN
        LAIRMNT_CODE = 'BP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHILD SEAT' ) THEN
        LAIRMNT_CODE = 'CH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL - MID' ) THEN
        LAIRMNT_CODE = 'DM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL - TOP' ) THEN
        LAIRMNT_CODE = 'DT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL - UNSPECIFIED' ) THEN
        LAIRMNT_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR PANEL' ) THEN
        LAIRMNT_CODE = 'DR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST' ) THEN
        LAIRMNT_CODE = 'HR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - SIDE' ) THEN
        LAIRMNT_CODE = 'HS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - WINDSHIELD' ) THEN
        LAIRMNT_CODE = 'HW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LAIRMNT_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LAIRMNT_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LAIRMNT_CODE = 'SB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIDE PANEL' ) THEN
        LAIRMNT_CODE = 'SP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIDE WINDOW SILL' ) THEN
        LAIRMNT_CODE = 'SS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL' ) THEN
        LAIRMNT_CODE = 'SW'
        RETURN
      END IF

      RETURN

      END

! end of LAIRMNT_CODE()


!***********************************************************************
!
! Function:  LAIS_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:38 1999
!
! Description:
!  Lookup function for matching AIS code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAIS_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LAIS_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '0' ) THEN
        LAIS_DESC = 'NO INJURY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1' ) THEN
        LAIS_DESC = 'MINOR INJURY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2' ) THEN
        LAIS_DESC = 'MODERATE INJURY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3' ) THEN
        LAIS_DESC = 'SERIOUS INJURY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4' ) THEN
        LAIS_DESC = 'SEVERE INJURY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5' ) THEN
        LAIS_DESC = 'CRITICAL INJURY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6' ) THEN
        LAIS_DESC = 'MAXIMUM INJURY'
        RETURN
      END IF

      RETURN

      END

! end of LAIS_DESC()


!***********************************************************************
!
! Function:  LAIS_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:38 1999
!
! Description:
!  Lookup function for matching AIS description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAIS_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LAIS_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'CRITICAL INJURY' ) THEN
        LAIS_CODE = '5'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MAXIMUM INJURY' ) THEN
        LAIS_CODE = '6'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MINOR INJURY' ) THEN
        LAIS_CODE = '1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MODERATE INJURY' ) THEN
        LAIS_CODE = '2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NO INJURY' ) THEN
        LAIS_CODE = '0'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SERIOUS INJURY' ) THEN
        LAIS_CODE = '3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEVERE INJURY' ) THEN
        LAIS_CODE = '4'
        RETURN
      END IF

      RETURN

      END

! end of LAIS_CODE()


!***********************************************************************
!
! Function:  LAPLENG_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:38 1999
!
! Description:
!  Lookup function for matching APLENG code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAPLENG_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LAPLENG_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'DE' ) THEN
        LAPLENG_DESC = 'DIRECT ENGAGEMENT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LAPLENG_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LAPLENG_DESC = 'NO DIRECT ENGAGEMENT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LAPLENG_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LAPLENG_DESC()


!***********************************************************************
!
! Function:  LAPLENG_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:38 1999
!
! Description:
!  Lookup function for matching APLENG description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAPLENG_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LAPLENG_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DIRECT ENGAGEMENT' ) THEN
        LAPLENG_CODE = 'DE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NO DIRECT ENGAGEMENT' ) THEN
        LAPLENG_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LAPLENG_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LAPLENG_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LAPLENG_CODE()


!***********************************************************************
!
! Function:  LASPECT_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:38 1999
!
! Description:
!  Lookup function for matching ASPECT code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LASPECT_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LASPECT_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'A' ) THEN
        LASPECT_DESC = 'ANTERIOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'B' ) THEN
        LASPECT_DESC = 'BILATERAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'C' ) THEN
        LASPECT_DESC = 'CENTRAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'I' ) THEN
        LASPECT_DESC = 'INFERIOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'L' ) THEN
        LASPECT_DESC = 'LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'P' ) THEN
        LASPECT_DESC = 'POSTERIOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'R' ) THEN
        LASPECT_DESC = 'RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'S' ) THEN
        LASPECT_DESC = 'SUPERIOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W' ) THEN
        LASPECT_DESC = 'WHOLE REGION'
        RETURN
      END IF

      RETURN

      END

! end of LASPECT_DESC()


!***********************************************************************
!
! Function:  LASPECT_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching ASPECT description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LASPECT_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LASPECT_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'ANTERIOR' ) THEN
        LASPECT_CODE = 'A'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BILATERAL' ) THEN
        LASPECT_CODE = 'B'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CENTRAL' ) THEN
        LASPECT_CODE = 'C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'INFERIOR' ) THEN
        LASPECT_CODE = 'I'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEFT' ) THEN
        LASPECT_CODE = 'L'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'POSTERIOR' ) THEN
        LASPECT_CODE = 'P'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGHT' ) THEN
        LASPECT_CODE = 'R'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUPERIOR' ) THEN
        LASPECT_CODE = 'S'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WHOLE REGION' ) THEN
        LASPECT_CODE = 'W'
        RETURN
      END IF

      RETURN

      END

! end of LASPECT_CODE()


!***********************************************************************
!
! Function:  LAXIS_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching AXIS code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAXIS_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LAXIS_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LAXIS_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NR' ) THEN
        LAXIS_DESC = 'NORMAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LAXIS_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RS' ) THEN
        LAXIS_DESC = 'RESULTANT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'XG' ) THEN
        LAXIS_DESC = 'X - GLOBAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'XL' ) THEN
        LAXIS_DESC = 'X - LOCAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'YG' ) THEN
        LAXIS_DESC = 'Y - GLOBAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'YL' ) THEN
        LAXIS_DESC = 'Y - LOCAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ZG' ) THEN
        LAXIS_DESC = 'Z - GLOBAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ZL' ) THEN
        LAXIS_DESC = 'Z - LOCAL'
        RETURN
      END IF

      RETURN

      END

! end of LAXIS_DESC()


!***********************************************************************
!
! Function:  LAXIS_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching AXIS description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LAXIS_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LAXIS_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'NORMAL' ) THEN
        LAXIS_CODE = 'NR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LAXIS_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LAXIS_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RESULTANT' ) THEN
        LAXIS_CODE = 'RS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'X - GLOBAL' ) THEN
        LAXIS_CODE = 'XG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'X - LOCAL' ) THEN
        LAXIS_CODE = 'XL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'Y - GLOBAL' ) THEN
        LAXIS_CODE = 'YG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'Y - LOCAL' ) THEN
        LAXIS_CODE = 'YL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'Z - GLOBAL' ) THEN
        LAXIS_CODE = 'ZG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'Z - LOCAL' ) THEN
        LAXIS_CODE = 'ZL'
        RETURN
      END IF

      RETURN

      END

! end of LAXIS_CODE()


!***********************************************************************
!
! Function:  LBARRIG_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching BARRIG code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBARRIG_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LBARRIG_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'D' ) THEN
        LBARRIG_DESC = 'DEFORMABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'R' ) THEN
        LBARRIG_DESC = 'RIGID'
        RETURN
      END IF

      RETURN

      END

! end of LBARRIG_DESC()


!***********************************************************************
!
! Function:  LBARRIG_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching BARRIG description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBARRIG_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LBARRIG_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DEFORMABLE' ) THEN
        LBARRIG_CODE = 'D'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGID' ) THEN
        LBARRIG_CODE = 'R'
        RETURN
      END IF

      RETURN

      END

! end of LBARRIG_CODE()


!***********************************************************************
!
! Function:  LBARSHP_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching BARSHP code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBARSHP_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LBARSHP_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'BRL' ) THEN
        LBARSHP_DESC = 'BRIDGE RAIL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FAB' ) THEN
        LBARSHP_DESC = 'FLAT ANGLED BARRIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLB' ) THEN
        LBARSHP_DESC = 'FLAT BARRIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'GRL' ) THEN
        LBARSHP_DESC = 'GUARD RAIL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'GRT' ) THEN
        LBARSHP_DESC = 'GUARD RAIL TERMINAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'IAT' ) THEN
        LBARSHP_DESC = 'IMPACT ATTENUATOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB' ) THEN
        LBARSHP_DESC = 'LOAD CELL BARRIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LUM' ) THEN
        LBARSHP_DESC = 'LUMINARE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MBR' ) THEN
        LBARSHP_DESC = 'MEDIAN BARRIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LBARSHP_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'POL' ) THEN
        LBARSHP_DESC = 'POLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ROR' ) THEN
        LBARSHP_DESC = 'ROLLOVER RAMP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SGN' ) THEN
        LBARSHP_DESC = 'SIGN SUPPORT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LBARSHP_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LBARSHP_DESC()


!***********************************************************************
!
! Function:  LBARSHP_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching BARSHP description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBARSHP_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LBARSHP_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'BRIDGE RAIL' ) THEN
        LBARSHP_CODE = 'BRL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLAT ANGLED BARRIER' ) THEN
        LBARSHP_CODE = 'FAB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLAT BARRIER' ) THEN
        LBARSHP_CODE = 'FLB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GUARD RAIL' ) THEN
        LBARSHP_CODE = 'GRL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GUARD RAIL TERMINAL' ) THEN
        LBARSHP_CODE = 'GRT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IMPACT ATTENUATOR' ) THEN
        LBARSHP_CODE = 'IAT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL BARRIER' ) THEN
        LBARSHP_CODE = 'LCB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LUMINARE' ) THEN
        LBARSHP_CODE = 'LUM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MEDIAN BARRIER' ) THEN
        LBARSHP_CODE = 'MBR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LBARSHP_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'POLE' ) THEN
        LBARSHP_CODE = 'POL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROLLOVER RAMP' ) THEN
        LBARSHP_CODE = 'ROR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIGN SUPPORT' ) THEN
        LBARSHP_CODE = 'SGN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LBARSHP_CODE = 'UNK'
        RETURN
      END IF

      RETURN

      END

! end of LBARSHP_CODE()


!***********************************************************************
!
! Function:  LBMPENG_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching BMPENG code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBMPENG_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LBMPENG_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'DE' ) THEN
        LBMPENG_DESC = 'DIRECT ENGAGEMENT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LBMPENG_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LBMPENG_DESC = 'NOT DEFINED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OR' ) THEN
        LBMPENG_DESC = 'OVERRIDE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LBMPENG_DESC = 'UNKNOWN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UR' ) THEN
        LBMPENG_DESC = 'UNDERRIDE'
        RETURN
      END IF

      RETURN

      END

! end of LBMPENG_DESC()


!***********************************************************************
!
! Function:  LBMPENG_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching BMPENG description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBMPENG_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LBMPENG_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DIRECT ENGAGEMENT' ) THEN
        LBMPENG_CODE = 'DE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LBMPENG_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT DEFINED' ) THEN
        LBMPENG_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OVERRIDE' ) THEN
        LBMPENG_CODE = 'OR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNDERRIDE' ) THEN
        LBMPENG_CODE = 'UR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LBMPENG_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LBMPENG_CODE()


!***********************************************************************
!
! Function:  LBODY_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:39 1999
!
! Description:
!  Lookup function for matching BODY code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBODY_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LBODY_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '2C' ) THEN
        LBODY_DESC = 'TWO DOOR COUPE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2D' ) THEN
        LBODY_DESC = '2 DOOR CAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2S' ) THEN
        LBODY_DESC = 'TWO DOOR SEDAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3H' ) THEN
        LBODY_DESC = 'THREE DOOR HATCHBACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4D' ) THEN
        LBODY_DESC = '4 DOOR CAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4H' ) THEN
        LBODY_DESC = 'FOUR DOOR SEDAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4S' ) THEN
        LBODY_DESC = 'FOUR DOOR SEDAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5H' ) THEN
        LBODY_DESC = 'FIVE DOOR HATCHBACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BU' ) THEN
        LBODY_DESC = 'BUS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CV' ) THEN
        LBODY_DESC = 'CONVERTIBLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'EX' ) THEN
        LBODY_DESC = 'EXTENDED CAB PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LM' ) THEN
        LBODY_DESC = 'LIMOUSINE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MH' ) THEN
        LBODY_DESC = 'MOTOR HOME'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MV' ) THEN
        LBODY_DESC = 'MINIVAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LBODY_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LBODY_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PU' ) THEN
        LBODY_DESC = 'PICKUP TRUCK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW' ) THEN
        LBODY_DESC = 'STATION WAGON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TR' ) THEN
        LBODY_DESC = 'TRUCK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LBODY_DESC = 'UN-DEFINED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UV' ) THEN
        LBODY_DESC = 'UTILITY VEHICLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VN' ) THEN
        LBODY_DESC = 'VAN'
        RETURN
      END IF

      RETURN

      END

! end of LBODY_DESC()


!***********************************************************************
!
! Function:  LBODY_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:40 1999
!
! Description:
!  Lookup function for matching BODY description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBODY_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LBODY_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. '2 DOOR CAR' ) THEN
        LBODY_CODE = '2D'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '4 DOOR CAR' ) THEN
        LBODY_CODE = '4D'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BUS' ) THEN
        LBODY_CODE = 'BU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONVERTIBLE' ) THEN
        LBODY_CODE = 'CV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EXTENDED CAB PICKUP' ) THEN
        LBODY_CODE = 'EX'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FIVE DOOR HATCHBACK' ) THEN
        LBODY_CODE = '5H'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FOUR DOOR SEDAN' ) THEN
        LBODY_CODE = '4H'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LIMOUSINE' ) THEN
        LBODY_CODE = 'LM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MINIVAN' ) THEN
        LBODY_CODE = 'MV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MOTOR HOME' ) THEN
        LBODY_CODE = 'MH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LBODY_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LBODY_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PICKUP TRUCK' ) THEN
        LBODY_CODE = 'PU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STATION WAGON' ) THEN
        LBODY_CODE = 'SW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'THREE DOOR HATCHBACK' ) THEN
        LBODY_CODE = '3H'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRUCK' ) THEN
        LBODY_CODE = 'TR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TWO DOOR COUPE' ) THEN
        LBODY_CODE = '2C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TWO DOOR SEDAN' ) THEN
        LBODY_CODE = '2S'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UN-DEFINED' ) THEN
        LBODY_CODE = 'UN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UTILITY VEHICLE' ) THEN
        LBODY_CODE = 'UV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VAN' ) THEN
        LBODY_CODE = 'VN'
        RETURN
      END IF

      RETURN

      END

! end of LBODY_CODE()


!***********************************************************************
!
! Function:  LBODYRG_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:40 1999
!
! Description:
!  Lookup function for matching BODYRG code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBODYRG_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LBODYRG_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'A' ) THEN
        LBODYRG_DESC = 'UPPER ARM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'B' ) THEN
        LBODYRG_DESC = 'BACK/THORACO-LUMBAR SPINE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'C' ) THEN
        LBODYRG_DESC = 'CHEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'E' ) THEN
        LBODYRG_DESC = 'ELBOW'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'F' ) THEN
        LBODYRG_DESC = 'FACE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'H' ) THEN
        LBODYRG_DESC = 'HEAD/SKULL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'K' ) THEN
        LBODYRG_DESC = 'KNEE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'L' ) THEN
        LBODYRG_DESC = 'LOWER LEG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'M' ) THEN
        LBODYRG_DESC = 'ABDOMEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'N' ) THEN
        LBODYRG_DESC = 'NECK/CERVICAL SPINE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'O' ) THEN
        LBODYRG_DESC = 'WHOLE BODY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'P' ) THEN
        LBODYRG_DESC = 'PELVIC/HIP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'Q' ) THEN
        LBODYRG_DESC = 'ANKLE/FOOT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'R' ) THEN
        LBODYRG_DESC = 'FOREARM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'S' ) THEN
        LBODYRG_DESC = 'SHOULDER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'T' ) THEN
        LBODYRG_DESC = 'THIGH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W' ) THEN
        LBODYRG_DESC = 'WRIST/HAND'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'X' ) THEN
        LBODYRG_DESC = 'UPPER EXTREMITY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'Y' ) THEN
        LBODYRG_DESC = 'LOWER EXTREMITY'
        RETURN
      END IF

      RETURN

      END

! end of LBODYRG_DESC()


!***********************************************************************
!
! Function:  LBODYRG_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:40 1999
!
! Description:
!  Lookup function for matching BODYRG description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LBODYRG_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LBODYRG_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'ABDOMEN' ) THEN
        LBODYRG_CODE = 'M'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ANKLE/FOOT' ) THEN
        LBODYRG_CODE = 'Q'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BACK/THORACO-LUMBAR SPINE' ) THEN
        LBODYRG_CODE = 'B'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEST' ) THEN
        LBODYRG_CODE = 'C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELBOW' ) THEN
        LBODYRG_CODE = 'E'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FACE' ) THEN
        LBODYRG_CODE = 'F'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FOREARM' ) THEN
        LBODYRG_CODE = 'R'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD/SKULL' ) THEN
        LBODYRG_CODE = 'H'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE' ) THEN
        LBODYRG_CODE = 'K'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOWER EXTREMITY' ) THEN
        LBODYRG_CODE = 'Y'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOWER LEG' ) THEN
        LBODYRG_CODE = 'L'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NECK/CERVICAL SPINE' ) THEN
        LBODYRG_CODE = 'N'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIC/HIP' ) THEN
        LBODYRG_CODE = 'P'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER' ) THEN
        LBODYRG_CODE = 'S'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'THIGH' ) THEN
        LBODYRG_CODE = 'T'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UPPER ARM' ) THEN
        LBODYRG_CODE = 'A'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UPPER EXTREMITY' ) THEN
        LBODYRG_CODE = 'X'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WHOLE BODY' ) THEN
        LBODYRG_CODE = 'O'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WRIST/HAND' ) THEN
        LBODYRG_CODE = 'W'
        RETURN
      END IF

      RETURN

      END

! end of LBODYRG_CODE()


!***********************************************************************
!
! Function:  LCHSTAT_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:40 1999
!
! Description:
!  Lookup function for matching CHSTAT code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCHSTAT_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCHSTAT_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'P' ) THEN
        LCHSTAT_DESC = 'PRIMARY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'R' ) THEN
        LCHSTAT_DESC = 'REDUNDANT'
        RETURN
      END IF

      RETURN

      END

! end of LCHSTAT_DESC()


!***********************************************************************
!
! Function:  LCHSTAT_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:40 1999
!
! Description:
!  Lookup function for matching CHSTAT description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCHSTAT_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCHSTAT_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'PRIMARY' ) THEN
        LCHSTAT_CODE = 'P'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'REDUNDANT' ) THEN
        LCHSTAT_CODE = 'R'
        RETURN
      END IF

      RETURN

      END

! end of LCHSTAT_CODE()


!***********************************************************************
!
! Function:  LCMPNT_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:40 1999
!
! Description:
!  Lookup function for matching CMPNT code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCMPNT_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCMPNT_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'ABDO' ) THEN
        LCMPNT_DESC = 'ABDOMEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALC1' ) THEN
        LCMPNT_DESC = 'LAP BELT CENTER MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALC2' ) THEN
        LCMPNT_DESC = ' LAP BELT CENTER MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALCF' ) THEN
        LCMPNT_DESC = 'LAP BELT CENTER FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALCR' ) THEN
        LCMPNT_DESC = 'LAP BELT CENTER REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALL1' ) THEN
        LCMPNT_DESC = 'LAP BELT LEFT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALL2' ) THEN
        LCMPNT_DESC = 'LAP BELT LEFT MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALLF' ) THEN
        LCMPNT_DESC = 'LAP BELT LEFT FRONT DSP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALLR' ) THEN
        LCMPNT_DESC = 'LAP BELT LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALOT' ) THEN
        LCMPNT_DESC = 'LAP BELT OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALR1' ) THEN
        LCMPNT_DESC = 'LAP BELT RIGHT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALR2' ) THEN
        LCMPNT_DESC = 'LAP BELT RIGHT  MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALRF' ) THEN
        LCMPNT_DESC = 'LAP BELT RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ALRR' ) THEN
        LCMPNT_DESC = ' LAP BELT RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ANKL' ) THEN
        LCMPNT_DESC = 'ANKLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASC1' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT CENTER MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASC2' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT CENTER MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASCF' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT CENTER FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASCR' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT CENTER REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASL1' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT LEFT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASL2' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT LEFT MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASLF' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT LEFT FRONT DSP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASLR' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASOT' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASR1' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT RIGHT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASR2' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT RIGHT MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASRF' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ASRR' ) THEN
        LCMPNT_DESC = 'SHOULDER BELT RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BMPF' ) THEN
        LCMPNT_DESC = 'BUMPER FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BMPR' ) THEN
        LCMPNT_DESC = 'BUMPER REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHST' ) THEN
        LCMPNT_DESC = 'CHEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CRBV' ) THEN
        LCMPNT_DESC = 'CEREBOVASCULAR SYSTEM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CRDV' ) THEN
        LCMPNT_DESC = 'CARDIOVASCULAR SYSTEM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP01' ) THEN
        LCMPNT_DESC = 'DASH PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP02' ) THEN
        LCMPNT_DESC = 'DASH PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP03' ) THEN
        LCMPNT_DESC = 'DASH PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP04' ) THEN
        LCMPNT_DESC = 'DASH PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP05' ) THEN
        LCMPNT_DESC = 'DASH PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP06' ) THEN
        LCMPNT_DESC = 'DASH PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP07' ) THEN
        LCMPNT_DESC = 'DASH PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP08' ) THEN
        LCMPNT_DESC = 'DASH PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP09' ) THEN
        LCMPNT_DESC = 'DASH PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRLF' ) THEN
        LCMPNT_DESC = 'LEFT FRONT DOOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRLR' ) THEN
        LCMPNT_DESC = 'LEFT REAR DOOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DROT' ) THEN
        LCMPNT_DESC = 'DOOR   OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRRF' ) THEN
        LCMPNT_DESC = 'RIGHT FRONT  DOOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRRR' ) THEN
        LCMPNT_DESC = 'RIGHT REAR DOOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRSL' ) THEN
        LCMPNT_DESC = 'SLIDING DOOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DSLF' ) THEN
        LCMPNT_DESC = 'DOOR SILL LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DSLR' ) THEN
        LCMPNT_DESC = 'DOOR SILL LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DSRF' ) THEN
        LCMPNT_DESC = 'DOOR SILL RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DSRR' ) THEN
        LCMPNT_DESC = 'DOOR SILL RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ENGN' ) THEN
        LCMPNT_DESC = 'ENGINE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FASC' ) THEN
        LCMPNT_DESC = 'FASCIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FEMR' ) THEN
        LCMPNT_DESC = 'FEMUR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLLF' ) THEN
        LCMPNT_DESC = 'FLOORPAN  LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLLR' ) THEN
        LCMPNT_DESC = 'FLOORPAN  LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLOT' ) THEN
        LCMPNT_DESC = 'FLOORPAN  OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLRF' ) THEN
        LCMPNT_DESC = 'FLOORPAN  RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLRR' ) THEN
        LCMPNT_DESC = 'FLOORPAN  RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FNLF' ) THEN
        LCMPNT_DESC = 'FENDER LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FNLR' ) THEN
        LCMPNT_DESC = 'FENDER LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FNRF' ) THEN
        LCMPNT_DESC = 'FENDER RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FNRR' ) THEN
        LCMPNT_DESC = 'FENDER RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FOOT' ) THEN
        LCMPNT_DESC = 'FOOT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FROT' ) THEN
        LCMPNT_DESC = 'FRAME RAIL OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRRF' ) THEN
        LCMPNT_DESC = 'FRAME RAIL FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRRR' ) THEN
        LCMPNT_DESC = 'FRAME RAIL REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRSL' ) THEN
        LCMPNT_DESC = 'FRAME RAIL LEFT SIDE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRSR' ) THEN
        LCMPNT_DESC = 'FRAME RAIL RIGHT SIDE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FWLL' ) THEN
        LCMPNT_DESC = 'FRAME RAIL: FIREWALL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDFR' ) THEN
        LCMPNT_DESC = 'HEADER FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDLF' ) THEN
        LCMPNT_DESC = 'HEADER LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDLR' ) THEN
        LCMPNT_DESC = 'HEADER LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDRE' ) THEN
        LCMPNT_DESC = 'HEADER  REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDRF' ) THEN
        LCMPNT_DESC = 'HEADER RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDRR' ) THEN
        LCMPNT_DESC = 'HEADER RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HEAD' ) THEN
        LCMPNT_DESC = 'HEAD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HOOD' ) THEN
        LCMPNT_DESC = 'HEAD REST HOOD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRC1' ) THEN
        LCMPNT_DESC = 'HEAD REST CENTER MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRC2' ) THEN
        LCMPNT_DESC = 'HEAD REST CENTER MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRCR' ) THEN
        LCMPNT_DESC = 'HEAD REST CENTER REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRL1' ) THEN
        LCMPNT_DESC = 'HEAD REST LEFT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRL2' ) THEN
        LCMPNT_DESC = 'HEAD REST LEFT MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRLF' ) THEN
        LCMPNT_DESC = 'HEAD REST LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRLR' ) THEN
        LCMPNT_DESC = 'HEAD REST LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HROT' ) THEN
        LCMPNT_DESC = 'HEAD REST OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRR1' ) THEN
        LCMPNT_DESC = 'HEAD REST RIGHT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRR2' ) THEN
        LCMPNT_DESC = 'HEAD REST RIGHT MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRRF' ) THEN
        LCMPNT_DESC = 'HEAD REST RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HRRR' ) THEN
        LCMPNT_DESC = 'HEAD REST RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KNEE' ) THEN
        LCMPNT_DESC = 'KNEE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NECK' ) THEN
        LCMPNT_DESC = 'NECK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTHE' ) THEN
        LCMPNT_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PELV' ) THEN
        LCMPNT_DESC = 'PELVIS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PL2L' ) THEN
        LCMPNT_DESC = 'PILLAR MID ROW 2 LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PL2R' ) THEN
        LCMPNT_DESC = 'PILLAR MID ROW 2 RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLAL' ) THEN
        LCMPNT_DESC = 'A PILLAR LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLAR' ) THEN
        LCMPNT_DESC = 'A PILLAR RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLBL' ) THEN
        LCMPNT_DESC = 'B PILLAR LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLBR' ) THEN
        LCMPNT_DESC = 'B PILLAR RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLIR' ) THEN
        LCMPNT_DESC = 'PILLAR MID ROW 1 RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLRL' ) THEN
        LCMPNT_DESC = 'REAR PILLAR LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLRR' ) THEN
        LCMPNT_DESC = 'REAR PILLAR RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PULM' ) THEN
        LCMPNT_DESC = 'PULMONARY SYSTEM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RFEX' ) THEN
        LCMPNT_DESC = 'ROOF EXTERIOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RFIN' ) THEN
        LCMPNT_DESC = 'ROOF INTERIOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RIBS' ) THEN
        LCMPNT_DESC = 'RIB CAGE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ROLB' ) THEN
        LCMPNT_DESC = 'ROLLBAR   BRACE (LEFT OR RIGHT)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ROLL' ) THEN
        LCMPNT_DESC = 'ROLLBAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RRLF' ) THEN
        LCMPNT_DESC = 'ROOF RAIL LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RRLR' ) THEN
        LCMPNT_DESC = 'ROOF RAIL LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RRRF' ) THEN
        LCMPNT_DESC = 'ROOF RAIL RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RRRR' ) THEN
        LCMPNT_DESC = 'ROOF RAIL RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBBF' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBBR' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBC1' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE CENTER MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBC2' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE CENTER MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBCF' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE CENTER FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBCR' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE CENTER REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBL1' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE LEFT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBL2' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE LEFT MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBLF' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBLR' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE LEFT RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBM1' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBM2' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBOT' ) THEN
        LCMPNT_DESC = 'OTHER SEAT BASE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBR1' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE RIGHT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBR2' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE RIGHT MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBRF' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBRR' ) THEN
        LCMPNT_DESC = 'SEAT BASE ASSEMBLE RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEBF' ) THEN
        LCMPNT_DESC = 'SEAT BENCH FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEBK' ) THEN
        LCMPNT_DESC = 'SEAT  BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEBR' ) THEN
        LCMPNT_DESC = 'SEAT BENCH REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEC1' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET CENTER MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEC2' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET CENTER MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SECF' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET CENTER FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SECR' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET CENTER REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SECU' ) THEN
        LCMPNT_DESC = 'SEAT CUSHION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEL1' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET LEFT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEL2' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET LEFT MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SELF' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SELR' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEM1' ) THEN
        LCMPNT_DESC = 'SEAT BENCH MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEM2' ) THEN
        LCMPNT_DESC = 'SEAT BENCH MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEOT' ) THEN
        LCMPNT_DESC = 'OTHER SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SER1' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET RIGHT MID ROW 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SER2' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET RIGHT MID ROW 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SERF' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SERR' ) THEN
        LCMPNT_DESC = 'SEAT BUCKET RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SHLD' ) THEN
        LCMPNT_DESC = 'SHOULDER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SPNE' ) THEN
        LCMPNT_DESC = 'SPINE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STCL' ) THEN
        LCMPNT_DESC = 'STEERING ASSEMBLY COLUMN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STLF' ) THEN
        LCMPNT_DESC = ' SEAT TRACK LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STLR' ) THEN
        LCMPNT_DESC = ' SEAT TRACK LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STOT' ) THEN
        LCMPNT_DESC = 'OTHER SEAT TRACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STRF' ) THEN
        LCMPNT_DESC = ' SEAT TRACK RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STRN' ) THEN
        LCMPNT_DESC = 'STERNUM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STRR' ) THEN
        LCMPNT_DESC = ' SEAT TRACK RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SULF' ) THEN
        LCMPNT_DESC = 'SUSPENSION LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SULR' ) THEN
        LCMPNT_DESC = 'SUSPENSION LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SURF' ) THEN
        LCMPNT_DESC = 'SUSPENSION RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SURR' ) THEN
        LCMPNT_DESC = 'SUSPENSION RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWHB' ) THEN
        LCMPNT_DESC = 'STEERING ASSEMBLY WHEEL/HUB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWRM' ) THEN
        LCMPNT_DESC = 'STEERING ASSEMBLY WHEEL RIM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TIBA' ) THEN
        LCMPNT_DESC = 'TIBIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TRFC' ) THEN
        LCMPNT_DESC = 'TRUNK FLOOR CENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TRFL' ) THEN
        LCMPNT_DESC = 'TRUNK FLOOR LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TRFR' ) THEN
        LCMPNT_DESC = 'TRUNK FLOOR RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'WNDF' ) THEN
        LCMPNT_DESC = 'WINDSHIELD FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'WNDR' ) THEN
        LCMPNT_DESC = 'WINDSHIELD REAR'
        RETURN
      END IF

      RETURN

      END

! end of LCMPNT_DESC()


!***********************************************************************
!
! Function:  LCMPNT_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:40 1999
!
! Description:
!  Lookup function for matching CMPNT description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCMPNT_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCMPNT_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ.
     & ' LAP BELT CENTER MID ROW 2' ) THEN
        LCMPNT_CODE = 'ALC2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. ' LAP BELT RIGHT REAR' ) THEN
        LCMPNT_CODE = 'ALRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. ' SEAT TRACK LEFT FRONT' ) THEN
        LCMPNT_CODE = 'STLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. ' SEAT TRACK LEFT REAR' ) THEN
        LCMPNT_CODE = 'STLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. ' SEAT TRACK RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'STRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. ' SEAT TRACK RIGHT REAR' ) THEN
        LCMPNT_CODE = 'STRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'A PILLAR LEFT' ) THEN
        LCMPNT_CODE = 'PLAL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'A PILLAR RIGHT' ) THEN
        LCMPNT_CODE = 'PLAR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ABDOMEN' ) THEN
        LCMPNT_CODE = 'ABDO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ANKLE' ) THEN
        LCMPNT_CODE = 'ANKL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B PILLAR LEFT' ) THEN
        LCMPNT_CODE = 'PLBL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B PILLAR RIGHT' ) THEN
        LCMPNT_CODE = 'PLBR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BUMPER FRONT' ) THEN
        LCMPNT_CODE = 'BMPF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BUMPER REAR' ) THEN
        LCMPNT_CODE = 'BMPR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CARDIOVASCULAR SYSTEM' ) THEN
        LCMPNT_CODE = 'CRDV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CEREBOVASCULAR SYSTEM' ) THEN
        LCMPNT_CODE = 'CRBV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEST' ) THEN
        LCMPNT_CODE = 'CHST'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASH PANEL' ) THEN
        LCMPNT_CODE = 'DP01'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR   OTHER' ) THEN
        LCMPNT_CODE = 'DROT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR SILL LEFT FRONT' ) THEN
        LCMPNT_CODE = 'DSLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR SILL LEFT REAR' ) THEN
        LCMPNT_CODE = 'DSLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR SILL RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'DSRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR SILL RIGHT REAR' ) THEN
        LCMPNT_CODE = 'DSRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ENGINE' ) THEN
        LCMPNT_CODE = 'ENGN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FASCIA' ) THEN
        LCMPNT_CODE = 'FASC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FEMUR' ) THEN
        LCMPNT_CODE = 'FEMR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FENDER LEFT FRONT' ) THEN
        LCMPNT_CODE = 'FNLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FENDER LEFT REAR' ) THEN
        LCMPNT_CODE = 'FNLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FENDER RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'FNRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FENDER RIGHT REAR' ) THEN
        LCMPNT_CODE = 'FNRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN  LEFT FRONT' ) THEN
        LCMPNT_CODE = 'FLLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN  LEFT REAR' ) THEN
        LCMPNT_CODE = 'FLLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN  OTHER' ) THEN
        LCMPNT_CODE = 'FLOT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN  RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'FLRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN  RIGHT REAR' ) THEN
        LCMPNT_CODE = 'FLRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FOOT' ) THEN
        LCMPNT_CODE = 'FOOT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME RAIL FRONT' ) THEN
        LCMPNT_CODE = 'FRRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME RAIL LEFT SIDE' ) THEN
        LCMPNT_CODE = 'FRSL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME RAIL OTHER' ) THEN
        LCMPNT_CODE = 'FROT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME RAIL REAR' ) THEN
        LCMPNT_CODE = 'FRRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME RAIL RIGHT SIDE' ) THEN
        LCMPNT_CODE = 'FRSR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME RAIL: FIREWALL' ) THEN
        LCMPNT_CODE = 'FWLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD' ) THEN
        LCMPNT_CODE = 'HEAD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'HEAD REST CENTER MID ROW 1' ) THEN
        LCMPNT_CODE = 'HRC1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST CENTER REAR' ) THEN
        LCMPNT_CODE = 'HRCR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST HOOD' ) THEN
        LCMPNT_CODE = 'HOOD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST LEFT FRONT' ) THEN
        LCMPNT_CODE = 'HRLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST LEFT MID ROW 1' ) THEN
        LCMPNT_CODE = 'HRL1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST LEFT MID ROW 2' ) THEN
        LCMPNT_CODE = 'HRL2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST LEFT REAR' ) THEN
        LCMPNT_CODE = 'HRLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST OTHER' ) THEN
        LCMPNT_CODE = 'HROT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'HRRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST RIGHT MID ROW 1' ) THEN
        LCMPNT_CODE = 'HRR1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST RIGHT MID ROW 2' ) THEN
        LCMPNT_CODE = 'HRR2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST RIGHT REAR' ) THEN
        LCMPNT_CODE = 'HRRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER  REAR' ) THEN
        LCMPNT_CODE = 'HDRE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER FRONT' ) THEN
        LCMPNT_CODE = 'HDFR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER LEFT FRONT' ) THEN
        LCMPNT_CODE = 'HDLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER LEFT REAR' ) THEN
        LCMPNT_CODE = 'HDLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'HDRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER RIGHT REAR' ) THEN
        LCMPNT_CODE = 'HDRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE' ) THEN
        LCMPNT_CODE = 'KNEE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT CENTER FRONT' ) THEN
        LCMPNT_CODE = 'ALCF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT CENTER MID ROW 1' ) THEN
        LCMPNT_CODE = 'ALC1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT CENTER REAR' ) THEN
        LCMPNT_CODE = 'ALCR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT LEFT FRONT DSP' ) THEN
        LCMPNT_CODE = 'ALLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT LEFT MID ROW 1' ) THEN
        LCMPNT_CODE = 'ALL1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT LEFT MID ROW 2' ) THEN
        LCMPNT_CODE = 'ALL2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT LEFT REAR' ) THEN
        LCMPNT_CODE = 'ALLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT OTHER' ) THEN
        LCMPNT_CODE = 'ALOT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT RIGHT  MID ROW 2' ) THEN
        LCMPNT_CODE = 'ALR2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'ALRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT RIGHT MID ROW 1' ) THEN
        LCMPNT_CODE = 'ALR1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEFT FRONT DOOR' ) THEN
        LCMPNT_CODE = 'DRLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEFT REAR DOOR' ) THEN
        LCMPNT_CODE = 'DRLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NECK' ) THEN
        LCMPNT_CODE = 'NECK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LCMPNT_CODE = 'OTHE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER SEAT' ) THEN
        LCMPNT_CODE = 'SEOT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER SEAT BASE' ) THEN
        LCMPNT_CODE = 'SBOT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER SEAT TRACK' ) THEN
        LCMPNT_CODE = 'STOT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS' ) THEN
        LCMPNT_CODE = 'PELV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PILLAR MID ROW 1 RIGHT' ) THEN
        LCMPNT_CODE = 'PLIR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PILLAR MID ROW 2 LEFT' ) THEN
        LCMPNT_CODE = 'PL2L'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PILLAR MID ROW 2 RIGHT' ) THEN
        LCMPNT_CODE = 'PL2R'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PULMONARY SYSTEM' ) THEN
        LCMPNT_CODE = 'PULM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'REAR PILLAR LEFT' ) THEN
        LCMPNT_CODE = 'PLRL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'REAR PILLAR RIGHT' ) THEN
        LCMPNT_CODE = 'PLRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIB CAGE' ) THEN
        LCMPNT_CODE = 'RIBS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGHT FRONT  DOOR' ) THEN
        LCMPNT_CODE = 'DRRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGHT REAR DOOR' ) THEN
        LCMPNT_CODE = 'DRRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROLLBAR' ) THEN
        LCMPNT_CODE = 'ROLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'ROLLBAR   BRACE (LEFT OR RIGHT)' ) THEN
        LCMPNT_CODE = 'ROLB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF EXTERIOR' ) THEN
        LCMPNT_CODE = 'RFEX'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF INTERIOR' ) THEN
        LCMPNT_CODE = 'RFIN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF RAIL LEFT FRONT' ) THEN
        LCMPNT_CODE = 'RRLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF RAIL LEFT REAR' ) THEN
        LCMPNT_CODE = 'RRLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF RAIL RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'RRRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF RAIL RIGHT REAR' ) THEN
        LCMPNT_CODE = 'RRRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT  BACK' ) THEN
        LCMPNT_CODE = 'SEBK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE CENTER FRONT' ) THEN
        LCMPNT_CODE = 'SBCF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE CENTER MID ROW 1' ) THEN
        LCMPNT_CODE = 'SBC1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE CENTER MID ROW 2' ) THEN
        LCMPNT_CODE = 'SBC2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE CENTER REAR' ) THEN
        LCMPNT_CODE = 'SBCR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BASE ASSEMBLE FRONT' ) THEN
        LCMPNT_CODE = 'SBBF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE LEFT FRONT' ) THEN
        LCMPNT_CODE = 'SBLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE LEFT MID ROW 1' ) THEN
        LCMPNT_CODE = 'SBL1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE LEFT MID ROW 2' ) THEN
        LCMPNT_CODE = 'SBL2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE LEFT RIGHT' ) THEN
        LCMPNT_CODE = 'SBLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE MID ROW 1' ) THEN
        LCMPNT_CODE = 'SBM1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE MID ROW 2' ) THEN
        LCMPNT_CODE = 'SBM2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BASE ASSEMBLE REAR' ) THEN
        LCMPNT_CODE = 'SBBR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'SBRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE RIGHT MID ROW 1' ) THEN
        LCMPNT_CODE = 'SBR1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE RIGHT MID ROW 2' ) THEN
        LCMPNT_CODE = 'SBR2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BASE ASSEMBLE RIGHT REAR' ) THEN
        LCMPNT_CODE = 'SBRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BENCH FRONT' ) THEN
        LCMPNT_CODE = 'SEBF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BENCH MID ROW 1' ) THEN
        LCMPNT_CODE = 'SEM1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BENCH MID ROW 2' ) THEN
        LCMPNT_CODE = 'SEM2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BENCH REAR' ) THEN
        LCMPNT_CODE = 'SEBR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BUCKET CENTER FRONT' ) THEN
        LCMPNT_CODE = 'SECF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BUCKET CENTER MID ROW 1' ) THEN
        LCMPNT_CODE = 'SEC1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BUCKET CENTER MID ROW 2' ) THEN
        LCMPNT_CODE = 'SEC2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BUCKET CENTER REAR' ) THEN
        LCMPNT_CODE = 'SECR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BUCKET LEFT FRONT' ) THEN
        LCMPNT_CODE = 'SELF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BUCKET LEFT MID ROW 1' ) THEN
        LCMPNT_CODE = 'SEL1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BUCKET LEFT MID ROW 2' ) THEN
        LCMPNT_CODE = 'SEL2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BUCKET LEFT REAR' ) THEN
        LCMPNT_CODE = 'SELR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BUCKET RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'SERF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BUCKET RIGHT MID ROW 1' ) THEN
        LCMPNT_CODE = 'SER1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SEAT BUCKET RIGHT MID ROW 2' ) THEN
        LCMPNT_CODE = 'SER2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BUCKET RIGHT REAR' ) THEN
        LCMPNT_CODE = 'SERR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT CUSHION' ) THEN
        LCMPNT_CODE = 'SECU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER' ) THEN
        LCMPNT_CODE = 'SHLD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SHOULDER BELT CENTER FRONT' ) THEN
        LCMPNT_CODE = 'ASCF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SHOULDER BELT CENTER MID ROW 1' ) THEN
        LCMPNT_CODE = 'ASC1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SHOULDER BELT CENTER MID ROW 2' ) THEN
        LCMPNT_CODE = 'ASC2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER BELT CENTER REAR' ) THEN
        LCMPNT_CODE = 'ASCR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SHOULDER BELT LEFT FRONT DSP' ) THEN
        LCMPNT_CODE = 'ASLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SHOULDER BELT LEFT MID ROW 1' ) THEN
        LCMPNT_CODE = 'ASL1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SHOULDER BELT LEFT MID ROW 2' ) THEN
        LCMPNT_CODE = 'ASL2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER BELT LEFT REAR' ) THEN
        LCMPNT_CODE = 'ASLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER BELT OTHER' ) THEN
        LCMPNT_CODE = 'ASOT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER BELT RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'ASRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SHOULDER BELT RIGHT MID ROW 1' ) THEN
        LCMPNT_CODE = 'ASR1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SHOULDER BELT RIGHT MID ROW 2' ) THEN
        LCMPNT_CODE = 'ASR2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER BELT RIGHT REAR' ) THEN
        LCMPNT_CODE = 'ASRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SLIDING DOOR' ) THEN
        LCMPNT_CODE = 'DRSL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPINE' ) THEN
        LCMPNT_CODE = 'SPNE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING ASSEMBLY COLUMN' ) THEN
        LCMPNT_CODE = 'STCL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'STEERING ASSEMBLY WHEEL RIM' ) THEN
        LCMPNT_CODE = 'SWRM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'STEERING ASSEMBLY WHEEL/HUB' ) THEN
        LCMPNT_CODE = 'SWHB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STERNUM' ) THEN
        LCMPNT_CODE = 'STRN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUSPENSION LEFT FRONT' ) THEN
        LCMPNT_CODE = 'SULF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUSPENSION LEFT REAR' ) THEN
        LCMPNT_CODE = 'SULR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUSPENSION RIGHT FRONT' ) THEN
        LCMPNT_CODE = 'SURF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUSPENSION RIGHT REAR' ) THEN
        LCMPNT_CODE = 'SURR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIBIA' ) THEN
        LCMPNT_CODE = 'TIBA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRUNK FLOOR CENTER' ) THEN
        LCMPNT_CODE = 'TRFC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRUNK FLOOR LEFT' ) THEN
        LCMPNT_CODE = 'TRFL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRUNK FLOOR RIGHT' ) THEN
        LCMPNT_CODE = 'TRFR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WINDSHIELD FRONT' ) THEN
        LCMPNT_CODE = 'WNDF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WINDSHIELD REAR' ) THEN
        LCMPNT_CODE = 'WNDR'
        RETURN
      END IF

      RETURN

      END

! end of LCMPNT_CODE()


!***********************************************************************
!
! Function:  LCMPTYP_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:41 1999
!
! Description:
!  Lookup function for matching CMPTYP code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCMPTYP_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCMPTYP_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'OC' ) THEN
        LCMPTYP_DESC = 'OCCUPANT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VE' ) THEN
        LCMPTYP_DESC = 'VEHICLE'
        RETURN
      END IF

      RETURN

      END

! end of LCMPTYP_DESC()


!***********************************************************************
!
! Function:  LCMPTYP_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:41 1999
!
! Description:
!  Lookup function for matching CMPTYP description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCMPTYP_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCMPTYP_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'OCCUPANT' ) THEN
        LCMPTYP_CODE = 'OC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VEHICLE' ) THEN
        LCMPTYP_CODE = 'VE'
        RETURN
      END IF

      RETURN

      END

! end of LCMPTYP_CODE()


!***********************************************************************
!
! Function:  LCNTRC1_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:41 1999
!
! Description:
!  Lookup function for matching CNTRC1 code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRC1_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCNTRC1_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AB' ) THEN
        LCNTRC1_DESC = 'AIR BAG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LCNTRC1_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LCNTRC1_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LCNTRC1_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SB' ) THEN
        LCNTRC1_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW' ) THEN
        LCNTRC1_DESC = 'STEERING WHEEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LCNTRC1_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRC1_DESC()


!***********************************************************************
!
! Function:  LCNTRC1_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:41 1999
!
! Description:
!  Lookup function for matching CNTRC1 description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRC1_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCNTRC1_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'AIR BAG' ) THEN
        LCNTRC1_CODE = 'AB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LCNTRC1_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LCNTRC1_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LCNTRC1_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LCNTRC1_CODE = 'SB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL' ) THEN
        LCNTRC1_CODE = 'SW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LCNTRC1_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRC1_CODE()


!***********************************************************************
!
! Function:  LCNTRC2_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:41 1999
!
! Description:
!  Lookup function for matching CNTRC2 code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRC2_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCNTRC2_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AB' ) THEN
        LCNTRC2_DESC = 'AIR BAG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LCNTRC2_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LCNTRC2_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LCNTRC2_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SB' ) THEN
        LCNTRC2_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW' ) THEN
        LCNTRC2_DESC = 'STEERING WHEEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LCNTRC2_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRC2_DESC()


!***********************************************************************
!
! Function:  LCNTRC2_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:41 1999
!
! Description:
!  Lookup function for matching CNTRC2 description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRC2_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCNTRC2_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'AIR BAG' ) THEN
        LCNTRC2_CODE = 'AB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LCNTRC2_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LCNTRC2_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LCNTRC2_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LCNTRC2_CODE = 'SB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL' ) THEN
        LCNTRC2_CODE = 'SW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LCNTRC2_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRC2_CODE()


!***********************************************************************
!
! Function:  LCNTRH1_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:41 1999
!
! Description:
!  Lookup function for matching CNTRH1 code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRH1_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCNTRH1_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AB' ) THEN
        LCNTRH1_DESC = 'AIR BAG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AP' ) THEN
        LCNTRH1_DESC = 'A PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BP' ) THEN
        LCNTRH1_DESC = 'B PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CP' ) THEN
        LCNTRH1_DESC = 'C PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LCNTRH1_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HC' ) THEN
        LCNTRH1_DESC = 'CHEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HK' ) THEN
        LCNTRH1_DESC = 'KNEE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HS' ) THEN
        LCNTRH1_DESC = 'HEADER - SIDE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HW' ) THEN
        LCNTRH1_DESC = 'HEADER - WINDSHIELD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LCNTRH1_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OC' ) THEN
        LCNTRH1_DESC = 'ANOTHER OCCUPANT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LCNTRH1_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OV' ) THEN
        LCNTRH1_DESC = 'OTHER VEHICLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SB' ) THEN
        LCNTRH1_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SD' ) THEN
        LCNTRH1_DESC = 'SIDE WINDOW'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SH' ) THEN
        LCNTRH1_DESC = 'STEERING WHEEL HUB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SR' ) THEN
        LCNTRH1_DESC = 'STEERING WHEEL RIM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW' ) THEN
        LCNTRH1_DESC = 'STEERING WHEEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LCNTRH1_DESC = 'UNKNOWN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'WS' ) THEN
        LCNTRH1_DESC = 'WINDSHIELD'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRH1_DESC()


!***********************************************************************
!
! Function:  LCNTRH1_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:41 1999
!
! Description:
!  Lookup function for matching CNTRH1 description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRH1_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCNTRH1_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'A PILLAR' ) THEN
        LCNTRH1_CODE = 'AP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BAG' ) THEN
        LCNTRH1_CODE = 'AB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ANOTHER OCCUPANT' ) THEN
        LCNTRH1_CODE = 'OC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B PILLAR' ) THEN
        LCNTRH1_CODE = 'BP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C PILLAR' ) THEN
        LCNTRH1_CODE = 'CP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEST' ) THEN
        LCNTRH1_CODE = 'HC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LCNTRH1_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - SIDE' ) THEN
        LCNTRH1_CODE = 'HS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - WINDSHIELD' ) THEN
        LCNTRH1_CODE = 'HW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE' ) THEN
        LCNTRH1_CODE = 'HK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LCNTRH1_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LCNTRH1_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER VEHICLE' ) THEN
        LCNTRH1_CODE = 'OV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LCNTRH1_CODE = 'SB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIDE WINDOW' ) THEN
        LCNTRH1_CODE = 'SD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL' ) THEN
        LCNTRH1_CODE = 'SW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL HUB' ) THEN
        LCNTRH1_CODE = 'SH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL RIM' ) THEN
        LCNTRH1_CODE = 'SR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LCNTRH1_CODE = 'UN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WINDSHIELD' ) THEN
        LCNTRH1_CODE = 'WS'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRH1_CODE()


!***********************************************************************
!
! Function:  LCNTRH2_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:41 1999
!
! Description:
!  Lookup function for matching CNTRH2 code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRH2_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCNTRH2_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AB' ) THEN
        LCNTRH2_DESC = 'AIR BAG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AP' ) THEN
        LCNTRH2_DESC = 'A PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BP' ) THEN
        LCNTRH2_DESC = 'B PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CP' ) THEN
        LCNTRH2_DESC = 'C PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LCNTRH2_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HC' ) THEN
        LCNTRH2_DESC = 'CHEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HK' ) THEN
        LCNTRH2_DESC = 'KNEE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HS' ) THEN
        LCNTRH2_DESC = 'HEADER - SIDE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HW' ) THEN
        LCNTRH2_DESC = 'HEADER - WINDSHIELD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LCNTRH2_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OC' ) THEN
        LCNTRH2_DESC = 'ANOTHER OCCUPANT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LCNTRH2_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OV' ) THEN
        LCNTRH2_DESC = 'OTHER VEHICLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SB' ) THEN
        LCNTRH2_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SD' ) THEN
        LCNTRH2_DESC = 'SIDE WINDOW'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SH' ) THEN
        LCNTRH2_DESC = 'STEERING WHEEL HUB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SR' ) THEN
        LCNTRH2_DESC = 'STEERING WHEEL RIM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW' ) THEN
        LCNTRH2_DESC = 'STEERING WHEEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LCNTRH2_DESC = 'UNKNOWN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'WS' ) THEN
        LCNTRH2_DESC = 'WINDSHIELD'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRH2_DESC()


!***********************************************************************
!
! Function:  LCNTRH2_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching CNTRH2 description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRH2_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCNTRH2_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'A PILLAR' ) THEN
        LCNTRH2_CODE = 'AP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BAG' ) THEN
        LCNTRH2_CODE = 'AB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ANOTHER OCCUPANT' ) THEN
        LCNTRH2_CODE = 'OC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B PILLAR' ) THEN
        LCNTRH2_CODE = 'BP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C PILLAR' ) THEN
        LCNTRH2_CODE = 'CP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEST' ) THEN
        LCNTRH2_CODE = 'HC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LCNTRH2_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - SIDE' ) THEN
        LCNTRH2_CODE = 'HS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - WINDSHIELD' ) THEN
        LCNTRH2_CODE = 'HW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE' ) THEN
        LCNTRH2_CODE = 'HK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LCNTRH2_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LCNTRH2_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER VEHICLE' ) THEN
        LCNTRH2_CODE = 'OV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LCNTRH2_CODE = 'SB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIDE WINDOW' ) THEN
        LCNTRH2_CODE = 'SD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL' ) THEN
        LCNTRH2_CODE = 'SW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL HUB' ) THEN
        LCNTRH2_CODE = 'SH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL RIM' ) THEN
        LCNTRH2_CODE = 'SR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LCNTRH2_CODE = 'UN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WINDSHIELD' ) THEN
        LCNTRH2_CODE = 'WS'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRH2_CODE()


!***********************************************************************
!
! Function:  LCNTRL1_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching CNTRL1 code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRL1_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCNTRL1_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LCNTRL1_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KR' ) THEN
        LCNTRL1_DESC = 'KNEE RESTRAINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LCNTRL1_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LCNTRL1_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SB' ) THEN
        LCNTRL1_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SC' ) THEN
        LCNTRL1_DESC = 'STEERING COLUMN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LCNTRL1_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRL1_DESC()


!***********************************************************************
!
! Function:  LCNTRL1_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching CNTRL1 description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRL1_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCNTRL1_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LCNTRL1_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE RESTRAINT' ) THEN
        LCNTRL1_CODE = 'KR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LCNTRL1_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LCNTRL1_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LCNTRL1_CODE = 'SB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING COLUMN' ) THEN
        LCNTRL1_CODE = 'SC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LCNTRL1_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRL1_CODE()


!***********************************************************************
!
! Function:  LCNTRL2_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching CNTRL2 code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRL2_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCNTRL2_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LCNTRL2_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR' ) THEN
        LCNTRL2_DESC = 'DOOR PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LCNTRL2_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LCNTRL2_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SB' ) THEN
        LCNTRL2_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SC' ) THEN
        LCNTRL2_DESC = 'STEERING COLUMN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LCNTRL2_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRL2_DESC()


!***********************************************************************
!
! Function:  LCNTRL2_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching CNTRL2 description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCNTRL2_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCNTRL2_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LCNTRL2_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR PANEL' ) THEN
        LCNTRL2_CODE = 'DR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LCNTRL2_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LCNTRL2_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LCNTRL2_CODE = 'SB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING COLUMN' ) THEN
        LCNTRL2_CODE = 'SC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LCNTRL2_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LCNTRL2_CODE()


!***********************************************************************
!
! Function:  LCOLMEC_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching COLMEC code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCOLMEC_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCOLMEC_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'BWU' ) THEN
        LCOLMEC_DESC = 'BEHIND WHEEL UNITS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CON' ) THEN
        LCOLMEC_DESC = 'CONVOLUTED TUBE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CYL' ) THEN
        LCOLMEC_DESC = 'CYLINDRICAL MESH TUBE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'EMB' ) THEN
        LCOLMEC_DESC = 'EMBEDDED BALL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'EXA' ) THEN
        LCOLMEC_DESC = 'EXTRUDED ABSORBER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA ' ) THEN
        LCOLMEC_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NAP' ) THEN
        LCOLMEC_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NON' ) THEN
        LCOLMEC_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LCOLMEC_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LCOLMEC_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LCOLMEC_DESC()


!***********************************************************************
!
! Function:  LCOLMEC_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching COLMEC description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCOLMEC_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCOLMEC_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'BEHIND WHEEL UNITS' ) THEN
        LCOLMEC_CODE = 'BWU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONVOLUTED TUBE' ) THEN
        LCOLMEC_CODE = 'CON'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CYLINDRICAL MESH TUBE' ) THEN
        LCOLMEC_CODE = 'CYL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EMBEDDED BALL' ) THEN
        LCOLMEC_CODE = 'EMB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EXTRUDED ABSORBER' ) THEN
        LCOLMEC_CODE = 'EXA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LCOLMEC_CODE = 'NON'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LCOLMEC_CODE = 'NA '
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LCOLMEC_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LCOLMEC_CODE = 'UNK'
        RETURN
      END IF

      RETURN

      END

! end of LCOLMEC_CODE()


!***********************************************************************
!
! Function:  LCONFIG_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching CONFIG code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCONFIG_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCONFIG_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'ABD' ) THEN
        LCONFIG_DESC = 'ABDOMINAL COMPRESSION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CGF' ) THEN
        LCONFIG_DESC =
     & 'CG POSITION STATIC LOADING - FORWARD (FMVSS207)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CGR' ) THEN
        LCONFIG_DESC =
     & 'CG POSITION STATIC LOADING - REARWARD (FMVSS207)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CLF' ) THEN
        LCONFIG_DESC =
     & 'COMBINED STATIC LOADING OF FMVSS207 AND FMVSS210- FORWARD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CLR' ) THEN
        LCONFIG_DESC =
     & 'COMBINED STATIC LOADING OF FMVSS207 AND FMVSS210- REARWARD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CRU' ) THEN
        LCONFIG_DESC = 'STATIC CRUSH (FMVSS214 ,FMVSS207)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DYI' ) THEN
        LCONFIG_DESC = 'GENERIC DYNAMIC IMPACT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLY' ) THEN
        LCONFIG_DESC = 'FREE FLYING (DROP TEST)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDF' ) THEN
        LCONFIG_DESC =
     & 'FREE FLYING HEADFORM DYNAMIC IMPACT(FMVSS201)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDG' ) THEN
        LCONFIG_DESC =
     & 'GUIDEDHEADFORM DYNAMIC IMPACT(FMVSS201)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LCONFIG_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RML' ) THEN
        LCONFIG_DESC =
     & 'REARWARD MOMENT STATIC LOADING (FMVSS207)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SLB' ) THEN
        LCONFIG_DESC = 'SLED WITH VEHICLE BODY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SLN' ) THEN
        LCONFIG_DESC = 'SLED WITHOUT VEHICLE BODY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STC' ) THEN
        LCONFIG_DESC = 'GENERIC COMBINED STATIC LOADING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STL' ) THEN
        LCONFIG_DESC =
     & 'GENERIC STATIC LOADING  (FMVSS202,FMVSS210)'
        RETURN
      END IF

      RETURN

      END

! end of LCONFIG_DESC()


!***********************************************************************
!
! Function:  LCONFIG_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching CONFIG description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCONFIG_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCONFIG_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'ABDOMINAL COMPRESSION' ) THEN
        LCONFIG_CODE = 'ABD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'CG POSITION STATIC LOADING - FORWARD (FMVSS207)' ) THEN
        LCONFIG_CODE = 'CGF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'CG POSITION STATIC LOADING - REARWARD (FMVSS207)' ) THEN
        LCONFIG_CODE = 'CGR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'COMBINED STATIC LOADING OF FMVSS207 AND FMVSS210- '//
     & 'FORWARD' ) THEN
        LCONFIG_CODE = 'CLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'COMBINED STATIC LOADING OF FMVSS207 AND FMVSS210- '//
     & 'REARWARD' ) THEN
        LCONFIG_CODE = 'CLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FREE FLYING (DROP TEST)' ) THEN
        LCONFIG_CODE = 'FLY'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FREE FLYING HEADFORM DYNAMIC IMPACT(FMVSS201)' ) THEN
        LCONFIG_CODE = 'HDF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GENERIC COMBINED STATIC LOADING' ) THEN
        LCONFIG_CODE = 'STC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GENERIC DYNAMIC IMPACT' ) THEN
        LCONFIG_CODE = 'DYI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GENERIC STATIC LOADING  (FMVSS202,FMVSS210)' ) THEN
        LCONFIG_CODE = 'STL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GUIDEDHEADFORM DYNAMIC IMPACT(FMVSS201)' ) THEN
        LCONFIG_CODE = 'HDG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LCONFIG_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'REARWARD MOMENT STATIC LOADING (FMVSS207)' ) THEN
        LCONFIG_CODE = 'RML'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SLED WITH VEHICLE BODY' ) THEN
        LCONFIG_CODE = 'SLB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SLED WITHOUT VEHICLE BODY' ) THEN
        LCONFIG_CODE = 'SLN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'STATIC CRUSH (FMVSS214 ,FMVSS207)' ) THEN
        LCONFIG_CODE = 'CRU'
        RETURN
      END IF

      RETURN

      END

! end of LCONFIG_CODE()


!***********************************************************************
!
! Function:  LCRTEST_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:42 1999
!
! Description:
!  Lookup function for matching CRTEST code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCRTEST_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LCRTEST_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'N' ) THEN
        LCRTEST_DESC = 'NO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'U' ) THEN
        LCRTEST_DESC = 'UNKNOWN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'Y' ) THEN
        LCRTEST_DESC = 'YES'
        RETURN
      END IF

      RETURN

      END

! end of LCRTEST_DESC()


!***********************************************************************
!
! Function:  LCRTEST_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:43 1999
!
! Description:
!  Lookup function for matching CRTEST description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LCRTEST_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LCRTEST_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'NO' ) THEN
        LCRTEST_CODE = 'N'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LCRTEST_CODE = 'U'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'YES' ) THEN
        LCRTEST_CODE = 'Y'
        RETURN
      END IF

      RETURN

      END

! end of LCRTEST_CODE()


!***********************************************************************
!
! Function:  LDASTAT_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:43 1999
!
! Description:
!  Lookup function for matching DASTAT code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LDASTAT_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LDASTAT_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AM' ) THEN
        LDASTAT_DESC = 'AS MEASURED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CF' ) THEN
        LDASTAT_DESC = 'CHANNEL FAILED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CM' ) THEN
        LDASTAT_DESC = 'COMPUTED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MN' ) THEN
        LDASTAT_DESC = 'MEANINGLESS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LDASTAT_DESC = 'NO DATA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'QD' ) THEN
        LDASTAT_DESC = 'QUESTIONABLE DATA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SC' ) THEN
        LDASTAT_DESC = 'SCALING FACTOR APPLIED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SF' ) THEN
        LDASTAT_DESC = 'SYSTEM FAILED'
        RETURN
      END IF

      RETURN

      END

! end of LDASTAT_DESC()


!***********************************************************************
!
! Function:  LDASTAT_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:43 1999
!
! Description:
!  Lookup function for matching DASTAT description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LDASTAT_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LDASTAT_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'AS MEASURED' ) THEN
        LDASTAT_CODE = 'AM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHANNEL FAILED' ) THEN
        LDASTAT_CODE = 'CF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COMPUTED' ) THEN
        LDASTAT_CODE = 'CM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MEANINGLESS' ) THEN
        LDASTAT_CODE = 'MN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NO DATA' ) THEN
        LDASTAT_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'QUESTIONABLE DATA' ) THEN
        LDASTAT_CODE = 'QD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SCALING FACTOR APPLIED' ) THEN
        LDASTAT_CODE = 'SC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SYSTEM FAILED' ) THEN
        LDASTAT_CODE = 'SF'
        RETURN
      END IF

      RETURN

      END

! end of LDASTAT_CODE()


!***********************************************************************
!
! Function:  LDEPLOY_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:43 1999
!
! Description:
!  Lookup function for matching DEPLOY code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LDEPLOY_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LDEPLOY_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LDEPLOY_DESC = 'DEPLOYED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LDEPLOY_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LDEPLOY_DESC = 'NO DEPLOYMENT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LDEPLOY_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LDEPLOY_DESC()


!***********************************************************************
!
! Function:  LDEPLOY_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:43 1999
!
! Description:
!  Lookup function for matching DEPLOY description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LDEPLOY_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LDEPLOY_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DEPLOYED' ) THEN
        LDEPLOY_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NO DEPLOYMENT' ) THEN
        LDEPLOY_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LDEPLOY_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LDEPLOY_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LDEPLOY_CODE()


!***********************************************************************
!
! Function:  LDUMSIZ_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:43 1999
!
! Description:
!  Lookup function for matching DUMSIZ code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LDUMSIZ_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LDUMSIZ_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '05' ) THEN
        LDUMSIZ_DESC = '5 PERCENTILE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '06' ) THEN
        LDUMSIZ_DESC = '6 MONTH OLD CHILD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '12' ) THEN
        LDUMSIZ_DESC = '12 MONTH OLD CHILD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '18' ) THEN
        LDUMSIZ_DESC = '18 MONTH OLD CHILD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3C' ) THEN
        LDUMSIZ_DESC = '3 YEAR OLD CHILD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '50' ) THEN
        LDUMSIZ_DESC = '50 PERCENTILE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6C' ) THEN
        LDUMSIZ_DESC = '6 YEAR OLD CHILD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '95' ) THEN
        LDUMSIZ_DESC = '95 PERCENTILE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LDUMSIZ_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LDUMSIZ_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LDUMSIZ_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LDUMSIZ_DESC()


!***********************************************************************
!
! Function:  LDUMSIZ_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:43 1999
!
! Description:
!  Lookup function for matching DUMSIZ description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LDUMSIZ_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LDUMSIZ_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. '12 MONTH OLD CHILD' ) THEN
        LDUMSIZ_CODE = '12'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '18 MONTH OLD CHILD' ) THEN
        LDUMSIZ_CODE = '18'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '3 YEAR OLD CHILD' ) THEN
        LDUMSIZ_CODE = '3C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '5 PERCENTILE' ) THEN
        LDUMSIZ_CODE = '05'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '50 PERCENTILE' ) THEN
        LDUMSIZ_CODE = '50'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '6 MONTH OLD CHILD' ) THEN
        LDUMSIZ_CODE = '06'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '6 YEAR OLD CHILD' ) THEN
        LDUMSIZ_CODE = '6C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '95 PERCENTILE' ) THEN
        LDUMSIZ_CODE = '95'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LDUMSIZ_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LDUMSIZ_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LDUMSIZ_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LDUMSIZ_CODE()


!***********************************************************************
!
! Function:  LENGINE_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:43 1999
!
! Description:
!  Lookup function for matching ENGINE code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LENGINE_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LENGINE_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '3CTF' ) THEN
        LENGINE_DESC = '3 CYLINDER TRANSVERSE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4CIF' ) THEN
        LENGINE_DESC = '4 CYLINDER INLINE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4CLM' ) THEN
        LENGINE_DESC = '4 CYLINDER MID'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4CLR' ) THEN
        LENGINE_DESC = '4 CYLINDER REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4CTF' ) THEN
        LENGINE_DESC = '4 CYLINDER TRANSVERSE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ELEC' ) THEN
        LENGINE_DESC = 'ELECTRIC MOTOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NAPP' ) THEN
        LENGINE_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTHR' ) THEN
        LENGINE_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ROTR' ) THEN
        LENGINE_DESC = 'ROTARY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'S5IF' ) THEN
        LENGINE_DESC = 'STRAIGHT 5 INLINE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'S6IF' ) THEN
        LENGINE_DESC = 'STRAIGHT 6 INLINE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'S6TF' ) THEN
        LENGINE_DESC = 'STRAIGHT 6 TRANSVERSE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNKN' ) THEN
        LENGINE_DESC = 'UNKNOWN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'V6IF' ) THEN
        LENGINE_DESC = 'V6 INLINE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'V6TF' ) THEN
        LENGINE_DESC = 'V6 TRANSVERSE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'V8IF' ) THEN
        LENGINE_DESC = 'V8 INLINE FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'V8TF' ) THEN
        LENGINE_DESC = 'V8 TRANSVERSE FRONT'
        RETURN
      END IF

      RETURN

      END

! end of LENGINE_DESC()


!***********************************************************************
!
! Function:  LENGINE_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:43 1999
!
! Description:
!  Lookup function for matching ENGINE description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LENGINE_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LENGINE_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ.
     & '3 CYLINDER TRANSVERSE FRONT' ) THEN
        LENGINE_CODE = '3CTF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '4 CYLINDER INLINE FRONT' ) THEN
        LENGINE_CODE = '4CIF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '4 CYLINDER MID' ) THEN
        LENGINE_CODE = '4CLM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '4 CYLINDER REAR' ) THEN
        LENGINE_CODE = '4CLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & '4 CYLINDER TRANSVERSE FRONT' ) THEN
        LENGINE_CODE = '4CTF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELECTRIC MOTOR' ) THEN
        LENGINE_CODE = 'ELEC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LENGINE_CODE = 'NAPP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LENGINE_CODE = 'OTHR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROTARY' ) THEN
        LENGINE_CODE = 'ROTR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STRAIGHT 5 INLINE FRONT' ) THEN
        LENGINE_CODE = 'S5IF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STRAIGHT 6 INLINE FRONT' ) THEN
        LENGINE_CODE = 'S6IF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'STRAIGHT 6 TRANSVERSE FRONT' ) THEN
        LENGINE_CODE = 'S6TF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LENGINE_CODE = 'UNKN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'V6 INLINE FRONT' ) THEN
        LENGINE_CODE = 'V6IF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'V6 TRANSVERSE FRONT' ) THEN
        LENGINE_CODE = 'V6TF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'V8 INLINE FRONT' ) THEN
        LENGINE_CODE = 'V8IF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'V8 TRANSVERSE FRONT' ) THEN
        LENGINE_CODE = 'V8TF'
        RETURN
      END IF

      RETURN

      END

! end of LENGINE_CODE()


!***********************************************************************
!
! Function:  LLESION_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:44 1999
!
! Description:
!  Lookup function for matching LESION code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LLESION_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LLESION_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'A' ) THEN
        LLESION_DESC = 'ABRASION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'B' ) THEN
        LLESION_DESC = 'BURN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'C' ) THEN
        LLESION_DESC = 'CONTUSION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'D' ) THEN
        LLESION_DESC = 'DISLOCATION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'E' ) THEN
        LLESION_DESC = 'SEVERANCE/TRANSECTION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'F' ) THEN
        LLESION_DESC = 'FRACTURE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'G' ) THEN
        LLESION_DESC = 'DETACHMENT/SEPARATION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'K' ) THEN
        LLESION_DESC = 'CONCUSSION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'L' ) THEN
        LLESION_DESC = 'LACERATION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'M' ) THEN
        LLESION_DESC = 'AMPUTATION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'N' ) THEN
        LLESION_DESC = 'CRUSHING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'P' ) THEN
        LLESION_DESC = 'PERFORATION/PUNCTURE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'R' ) THEN
        LLESION_DESC = 'RUPTURE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'S' ) THEN
        LLESION_DESC = 'SPRAIN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'T' ) THEN
        LLESION_DESC = 'STRAIN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'V' ) THEN
        LLESION_DESC = 'AVULSION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'Z' ) THEN
        LLESION_DESC = 'FRACTURE AND DISLOCATION'
        RETURN
      END IF

      RETURN

      END

! end of LLESION_DESC()


!***********************************************************************
!
! Function:  LLESION_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:44 1999
!
! Description:
!  Lookup function for matching LESION description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LLESION_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LLESION_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'ABRASION' ) THEN
        LLESION_CODE = 'A'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AMPUTATION' ) THEN
        LLESION_CODE = 'M'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AVULSION' ) THEN
        LLESION_CODE = 'V'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BURN' ) THEN
        LLESION_CODE = 'B'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONCUSSION' ) THEN
        LLESION_CODE = 'K'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONTUSION' ) THEN
        LLESION_CODE = 'C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CRUSHING' ) THEN
        LLESION_CODE = 'N'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DETACHMENT/SEPARATION' ) THEN
        LLESION_CODE = 'G'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DISLOCATION' ) THEN
        LLESION_CODE = 'D'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRACTURE' ) THEN
        LLESION_CODE = 'F'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRACTURE AND DISLOCATION' ) THEN
        LLESION_CODE = 'Z'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LACERATION' ) THEN
        LLESION_CODE = 'L'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PERFORATION/PUNCTURE' ) THEN
        LLESION_CODE = 'P'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RUPTURE' ) THEN
        LLESION_CODE = 'R'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEVERANCE/TRANSECTION' ) THEN
        LLESION_CODE = 'E'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPRAIN' ) THEN
        LLESION_CODE = 'S'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STRAIN' ) THEN
        LLESION_CODE = 'T'
        RETURN
      END IF

      RETURN

      END

! end of LLESION_CODE()


!***********************************************************************
!
! Function:  LLINK_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:44 1999
!
! Description:
!  Lookup function for matching LINK code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LLINK_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LLINK_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LLINK_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TEL' ) THEN
        LLINK_DESC = 'TELEMETRY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UCT' ) THEN
        LLINK_DESC = 'UMBILICAL CABLE AND TELEMETRY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UMB' ) THEN
        LLINK_DESC = 'UMBILICAL CABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LLINK_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LLINK_DESC()


!***********************************************************************
!
! Function:  LLINK_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:44 1999
!
! Description:
!  Lookup function for matching LINK description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LLINK_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LLINK_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LLINK_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TELEMETRY' ) THEN
        LLINK_CODE = 'TEL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UMBILICAL CABLE' ) THEN
        LLINK_CODE = 'UMB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'UMBILICAL CABLE AND TELEMETRY' ) THEN
        LLINK_CODE = 'UCT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LLINK_CODE = 'UNK'
        RETURN
      END IF

      RETURN

      END

! end of LLINK_CODE()


!***********************************************************************
!
! Function:  LMAKE_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:44 1999
!
! Description:
!  Lookup function for matching MAKE code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMAKE_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LMAKE_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '01' ) THEN
        LMAKE_DESC = 'CHEVROLET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '02' ) THEN
        LMAKE_DESC = 'FORD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '03' ) THEN
        LMAKE_DESC = 'PONTIAC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '04' ) THEN
        LMAKE_DESC = 'BUICK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '05' ) THEN
        LMAKE_DESC = 'PLYMOUTH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '06' ) THEN
        LMAKE_DESC = 'OLDSMOBILE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '07' ) THEN
        LMAKE_DESC = 'DODGE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '08' ) THEN
        LMAKE_DESC = 'VOLKSWAGEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '09' ) THEN
        LMAKE_DESC = 'MERCURY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '10' ) THEN
        LMAKE_DESC = 'CADILLAC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '11' ) THEN
        LMAKE_DESC = 'AMERICAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '12' ) THEN
        LMAKE_DESC = 'AUDI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '13' ) THEN
        LMAKE_DESC = 'LINCOLN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '14' ) THEN
        LMAKE_DESC = 'PEUGEOT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '15' ) THEN
        LMAKE_DESC = 'NISSAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '16' ) THEN
        LMAKE_DESC = 'TOYOTA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '17' ) THEN
        LMAKE_DESC = 'RENAULT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '18' ) THEN
        LMAKE_DESC = 'MAZDA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '19' ) THEN
        LMAKE_DESC = 'FIAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '20' ) THEN
        LMAKE_DESC = 'VOLVO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '21' ) THEN
        LMAKE_DESC = 'CHRYSLER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '22' ) THEN
        LMAKE_DESC = 'LECTRA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '23' ) THEN
        LMAKE_DESC = 'HONDA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '24' ) THEN
        LMAKE_DESC = 'YUGO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '25' ) THEN
        LMAKE_DESC = 'MG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '26' ) THEN
        LMAKE_DESC = 'SUBARU'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '27' ) THEN
        LMAKE_DESC = 'BMW'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '28' ) THEN
        LMAKE_DESC = 'MERCEDES'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '29' ) THEN
        LMAKE_DESC = 'COMUTA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '30' ) THEN
        LMAKE_DESC = 'SAAB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '31' ) THEN
        LMAKE_DESC = 'TRIUMPH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '32' ) THEN
        LMAKE_DESC = 'NHTSA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '33' ) THEN
        LMAKE_DESC = 'SUZUKI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '34' ) THEN
        LMAKE_DESC = 'HYUNDAI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '35' ) THEN
        LMAKE_DESC = 'CHAMPION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '36' ) THEN
        LMAKE_DESC = 'CHECKER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '37' ) THEN
        LMAKE_DESC = 'CHINOOK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '38' ) THEN
        LMAKE_DESC = 'DELOREAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '39' ) THEN
        LMAKE_DESC = 'DAIHATSU'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '40' ) THEN
        LMAKE_DESC = 'GMC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '41' ) THEN
        LMAKE_DESC = 'IH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '42' ) THEN
        LMAKE_DESC = 'ISUZU'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '44' ) THEN
        LMAKE_DESC = 'JEEP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '48' ) THEN
        LMAKE_DESC = 'ODYSSEY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '53' ) THEN
        LMAKE_DESC = 'BATTRONICS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '54' ) THEN
        LMAKE_DESC = 'JET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '55' ) THEN
        LMAKE_DESC = 'EEVC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '56' ) THEN
        LMAKE_DESC = 'UM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '58' ) THEN
        LMAKE_DESC = 'EVA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '59' ) THEN
        LMAKE_DESC = 'LECTRIC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '60' ) THEN
        LMAKE_DESC = 'WINNEBAGO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '62' ) THEN
        LMAKE_DESC = 'MITSUBISHI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '63' ) THEN
        LMAKE_DESC = 'GEO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '64' ) THEN
        LMAKE_DESC = 'LEXUS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '65' ) THEN
        LMAKE_DESC = 'LEWIS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '66' ) THEN
        LMAKE_DESC = 'WAYNE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '67' ) THEN
        LMAKE_DESC = 'THOMAS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '68' ) THEN
        LMAKE_DESC = 'CARPENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '69' ) THEN
        LMAKE_DESC = 'SATURN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '70' ) THEN
        LMAKE_DESC = 'EAGLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '71' ) THEN
        LMAKE_DESC = 'WARD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '72' ) THEN
        LMAKE_DESC = 'INFINITI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '73' ) THEN
        LMAKE_DESC = 'ACURA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '74' ) THEN
        LMAKE_DESC = 'TIARA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '75' ) THEN
        LMAKE_DESC = 'COLLINS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '76' ) THEN
        LMAKE_DESC = 'SOLECTRIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '77' ) THEN
        LMAKE_DESC = 'KIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '78' ) THEN
        LMAKE_DESC = 'SEBRING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '79' ) THEN
        LMAKE_DESC = 'CHOO-CHOO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '80' ) THEN
        LMAKE_DESC = 'LAND ROVER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '81' ) THEN
        LMAKE_DESC = 'RENAISSANCE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '82' ) THEN
        LMAKE_DESC = 'HOLDEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '83' ) THEN
        LMAKE_DESC = 'EU96/27/EC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '84' ) THEN
        LMAKE_DESC = 'DAEWOO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '99' ) THEN
        LMAKE_DESC = 'OTHER'
        RETURN
      END IF

      RETURN

      END

! end of LMAKE_DESC()


!***********************************************************************
!
! Function:  LMAKE_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:44 1999
!
! Description:
!  Lookup function for matching MAKE description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMAKE_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LMAKE_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'ACURA' ) THEN
        LMAKE_CODE = '73'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AMERICAN' ) THEN
        LMAKE_CODE = '11'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AUDI' ) THEN
        LMAKE_CODE = '12'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BATTRONICS' ) THEN
        LMAKE_CODE = '53'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BMW' ) THEN
        LMAKE_CODE = '27'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BUICK' ) THEN
        LMAKE_CODE = '04'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CADILLAC' ) THEN
        LMAKE_CODE = '10'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CARPENTER' ) THEN
        LMAKE_CODE = '68'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHAMPION' ) THEN
        LMAKE_CODE = '35'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHECKER' ) THEN
        LMAKE_CODE = '36'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEVROLET' ) THEN
        LMAKE_CODE = '01'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHINOOK' ) THEN
        LMAKE_CODE = '37'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHOO-CHOO' ) THEN
        LMAKE_CODE = '79'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHRYSLER' ) THEN
        LMAKE_CODE = '21'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COLLINS' ) THEN
        LMAKE_CODE = '75'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COMUTA' ) THEN
        LMAKE_CODE = '29'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DAEWOO' ) THEN
        LMAKE_CODE = '84'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DAIHATSU' ) THEN
        LMAKE_CODE = '39'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DELOREAN' ) THEN
        LMAKE_CODE = '38'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DODGE' ) THEN
        LMAKE_CODE = '07'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EAGLE' ) THEN
        LMAKE_CODE = '70'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EEVC' ) THEN
        LMAKE_CODE = '55'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EU96/27/EC' ) THEN
        LMAKE_CODE = '83'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EVA' ) THEN
        LMAKE_CODE = '58'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FIAT' ) THEN
        LMAKE_CODE = '19'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FORD' ) THEN
        LMAKE_CODE = '02'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GEO' ) THEN
        LMAKE_CODE = '63'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GMC' ) THEN
        LMAKE_CODE = '40'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HOLDEN' ) THEN
        LMAKE_CODE = '82'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HONDA' ) THEN
        LMAKE_CODE = '23'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HYUNDAI' ) THEN
        LMAKE_CODE = '34'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IH' ) THEN
        LMAKE_CODE = '41'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'INFINITI' ) THEN
        LMAKE_CODE = '72'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ISUZU' ) THEN
        LMAKE_CODE = '42'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'JEEP' ) THEN
        LMAKE_CODE = '44'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'JET' ) THEN
        LMAKE_CODE = '54'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KIA' ) THEN
        LMAKE_CODE = '77'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAND ROVER' ) THEN
        LMAKE_CODE = '80'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LECTRA' ) THEN
        LMAKE_CODE = '22'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LECTRIC' ) THEN
        LMAKE_CODE = '59'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEWIS' ) THEN
        LMAKE_CODE = '65'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEXUS' ) THEN
        LMAKE_CODE = '64'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LINCOLN' ) THEN
        LMAKE_CODE = '13'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MAZDA' ) THEN
        LMAKE_CODE = '18'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MERCEDES' ) THEN
        LMAKE_CODE = '28'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MERCURY' ) THEN
        LMAKE_CODE = '09'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MG' ) THEN
        LMAKE_CODE = '25'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MITSUBISHI' ) THEN
        LMAKE_CODE = '62'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NHTSA' ) THEN
        LMAKE_CODE = '32'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NISSAN' ) THEN
        LMAKE_CODE = '15'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ODYSSEY' ) THEN
        LMAKE_CODE = '48'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OLDSMOBILE' ) THEN
        LMAKE_CODE = '06'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LMAKE_CODE = '99'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PEUGEOT' ) THEN
        LMAKE_CODE = '14'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PLYMOUTH' ) THEN
        LMAKE_CODE = '05'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PONTIAC' ) THEN
        LMAKE_CODE = '03'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RENAISSANCE' ) THEN
        LMAKE_CODE = '81'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RENAULT' ) THEN
        LMAKE_CODE = '17'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SAAB' ) THEN
        LMAKE_CODE = '30'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SATURN' ) THEN
        LMAKE_CODE = '69'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEBRING' ) THEN
        LMAKE_CODE = '78'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SOLECTRIA' ) THEN
        LMAKE_CODE = '76'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUBARU' ) THEN
        LMAKE_CODE = '26'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUZUKI' ) THEN
        LMAKE_CODE = '33'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'THOMAS' ) THEN
        LMAKE_CODE = '67'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIARA' ) THEN
        LMAKE_CODE = '74'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TOYOTA' ) THEN
        LMAKE_CODE = '16'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRIUMPH' ) THEN
        LMAKE_CODE = '31'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UM' ) THEN
        LMAKE_CODE = '56'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VOLKSWAGEN' ) THEN
        LMAKE_CODE = '08'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VOLVO' ) THEN
        LMAKE_CODE = '20'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WARD' ) THEN
        LMAKE_CODE = '71'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WAYNE' ) THEN
        LMAKE_CODE = '66'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WINNEBAGO' ) THEN
        LMAKE_CODE = '60'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'YUGO' ) THEN
        LMAKE_CODE = '24'
        RETURN
      END IF

      RETURN

      END

! end of LMAKE_CODE()


!***********************************************************************
!
! Function:  LMEASUR_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:44 1999
!
! Description:
!  Lookup function for matching MEASUR code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMEASUR_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LMEASUR_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'MET' ) THEN
        LMEASUR_DESC = 'METRIC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STD' ) THEN
        LMEASUR_DESC = 'STANDARD METRIC'
        RETURN
      END IF

      RETURN

      END

! end of LMEASUR_DESC()


!***********************************************************************
!
! Function:  LMEASUR_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:44 1999
!
! Description:
!  Lookup function for matching MEASUR description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMEASUR_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LMEASUR_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'METRIC' ) THEN
        LMEASUR_CODE = 'MET'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STANDARD METRIC' ) THEN
        LMEASUR_CODE = 'STD'
        RETURN
      END IF

      RETURN

      END

! end of LMEASUR_CODE()


!***********************************************************************
!
! Function:  LMODIND_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:44 1999
!
! Description:
!  Lookup function for matching MODIND code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMODIND_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LMODIND_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'M' ) THEN
        LMODIND_DESC = 'MODIFIED VEHICLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'N' ) THEN
        LMODIND_DESC = 'NO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'P' ) THEN
        LMODIND_DESC = 'PRODUCTION VEHICLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'R' ) THEN
        LMODIND_DESC = 'RESEARCH VEHICLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'U' ) THEN
        LMODIND_DESC = 'UNKNOWN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'Y' ) THEN
        LMODIND_DESC = 'YES'
        RETURN
      END IF

      RETURN

      END

! end of LMODIND_DESC()


!***********************************************************************
!
! Function:  LMODIND_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching MODIND description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMODIND_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LMODIND_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'MODIFIED VEHICLE' ) THEN
        LMODIND_CODE = 'M'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NO' ) THEN
        LMODIND_CODE = 'N'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PRODUCTION VEHICLE' ) THEN
        LMODIND_CODE = 'P'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RESEARCH VEHICLE' ) THEN
        LMODIND_CODE = 'R'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LMODIND_CODE = 'U'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'YES' ) THEN
        LMODIND_CODE = 'Y'
        RETURN
      END IF

      RETURN

      END

! end of LMODIND_CODE()


!***********************************************************************
!
! Function:  LMTHCAL_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching MTHCAL code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMTHCAL_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LMTHCAL_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'H3' ) THEN
        LMTHCAL_DESC = 'HYBRID III'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HS' ) THEN
        LMTHCAL_DESC = 'SIDE IMPACT DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LMTHCAL_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LMTHCAL_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LMTHCAL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'P5' ) THEN
        LMTHCAL_DESC = 'PART 572'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LMTHCAL_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LMTHCAL_DESC()


!***********************************************************************
!
! Function:  LMTHCAL_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching MTHCAL description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMTHCAL_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LMTHCAL_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'HYBRID III' ) THEN
        LMTHCAL_CODE = 'H3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LMTHCAL_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LMTHCAL_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LMTHCAL_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PART 572' ) THEN
        LMTHCAL_CODE = 'P5'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIDE IMPACT DUMMY' ) THEN
        LMTHCAL_CODE = 'HS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LMTHCAL_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LMTHCAL_CODE()


!***********************************************************************
!
! Function:  LOCCLOC_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching OCCLOC code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LOCCLOC_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LOCCLOC_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '01' ) THEN
        LOCCLOC_DESC = 'LEFT FRONT SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '02' ) THEN
        LOCCLOC_DESC = 'RIGHT FRONT SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '03' ) THEN
        LOCCLOC_DESC = 'RIGHT REAR SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '04' ) THEN
        LOCCLOC_DESC = 'LEFT REAR SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '05' ) THEN
        LOCCLOC_DESC = 'CENTER FRONT SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '06' ) THEN
        LOCCLOC_DESC = 'CENTER REAR SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '07' ) THEN
        LOCCLOC_DESC = 'LEFT THIRD SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '08' ) THEN
        LOCCLOC_DESC = 'CENTER THIRD SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '09' ) THEN
        LOCCLOC_DESC = 'RIGHT THIRD SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LOCCLOC_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LOCCLOC_DESC = 'OTHER'
        RETURN
      END IF

      RETURN

      END

! end of LOCCLOC_DESC()


!***********************************************************************
!
! Function:  LOCCLOC_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching OCCLOC description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LOCCLOC_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LOCCLOC_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'CENTER FRONT SEAT' ) THEN
        LOCCLOC_CODE = '05'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CENTER REAR SEAT' ) THEN
        LOCCLOC_CODE = '06'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CENTER THIRD SEAT' ) THEN
        LOCCLOC_CODE = '08'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEFT FRONT SEAT' ) THEN
        LOCCLOC_CODE = '01'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEFT REAR SEAT' ) THEN
        LOCCLOC_CODE = '04'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEFT THIRD SEAT' ) THEN
        LOCCLOC_CODE = '07'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LOCCLOC_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LOCCLOC_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGHT FRONT SEAT' ) THEN
        LOCCLOC_CODE = '02'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGHT REAR SEAT' ) THEN
        LOCCLOC_CODE = '03'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGHT THIRD SEAT' ) THEN
        LOCCLOC_CODE = '09'
        RETURN
      END IF

      RETURN

      END

! end of LOCCLOC_CODE()


!***********************************************************************
!
! Function:  LOCCSEX_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching OCCSEX code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LOCCSEX_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LOCCSEX_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'F' ) THEN
        LOCCSEX_DESC = 'FEMALE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'M' ) THEN
        LOCCSEX_DESC = 'MALE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'N' ) THEN
        LOCCSEX_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'U' ) THEN
        LOCCSEX_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LOCCSEX_DESC()


!***********************************************************************
!
! Function:  LOCCSEX_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching OCCSEX description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LOCCSEX_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LOCCSEX_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'FEMALE' ) THEN
        LOCCSEX_CODE = 'F'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MALE' ) THEN
        LOCCSEX_CODE = 'M'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LOCCSEX_CODE = 'N'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LOCCSEX_CODE = 'U'
        RETURN
      END IF

      RETURN

      END

! end of LOCCSEX_CODE()


!***********************************************************************
!
! Function:  LOCCTYP_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching OCCTYP code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LOCCTYP_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LOCCTYP_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AN' ) THEN
        LOCCTYP_DESC = 'ANIMAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AO' ) THEN
        LOCCTYP_DESC = 'APR OMNI-DIRECTIONAL DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AS' ) THEN
        LOCCTYP_DESC = 'APR SIDE IMPACT DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BS' ) THEN
        LOCCTYP_DESC = 'BIO-SID DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CD' ) THEN
        LOCCTYP_DESC = 'CADAVER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CH' ) THEN
        LOCCTYP_DESC = 'CHILD DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CR' ) THEN
        LOCCTYP_DESC = 'CRABI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ES' ) THEN
        LOCCTYP_DESC = 'EURO-SID DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'H3' ) THEN
        LOCCTYP_DESC = 'HYBRID III DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HP' ) THEN
        LOCCTYP_DESC = 'HUMANOID PEDESTRIAN DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HU' ) THEN
        LOCCTYP_DESC = 'HUMAN VOLUNTEER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LOCCTYP_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LOCCTYP_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'P5' ) THEN
        LOCCTYP_DESC = 'PART 572 DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SD' ) THEN
        LOCCTYP_DESC = 'NHTSA SIDE IMPACT DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SH' ) THEN
        LOCCTYP_DESC =
     & 'SID WITH HYBRID III HEAD & NECK, AND MODIFIED NECK BRACKET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TD' ) THEN
        LOCCTYP_DESC = 'TAD DUMMY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TR' ) THEN
        LOCCTYP_DESC = 'TRRL DUMMY'
        RETURN
      END IF

      RETURN

      END

! end of LOCCTYP_DESC()


!***********************************************************************
!
! Function:  LOCCTYP_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching OCCTYP description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LOCCTYP_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LOCCTYP_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'ANIMAL' ) THEN
        LOCCTYP_CODE = 'AN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'APR OMNI-DIRECTIONAL DUMMY' ) THEN
        LOCCTYP_CODE = 'AO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'APR SIDE IMPACT DUMMY' ) THEN
        LOCCTYP_CODE = 'AS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BIO-SID DUMMY' ) THEN
        LOCCTYP_CODE = 'BS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CADAVER' ) THEN
        LOCCTYP_CODE = 'CD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHILD DUMMY' ) THEN
        LOCCTYP_CODE = 'CH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CRABI' ) THEN
        LOCCTYP_CODE = 'CR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EURO-SID DUMMY' ) THEN
        LOCCTYP_CODE = 'ES'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HUMAN VOLUNTEER' ) THEN
        LOCCTYP_CODE = 'HU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HUMANOID PEDESTRIAN DUMMY' ) THEN
        LOCCTYP_CODE = 'HP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HYBRID III DUMMY' ) THEN
        LOCCTYP_CODE = 'H3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NHTSA SIDE IMPACT DUMMY' ) THEN
        LOCCTYP_CODE = 'SD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LOCCTYP_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LOCCTYP_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PART 572 DUMMY' ) THEN
        LOCCTYP_CODE = 'P5'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SID WITH HYBRID III HEAD & NECK, AND MODIFIED NECK'//
     & ' BRACKET' ) THEN
        LOCCTYP_CODE = 'SH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TAD DUMMY' ) THEN
        LOCCTYP_CODE = 'TD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRRL DUMMY' ) THEN
        LOCCTYP_CODE = 'TR'
        RETURN
      END IF

      RETURN

      END

! end of LOCCTYP_CODE()


!***********************************************************************
!
! Function:  LRECTYP_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:45 1999
!
! Description:
!  Lookup function for matching RECTYP code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRECTYP_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LRECTYP_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'DDA' ) THEN
        LRECTYP_DESC = 'DIGITAL DATA ACQUISITION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DIG' ) THEN
        LRECTYP_DESC = 'DIGITAL TAPE RECORDER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FMM' ) THEN
        LRECTYP_DESC = 'FM MULTIPLEXOR TAPE RECORDER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FMT' ) THEN
        LRECTYP_DESC = 'FM TAPE RECORDER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OSC' ) THEN
        LRECTYP_DESC = 'OSCILLOGRAPH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LRECTYP_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LRECTYP_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LRECTYP_DESC()


!***********************************************************************
!
! Function:  LRECTYP_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:46 1999
!
! Description:
!  Lookup function for matching RECTYP description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRECTYP_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LRECTYP_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DIGITAL DATA ACQUISITION' ) THEN
        LRECTYP_CODE = 'DDA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DIGITAL TAPE RECORDER' ) THEN
        LRECTYP_CODE = 'DIG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FM MULTIPLEXOR TAPE RECORDER' ) THEN
        LRECTYP_CODE = 'FMM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FM TAPE RECORDER' ) THEN
        LRECTYP_CODE = 'FMT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OSCILLOGRAPH' ) THEN
        LRECTYP_CODE = 'OSC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LRECTYP_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LRECTYP_CODE = 'UNK'
        RETURN
      END IF

      RETURN

      END

! end of LRECTYP_CODE()


!***********************************************************************
!
! Function:  LRESTR1_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:46 1999
!
! Description:
!  Lookup function for matching RESTR1 code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRESTR1_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LRESTR1_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '3PT' ) THEN
        LRESTR1_DESC = '3 POINT BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABG' ) THEN
        LRESTR1_DESC = 'AIR BAG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABT' ) THEN
        LRESTR1_DESC = 'AIR BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'APP' ) THEN
        LRESTR1_DESC = 'APR PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHD' ) THEN
        LRESTR1_DESC = 'CHILD RESTRAINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DPL' ) THEN
        LRESTR1_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FHP' ) THEN
        LRESTR1_DESC = 'FBRGL. HNCB. PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KNE' ) THEN
        LRESTR1_DESC = 'KNEE RESTRAINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LAP' ) THEN
        LRESTR1_DESC = 'LAP BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MCP' ) THEN
        LRESTR1_DESC = 'MINICARS PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NON' ) THEN
        LRESTR1_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LRESTR1_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PAD' ) THEN
        LRESTR1_DESC = 'PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PS2' ) THEN
        LRESTR1_DESC = 'PASSIVE 2 POINT BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PS3' ) THEN
        LRESTR1_DESC = 'PASSIVE 3 POINT BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RIG' ) THEN
        LRESTR1_DESC = 'RIGID WALL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBK' ) THEN
        LRESTR1_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SCE' ) THEN
        LRESTR1_DESC = 'STR. COLUMN - EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SCN' ) THEN
        LRESTR1_DESC = 'STR. COLUMN - NO EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWE' ) THEN
        LRESTR1_DESC = 'STR. WHEEL - EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWN' ) THEN
        LRESTR1_DESC = 'STR. WHEEL - NO EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LRESTR1_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LRESTR1_DESC()


!***********************************************************************
!
! Function:  LRESTR1_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:46 1999
!
! Description:
!  Lookup function for matching RESTR1 description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRESTR1_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LRESTR1_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. '3 POINT BELT' ) THEN
        LRESTR1_CODE = '3PT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BAG' ) THEN
        LRESTR1_CODE = 'ABG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BELT' ) THEN
        LRESTR1_CODE = 'ABT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'APR PADDING' ) THEN
        LRESTR1_CODE = 'APP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHILD RESTRAINT' ) THEN
        LRESTR1_CODE = 'CHD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LRESTR1_CODE = 'DPL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FBRGL. HNCB. PADDING' ) THEN
        LRESTR1_CODE = 'FHP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE RESTRAINT' ) THEN
        LRESTR1_CODE = 'KNE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT' ) THEN
        LRESTR1_CODE = 'LAP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MINICARS PADDING' ) THEN
        LRESTR1_CODE = 'MCP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LRESTR1_CODE = 'NON'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LRESTR1_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PADDING' ) THEN
        LRESTR1_CODE = 'PAD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PASSIVE 2 POINT BELT' ) THEN
        LRESTR1_CODE = 'PS2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PASSIVE 3 POINT BELT' ) THEN
        LRESTR1_CODE = 'PS3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGID WALL' ) THEN
        LRESTR1_CODE = 'RIG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LRESTR1_CODE = 'SBK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. COLUMN - EA' ) THEN
        LRESTR1_CODE = 'SCE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. COLUMN - NO EA' ) THEN
        LRESTR1_CODE = 'SCN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. WHEEL - EA' ) THEN
        LRESTR1_CODE = 'SWE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. WHEEL - NO EA' ) THEN
        LRESTR1_CODE = 'SWN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LRESTR1_CODE = 'UNK'
        RETURN
      END IF

      RETURN

      END

! end of LRESTR1_CODE()


!***********************************************************************
!
! Function:  LRESTR2_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:46 1999
!
! Description:
!  Lookup function for matching RESTR2 code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRESTR2_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LRESTR2_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '3PT' ) THEN
        LRESTR2_DESC = '3 POINT BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABG' ) THEN
        LRESTR2_DESC = 'AIR BAG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABT' ) THEN
        LRESTR2_DESC = 'AIR BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'APP' ) THEN
        LRESTR2_DESC = 'APR PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHD' ) THEN
        LRESTR2_DESC = 'CHILD RESTRAINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DPL' ) THEN
        LRESTR2_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FHP' ) THEN
        LRESTR2_DESC = 'FBRGL. HNCB. PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KNE' ) THEN
        LRESTR2_DESC = 'KNEE RESTRAINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LAP' ) THEN
        LRESTR2_DESC = 'LAP BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MCP' ) THEN
        LRESTR2_DESC = 'MINICARS PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NON' ) THEN
        LRESTR2_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LRESTR2_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PAD' ) THEN
        LRESTR2_DESC = 'PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PS2' ) THEN
        LRESTR2_DESC = 'PASSIVE 2 POINT BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PS3' ) THEN
        LRESTR2_DESC = 'PASSIVE 3 POINT BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RIG' ) THEN
        LRESTR2_DESC = 'RIGID WALL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBK' ) THEN
        LRESTR2_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SCE' ) THEN
        LRESTR2_DESC = 'STR. COLUMN - EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SCN' ) THEN
        LRESTR2_DESC = 'STR. COLUMN - NO EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWE' ) THEN
        LRESTR2_DESC = 'STR. WHEEL - EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWN' ) THEN
        LRESTR2_DESC = 'STR. WHEEL - NO EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LRESTR2_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LRESTR2_DESC()


!***********************************************************************
!
! Function:  LRESTR2_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:46 1999
!
! Description:
!  Lookup function for matching RESTR2 description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRESTR2_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LRESTR2_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. '3 POINT BELT' ) THEN
        LRESTR2_CODE = '3PT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BAG' ) THEN
        LRESTR2_CODE = 'ABG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BELT' ) THEN
        LRESTR2_CODE = 'ABT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'APR PADDING' ) THEN
        LRESTR2_CODE = 'APP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHILD RESTRAINT' ) THEN
        LRESTR2_CODE = 'CHD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LRESTR2_CODE = 'DPL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FBRGL. HNCB. PADDING' ) THEN
        LRESTR2_CODE = 'FHP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE RESTRAINT' ) THEN
        LRESTR2_CODE = 'KNE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT' ) THEN
        LRESTR2_CODE = 'LAP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MINICARS PADDING' ) THEN
        LRESTR2_CODE = 'MCP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LRESTR2_CODE = 'NON'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LRESTR2_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PADDING' ) THEN
        LRESTR2_CODE = 'PAD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PASSIVE 2 POINT BELT' ) THEN
        LRESTR2_CODE = 'PS2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PASSIVE 3 POINT BELT' ) THEN
        LRESTR2_CODE = 'PS3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGID WALL' ) THEN
        LRESTR2_CODE = 'RIG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LRESTR2_CODE = 'SBK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. COLUMN - EA' ) THEN
        LRESTR2_CODE = 'SCE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. COLUMN - NO EA' ) THEN
        LRESTR2_CODE = 'SCN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. WHEEL - EA' ) THEN
        LRESTR2_CODE = 'SWE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. WHEEL - NO EA' ) THEN
        LRESTR2_CODE = 'SWN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LRESTR2_CODE = 'UNK'
        RETURN
      END IF

      RETURN

      END

! end of LRESTR2_CODE()


!***********************************************************************
!
! Function:  LRSTMNT_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:46 1999
!
! Description:
!  Lookup function for matching RSTMNT code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRSTMNT_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LRSTMNT_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AP' ) THEN
        LRSTMNT_DESC = 'A PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AR' ) THEN
        LRSTMNT_DESC = 'ARM REST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BC' ) THEN
        LRSTMNT_DESC = 'BELT - CONVENTIONAL MOUNT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BD' ) THEN
        LRSTMNT_DESC = 'BELT - DOOR MOUNT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BI' ) THEN
        LRSTMNT_DESC = 'BELT - INTEGRATED SEAT MOUNT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BP' ) THEN
        LRSTMNT_DESC = 'B PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CP' ) THEN
        LRSTMNT_DESC = 'C PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DM' ) THEN
        LRSTMNT_DESC = 'DASH PANEL - MID'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP' ) THEN
        LRSTMNT_DESC = 'DASH PANEL - UNSPECIFIED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR' ) THEN
        LRSTMNT_DESC = 'DOOR PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DT' ) THEN
        LRSTMNT_DESC = 'DASH PANEL - TOP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HR' ) THEN
        LRSTMNT_DESC = 'HEAD REST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HS' ) THEN
        LRSTMNT_DESC = 'HEADER - SIDE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HW' ) THEN
        LRSTMNT_DESC = 'HEADER - WINDSHIELD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LRSTMNT_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LRSTMNT_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SB' ) THEN
        LRSTMNT_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SP' ) THEN
        LRSTMNT_DESC = 'SIDE PANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SS' ) THEN
        LRSTMNT_DESC = 'SIDE WINDOW SILL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW' ) THEN
        LRSTMNT_DESC = 'STEERING WHEEL'
        RETURN
      END IF

      RETURN

      END

! end of LRSTMNT_DESC()


!***********************************************************************
!
! Function:  LRSTMNT_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:46 1999
!
! Description:
!  Lookup function for matching RSTMNT description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRSTMNT_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LRSTMNT_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'A PILLAR' ) THEN
        LRSTMNT_CODE = 'AP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ARM REST' ) THEN
        LRSTMNT_CODE = 'AR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B PILLAR' ) THEN
        LRSTMNT_CODE = 'BP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BELT - CONVENTIONAL MOUNT' ) THEN
        LRSTMNT_CODE = 'BC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BELT - DOOR MOUNT' ) THEN
        LRSTMNT_CODE = 'BD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'BELT - INTEGRATED SEAT MOUNT' ) THEN
        LRSTMNT_CODE = 'BI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C PILLAR' ) THEN
        LRSTMNT_CODE = 'CP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASH PANEL - MID' ) THEN
        LRSTMNT_CODE = 'DM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASH PANEL - TOP' ) THEN
        LRSTMNT_CODE = 'DT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASH PANEL - UNSPECIFIED' ) THEN
        LRSTMNT_CODE = 'DP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR PANEL' ) THEN
        LRSTMNT_CODE = 'DR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD REST' ) THEN
        LRSTMNT_CODE = 'HR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - SIDE' ) THEN
        LRSTMNT_CODE = 'HS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - WINDSHIELD' ) THEN
        LRSTMNT_CODE = 'HW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LRSTMNT_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LRSTMNT_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LRSTMNT_CODE = 'SB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIDE PANEL' ) THEN
        LRSTMNT_CODE = 'SP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIDE WINDOW SILL' ) THEN
        LRSTMNT_CODE = 'SS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL' ) THEN
        LRSTMNT_CODE = 'SW'
        RETURN
      END IF

      RETURN

      END

! end of LRSTMNT_CODE()


!***********************************************************************
!
! Function:  LRSTTYP_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:46 1999
!
! Description:
!  Lookup function for matching RSTTYP code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRSTTYP_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LRSTTYP_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '3PT' ) THEN
        LRSTTYP_DESC = '3 POINT BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABG' ) THEN
        LRSTTYP_DESC = 'AIR BAG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABT' ) THEN
        LRSTTYP_DESC = 'AIR BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'APP' ) THEN
        LRSTTYP_DESC = 'APR PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHD' ) THEN
        LRSTTYP_DESC = 'CHILD RESTRAINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DPL' ) THEN
        LRSTTYP_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FHP' ) THEN
        LRSTTYP_DESC = 'FBRGL. HNCB. PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KNE' ) THEN
        LRSTTYP_DESC = 'KNEE RESTRAINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LAP' ) THEN
        LRSTTYP_DESC = 'LAP BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MCP' ) THEN
        LRSTTYP_DESC = 'MINICARS PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NON' ) THEN
        LRSTTYP_DESC = 'NONE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LRSTTYP_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PAD' ) THEN
        LRSTTYP_DESC = 'PADDING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PS2' ) THEN
        LRSTTYP_DESC = 'PASSIVE 2 POINT BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PS3' ) THEN
        LRSTTYP_DESC = 'PASSIVE 3 POINT BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RIG' ) THEN
        LRSTTYP_DESC = 'RIGID WALL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SBK' ) THEN
        LRSTTYP_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SCE' ) THEN
        LRSTTYP_DESC = 'STR. COLUMN - EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SCN' ) THEN
        LRSTTYP_DESC = 'STR. COLUMN - NO EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWE' ) THEN
        LRSTTYP_DESC = 'STR. WHEEL - EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWN' ) THEN
        LRSTTYP_DESC = 'STR. WHEEL - NO EA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LRSTTYP_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LRSTTYP_DESC()


!***********************************************************************
!
! Function:  LRSTTYP_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:46 1999
!
! Description:
!  Lookup function for matching RSTTYP description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LRSTTYP_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LRSTTYP_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. '3 POINT BELT' ) THEN
        LRSTTYP_CODE = '3PT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BAG' ) THEN
        LRSTTYP_CODE = 'ABG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BELT' ) THEN
        LRSTTYP_CODE = 'ABT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'APR PADDING' ) THEN
        LRSTTYP_CODE = 'APP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHILD RESTRAINT' ) THEN
        LRSTTYP_CODE = 'CHD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LRSTTYP_CODE = 'DPL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FBRGL. HNCB. PADDING' ) THEN
        LRSTTYP_CODE = 'FHP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE RESTRAINT' ) THEN
        LRSTTYP_CODE = 'KNE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT' ) THEN
        LRSTTYP_CODE = 'LAP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MINICARS PADDING' ) THEN
        LRSTTYP_CODE = 'MCP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NONE' ) THEN
        LRSTTYP_CODE = 'NON'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LRSTTYP_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PADDING' ) THEN
        LRSTTYP_CODE = 'PAD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PASSIVE 2 POINT BELT' ) THEN
        LRSTTYP_CODE = 'PS2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PASSIVE 3 POINT BELT' ) THEN
        LRSTTYP_CODE = 'PS3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGID WALL' ) THEN
        LRSTTYP_CODE = 'RIG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LRSTTYP_CODE = 'SBK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. COLUMN - EA' ) THEN
        LRSTTYP_CODE = 'SCE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. COLUMN - NO EA' ) THEN
        LRSTTYP_CODE = 'SCN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. WHEEL - EA' ) THEN
        LRSTTYP_CODE = 'SWE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STR. WHEEL - NO EA' ) THEN
        LRSTTYP_CODE = 'SWN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LRSTTYP_CODE = 'UNK'
        RETURN
      END IF

      RETURN

      END

! end of LRSTTYP_CODE()


!***********************************************************************
!
! Function:  LSENATT_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:47 1999
!
! Description:
!  Lookup function for matching SENATT code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSENATT_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LSENATT_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'ABD1' ) THEN
        LSENATT_DESC = 'ABDOMEN CONTACT SWITCH 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABD2' ) THEN
        LSENATT_DESC = 'ABDOMEN  CONTACT SWITCH 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABD3' ) THEN
        LSENATT_DESC = 'ABDOMEN CONTACT SWITCH 3'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABDO' ) THEN
        LSENATT_DESC = 'ABDOMEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABGD' ) THEN
        LSENATT_DESC = 'AIR BAG DIAGNOSTIC CIRCUIT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABGL' ) THEN
        LSENATT_DESC = 'AIR BAG LAMP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABGM' ) THEN
        LSENATT_DESC = 'AIR BAG MANIFOLD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABSF' ) THEN
        LSENATT_DESC = 'AIR BAG SAFING CIRCUIT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABSQ' ) THEN
        LSENATT_DESC = 'AIR BAG SQUIB CIRCUIT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABTF' ) THEN
        LSENATT_DESC = 'AIR BAG TRIP-SWITCH - FIREWALL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABTK' ) THEN
        LSENATT_DESC = 'KNEE CONTACT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABTL' ) THEN
        LSENATT_DESC = 'AIR BAG TRIP-SWITCH - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABTM' ) THEN
        LSENATT_DESC = 'AIR BELT MANIFOLD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABTR' ) THEN
        LSENATT_DESC = 'AIR BAG TRIP-SWITCH - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ABTS' ) THEN
        LSENATT_DESC = 'AIR BAG TRIP-SWITCH - ST. COL.'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ANKL' ) THEN
        LSENATT_DESC = 'ANKLE - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ANKR' ) THEN
        LSENATT_DESC = 'ANKLE - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'APLL' ) THEN
        LSENATT_DESC = 'A PILLAR - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'APLR' ) THEN
        LSENATT_DESC = 'A PILLAR - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BAFF' ) THEN
        LSENATT_DESC = 'BARRIER FRONT FACING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BMPF' ) THEN
        LSENATT_DESC = 'BUMPER - FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BMPP' ) THEN
        LSENATT_DESC = 'BUMPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BMPR' ) THEN
        LSENATT_DESC = 'BUMPER - REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BODY' ) THEN
        LSENATT_DESC = 'BODY BLOCK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BPLL' ) THEN
        LSENATT_DESC = 'B PILLAR - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BPLR' ) THEN
        LSENATT_DESC = 'B PILLAR - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BRCL' ) THEN
        LSENATT_DESC = 'BRAKE CALIPER - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BRCR' ) THEN
        LSENATT_DESC = 'BRAKE CALIPER - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHBD' ) THEN
        LSENATT_DESC = 'CHESTBAND'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHLL' ) THEN
        LSENATT_DESC = 'CHEST - LEFT LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHLU' ) THEN
        LSENATT_DESC = 'CHEST - LEFT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHRL' ) THEN
        LSENATT_DESC = 'CHEST RIGHT - LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHRU' ) THEN
        LSENATT_DESC = 'CHEST - RIGHT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CHST' ) THEN
        LSENATT_DESC = 'CHEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CLLC' ) THEN
        LSENATT_DESC = '8 STRING POT - STRAIGHT LL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CLLS' ) THEN
        LSENATT_DESC = '8 STRING POT - STRAIGHT LL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CLRC' ) THEN
        LSENATT_DESC = '8 STRING POT - CROSS LR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CLRS' ) THEN
        LSENATT_DESC = '8 STRING POT - STRAIGHT LR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CPLL' ) THEN
        LSENATT_DESC = 'C PILLAR - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CPLR' ) THEN
        LSENATT_DESC = 'C PILLAR - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CRBV' ) THEN
        LSENATT_DESC = 'CEREBROVASCULAR SYSTEM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CRDV' ) THEN
        LSENATT_DESC = 'CARDIOVASCULAR SYSTEM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CULC' ) THEN
        LSENATT_DESC = '8 STRING POT - CROSS UL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CULS' ) THEN
        LSENATT_DESC = '8 STRING POT - STRAIGHT UL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CURC' ) THEN
        LSENATT_DESC = '8 STRING POT - CROSS UR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CURS' ) THEN
        LSENATT_DESC = '8 STRING POT - STRAIGHT UR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DASH' ) THEN
        LSENATT_DESC = 'DASHPANEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DOOR' ) THEN
        LSENATT_DESC = 'DOOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DPLC' ) THEN
        LSENATT_DESC = 'DASHPANEL - CENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DPLL' ) THEN
        LSENATT_DESC = 'DASHPANEL - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DPLR' ) THEN
        LSENATT_DESC = 'DASHPANEL - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR1C' ) THEN
        LSENATT_DESC = 'DOOR - LEFT FRONT CENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR1M' ) THEN
        LSENATT_DESC = 'DOOR - LEFT FRONT MID-REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR1U' ) THEN
        LSENATT_DESC = 'DOOR - LEFT FRONT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR2C' ) THEN
        LSENATT_DESC = 'DOOR - RIGHT FRONT CENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR2M' ) THEN
        LSENATT_DESC = 'DOOR - RIGHT FRONT MID-REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR2U' ) THEN
        LSENATT_DESC = 'DOOR - RIGHT FRONT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR3C' ) THEN
        LSENATT_DESC = 'DOOR - RIGHT REAR CENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR3M' ) THEN
        LSENATT_DESC = 'DOOR - RIGHT REAR MID-REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR3U' ) THEN
        LSENATT_DESC = 'DOOR - RIGHT REAR UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR4C' ) THEN
        LSENATT_DESC = 'DOOR - LEFT REAR CENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR4M' ) THEN
        LSENATT_DESC = 'DOOR - LEFT REAR MID-REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DR4U' ) THEN
        LSENATT_DESC = 'DOOR - LEFT REAR UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRLF' ) THEN
        LSENATT_DESC = 'DOOR - LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRLR' ) THEN
        LSENATT_DESC = 'DOOR - LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRRF' ) THEN
        LSENATT_DESC = 'DOOR - RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRRR' ) THEN
        LSENATT_DESC = 'DOOR - RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRSL' ) THEN
        LSENATT_DESC = 'DOOR SILL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DSLF' ) THEN
        LSENATT_DESC = 'SILL - LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DSLR' ) THEN
        LSENATT_DESC = 'SILL - LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DSRF' ) THEN
        LSENATT_DESC = 'SILL - RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DSRR' ) THEN
        LSENATT_DESC = 'SILL - RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ENGN' ) THEN
        LSENATT_DESC = 'ENGINE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ESAF' ) THEN
        LSENATT_DESC = 'EUROSID FRONT ABDOMEN LOAD CELL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ESAM' ) THEN
        LSENATT_DESC = 'EUROSID MIDDLE ABDOMEN LOAD CELL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ESAR' ) THEN
        LSENATT_DESC = 'EUROSID REAR ABDOMEN LOAD CELL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'EVNT' ) THEN
        LSENATT_DESC = 'EVENT TIMES'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FFNL' ) THEN
        LSENATT_DESC = 'FENDER FRONT LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FFNR' ) THEN
        LSENATT_DESC = 'FENDER FRONT RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLLF' ) THEN
        LSENATT_DESC = 'FLOORPAN - LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLLR' ) THEN
        LSENATT_DESC = 'FLOORPAN - LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLOR' ) THEN
        LSENATT_DESC = 'FLOORPAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLRF' ) THEN
        LSENATT_DESC = 'FLOORPAN - RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLRR' ) THEN
        LSENATT_DESC = 'FLOORPAN - RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FLTU' ) THEN
        LSENATT_DESC = 'FLOORPAN TUNNEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FMRL' ) THEN
        LSENATT_DESC = 'FEMUR - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FMRR' ) THEN
        LSENATT_DESC = 'FEMUR - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FNDR' ) THEN
        LSENATT_DESC = 'FENDER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FOTL' ) THEN
        LSENATT_DESC = 'FOOT - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FOTR' ) THEN
        LSENATT_DESC = 'FOOT - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRAM' ) THEN
        LSENATT_DESC = 'FRAME RAIL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRCF' ) THEN
        LSENATT_DESC = 'FRAME CROSSMEMBER - FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRCR' ) THEN
        LSENATT_DESC = 'FRAME CROSSMEMBER - REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRRF' ) THEN
        LSENATT_DESC = 'FRAME RAIL - FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRRR' ) THEN
        LSENATT_DESC = 'FRAME RAIL - REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRSL' ) THEN
        LSENATT_DESC = 'FRAME SIDE RAIL - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRSR' ) THEN
        LSENATT_DESC = 'FRAME SIDE RAIL - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FWLL' ) THEN
        LSENATT_DESC = 'FIREWALL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'GUAL' ) THEN
        LSENATT_DESC = 'GUARD LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'GUAR' ) THEN
        LSENATT_DESC = 'GUARD RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HD90' ) THEN
        LSENATT_DESC = 'HEAD 9 ARRAY - CENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HD9X' ) THEN
        LSENATT_DESC = 'HEAD 9 ARRAY - X ARM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HD9Y' ) THEN
        LSENATT_DESC = 'HEAD 9 ARRAY - Y ARM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HD9Z' ) THEN
        LSENATT_DESC = 'HEAD 9 ARRAY - Z ARM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDCG' ) THEN
        LSENATT_DESC = 'HEAD CG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDFR' ) THEN
        LSENATT_DESC = 'RIGID HEADFORM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDOT' ) THEN
        LSENATT_DESC = 'HEAD OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDRL' ) THEN
        LSENATT_DESC = 'HEADER - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDRR' ) THEN
        LSENATT_DESC = 'HEADER - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HEMI' ) THEN
        LSENATT_DESC = 'RIGID HEMISPHERE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HLCR' ) THEN
        LSENATT_DESC = 'HOOD LATCH CROSSMEMBER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'IMCG' ) THEN
        LSENATT_DESC = 'IMPACTOR CG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KNEL' ) THEN
        LSENATT_DESC = 'KNEE - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KNER' ) THEN
        LSENATT_DESC = 'KNEE - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA0' ) THEN
        LSENATT_DESC = 'LOAD CELL A10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA1' ) THEN
        LSENATT_DESC = 'LOAD CELL A1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA2' ) THEN
        LSENATT_DESC = 'LOAD CELL A2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA3' ) THEN
        LSENATT_DESC = 'LOAD CELL A3'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA4' ) THEN
        LSENATT_DESC = 'LOAD CELL A4'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA5' ) THEN
        LSENATT_DESC = 'LOAD CELL A5'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA6' ) THEN
        LSENATT_DESC = 'LOAD CELL A6'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA7' ) THEN
        LSENATT_DESC = 'LOAD CELL A7'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA8' ) THEN
        LSENATT_DESC = 'LOAD CELL A8'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCA9' ) THEN
        LSENATT_DESC = 'LOAD CELL A9'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB0' ) THEN
        LSENATT_DESC = 'LOAD CELL B10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB1' ) THEN
        LSENATT_DESC = 'LOAD CELL B1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB2' ) THEN
        LSENATT_DESC = 'LOAD CELL B2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB3' ) THEN
        LSENATT_DESC = 'LOAD CELL B3'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB4' ) THEN
        LSENATT_DESC = 'LOAD CELL B4'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB5' ) THEN
        LSENATT_DESC = 'LOAD CELL B5'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB6' ) THEN
        LSENATT_DESC = 'LOAD CELL B6'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB7' ) THEN
        LSENATT_DESC = 'LOAD CELL B7'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB8' ) THEN
        LSENATT_DESC = 'LOAD CELL B8'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCB9' ) THEN
        LSENATT_DESC = 'LOAD CELL B9'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC0' ) THEN
        LSENATT_DESC = 'LOAD CELL C10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC1' ) THEN
        LSENATT_DESC = 'LOAD CELL C1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC2' ) THEN
        LSENATT_DESC = 'LOAD CELL C2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC3' ) THEN
        LSENATT_DESC = 'LOAD CELL C3'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC4' ) THEN
        LSENATT_DESC = 'LOAD CELL C4'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC5' ) THEN
        LSENATT_DESC = 'LOAD CELL C5'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC6' ) THEN
        LSENATT_DESC = 'LOAD CELL C6'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC7' ) THEN
        LSENATT_DESC = 'LOAD CELL C7'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC8' ) THEN
        LSENATT_DESC = 'LOAD CELL C8'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCC9' ) THEN
        LSENATT_DESC = 'LOAD CELL C9'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD0' ) THEN
        LSENATT_DESC = 'LOAD CELL D10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD1' ) THEN
        LSENATT_DESC = 'LOAD CELL D1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD2' ) THEN
        LSENATT_DESC = 'LOAD CELL D2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD3' ) THEN
        LSENATT_DESC = 'LOAD CELL D3'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD4' ) THEN
        LSENATT_DESC = 'LOAD CELL D4'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD5' ) THEN
        LSENATT_DESC = 'LOAD CELL D5'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD6' ) THEN
        LSENATT_DESC = 'LOAD CELL D6'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD7' ) THEN
        LSENATT_DESC = 'LOAD CELL D7'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD8' ) THEN
        LSENATT_DESC = 'LOAD CELL D8'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LCD9' ) THEN
        LSENATT_DESC = 'LOAD CELL D9'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LPBI' ) THEN
        LSENATT_DESC = 'LAP BELT - INBOARD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LPBO' ) THEN
        LSENATT_DESC = 'LAP BELT - OUTBOARD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LUMB' ) THEN
        LSENATT_DESC = 'LUMBAR SPINE LOAD CELL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NECK' ) THEN
        LSENATT_DESC = 'NECK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NEKL' ) THEN
        LSENATT_DESC = 'NECK - LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NEKU' ) THEN
        LSENATT_DESC = 'NECK - UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTHR' ) THEN
        LSENATT_DESC = 'OTHER SENATT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PEND' ) THEN
        LSENATT_DESC = 'PENDULUM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLAP' ) THEN
        LSENATT_DESC = 'A PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLBP' ) THEN
        LSENATT_DESC = 'B PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PLCP' ) THEN
        LSENATT_DESC = 'C PILLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'POLE' ) THEN
        LSENATT_DESC = 'POLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PULM' ) THEN
        LSENATT_DESC = 'PULMONARY SYSTEM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVCN' ) THEN
        LSENATT_DESC = 'PELVIS - CENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVHP' ) THEN
        LSENATT_DESC = 'PELVIS - H-POINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVIL' ) THEN
        LSENATT_DESC = 'PELVIS - ILIAC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVLL' ) THEN
        LSENATT_DESC = 'PELVIS - LEFT LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVLM' ) THEN
        LSENATT_DESC = 'PELVIS - LEFT MIDDLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVLU' ) THEN
        LSENATT_DESC = 'PELVIS - LEFT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVPS' ) THEN
        LSENATT_DESC = 'PELVIS PUBIC SYMPHYSIS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVRL' ) THEN
        LSENATT_DESC = 'PELVIS - RIGHT LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVRM' ) THEN
        LSENATT_DESC = 'PELVIS - RIGHT MIDDLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVRU' ) THEN
        LSENATT_DESC = 'PELVIS - RIGHT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PVSA' ) THEN
        LSENATT_DESC = 'PELVIS, SACRUM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RAXL' ) THEN
        LSENATT_DESC = 'REAR AXLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RBLA' ) THEN
        LSENATT_DESC = 'RIB, LOWER ABDOMEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RBLL' ) THEN
        LSENATT_DESC = 'RIB - LEFT LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RBLM' ) THEN
        LSENATT_DESC = 'RIB - LEFT MIDDLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RBLU' ) THEN
        LSENATT_DESC = 'RIB - LEFT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RBRL' ) THEN
        LSENATT_DESC = 'RIB - RIGHT LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RBRM' ) THEN
        LSENATT_DESC = 'RIB - RIGHT MIDDLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RBRU' ) THEN
        LSENATT_DESC = 'RIB - RIGHT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RBUA' ) THEN
        LSENATT_DESC = 'RIB, UPPER ABDOMEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'REDK' ) THEN
        LSENATT_DESC = 'REAR DECK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ROFR' ) THEN
        LSENATT_DESC = 'ROOF RAIL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ROLC' ) THEN
        LSENATT_DESC = 'ROLLOVER CART'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ROOF' ) THEN
        LSENATT_DESC = 'ROOF'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RRLF' ) THEN
        LSENATT_DESC = 'ROOF RAIL LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RRLR' ) THEN
        LSENATT_DESC = 'ROOF RAIL LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RRRF' ) THEN
        LSENATT_DESC = 'ROOF RAIL RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RRRR' ) THEN
        LSENATT_DESC = 'ROOF RAIL RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEAT' ) THEN
        LSENATT_DESC = 'SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEBK' ) THEN
        LSENATT_DESC = 'SEAT BACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEBS' ) THEN
        LSENATT_DESC = 'SEAT BASE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SECF' ) THEN
        LSENATT_DESC = 'SEAT - CENTER FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SECR' ) THEN
        LSENATT_DESC = 'SEAT - CENTER REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SECU' ) THEN
        LSENATT_DESC = 'SEAT CUSHION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SELF' ) THEN
        LSENATT_DESC = 'SEAT - LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SELR' ) THEN
        LSENATT_DESC = 'SEAT - LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SERF' ) THEN
        LSENATT_DESC = 'SEAT - RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SERR' ) THEN
        LSENATT_DESC = 'SEAT - RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SETR' ) THEN
        LSENATT_DESC = 'SEAT TRACK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SHBE' ) THEN
        LSENATT_DESC = 'SHOULDER BELT EXTENSION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SHBT' ) THEN
        LSENATT_DESC = 'SHOULDER BELT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SHLL' ) THEN
        LSENATT_DESC = 'SHOULDER - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SHLR' ) THEN
        LSENATT_DESC = 'SHOULDER - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SLED' ) THEN
        LSENATT_DESC = 'SLED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SNML' ) THEN
        LSENATT_DESC = 'STERNUM - LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SNMU' ) THEN
        LSENATT_DESC = 'STERNUM - UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SPNL' ) THEN
        LSENATT_DESC = 'SPINE - LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SPNM' ) THEN
        LSENATT_DESC = 'SPINE - MIDDLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SPNU' ) THEN
        LSENATT_DESC = 'SPINE - UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STCL' ) THEN
        LSENATT_DESC = 'STEERING COLUMN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STER' ) THEN
        LSENATT_DESC = 'STEERING WHEEL/COLUMN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STLF' ) THEN
        LSENATT_DESC = 'SEAT TRACK - LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STLR' ) THEN
        LSENATT_DESC = 'SEAT TRACK - LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STRF' ) THEN
        LSENATT_DESC = 'SEAT TRACK - RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'STRR' ) THEN
        LSENATT_DESC = 'SEAT TRACK - RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SULF' ) THEN
        LSENATT_DESC = 'SUSPENSION - LEFT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SULR' ) THEN
        LSENATT_DESC = 'SUSPENSION - LEFT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SURF' ) THEN
        LSENATT_DESC = 'SUSPENSION - RIGHT FRONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SURR' ) THEN
        LSENATT_DESC = 'SUSPENSION - RIGHT REAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SUSP' ) THEN
        LSENATT_DESC = 'SUSPENSION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWHB' ) THEN
        LSENATT_DESC = 'STEERING WHEEL HUB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWRM' ) THEN
        LSENATT_DESC = 'STEERING WHEEL RIM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TBLL' ) THEN
        LSENATT_DESC = 'TIBIA - LEFT LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TBLU' ) THEN
        LSENATT_DESC = 'TIBIA - LEFT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TBRL' ) THEN
        LSENATT_DESC = 'TIBIA - RIGHT LOWER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TBRU' ) THEN
        LSENATT_DESC = 'TIBIA - RIGHT UPPER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TIBL' ) THEN
        LSENATT_DESC = 'TIBIA - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TIBR' ) THEN
        LSENATT_DESC = 'TIBIA - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TRFC' ) THEN
        LSENATT_DESC = 'TRUNK FLOOR - CENTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TRFL' ) THEN
        LSENATT_DESC = 'TRUNK FLOOR - LEFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TRFR' ) THEN
        LSENATT_DESC = 'TRUNK FLOOR - RIGHT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TSTD' ) THEN
        LSENATT_DESC = 'TEST DEVICE (GENERAL)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VECG' ) THEN
        LSENATT_DESC = 'VEHICLE CG'
        RETURN
      END IF

      RETURN

      END

! end of LSENATT_DESC()


!***********************************************************************
!
! Function:  LSENATT_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:47 1999
!
! Description:
!  Lookup function for matching SENATT description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSENATT_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LSENATT_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. '8 STRING POT - CROSS LR' ) THEN
        LSENATT_CODE = 'CLRC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '8 STRING POT - CROSS UL' ) THEN
        LSENATT_CODE = 'CULC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '8 STRING POT - CROSS UR' ) THEN
        LSENATT_CODE = 'CURC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & '8 STRING POT - STRAIGHT LL' ) THEN
        LSENATT_CODE = 'CLLC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & '8 STRING POT - STRAIGHT LR' ) THEN
        LSENATT_CODE = 'CLRS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & '8 STRING POT - STRAIGHT UL' ) THEN
        LSENATT_CODE = 'CULS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & '8 STRING POT - STRAIGHT UR' ) THEN
        LSENATT_CODE = 'CURS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'A PILLAR' ) THEN
        LSENATT_CODE = 'PLAP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'A PILLAR - LEFT' ) THEN
        LSENATT_CODE = 'APLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'A PILLAR - RIGHT' ) THEN
        LSENATT_CODE = 'APLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ABDOMEN' ) THEN
        LSENATT_CODE = 'ABDO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ABDOMEN  CONTACT SWITCH 2' ) THEN
        LSENATT_CODE = 'ABD2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ABDOMEN CONTACT SWITCH 1' ) THEN
        LSENATT_CODE = 'ABD1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ABDOMEN CONTACT SWITCH 3' ) THEN
        LSENATT_CODE = 'ABD3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'AIR BAG DIAGNOSTIC CIRCUIT' ) THEN
        LSENATT_CODE = 'ABGD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BAG LAMP' ) THEN
        LSENATT_CODE = 'ABGL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BAG MANIFOLD' ) THEN
        LSENATT_CODE = 'ABGM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BAG SAFING CIRCUIT' ) THEN
        LSENATT_CODE = 'ABSF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BAG SQUIB CIRCUIT' ) THEN
        LSENATT_CODE = 'ABSQ'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'AIR BAG TRIP-SWITCH - FIREWALL' ) THEN
        LSENATT_CODE = 'ABTF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'AIR BAG TRIP-SWITCH - LEFT' ) THEN
        LSENATT_CODE = 'ABTL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'AIR BAG TRIP-SWITCH - RIGHT' ) THEN
        LSENATT_CODE = 'ABTR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'AIR BAG TRIP-SWITCH - ST. COL.' ) THEN
        LSENATT_CODE = 'ABTS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AIR BELT MANIFOLD' ) THEN
        LSENATT_CODE = 'ABTM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ANKLE - LEFT' ) THEN
        LSENATT_CODE = 'ANKL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ANKLE - RIGHT' ) THEN
        LSENATT_CODE = 'ANKR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B PILLAR' ) THEN
        LSENATT_CODE = 'PLBP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B PILLAR - LEFT' ) THEN
        LSENATT_CODE = 'BPLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B PILLAR - RIGHT' ) THEN
        LSENATT_CODE = 'BPLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BARRIER FRONT FACING' ) THEN
        LSENATT_CODE = 'BAFF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BODY BLOCK' ) THEN
        LSENATT_CODE = 'BODY'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BRAKE CALIPER - LEFT' ) THEN
        LSENATT_CODE = 'BRCL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BRAKE CALIPER - RIGHT' ) THEN
        LSENATT_CODE = 'BRCR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BUMPER' ) THEN
        LSENATT_CODE = 'BMPP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BUMPER - FRONT' ) THEN
        LSENATT_CODE = 'BMPF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BUMPER - REAR' ) THEN
        LSENATT_CODE = 'BMPR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C PILLAR' ) THEN
        LSENATT_CODE = 'PLCP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C PILLAR - LEFT' ) THEN
        LSENATT_CODE = 'CPLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C PILLAR - RIGHT' ) THEN
        LSENATT_CODE = 'CPLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CARDIOVASCULAR SYSTEM' ) THEN
        LSENATT_CODE = 'CRDV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CEREBROVASCULAR SYSTEM' ) THEN
        LSENATT_CODE = 'CRBV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEST' ) THEN
        LSENATT_CODE = 'CHST'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEST - LEFT LOWER' ) THEN
        LSENATT_CODE = 'CHLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEST - LEFT UPPER' ) THEN
        LSENATT_CODE = 'CHLU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEST - RIGHT UPPER' ) THEN
        LSENATT_CODE = 'CHRU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEST RIGHT - LOWER' ) THEN
        LSENATT_CODE = 'CHRL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHESTBAND' ) THEN
        LSENATT_CODE = 'CHBD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL' ) THEN
        LSENATT_CODE = 'DASH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL - CENTER' ) THEN
        LSENATT_CODE = 'DPLC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL - LEFT' ) THEN
        LSENATT_CODE = 'DPLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHPANEL - RIGHT' ) THEN
        LSENATT_CODE = 'DPLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR' ) THEN
        LSENATT_CODE = 'DOOR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - LEFT FRONT' ) THEN
        LSENATT_CODE = 'DRLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - LEFT FRONT CENTER' ) THEN
        LSENATT_CODE = 'DR1C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'DOOR - LEFT FRONT MID-REAR' ) THEN
        LSENATT_CODE = 'DR1M'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - LEFT FRONT UPPER' ) THEN
        LSENATT_CODE = 'DR1U'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - LEFT REAR' ) THEN
        LSENATT_CODE = 'DRLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - LEFT REAR CENTER' ) THEN
        LSENATT_CODE = 'DR4C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - LEFT REAR MID-REAR' ) THEN
        LSENATT_CODE = 'DR4M'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - LEFT REAR UPPER' ) THEN
        LSENATT_CODE = 'DR4U'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - RIGHT FRONT' ) THEN
        LSENATT_CODE = 'DRRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - RIGHT FRONT CENTER' ) THEN
        LSENATT_CODE = 'DR2C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'DOOR - RIGHT FRONT MID-REAR' ) THEN
        LSENATT_CODE = 'DR2M'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - RIGHT FRONT UPPER' ) THEN
        LSENATT_CODE = 'DR2U'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - RIGHT REAR' ) THEN
        LSENATT_CODE = 'DRRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - RIGHT REAR CENTER' ) THEN
        LSENATT_CODE = 'DR3C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'DOOR - RIGHT REAR MID-REAR' ) THEN
        LSENATT_CODE = 'DR3M'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR - RIGHT REAR UPPER' ) THEN
        LSENATT_CODE = 'DR3U'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DOOR SILL' ) THEN
        LSENATT_CODE = 'DRSL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ENGINE' ) THEN
        LSENATT_CODE = 'ENGN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EUROSID FRONT ABDOMEN LOAD CELL' ) THEN
        LSENATT_CODE = 'ESAF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EUROSID MIDDLE ABDOMEN LOAD CELL' ) THEN
        LSENATT_CODE = 'ESAM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EUROSID REAR ABDOMEN LOAD CELL' ) THEN
        LSENATT_CODE = 'ESAR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EVENT TIMES' ) THEN
        LSENATT_CODE = 'EVNT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FEMUR - LEFT' ) THEN
        LSENATT_CODE = 'FMRL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FEMUR - RIGHT' ) THEN
        LSENATT_CODE = 'FMRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FENDER' ) THEN
        LSENATT_CODE = 'FNDR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FENDER FRONT LEFT' ) THEN
        LSENATT_CODE = 'FFNL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FENDER FRONT RIGHT' ) THEN
        LSENATT_CODE = 'FFNR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FIREWALL' ) THEN
        LSENATT_CODE = 'FWLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN' ) THEN
        LSENATT_CODE = 'FLOR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN - LEFT FRONT' ) THEN
        LSENATT_CODE = 'FLLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN - LEFT REAR' ) THEN
        LSENATT_CODE = 'FLLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN - RIGHT FRONT' ) THEN
        LSENATT_CODE = 'FLRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN - RIGHT REAR' ) THEN
        LSENATT_CODE = 'FLRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLOORPAN TUNNEL' ) THEN
        LSENATT_CODE = 'FLTU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FOOT - LEFT' ) THEN
        LSENATT_CODE = 'FOTL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FOOT - RIGHT' ) THEN
        LSENATT_CODE = 'FOTR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME CROSSMEMBER - FRONT' ) THEN
        LSENATT_CODE = 'FRCF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME CROSSMEMBER - REAR' ) THEN
        LSENATT_CODE = 'FRCR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME RAIL' ) THEN
        LSENATT_CODE = 'FRAM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME RAIL - FRONT' ) THEN
        LSENATT_CODE = 'FRRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME RAIL - REAR' ) THEN
        LSENATT_CODE = 'FRRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME SIDE RAIL - LEFT' ) THEN
        LSENATT_CODE = 'FRSL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRAME SIDE RAIL - RIGHT' ) THEN
        LSENATT_CODE = 'FRSR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GUARD LEFT' ) THEN
        LSENATT_CODE = 'GUAL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GUARD RIGHT' ) THEN
        LSENATT_CODE = 'GUAR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD 9 ARRAY - CENTER' ) THEN
        LSENATT_CODE = 'HD90'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD 9 ARRAY - X ARM' ) THEN
        LSENATT_CODE = 'HD9X'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD 9 ARRAY - Y ARM' ) THEN
        LSENATT_CODE = 'HD9Y'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD 9 ARRAY - Z ARM' ) THEN
        LSENATT_CODE = 'HD9Z'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD CG' ) THEN
        LSENATT_CODE = 'HDCG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEAD OTHER' ) THEN
        LSENATT_CODE = 'HDOT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - LEFT' ) THEN
        LSENATT_CODE = 'HDRL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEADER - RIGHT' ) THEN
        LSENATT_CODE = 'HDRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HOOD LATCH CROSSMEMBER' ) THEN
        LSENATT_CODE = 'HLCR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IMPACTOR CG' ) THEN
        LSENATT_CODE = 'IMCG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE - LEFT' ) THEN
        LSENATT_CODE = 'KNEL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE - RIGHT' ) THEN
        LSENATT_CODE = 'KNER'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KNEE CONTACT' ) THEN
        LSENATT_CODE = 'ABTK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT - INBOARD' ) THEN
        LSENATT_CODE = 'LPBI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAP BELT - OUTBOARD' ) THEN
        LSENATT_CODE = 'LPBO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A1' ) THEN
        LSENATT_CODE = 'LCA1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A10' ) THEN
        LSENATT_CODE = 'LCA0'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A2' ) THEN
        LSENATT_CODE = 'LCA2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A3' ) THEN
        LSENATT_CODE = 'LCA3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A4' ) THEN
        LSENATT_CODE = 'LCA4'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A5' ) THEN
        LSENATT_CODE = 'LCA5'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A6' ) THEN
        LSENATT_CODE = 'LCA6'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A7' ) THEN
        LSENATT_CODE = 'LCA7'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A8' ) THEN
        LSENATT_CODE = 'LCA8'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL A9' ) THEN
        LSENATT_CODE = 'LCA9'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B1' ) THEN
        LSENATT_CODE = 'LCB1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B10' ) THEN
        LSENATT_CODE = 'LCB0'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B2' ) THEN
        LSENATT_CODE = 'LCB2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B3' ) THEN
        LSENATT_CODE = 'LCB3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B4' ) THEN
        LSENATT_CODE = 'LCB4'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B5' ) THEN
        LSENATT_CODE = 'LCB5'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B6' ) THEN
        LSENATT_CODE = 'LCB6'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B7' ) THEN
        LSENATT_CODE = 'LCB7'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B8' ) THEN
        LSENATT_CODE = 'LCB8'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL B9' ) THEN
        LSENATT_CODE = 'LCB9'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C1' ) THEN
        LSENATT_CODE = 'LCC1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C10' ) THEN
        LSENATT_CODE = 'LCC0'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C2' ) THEN
        LSENATT_CODE = 'LCC2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C3' ) THEN
        LSENATT_CODE = 'LCC3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C4' ) THEN
        LSENATT_CODE = 'LCC4'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C5' ) THEN
        LSENATT_CODE = 'LCC5'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C6' ) THEN
        LSENATT_CODE = 'LCC6'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C7' ) THEN
        LSENATT_CODE = 'LCC7'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C8' ) THEN
        LSENATT_CODE = 'LCC8'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL C9' ) THEN
        LSENATT_CODE = 'LCC9'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D1' ) THEN
        LSENATT_CODE = 'LCD1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D10' ) THEN
        LSENATT_CODE = 'LCD0'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D2' ) THEN
        LSENATT_CODE = 'LCD2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D3' ) THEN
        LSENATT_CODE = 'LCD3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D4' ) THEN
        LSENATT_CODE = 'LCD4'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D5' ) THEN
        LSENATT_CODE = 'LCD5'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D6' ) THEN
        LSENATT_CODE = 'LCD6'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D7' ) THEN
        LSENATT_CODE = 'LCD7'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D8' ) THEN
        LSENATT_CODE = 'LCD8'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL D9' ) THEN
        LSENATT_CODE = 'LCD9'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LUMBAR SPINE LOAD CELL' ) THEN
        LSENATT_CODE = 'LUMB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NECK' ) THEN
        LSENATT_CODE = 'NECK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NECK - LOWER' ) THEN
        LSENATT_CODE = 'NEKL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NECK - UPPER' ) THEN
        LSENATT_CODE = 'NEKU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER SENATT' ) THEN
        LSENATT_CODE = 'OTHR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS - CENTER' ) THEN
        LSENATT_CODE = 'PVCN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS - H-POINT' ) THEN
        LSENATT_CODE = 'PVHP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS - ILIAC' ) THEN
        LSENATT_CODE = 'PVIL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS - LEFT LOWER' ) THEN
        LSENATT_CODE = 'PVLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS - LEFT MIDDLE' ) THEN
        LSENATT_CODE = 'PVLM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS - LEFT UPPER' ) THEN
        LSENATT_CODE = 'PVLU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS - RIGHT LOWER' ) THEN
        LSENATT_CODE = 'PVRL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS - RIGHT MIDDLE' ) THEN
        LSENATT_CODE = 'PVRM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS - RIGHT UPPER' ) THEN
        LSENATT_CODE = 'PVRU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS PUBIC SYMPHYSIS' ) THEN
        LSENATT_CODE = 'PVPS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PELVIS, SACRUM' ) THEN
        LSENATT_CODE = 'PVSA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PENDULUM' ) THEN
        LSENATT_CODE = 'PEND'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'POLE' ) THEN
        LSENATT_CODE = 'POLE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PULMONARY SYSTEM' ) THEN
        LSENATT_CODE = 'PULM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'REAR AXLE' ) THEN
        LSENATT_CODE = 'RAXL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'REAR DECK' ) THEN
        LSENATT_CODE = 'REDK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIB - LEFT LOWER' ) THEN
        LSENATT_CODE = 'RBLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIB - LEFT MIDDLE' ) THEN
        LSENATT_CODE = 'RBLM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIB - LEFT UPPER' ) THEN
        LSENATT_CODE = 'RBLU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIB - RIGHT LOWER' ) THEN
        LSENATT_CODE = 'RBRL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIB - RIGHT MIDDLE' ) THEN
        LSENATT_CODE = 'RBRM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIB - RIGHT UPPER' ) THEN
        LSENATT_CODE = 'RBRU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIB, LOWER ABDOMEN' ) THEN
        LSENATT_CODE = 'RBLA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIB, UPPER ABDOMEN' ) THEN
        LSENATT_CODE = 'RBUA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGID HEADFORM' ) THEN
        LSENATT_CODE = 'HDFR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGID HEMISPHERE' ) THEN
        LSENATT_CODE = 'HEMI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROLLOVER CART' ) THEN
        LSENATT_CODE = 'ROLC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF' ) THEN
        LSENATT_CODE = 'ROOF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF RAIL' ) THEN
        LSENATT_CODE = 'ROFR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF RAIL LEFT FRONT' ) THEN
        LSENATT_CODE = 'RRLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF RAIL LEFT REAR' ) THEN
        LSENATT_CODE = 'RRLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF RAIL RIGHT FRONT' ) THEN
        LSENATT_CODE = 'RRRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROOF RAIL RIGHT REAR' ) THEN
        LSENATT_CODE = 'RRRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT' ) THEN
        LSENATT_CODE = 'SEAT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT - CENTER FRONT' ) THEN
        LSENATT_CODE = 'SECF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT - CENTER REAR' ) THEN
        LSENATT_CODE = 'SECR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT - LEFT FRONT' ) THEN
        LSENATT_CODE = 'SELF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT - LEFT REAR' ) THEN
        LSENATT_CODE = 'SELR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT - RIGHT FRONT' ) THEN
        LSENATT_CODE = 'SERF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT - RIGHT REAR' ) THEN
        LSENATT_CODE = 'SERR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BACK' ) THEN
        LSENATT_CODE = 'SEBK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT BASE' ) THEN
        LSENATT_CODE = 'SEBS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT CUSHION' ) THEN
        LSENATT_CODE = 'SECU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT TRACK' ) THEN
        LSENATT_CODE = 'SETR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT TRACK - LEFT FRONT' ) THEN
        LSENATT_CODE = 'STLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT TRACK - LEFT REAR' ) THEN
        LSENATT_CODE = 'STLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT TRACK - RIGHT FRONT' ) THEN
        LSENATT_CODE = 'STRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEAT TRACK - RIGHT REAR' ) THEN
        LSENATT_CODE = 'STRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER - LEFT' ) THEN
        LSENATT_CODE = 'SHLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER - RIGHT' ) THEN
        LSENATT_CODE = 'SHLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER BELT' ) THEN
        LSENATT_CODE = 'SHBT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHOULDER BELT EXTENSION' ) THEN
        LSENATT_CODE = 'SHBE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SILL - LEFT FRONT' ) THEN
        LSENATT_CODE = 'DSLF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SILL - LEFT REAR' ) THEN
        LSENATT_CODE = 'DSLR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SILL - RIGHT FRONT' ) THEN
        LSENATT_CODE = 'DSRF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SILL - RIGHT REAR' ) THEN
        LSENATT_CODE = 'DSRR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SLED' ) THEN
        LSENATT_CODE = 'SLED'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPINE - LOWER' ) THEN
        LSENATT_CODE = 'SPNL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPINE - MIDDLE' ) THEN
        LSENATT_CODE = 'SPNM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPINE - UPPER' ) THEN
        LSENATT_CODE = 'SPNU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING COLUMN' ) THEN
        LSENATT_CODE = 'STCL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL HUB' ) THEN
        LSENATT_CODE = 'SWHB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL RIM' ) THEN
        LSENATT_CODE = 'SWRM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEERING WHEEL/COLUMN' ) THEN
        LSENATT_CODE = 'STER'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STERNUM - LOWER' ) THEN
        LSENATT_CODE = 'SNML'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STERNUM - UPPER' ) THEN
        LSENATT_CODE = 'SNMU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUSPENSION' ) THEN
        LSENATT_CODE = 'SUSP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUSPENSION - LEFT FRONT' ) THEN
        LSENATT_CODE = 'SULF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUSPENSION - LEFT REAR' ) THEN
        LSENATT_CODE = 'SULR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUSPENSION - RIGHT FRONT' ) THEN
        LSENATT_CODE = 'SURF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUSPENSION - RIGHT REAR' ) THEN
        LSENATT_CODE = 'SURR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TEST DEVICE (GENERAL)' ) THEN
        LSENATT_CODE = 'TSTD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIBIA - LEFT' ) THEN
        LSENATT_CODE = 'TIBL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIBIA - LEFT LOWER' ) THEN
        LSENATT_CODE = 'TBLL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIBIA - LEFT UPPER' ) THEN
        LSENATT_CODE = 'TBLU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIBIA - RIGHT' ) THEN
        LSENATT_CODE = 'TIBR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIBIA - RIGHT LOWER' ) THEN
        LSENATT_CODE = 'TBRL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIBIA - RIGHT UPPER' ) THEN
        LSENATT_CODE = 'TBRU'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRUNK FLOOR - CENTER' ) THEN
        LSENATT_CODE = 'TRFC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRUNK FLOOR - LEFT' ) THEN
        LSENATT_CODE = 'TRFL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRUNK FLOOR - RIGHT' ) THEN
        LSENATT_CODE = 'TRFR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VEHICLE CG' ) THEN
        LSENATT_CODE = 'VECG'
        RETURN
      END IF

      RETURN

      END

! end of LSENATT_CODE()


!***********************************************************************
!
! Function:  LSENLOC_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:47 1999
!
! Description:
!  Lookup function for matching SENLOC code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSENLOC_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LSENLOC_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '01' ) THEN
        LSENLOC_DESC = 'LEFT FRONT SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '02' ) THEN
        LSENLOC_DESC = 'RIGHT FRONT SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '03' ) THEN
        LSENLOC_DESC = 'RIGHT REAR SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '04' ) THEN
        LSENLOC_DESC = 'LEFT REAR SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '05' ) THEN
        LSENLOC_DESC = 'CENTER FRONT SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '06' ) THEN
        LSENLOC_DESC = 'CENTER REAR SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '07' ) THEN
        LSENLOC_DESC = 'LEFT THIRD SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '08' ) THEN
        LSENLOC_DESC = 'CENTER THIRD SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '09' ) THEN
        LSENLOC_DESC = 'RIGHT THIRD SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AP' ) THEN
        LSENLOC_DESC = 'TEST APPARATUS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LSENLOC_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OC' ) THEN
        LSENLOC_DESC = 'TEST OCCUPANT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LSENLOC_DESC = 'OTHER'
        RETURN
      END IF

      RETURN

      END

! end of LSENLOC_DESC()


!***********************************************************************
!
! Function:  LSENLOC_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:47 1999
!
! Description:
!  Lookup function for matching SENLOC description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSENLOC_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LSENLOC_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'CENTER FRONT SEAT' ) THEN
        LSENLOC_CODE = '05'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CENTER REAR SEAT' ) THEN
        LSENLOC_CODE = '06'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CENTER THIRD SEAT' ) THEN
        LSENLOC_CODE = '08'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEFT FRONT SEAT' ) THEN
        LSENLOC_CODE = '01'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEFT REAR SEAT' ) THEN
        LSENLOC_CODE = '04'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEFT THIRD SEAT' ) THEN
        LSENLOC_CODE = '07'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LSENLOC_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LSENLOC_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGHT FRONT SEAT' ) THEN
        LSENLOC_CODE = '02'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGHT REAR SEAT' ) THEN
        LSENLOC_CODE = '03'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIGHT THIRD SEAT' ) THEN
        LSENLOC_CODE = '09'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TEST APPARATUS' ) THEN
        LSENLOC_CODE = 'AP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TEST OCCUPANT' ) THEN
        LSENLOC_CODE = 'OC'
        RETURN
      END IF

      RETURN

      END

! end of LSENLOC_CODE()


!***********************************************************************
!
! Function:  LSENTYP_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:47 1999
!
! Description:
!  Lookup function for matching SENTYP code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSENTYP_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LSENTYP_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AA' ) THEN
        LSENTYP_DESC = 'ANGULAR ACCELEROMETER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AC' ) THEN
        LSENTYP_DESC = 'ACCELEROMETER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AD' ) THEN
        LSENTYP_DESC = 'ANGULAR DISPLACEMENT TRANSDUCER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AV' ) THEN
        LSENTYP_DESC = 'ANGULAR VELOCITY TRANSDUCER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CC' ) THEN
        LSENTYP_DESC = 'CHESTBAND CURVATURE GAUGE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DS' ) THEN
        LSENTYP_DESC = 'DISPLACEMENT TRANSDUCER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET' ) THEN
        LSENTYP_DESC = 'EVENT TIME INDICATOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HL' ) THEN
        LSENTYP_DESC = 'HIGH LEVEL SIGNAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LC' ) THEN
        LSENTYP_DESC = 'LOAD CELL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LL' ) THEN
        LSENTYP_DESC = 'LOW LEVEL SIGNAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LSENTYP_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PR' ) THEN
        LSENTYP_DESC = 'PRESSURE TRANSDUCER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SG' ) THEN
        LSENTYP_DESC = 'STRAIN GAUGE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TB' ) THEN
        LSENTYP_DESC = 'TIME BASED CHANNEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VL' ) THEN
        LSENTYP_DESC = 'VELOCITY TRANSDUCER'
        RETURN
      END IF

      RETURN

      END

! end of LSENTYP_DESC()


!***********************************************************************
!
! Function:  LSENTYP_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:48 1999
!
! Description:
!  Lookup function for matching SENTYP description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSENTYP_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LSENTYP_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'ACCELEROMETER' ) THEN
        LSENTYP_CODE = 'AC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ANGULAR ACCELEROMETER' ) THEN
        LSENTYP_CODE = 'AA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'ANGULAR DISPLACEMENT TRANSDUCER' ) THEN
        LSENTYP_CODE = 'AD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'ANGULAR VELOCITY TRANSDUCER' ) THEN
        LSENTYP_CODE = 'AV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHESTBAND CURVATURE GAUGE' ) THEN
        LSENTYP_CODE = 'CC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DISPLACEMENT TRANSDUCER' ) THEN
        LSENTYP_CODE = 'DS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EVENT TIME INDICATOR' ) THEN
        LSENTYP_CODE = 'ET'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HIGH LEVEL SIGNAL' ) THEN
        LSENTYP_CODE = 'HL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL' ) THEN
        LSENTYP_CODE = 'LC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOW LEVEL SIGNAL' ) THEN
        LSENTYP_CODE = 'LL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LSENTYP_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PRESSURE TRANSDUCER' ) THEN
        LSENTYP_CODE = 'PR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STRAIN GAUGE' ) THEN
        LSENTYP_CODE = 'SG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIME BASED CHANNEL' ) THEN
        LSENTYP_CODE = 'TB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VELOCITY TRANSDUCER' ) THEN
        LSENTYP_CODE = 'VL'
        RETURN
      END IF

      RETURN

      END

! end of LSENTYP_CODE()


!***********************************************************************
!
! Function:  LSEPOSN_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:48 1999
!
! Description:
!  Lookup function for matching SEPOSN code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSEPOSN_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LSEPOSN_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'CN' ) THEN
        LSEPOSN_DESC = 'CENTER POSITION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FW' ) THEN
        LSEPOSN_DESC = 'FORWARD OF CENTER POSITION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LSEPOSN_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LSEPOSN_DESC = 'NON-ADJUSTABLE SEAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RW' ) THEN
        LSEPOSN_DESC = 'REARWARD OF CENTER POSITION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LSEPOSN_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LSEPOSN_DESC()


!***********************************************************************
!
! Function:  LSEPOSN_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:48 1999
!
! Description:
!  Lookup function for matching SEPOSN description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSEPOSN_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LSEPOSN_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'CENTER POSITION' ) THEN
        LSEPOSN_CODE = 'CN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FORWARD OF CENTER POSITION' ) THEN
        LSEPOSN_CODE = 'FW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NON-ADJUSTABLE SEAT' ) THEN
        LSEPOSN_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LSEPOSN_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'REARWARD OF CENTER POSITION' ) THEN
        LSEPOSN_CODE = 'RW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LSEPOSN_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LSEPOSN_CODE()


!***********************************************************************
!
! Function:  LSIGSRC_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:48 1999
!
! Description:
!  Lookup function for matching SIGSRC code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSIGSRC_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LSIGSRC_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'ET01' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 01'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET02' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 02'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET03' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 03'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET04' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 04'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET05' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 05'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET06' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 06'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET07' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 07'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET08' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 08'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET09' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 09'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET10' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET11' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 11'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET12' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 12'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET13' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 13'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET14' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 14'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET15' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 15'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET16' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 16'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET17' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 17'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET18' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 18'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET19' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 19'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET20' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 20'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET21' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 21'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET22' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 22'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET23' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 23'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET24' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 24'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET25' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 25'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET26' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 26'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET27' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 27'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET28' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 28'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET29' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 29'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET30' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 30'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET31' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 31'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET32' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 32'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET33' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 33'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET34' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 34'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET35' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 35'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET36' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 36'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET37' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 37'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET38' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 38'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET39' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 39'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET40' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 40'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET41' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 41'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET42' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 42'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET43' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 43'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET44' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 44'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET45' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 45'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET46' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 46'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET47' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 47'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET48' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 48'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET49' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 49'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ET50' ) THEN
        LSIGSRC_DESC =
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 50'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W101' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W102' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W103' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 3'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W104' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 4'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W105' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 5'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W106' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 6'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W107' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 7'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W108' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 8'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W109' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 9'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W110' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W111' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 11'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W112' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 12'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W113' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 13'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W114' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 14'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W115' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 15'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W116' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 16'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W117' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 17'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W118' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 18'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W119' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 19'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W120' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 20'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W121' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 21'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W122' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 22'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W123' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 23'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W124' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 24'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W125' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 25'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W126' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 26'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W127' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 27'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W128' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 28'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W129' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 29'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W130' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 30'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W131' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 31'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W132' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 32'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W133' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 33'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W134' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 34'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W135' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 35'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W136' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 36'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W137' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 37'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W138' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 38'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W139' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 39'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W140' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 40'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W141' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 41'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W142' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 42'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W143' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 43'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W144' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 44'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W145' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 45'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W146' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 46'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W147' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 47'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W148' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 48'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W149' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 49'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W150' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 50'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W201' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W202' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W203' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 3'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W204' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 4'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W205' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 5'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W206' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 6'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W207' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 7'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W208' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 8'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W209' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 9'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W210' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W211' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 11'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W212' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 12'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W213' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 13'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W214' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 14'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W215' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 15'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W216' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 16'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W217' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 17'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W218' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 18'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W219' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 19'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W220' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 20'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W221' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 21'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W222' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 22'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W223' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 23'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W224' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 24'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W225' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 25'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W226' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 26'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W227' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 27'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W228' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 28'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W229' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 29'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W230' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 30'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W231' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 31'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W232' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 32'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W233' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 33'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W234' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 34'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W235' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 35'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W236' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 36'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W237' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 37'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W238' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 38'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W239' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 39'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W240' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 40'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W241' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 41'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W242' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 42'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W243' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 43'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W244' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 44'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W245' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 45'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W246' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 46'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W247' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 47'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W248' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 48'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W249' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 49'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W250' ) THEN
        LSIGSRC_DESC =
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME INDICAT'//
     & 'OR CHANNEL 50'
        RETURN
      END IF

      RETURN

      END

! end of LSIGSRC_DESC()


!***********************************************************************
!
! Function:  LSIGSRC_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:48 1999
!
! Description:
!  Lookup function for matching SIGSRC description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSIGSRC_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LSIGSRC_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 01' ) THEN
        LSIGSRC_CODE = 'ET01'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 02' ) THEN
        LSIGSRC_CODE = 'ET02'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 03' ) THEN
        LSIGSRC_CODE = 'ET03'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 04' ) THEN
        LSIGSRC_CODE = 'ET04'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 05' ) THEN
        LSIGSRC_CODE = 'ET05'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 06' ) THEN
        LSIGSRC_CODE = 'ET06'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 07' ) THEN
        LSIGSRC_CODE = 'ET07'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 08' ) THEN
        LSIGSRC_CODE = 'ET08'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 09' ) THEN
        LSIGSRC_CODE = 'ET09'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 10' ) THEN
        LSIGSRC_CODE = 'ET10'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 11' ) THEN
        LSIGSRC_CODE = 'ET11'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 12' ) THEN
        LSIGSRC_CODE = 'ET12'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 13' ) THEN
        LSIGSRC_CODE = 'ET13'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 14' ) THEN
        LSIGSRC_CODE = 'ET14'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 15' ) THEN
        LSIGSRC_CODE = 'ET15'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 16' ) THEN
        LSIGSRC_CODE = 'ET16'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 17' ) THEN
        LSIGSRC_CODE = 'ET17'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 18' ) THEN
        LSIGSRC_CODE = 'ET18'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 19' ) THEN
        LSIGSRC_CODE = 'ET19'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 20' ) THEN
        LSIGSRC_CODE = 'ET20'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 21' ) THEN
        LSIGSRC_CODE = 'ET21'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 22' ) THEN
        LSIGSRC_CODE = 'ET22'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 23' ) THEN
        LSIGSRC_CODE = 'ET23'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 24' ) THEN
        LSIGSRC_CODE = 'ET24'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 25' ) THEN
        LSIGSRC_CODE = 'ET25'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 26' ) THEN
        LSIGSRC_CODE = 'ET26'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 27' ) THEN
        LSIGSRC_CODE = 'ET27'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 28' ) THEN
        LSIGSRC_CODE = 'ET28'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 29' ) THEN
        LSIGSRC_CODE = 'ET29'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 30' ) THEN
        LSIGSRC_CODE = 'ET30'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 31' ) THEN
        LSIGSRC_CODE = 'ET31'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 32' ) THEN
        LSIGSRC_CODE = 'ET32'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 33' ) THEN
        LSIGSRC_CODE = 'ET33'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 34' ) THEN
        LSIGSRC_CODE = 'ET34'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 35' ) THEN
        LSIGSRC_CODE = 'ET35'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 36' ) THEN
        LSIGSRC_CODE = 'ET36'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 37' ) THEN
        LSIGSRC_CODE = 'ET37'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 38' ) THEN
        LSIGSRC_CODE = 'ET38'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 39' ) THEN
        LSIGSRC_CODE = 'ET39'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 40' ) THEN
        LSIGSRC_CODE = 'ET40'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 41' ) THEN
        LSIGSRC_CODE = 'ET41'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 42' ) THEN
        LSIGSRC_CODE = 'ET42'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 43' ) THEN
        LSIGSRC_CODE = 'ET43'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 44' ) THEN
        LSIGSRC_CODE = 'ET44'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 45' ) THEN
        LSIGSRC_CODE = 'ET45'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 46' ) THEN
        LSIGSRC_CODE = 'ET46'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 47' ) THEN
        LSIGSRC_CODE = 'ET47'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 48' ) THEN
        LSIGSRC_CODE = 'ET48'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 49' ) THEN
        LSIGSRC_CODE = 'ET49'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EVENT TIME INDICATOR REFERENCE CHANNEL 50' ) THEN
        LSIGSRC_CODE = 'ET50'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 1' ) THEN
        LSIGSRC_CODE = 'W101'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 10' ) THEN
        LSIGSRC_CODE = 'W110'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 11' ) THEN
        LSIGSRC_CODE = 'W111'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 12' ) THEN
        LSIGSRC_CODE = 'W112'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 13' ) THEN
        LSIGSRC_CODE = 'W113'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 14' ) THEN
        LSIGSRC_CODE = 'W114'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 15' ) THEN
        LSIGSRC_CODE = 'W115'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 16' ) THEN
        LSIGSRC_CODE = 'W116'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 17' ) THEN
        LSIGSRC_CODE = 'W117'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 18' ) THEN
        LSIGSRC_CODE = 'W118'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 19' ) THEN
        LSIGSRC_CODE = 'W119'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 2' ) THEN
        LSIGSRC_CODE = 'W102'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 20' ) THEN
        LSIGSRC_CODE = 'W120'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 21' ) THEN
        LSIGSRC_CODE = 'W121'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 22' ) THEN
        LSIGSRC_CODE = 'W122'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 23' ) THEN
        LSIGSRC_CODE = 'W123'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 24' ) THEN
        LSIGSRC_CODE = 'W124'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 25' ) THEN
        LSIGSRC_CODE = 'W125'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 26' ) THEN
        LSIGSRC_CODE = 'W126'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 27' ) THEN
        LSIGSRC_CODE = 'W127'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 28' ) THEN
        LSIGSRC_CODE = 'W128'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 29' ) THEN
        LSIGSRC_CODE = 'W129'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 3' ) THEN
        LSIGSRC_CODE = 'W103'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 30' ) THEN
        LSIGSRC_CODE = 'W130'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 31' ) THEN
        LSIGSRC_CODE = 'W131'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 32' ) THEN
        LSIGSRC_CODE = 'W132'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 33' ) THEN
        LSIGSRC_CODE = 'W133'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 34' ) THEN
        LSIGSRC_CODE = 'W134'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 35' ) THEN
        LSIGSRC_CODE = 'W135'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 36' ) THEN
        LSIGSRC_CODE = 'W136'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 37' ) THEN
        LSIGSRC_CODE = 'W137'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 38' ) THEN
        LSIGSRC_CODE = 'W138'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 39' ) THEN
        LSIGSRC_CODE = 'W139'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 4' ) THEN
        LSIGSRC_CODE = 'W104'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 40' ) THEN
        LSIGSRC_CODE = 'W140'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 41' ) THEN
        LSIGSRC_CODE = 'W141'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 42' ) THEN
        LSIGSRC_CODE = 'W142'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 43' ) THEN
        LSIGSRC_CODE = 'W143'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 44' ) THEN
        LSIGSRC_CODE = 'W144'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 45' ) THEN
        LSIGSRC_CODE = 'W145'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 46' ) THEN
        LSIGSRC_CODE = 'W146'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 47' ) THEN
        LSIGSRC_CODE = 'W147'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 48' ) THEN
        LSIGSRC_CODE = 'W148'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 49' ) THEN
        LSIGSRC_CODE = 'W149'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 5' ) THEN
        LSIGSRC_CODE = 'W105'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 50' ) THEN
        LSIGSRC_CODE = 'W150'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 6' ) THEN
        LSIGSRC_CODE = 'W106'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 7' ) THEN
        LSIGSRC_CODE = 'W107'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 8' ) THEN
        LSIGSRC_CODE = 'W108'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 1 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 9' ) THEN
        LSIGSRC_CODE = 'W109'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 1' ) THEN
        LSIGSRC_CODE = 'W201'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 10' ) THEN
        LSIGSRC_CODE = 'W210'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 11' ) THEN
        LSIGSRC_CODE = 'W211'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 12' ) THEN
        LSIGSRC_CODE = 'W212'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 13' ) THEN
        LSIGSRC_CODE = 'W213'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 14' ) THEN
        LSIGSRC_CODE = 'W214'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 15' ) THEN
        LSIGSRC_CODE = 'W215'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 16' ) THEN
        LSIGSRC_CODE = 'W216'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 17' ) THEN
        LSIGSRC_CODE = 'W217'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 18' ) THEN
        LSIGSRC_CODE = 'W218'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 19' ) THEN
        LSIGSRC_CODE = 'W219'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 2' ) THEN
        LSIGSRC_CODE = 'W202'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 20' ) THEN
        LSIGSRC_CODE = 'W220'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 21' ) THEN
        LSIGSRC_CODE = 'W221'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 22' ) THEN
        LSIGSRC_CODE = 'W222'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 23' ) THEN
        LSIGSRC_CODE = 'W223'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 24' ) THEN
        LSIGSRC_CODE = 'W224'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 25' ) THEN
        LSIGSRC_CODE = 'W225'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 26' ) THEN
        LSIGSRC_CODE = 'W226'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 27' ) THEN
        LSIGSRC_CODE = 'W227'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 28' ) THEN
        LSIGSRC_CODE = 'W228'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 29' ) THEN
        LSIGSRC_CODE = 'W229'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 3' ) THEN
        LSIGSRC_CODE = 'W203'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 30' ) THEN
        LSIGSRC_CODE = 'W230'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 31' ) THEN
        LSIGSRC_CODE = 'W231'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 32' ) THEN
        LSIGSRC_CODE = 'W232'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 33' ) THEN
        LSIGSRC_CODE = 'W233'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 34' ) THEN
        LSIGSRC_CODE = 'W234'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 35' ) THEN
        LSIGSRC_CODE = 'W235'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 36' ) THEN
        LSIGSRC_CODE = 'W236'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 37' ) THEN
        LSIGSRC_CODE = 'W237'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 38' ) THEN
        LSIGSRC_CODE = 'W238'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 39' ) THEN
        LSIGSRC_CODE = 'W239'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 4' ) THEN
        LSIGSRC_CODE = 'W204'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 40' ) THEN
        LSIGSRC_CODE = 'W240'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 41' ) THEN
        LSIGSRC_CODE = 'W241'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 42' ) THEN
        LSIGSRC_CODE = 'W242'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 43' ) THEN
        LSIGSRC_CODE = 'W243'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 44' ) THEN
        LSIGSRC_CODE = 'W244'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 45' ) THEN
        LSIGSRC_CODE = 'W245'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 46' ) THEN
        LSIGSRC_CODE = 'W246'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 47' ) THEN
        LSIGSRC_CODE = 'W247'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 48' ) THEN
        LSIGSRC_CODE = 'W248'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 49' ) THEN
        LSIGSRC_CODE = 'W249'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 5' ) THEN
        LSIGSRC_CODE = 'W205'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 50' ) THEN
        LSIGSRC_CODE = 'W250'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 6' ) THEN
        LSIGSRC_CODE = 'W206'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 7' ) THEN
        LSIGSRC_CODE = 'W207'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 8' ) THEN
        LSIGSRC_CODE = 'W208'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GROUP 2 OF SWG WAVEFORM ASSOCIATED WITH EVENT TIME'//
     & ' INDICATOR CHANNEL 9' ) THEN
        LSIGSRC_CODE = 'W209'
        RETURN
      END IF

      RETURN

      END

! end of LSIGSRC_CODE()


!***********************************************************************
!
! Function:  LSILENG_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:48 1999
!
! Description:
!  Lookup function for matching SILENG code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSILENG_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LSILENG_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'DE' ) THEN
        LSILENG_DESC = 'DIRECT ENGAGEMENT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LSILENG_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LSILENG_DESC = 'NO DIRECT ENGAGEMENT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LSILENG_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LSILENG_DESC()


!***********************************************************************
!
! Function:  LSILENG_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:48 1999
!
! Description:
!  Lookup function for matching SILENG description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSILENG_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LSILENG_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DIRECT ENGAGEMENT' ) THEN
        LSILENG_CODE = 'DE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NO DIRECT ENGAGEMENT' ) THEN
        LSILENG_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LSILENG_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LSILENG_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LSILENG_CODE()


!***********************************************************************
!
! Function:  LSTRSEP_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:49 1999
!
! Description:
!  Lookup function for matching STRSEP code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSTRSEP_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LSTRSEP_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LSTRSEP_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NO' ) THEN
        LSTRSEP_DESC = 'NO SEPARATION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SP' ) THEN
        LSTRSEP_DESC = 'SEPARATION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LSTRSEP_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LSTRSEP_DESC()


!***********************************************************************
!
! Function:  LSTRSEP_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:49 1999
!
! Description:
!  Lookup function for matching STRSEP description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSTRSEP_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LSTRSEP_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'NO SEPARATION' ) THEN
        LSTRSEP_CODE = 'NO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LSTRSEP_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEPARATION' ) THEN
        LSTRSEP_CODE = 'SP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LSTRSEP_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LSTRSEP_CODE()


!***********************************************************************
!
! Function:  LSYSORG_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:49 1999
!
! Description:
!  Lookup function for matching SYSORG code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSYSORG_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LSYSORG_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'A' ) THEN
        LSYSORG_DESC = 'ARTERIES/VEINS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'B' ) THEN
        LSYSORG_DESC = 'BRAIN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'C' ) THEN
        LSYSORG_DESC = 'SPINAL CORD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'D' ) THEN
        LSYSORG_DESC = 'DIGESTIVE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'E' ) THEN
        LSYSORG_DESC = 'EAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'G' ) THEN
        LSYSORG_DESC = 'UROGENITAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'H' ) THEN
        LSYSORG_DESC = 'HEART'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'I' ) THEN
        LSYSORG_DESC = 'INTEGUMENTARY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'J' ) THEN
        LSYSORG_DESC = 'JOINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'K' ) THEN
        LSYSORG_DESC = 'KIDNEY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'L' ) THEN
        LSYSORG_DESC = 'LIVER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'M' ) THEN
        LSYSORG_DESC = 'MUSCLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'N' ) THEN
        LSYSORG_DESC = 'NERVOUS SYSTEM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'O' ) THEN
        LSYSORG_DESC = 'EYE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'P' ) THEN
        LSYSORG_DESC = 'PULMONARY/LUNG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'Q' ) THEN
        LSYSORG_DESC = 'SPLEEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'R' ) THEN
        LSYSORG_DESC = 'RESPIRATORY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'S' ) THEN
        LSYSORG_DESC = 'SKELETAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'T' ) THEN
        LSYSORG_DESC = 'ENDOCRINE GLAND'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'V' ) THEN
        LSYSORG_DESC = 'VERTEBRAE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'W' ) THEN
        LSYSORG_DESC = 'ALL IN REGION'
        RETURN
      END IF

      RETURN

      END

! end of LSYSORG_DESC()


!***********************************************************************
!
! Function:  LSYSORG_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:49 1999
!
! Description:
!  Lookup function for matching SYSORG description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LSYSORG_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LSYSORG_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'ALL IN REGION' ) THEN
        LSYSORG_CODE = 'W'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ARTERIES/VEINS' ) THEN
        LSYSORG_CODE = 'A'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BRAIN' ) THEN
        LSYSORG_CODE = 'B'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DIGESTIVE' ) THEN
        LSYSORG_CODE = 'D'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EAR' ) THEN
        LSYSORG_CODE = 'E'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ENDOCRINE GLAND' ) THEN
        LSYSORG_CODE = 'T'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EYE' ) THEN
        LSYSORG_CODE = 'O'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEART' ) THEN
        LSYSORG_CODE = 'H'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'INTEGUMENTARY' ) THEN
        LSYSORG_CODE = 'I'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'JOINT' ) THEN
        LSYSORG_CODE = 'J'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KIDNEY' ) THEN
        LSYSORG_CODE = 'K'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LIVER' ) THEN
        LSYSORG_CODE = 'L'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MUSCLE' ) THEN
        LSYSORG_CODE = 'M'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NERVOUS SYSTEM' ) THEN
        LSYSORG_CODE = 'N'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PULMONARY/LUNG' ) THEN
        LSYSORG_CODE = 'P'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RESPIRATORY' ) THEN
        LSYSORG_CODE = 'R'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SKELETAL' ) THEN
        LSYSORG_CODE = 'S'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPINAL CORD' ) THEN
        LSYSORG_CODE = 'C'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPLEEN' ) THEN
        LSYSORG_CODE = 'Q'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UROGENITAL' ) THEN
        LSYSORG_CODE = 'G'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VERTEBRAE' ) THEN
        LSYSORG_CODE = 'V'
        RETURN
      END IF

      RETURN

      END

! end of LSYSORG_CODE()


!***********************************************************************
!
! Function:  LTKCOND_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:49 1999
!
! Description:
!  Lookup function for matching TKCOND code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTKCOND_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LTKCOND_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'DRY' ) THEN
        LTKCOND_DESC = 'DRY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ICY' ) THEN
        LTKCOND_DESC = 'ICY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MUD' ) THEN
        LTKCOND_DESC = 'MUDDY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LTKCOND_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SAN' ) THEN
        LTKCOND_DESC = 'SANDY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SNO' ) THEN
        LTKCOND_DESC = 'SNOWY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LTKCOND_DESC = 'UNKNOWN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'WET' ) THEN
        LTKCOND_DESC = 'WET'
        RETURN
      END IF

      RETURN

      END

! end of LTKCOND_DESC()


!***********************************************************************
!
! Function:  LTKCOND_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:49 1999
!
! Description:
!  Lookup function for matching TKCOND description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTKCOND_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LTKCOND_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'DRY' ) THEN
        LTKCOND_CODE = 'DRY'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ICY' ) THEN
        LTKCOND_CODE = 'ICY'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MUDDY' ) THEN
        LTKCOND_CODE = 'MUD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LTKCOND_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SANDY' ) THEN
        LTKCOND_CODE = 'SAN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SNOWY' ) THEN
        LTKCOND_CODE = 'SNO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LTKCOND_CODE = 'UNK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WET' ) THEN
        LTKCOND_CODE = 'WET'
        RETURN
      END IF

      RETURN

      END

! end of LTKCOND_CODE()


!***********************************************************************
!
! Function:  LTKSURF_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:49 1999
!
! Description:
!  Lookup function for matching TKSURF code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTKSURF_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LTKSURF_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'ASH' ) THEN
        LTKSURF_DESC = 'ASPHALT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CON' ) THEN
        LTKSURF_DESC = 'CONCRETE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DOS' ) THEN
        LTKSURF_DESC = 'DIRT OR SAND'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LTKSURF_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LTKSURF_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LTKSURF_DESC()


!***********************************************************************
!
! Function:  LTKSURF_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:49 1999
!
! Description:
!  Lookup function for matching TKSURF description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTKSURF_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LTKSURF_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'ASPHALT' ) THEN
        LTKSURF_CODE = 'ASH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONCRETE' ) THEN
        LTKSURF_CODE = 'CON'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DIRT OR SAND' ) THEN
        LTKSURF_CODE = 'DOS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LTKSURF_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LTKSURF_CODE = 'UNK'
        RETURN
      END IF

      RETURN

      END

! end of LTKSURF_CODE()


!***********************************************************************
!
! Function:  LTRANSM_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:49 1999
!
! Description:
!  Lookup function for matching TRANSM code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTRANSM_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LTRANSM_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'A4' ) THEN
        LTRANSM_DESC = 'AUTOMATIC - FOUR WHEEL DRIVE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AF' ) THEN
        LTRANSM_DESC = 'AUTOMATIC - FRONT WHEEL DRIVE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'AR' ) THEN
        LTRANSM_DESC = 'AUTOMATIC - REAR WHEEL DRIVE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'M4' ) THEN
        LTRANSM_DESC = 'MANUAL - FOUR WHEEL DRIVE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MF' ) THEN
        LTRANSM_DESC = 'MANUAL - FRONT WHEEL DRIVE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MR' ) THEN
        LTRANSM_DESC = 'MANUAL - REAR WHEEL DRIVE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NA' ) THEN
        LTRANSM_DESC = 'NOT APPLICABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OT' ) THEN
        LTRANSM_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UN' ) THEN
        LTRANSM_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LTRANSM_DESC()


!***********************************************************************
!
! Function:  LTRANSM_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:50 1999
!
! Description:
!  Lookup function for matching TRANSM description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTRANSM_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LTRANSM_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ.
     & 'AUTOMATIC - FOUR WHEEL DRIVE' ) THEN
        LTRANSM_CODE = 'A4'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'AUTOMATIC - FRONT WHEEL DRIVE' ) THEN
        LTRANSM_CODE = 'AF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'AUTOMATIC - REAR WHEEL DRIVE' ) THEN
        LTRANSM_CODE = 'AR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MANUAL - FOUR WHEEL DRIVE' ) THEN
        LTRANSM_CODE = 'M4'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'MANUAL - FRONT WHEEL DRIVE' ) THEN
        LTRANSM_CODE = 'MF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MANUAL - REAR WHEEL DRIVE' ) THEN
        LTRANSM_CODE = 'MR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOT APPLICABLE' ) THEN
        LTRANSM_CODE = 'NA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LTRANSM_CODE = 'OT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LTRANSM_CODE = 'UN'
        RETURN
      END IF

      RETURN

      END

! end of LTRANSM_CODE()


!***********************************************************************
!
! Function:  LTSTCFN_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:50 1999
!
! Description:
!  Lookup function for matching TSTCFN code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTSTCFN_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LTSTCFN_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'CAN' ) THEN
        LTSTCFN_DESC = 'CANNON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DRP' ) THEN
        LTSTCFN_DESC = 'DROP TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ITB' ) THEN
        LTSTCFN_DESC = 'IMPACTOR INTO BARRIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ITI' ) THEN
        LTSTCFN_DESC = 'IMPACTOR INTO IMPACTOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ITV' ) THEN
        LTSTCFN_DESC = 'IMPACTOR INTO VEHICLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LTSTCFN_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PED' ) THEN
        LTSTCFN_DESC = 'PEDESTRAIN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PEN' ) THEN
        LTSTCFN_DESC = 'PENDULUM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ROL' ) THEN
        LTSTCFN_DESC = 'ROLLOVER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SLB' ) THEN
        LTSTCFN_DESC = 'SLED WITH VEHICLE BODY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SLN' ) THEN
        LTSTCFN_DESC = 'SLED WITHOUT VEHICLE BODY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VTB' ) THEN
        LTSTCFN_DESC = 'VEHICLE INTO BARRIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VTI' ) THEN
        LTSTCFN_DESC = 'VEHICLE INTO IMPACTOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VTV' ) THEN
        LTSTCFN_DESC = 'VEHICLE INTO VEHICLE'
        RETURN
      END IF

      RETURN

      END

! end of LTSTCFN_DESC()


!***********************************************************************
!
! Function:  LTSTCFN_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:50 1999
!
! Description:
!  Lookup function for matching TSTCFN description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTSTCFN_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LTSTCFN_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'CANNON' ) THEN
        LTSTCFN_CODE = 'CAN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DROP TEST' ) THEN
        LTSTCFN_CODE = 'DRP'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IMPACTOR INTO BARRIER' ) THEN
        LTSTCFN_CODE = 'ITB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IMPACTOR INTO IMPACTOR' ) THEN
        LTSTCFN_CODE = 'ITI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IMPACTOR INTO VEHICLE' ) THEN
        LTSTCFN_CODE = 'ITV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LTSTCFN_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PEDESTRAIN' ) THEN
        LTSTCFN_CODE = 'PED'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PENDULUM' ) THEN
        LTSTCFN_CODE = 'PEN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROLLOVER' ) THEN
        LTSTCFN_CODE = 'ROL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SLED WITH VEHICLE BODY' ) THEN
        LTSTCFN_CODE = 'SLB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SLED WITHOUT VEHICLE BODY' ) THEN
        LTSTCFN_CODE = 'SLN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VEHICLE INTO BARRIER' ) THEN
        LTSTCFN_CODE = 'VTB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VEHICLE INTO IMPACTOR' ) THEN
        LTSTCFN_CODE = 'VTI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VEHICLE INTO VEHICLE' ) THEN
        LTSTCFN_CODE = 'VTV'
        RETURN
      END IF

      RETURN

      END

! end of LTSTCFN_CODE()


!***********************************************************************
!
! Function:  LTSTDEV_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:50 1999
!
! Description:
!  Lookup function for matching TSTDEV code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTSTDEV_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LTSTDEV_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'CAN' ) THEN
        LTSTDEV_DESC = 'CANNON - FREE FLYING LAUNCHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LIN' ) THEN
        LTSTDEV_DESC = 'LINEAR IMPACTOR (DYNAMIC)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LTSTDEV_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PIV' ) THEN
        LTSTDEV_DESC = 'PIVOTING IMPACTOR (DYNAMIC)'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SLD' ) THEN
        LTSTDEV_DESC = 'STATIC LOADING  DEVICE (STATIC)'
        RETURN
      END IF

      RETURN

      END

! end of LTSTDEV_DESC()


!***********************************************************************
!
! Function:  LTSTDEV_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:50 1999
!
! Description:
!  Lookup function for matching TSTDEV description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTSTDEV_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LTSTDEV_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ.
     & 'CANNON - FREE FLYING LAUNCHER' ) THEN
        LTSTDEV_CODE = 'CAN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LINEAR IMPACTOR (DYNAMIC)' ) THEN
        LTSTDEV_CODE = 'LIN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LTSTDEV_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'PIVOTING IMPACTOR (DYNAMIC)' ) THEN
        LTSTDEV_CODE = 'PIV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'STATIC LOADING  DEVICE (STATIC)' ) THEN
        LTSTDEV_CODE = 'SLD'
        RETURN
      END IF

      RETURN

      END

! end of LTSTDEV_CODE()


!***********************************************************************
!
! Function:  LTSTPRF_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:51 1999
!
! Description:
!  Lookup function for matching TSTPRF code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTSTPRF_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LTSTPRF_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'AUT' ) THEN
        LTSTPRF_DESC = 'AUTOLIV AUSTRALIA P/L'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BAS' ) THEN
        LTSTPRF_DESC = 'BUNDESANTALT FUER STRASSENWESEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BAT' ) THEN
        LTSTPRF_DESC = 'BATTELLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CAL' ) THEN
        LTSTPRF_DESC = 'CALSPAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CAN' ) THEN
        LTSTPRF_DESC = 'TRANSPORT CANADA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CIR' ) THEN
        LTSTPRF_DESC = 'CALIF. INJURY RES. ASSOC.'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DUK' ) THEN
        LTSTPRF_DESC = 'DUKE UNIVERSITY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DYS' ) THEN
        LTSTPRF_DESC = 'DYNAMIC SCIENCE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ENS' ) THEN
        LTSTPRF_DESC = 'ENSCO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FOI' ) THEN
        LTSTPRF_DESC = 'FEDERAL OUTDOOR IMPACT LABORATORY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'FRD' ) THEN
        LTSTPRF_DESC = 'FORD MOTOR COMPANY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'GMC' ) THEN
        LTSTPRF_DESC = 'GENERAL MOTORS CORPORATION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'GTL' ) THEN
        LTSTPRF_DESC = 'GENERAL TEST LABS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'HDL' ) THEN
        LTSTPRF_DESC = 'HEIDELBERG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'IIH' ) THEN
        LTSTPRF_DESC =
     & 'INSURANCE INSTITUTE FOR HIGHWAY SAFETY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'JAR' ) THEN
        LTSTPRF_DESC = 'JAPAN AUTO RESEARCH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KAR' ) THEN
        LTSTPRF_DESC = 'KARCO ENGINEERING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MCR' ) THEN
        LTSTPRF_DESC = 'MCR TECHNOLOGY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MCW' ) THEN
        LTSTPRF_DESC = 'MEDICAL COLLEGE OF WISCONSIN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MGA' ) THEN
        LTSTPRF_DESC = 'MGA RESEARCH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MSE' ) THEN
        LTSTPRF_DESC = 'MOBILITY SYSTEMS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NBL' ) THEN
        LTSTPRF_DESC = 'NAVAL BIODYNAMICS LAB.'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NTC' ) THEN
        LTSTPRF_DESC = 'NTS - CALIFORNIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NTV' ) THEN
        LTSTPRF_DESC = 'NTS - VIRGINIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ONS' ) THEN
        LTSTPRF_DESC = 'ONSER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LTSTPRF_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SWR' ) THEN
        LTSTPRF_DESC = 'SWRI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TNO' ) THEN
        LTSTPRF_DESC = 'TNO - ROAD VEHICLE RESEARCH INST.'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TRC' ) THEN
        LTSTPRF_DESC = 'TRC OF OHIO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'TTI' ) THEN
        LTSTPRF_DESC = 'TEXAS TRANSP. INSTITUTE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UVA' ) THEN
        LTSTPRF_DESC = 'UNIVERSITY OF VIRGINIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VRT' ) THEN
        LTSTPRF_DESC = 'VEHICLE RES. TEST CTR.'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VWG' ) THEN
        LTSTPRF_DESC = 'VOLKSWAGEN AG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'WSU' ) THEN
        LTSTPRF_DESC = 'WAYNE STATE UNIVERSITY'
        RETURN
      END IF

      RETURN

      END

! end of LTSTPRF_DESC()


!***********************************************************************
!
! Function:  LTSTPRF_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:51 1999
!
! Description:
!  Lookup function for matching TSTPRF description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTSTPRF_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LTSTPRF_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'AUTOLIV AUSTRALIA P/L' ) THEN
        LTSTPRF_CODE = 'AUT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BATTELLE' ) THEN
        LTSTPRF_CODE = 'BAT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'BUNDESANTALT FUER STRASSENWESEN' ) THEN
        LTSTPRF_CODE = 'BAS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CALIF. INJURY RES. ASSOC.' ) THEN
        LTSTPRF_CODE = 'CIR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CALSPAN' ) THEN
        LTSTPRF_CODE = 'CAL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DUKE UNIVERSITY' ) THEN
        LTSTPRF_CODE = 'DUK'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DYNAMIC SCIENCE' ) THEN
        LTSTPRF_CODE = 'DYS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ENSCO' ) THEN
        LTSTPRF_CODE = 'ENS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FEDERAL OUTDOOR IMPACT LABORATORY' ) THEN
        LTSTPRF_CODE = 'FOI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FORD MOTOR COMPANY' ) THEN
        LTSTPRF_CODE = 'FRD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'GENERAL MOTORS CORPORATION' ) THEN
        LTSTPRF_CODE = 'GMC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GENERAL TEST LABS' ) THEN
        LTSTPRF_CODE = 'GTL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HEIDELBERG' ) THEN
        LTSTPRF_CODE = 'HDL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'INSURANCE INSTITUTE FOR HIGHWAY SAFETY' ) THEN
        LTSTPRF_CODE = 'IIH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'JAPAN AUTO RESEARCH' ) THEN
        LTSTPRF_CODE = 'JAR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KARCO ENGINEERING' ) THEN
        LTSTPRF_CODE = 'KAR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MCR TECHNOLOGY' ) THEN
        LTSTPRF_CODE = 'MCR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'MEDICAL COLLEGE OF WISCONSIN' ) THEN
        LTSTPRF_CODE = 'MCW'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MGA RESEARCH' ) THEN
        LTSTPRF_CODE = 'MGA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MOBILITY SYSTEMS' ) THEN
        LTSTPRF_CODE = 'MSE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NAVAL BIODYNAMICS LAB.' ) THEN
        LTSTPRF_CODE = 'NBL'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NTS - CALIFORNIA' ) THEN
        LTSTPRF_CODE = 'NTC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NTS - VIRGINIA' ) THEN
        LTSTPRF_CODE = 'NTV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ONSER' ) THEN
        LTSTPRF_CODE = 'ONS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LTSTPRF_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SWRI' ) THEN
        LTSTPRF_CODE = 'SWR'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TEXAS TRANSP. INSTITUTE' ) THEN
        LTSTPRF_CODE = 'TTI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'TNO - ROAD VEHICLE RESEARCH INST.' ) THEN
        LTSTPRF_CODE = 'TNO'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRANSPORT CANADA' ) THEN
        LTSTPRF_CODE = 'CAN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRC OF OHIO' ) THEN
        LTSTPRF_CODE = 'TRC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNIVERSITY OF VIRGINIA' ) THEN
        LTSTPRF_CODE = 'UVA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VEHICLE RES. TEST CTR.' ) THEN
        LTSTPRF_CODE = 'VRT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VOLKSWAGEN AG' ) THEN
        LTSTPRF_CODE = 'VWG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WAYNE STATE UNIVERSITY' ) THEN
        LTSTPRF_CODE = 'WSU'
        RETURN
      END IF

      RETURN

      END

! end of LTSTPRF_CODE()


!***********************************************************************
!
! Function:  LTSTTYP_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:51 1999
!
! Description:
!  Lookup function for matching TSTTYP code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTSTTYP_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LTSTTYP_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '201' ) THEN
        LTSTTYP_DESC =
     & 'FMVSS 201 OCCUPANT PROTECTION IN INTERIOR IMPACTS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '202' ) THEN
        LTSTTYP_DESC = 'FMVSS 202 HEAD RESTRAINTS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '203' ) THEN
        LTSTTYP_DESC =
     & 'FMVSS 203 STEERING CONTROL IMPACT PROTECTION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '207' ) THEN
        LTSTTYP_DESC = 'FMVSS 207 SEATING SYSTEMS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '208' ) THEN
        LTSTTYP_DESC = 'FMVSS 208 OCCUPANT CRASH PROTECTION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '209' ) THEN
        LTSTTYP_DESC = 'FMVSS 209 SEAT BELT ASSEMBLIES'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '210' ) THEN
        LTSTTYP_DESC = 'FMVSS 210 SEAT BELT ANCHORAGES'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '213' ) THEN
        LTSTTYP_DESC = 'FMVSS 213 CHILD RESTRAINT SYSTEMS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '214' ) THEN
        LTSTTYP_DESC = 'FMVSS 214 SIDE IMPACT PROTECTION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '216' ) THEN
        LTSTTYP_DESC = 'FMVSS 216 ROOF CRUSH RESISTANCE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '218' ) THEN
        LTSTTYP_DESC = 'FMVSS 218 MOTORCYCLE HELMETS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2CC' ) THEN
        LTSTTYP_DESC = 'COMBINED FMVSS 207 AND FMVSS 210'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '301' ) THEN
        LTSTTYP_DESC = 'FMVSS 301 FUEL SYSTEM INTEGRITY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'BAS' ) THEN
        LTSTTYP_DESC = 'BASELINE TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CLB' ) THEN
        LTSTTYP_DESC = 'CALIBRATION TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'COM' ) THEN
        LTSTTYP_DESC = 'FMVSS COMPLIANCE TEST - UNSPECIFIED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CRC' ) THEN
        LTSTTYP_DESC = 'CRASH RECORDER TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DEV' ) THEN
        LTSTTYP_DESC = 'TEST PROCEDURE DEVELOPMENT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ELE' ) THEN
        LTSTTYP_DESC = 'ELECTRIC VEHICLE TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ENC' ) THEN
        LTSTTYP_DESC =
     & 'EXPERIMENTAL NEW CAR ASSESSMENT TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'INV' ) THEN
        LTSTTYP_DESC = 'INVALIDATED TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MOD' ) THEN
        LTSTTYP_DESC = 'MODIFIED VEHICLE TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NCA' ) THEN
        LTSTTYP_DESC = 'NEW CAR ASSESSMENT TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OCC' ) THEN
        LTSTTYP_DESC = 'OCCUPANT PERFORMANCE TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'ONC' ) THEN
        LTSTTYP_DESC = 'OPTIONAL NEW CAR ASSESSMENT TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LTSTTYP_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RES' ) THEN
        LTSTTYP_DESC = 'RESEARCH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RSB' ) THEN
        LTSTTYP_DESC = 'ROADSIDE BARRIER TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RSV' ) THEN
        LTSTTYP_DESC = 'RESEARCH SAFETY VEHICLE TEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RWS' ) THEN
        LTSTTYP_DESC = 'REAL WORLD SIMULATION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW1' ) THEN
        LTSTTYP_DESC = 'SIGNAL WAVEFORM GENERATOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW2' ) THEN
        LTSTTYP_DESC =
     & 'SIGNAL WAVEFORM GENERATOR HARDWARE UPGRADE WITH NO SOFTWAR'//
     & 'E UPDATE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SW3' ) THEN
        LTSTTYP_DESC =
     & 'SIGNAL WAVEFORM GENERATOR HARDWARE AND SOFTWARE UPGRADE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'UNK' ) THEN
        LTSTTYP_DESC = 'UNKNOWN'
        RETURN
      END IF

      RETURN

      END

! end of LTSTTYP_DESC()


!***********************************************************************
!
! Function:  LTSTTYP_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:51 1999
!
! Description:
!  Lookup function for matching TSTTYP description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LTSTTYP_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LTSTTYP_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'BASELINE TEST' ) THEN
        LTSTTYP_CODE = 'BAS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CALIBRATION TEST' ) THEN
        LTSTTYP_CODE = 'CLB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'COMBINED FMVSS 207 AND FMVSS 210' ) THEN
        LTSTTYP_CODE = '2CC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CRASH RECORDER TEST' ) THEN
        LTSTTYP_CODE = 'CRC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELECTRIC VEHICLE TEST' ) THEN
        LTSTTYP_CODE = 'ELE'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'EXPERIMENTAL NEW CAR ASSESSMENT TEST' ) THEN
        LTSTTYP_CODE = 'ENC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 201 OCCUPANT PROTECTION IN INTERIOR IMPACTS' ) THEN
        LTSTTYP_CODE = '201'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FMVSS 202 HEAD RESTRAINTS' ) THEN
        LTSTTYP_CODE = '202'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 203 STEERING CONTROL IMPACT PROTECTION' ) THEN
        LTSTTYP_CODE = '203'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FMVSS 207 SEATING SYSTEMS' ) THEN
        LTSTTYP_CODE = '207'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 208 OCCUPANT CRASH PROTECTION' ) THEN
        LTSTTYP_CODE = '208'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 209 SEAT BELT ASSEMBLIES' ) THEN
        LTSTTYP_CODE = '209'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 210 SEAT BELT ANCHORAGES' ) THEN
        LTSTTYP_CODE = '210'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 213 CHILD RESTRAINT SYSTEMS' ) THEN
        LTSTTYP_CODE = '213'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 214 SIDE IMPACT PROTECTION' ) THEN
        LTSTTYP_CODE = '214'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 216 ROOF CRUSH RESISTANCE' ) THEN
        LTSTTYP_CODE = '216'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 218 MOTORCYCLE HELMETS' ) THEN
        LTSTTYP_CODE = '218'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS 301 FUEL SYSTEM INTEGRITY' ) THEN
        LTSTTYP_CODE = '301'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'FMVSS COMPLIANCE TEST - UNSPECIFIED' ) THEN
        LTSTTYP_CODE = 'COM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'INVALIDATED TEST' ) THEN
        LTSTTYP_CODE = 'INV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MODIFIED VEHICLE TEST' ) THEN
        LTSTTYP_CODE = 'MOD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NEW CAR ASSESSMENT TEST' ) THEN
        LTSTTYP_CODE = 'NCA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OCCUPANT PERFORMANCE TEST' ) THEN
        LTSTTYP_CODE = 'OCC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'OPTIONAL NEW CAR ASSESSMENT TEST' ) THEN
        LTSTTYP_CODE = 'ONC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LTSTTYP_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'REAL WORLD SIMULATION' ) THEN
        LTSTTYP_CODE = 'RWS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RESEARCH' ) THEN
        LTSTTYP_CODE = 'RES'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'RESEARCH SAFETY VEHICLE TEST' ) THEN
        LTSTTYP_CODE = 'RSV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROADSIDE BARRIER TEST' ) THEN
        LTSTTYP_CODE = 'RSB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIGNAL WAVEFORM GENERATOR' ) THEN
        LTSTTYP_CODE = 'SW1'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SIGNAL WAVEFORM GENERATOR HARDWARE AND SOFTWARE UP'//
     & 'GRADE' ) THEN
        LTSTTYP_CODE = 'SW3'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'SIGNAL WAVEFORM GENERATOR HARDWARE UPGRADE WITH NO'//
     & ' SOFTWARE UPDATE' ) THEN
        LTSTTYP_CODE = 'SW2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ.
     & 'TEST PROCEDURE DEVELOPMENT' ) THEN
        LTSTTYP_CODE = 'DEV'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'UNKNOWN' ) THEN
        LTSTTYP_CODE = 'UNK'
        RETURN
      END IF

      RETURN

      END

! end of LTSTTYP_CODE()


!***********************************************************************
!
! Function:  LUNITS_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:51 1999
!
! Description:
!  Lookup function for matching UNITS code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LUNITS_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LUNITS_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. 'CEN' ) THEN
        LUNITS_DESC = 'DEGREES CELSIUS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'CMS' ) THEN
        LUNITS_DESC = 'CENTIMETERS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DEC' ) THEN
        LUNITS_DESC = 'DECIBELS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DEG' ) THEN
        LUNITS_DESC = 'DEGREES'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DP2' ) THEN
        LUNITS_DESC = 'DEGREES/SEC**2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'DPS' ) THEN
        LUNITS_DESC = 'DEGREES/SEC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'G''S' ) THEN
        LUNITS_DESC = 'G''S'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'INS' ) THEN
        LUNITS_DESC = 'INCHES'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KLB' ) THEN
        LUNITS_DESC = 'KILOPOUNDS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KPA' ) THEN
        LUNITS_DESC = 'KILOPASCALS AB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KPG' ) THEN
        LUNITS_DESC = 'KILOPASCALS GA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'KPH' ) THEN
        LUNITS_DESC = 'KILOMETERS/HOUR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LBF' ) THEN
        LUNITS_DESC = 'POUND-FEET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'LBS' ) THEN
        LUNITS_DESC = 'POUNDS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MET' ) THEN
        LUNITS_DESC = 'METERS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MM' ) THEN
        LUNITS_DESC = 'MILLIMETERS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MPH' ) THEN
        LUNITS_DESC = 'MILES/HOUR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MPI' ) THEN
        LUNITS_DESC = 'MICROIN/IN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'MPM' ) THEN
        LUNITS_DESC = 'MICROMET/MET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NON' ) THEN
        LUNITS_DESC = 'DIMENSIONLESS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NSC' ) THEN
        LUNITS_DESC = 'NEWTON-SECONDS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NWM' ) THEN
        LUNITS_DESC = 'NEWTON-METERS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'NWT' ) THEN
        LUNITS_DESC = 'NEWTONS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'OTH' ) THEN
        LUNITS_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PIA' ) THEN
        LUNITS_DESC = 'POUNDS/IN**2 AB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PIG' ) THEN
        LUNITS_DESC = 'POUNDS/IN**2 GA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PSC' ) THEN
        LUNITS_DESC = 'POUND-SECONDS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'PST' ) THEN
        LUNITS_DESC = 'PERCENT STRAIN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RAD' ) THEN
        LUNITS_DESC = 'RADIANS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RMM' ) THEN
        LUNITS_DESC = 'RECIPROCAL MM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RP2' ) THEN
        LUNITS_DESC = 'RADIANS/SEC**2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RPC' ) THEN
        LUNITS_DESC = 'RECIPROCAL CMS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RPI' ) THEN
        LUNITS_DESC = 'RECIPROCAL INS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RPM' ) THEN
        LUNITS_DESC = 'RECIPROCAL MET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'RPS' ) THEN
        LUNITS_DESC = 'RADIANS/SEC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'SEC' ) THEN
        LUNITS_DESC = 'SECONDS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. 'VOL' ) THEN
        LUNITS_DESC = 'VOLTS'
        RETURN
      END IF

      RETURN

      END

! end of LUNITS_DESC()


!***********************************************************************
!
! Function:  LUNITS_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:51 1999
!
! Description:
!  Lookup function for matching UNITS description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LUNITS_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LUNITS_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. 'CENTIMETERS' ) THEN
        LUNITS_CODE = 'CMS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DECIBELS' ) THEN
        LUNITS_CODE = 'DEC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DEGREES' ) THEN
        LUNITS_CODE = 'DEG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DEGREES CELSIUS' ) THEN
        LUNITS_CODE = 'CEN'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DEGREES/SEC' ) THEN
        LUNITS_CODE = 'DPS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DEGREES/SEC**2' ) THEN
        LUNITS_CODE = 'DP2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DIMENSIONLESS' ) THEN
        LUNITS_CODE = 'NON'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'G''S' ) THEN
        LUNITS_CODE = 'G''S'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'INCHES' ) THEN
        LUNITS_CODE = 'INS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KILOMETERS/HOUR' ) THEN
        LUNITS_CODE = 'KPH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KILOPASCALS AB' ) THEN
        LUNITS_CODE = 'KPA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KILOPASCALS GA' ) THEN
        LUNITS_CODE = 'KPG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KILOPOUNDS' ) THEN
        LUNITS_CODE = 'KLB'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'METERS' ) THEN
        LUNITS_CODE = 'MET'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MICROIN/IN' ) THEN
        LUNITS_CODE = 'MPI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MICROMET/MET' ) THEN
        LUNITS_CODE = 'MPM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MILES/HOUR' ) THEN
        LUNITS_CODE = 'MPH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MILLIMETERS' ) THEN
        LUNITS_CODE = 'MM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NEWTON-METERS' ) THEN
        LUNITS_CODE = 'NWM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NEWTON-SECONDS' ) THEN
        LUNITS_CODE = 'NSC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NEWTONS' ) THEN
        LUNITS_CODE = 'NWT'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LUNITS_CODE = 'OTH'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PERCENT STRAIN' ) THEN
        LUNITS_CODE = 'PST'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'POUND-FEET' ) THEN
        LUNITS_CODE = 'LBF'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'POUND-SECONDS' ) THEN
        LUNITS_CODE = 'PSC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'POUNDS' ) THEN
        LUNITS_CODE = 'LBS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'POUNDS/IN**2 AB' ) THEN
        LUNITS_CODE = 'PIA'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'POUNDS/IN**2 GA' ) THEN
        LUNITS_CODE = 'PIG'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RADIANS' ) THEN
        LUNITS_CODE = 'RAD'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RADIANS/SEC' ) THEN
        LUNITS_CODE = 'RPS'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RADIANS/SEC**2' ) THEN
        LUNITS_CODE = 'RP2'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RECIPROCAL CMS' ) THEN
        LUNITS_CODE = 'RPC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RECIPROCAL INS' ) THEN
        LUNITS_CODE = 'RPI'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RECIPROCAL MET' ) THEN
        LUNITS_CODE = 'RPM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RECIPROCAL MM' ) THEN
        LUNITS_CODE = 'RMM'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SECONDS' ) THEN
        LUNITS_CODE = 'SEC'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VOLTS' ) THEN
        LUNITS_CODE = 'VOL'
        RETURN
      END IF

      RETURN

      END

! end of LUNITS_CODE()


!***********************************************************************
!
! Function:  LMODEL_DESC( CODE )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:52 1999
!
! Description:
!  Lookup function for matching MODEL code to a description
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMODEL_DESC( CODE )

      ! Arguments ______________________________________________________
      CHARACTER*(*) CODE

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER CODE_L

      ! Initializations ________________________________________________
      LMODEL_DESC = ' '
      CODE_L = L_TRIM(CODE)

      IF ( CODE(:CODE_L) .EQ. '0101' ) THEN
        LMODEL_DESC = 'ASTRO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0102' ) THEN
        LMODEL_DESC = 'CHEVELLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0103' ) THEN
        LMODEL_DESC = 'MONTE CARLO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0104' ) THEN
        LMODEL_DESC = 'SPECTRUM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0105' ) THEN
        LMODEL_DESC = 'SPRINT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0106' ) THEN
        LMODEL_DESC = 'IMPALA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0107' ) THEN
        LMODEL_DESC = 'CAPRICE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0108' ) THEN
        LMODEL_DESC = 'CAMARO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0109' ) THEN
        LMODEL_DESC = 'CORVETTE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0110' ) THEN
        LMODEL_DESC = 'NOVA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0111' ) THEN
        LMODEL_DESC = 'SUBURBAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0112' ) THEN
        LMODEL_DESC = 'VEGA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0113' ) THEN
        LMODEL_DESC = 'CHEVETTE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0114' ) THEN
        LMODEL_DESC = 'MALIBU'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0115' ) THEN
        LMODEL_DESC = 'BELAIR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0116' ) THEN
        LMODEL_DESC = 'CORSICA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0117' ) THEN
        LMODEL_DESC = 'BERETTA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0119' ) THEN
        LMODEL_DESC = 'FULL SIZE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0120' ) THEN
        LMODEL_DESC = 'EL CAMINO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0121' ) THEN
        LMODEL_DESC = 'MONZA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0122' ) THEN
        LMODEL_DESC = 'LUV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0123' ) THEN
        LMODEL_DESC = 'CITATION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0124' ) THEN
        LMODEL_DESC = 'S-10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0125' ) THEN
        LMODEL_DESC = 'C30 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0126' ) THEN
        LMODEL_DESC = 'G-10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0127' ) THEN
        LMODEL_DESC = 'SPORTVAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0128' ) THEN
        LMODEL_DESC = 'BLAZER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0129' ) THEN
        LMODEL_DESC = 'G-30'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0130' ) THEN
        LMODEL_DESC = 'LUMINA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0131' ) THEN
        LMODEL_DESC = 'CAVALIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0132' ) THEN
        LMODEL_DESC = 'CELEBRITY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0133' ) THEN
        LMODEL_DESC = 'C10 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0134' ) THEN
        LMODEL_DESC = 'C-1500'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0135' ) THEN
        LMODEL_DESC = 'K10 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0136' ) THEN
        LMODEL_DESC = 'K20 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0137' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0138' ) THEN
        LMODEL_DESC = 'CHEVYII'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0139' ) THEN
        LMODEL_DESC = 'S10 BLAZER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0140' ) THEN
        LMODEL_DESC = 'BEAUVILLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0141' ) THEN
        LMODEL_DESC = 'TAHOE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0142' ) THEN
        LMODEL_DESC = 'VENTURE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0143' ) THEN
        LMODEL_DESC = 'K1500 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0144' ) THEN
        LMODEL_DESC = 'METRO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0145' ) THEN
        LMODEL_DESC = 'K2500 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0199' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0201' ) THEN
        LMODEL_DESC = 'TEMPO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0202' ) THEN
        LMODEL_DESC = 'MAVERICK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0203' ) THEN
        LMODEL_DESC = 'MERKUR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0204' ) THEN
        LMODEL_DESC = 'TORINO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0205' ) THEN
        LMODEL_DESC = 'MUSTANG'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0206' ) THEN
        LMODEL_DESC = 'AEROSTAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0207' ) THEN
        LMODEL_DESC = 'LTD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0208' ) THEN
        LMODEL_DESC = 'TAURUS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0209' ) THEN
        LMODEL_DESC = 'THUNDERBIRD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0210' ) THEN
        LMODEL_DESC = 'RANGER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0211' ) THEN
        LMODEL_DESC = 'FESTIVA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0212' ) THEN
        LMODEL_DESC = 'PROBE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0213' ) THEN
        LMODEL_DESC = 'PINTO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0214' ) THEN
        LMODEL_DESC = 'FIESTA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0215' ) THEN
        LMODEL_DESC = 'RANCHERO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0217' ) THEN
        LMODEL_DESC = 'FAIRMONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0218' ) THEN
        LMODEL_DESC = 'FULL SIZE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0219' ) THEN
        LMODEL_DESC = 'ELITE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0220' ) THEN
        LMODEL_DESC = 'GRANADA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0221' ) THEN
        LMODEL_DESC = 'BRONCO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0222' ) THEN
        LMODEL_DESC = 'BRONCO II'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0223' ) THEN
        LMODEL_DESC = 'VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0224' ) THEN
        LMODEL_DESC = 'E100 VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0225' ) THEN
        LMODEL_DESC = 'E150 VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0227' ) THEN
        LMODEL_DESC = 'ESCORT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0231' ) THEN
        LMODEL_DESC = 'GALAXIE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0232' ) THEN
        LMODEL_DESC = 'CLUBWAGON MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0233' ) THEN
        LMODEL_DESC = 'EXP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0234' ) THEN
        LMODEL_DESC = 'LASER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0235' ) THEN
        LMODEL_DESC = 'COURIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0236' ) THEN
        LMODEL_DESC = 'F150 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0237' ) THEN
        LMODEL_DESC = 'F250 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0238' ) THEN
        LMODEL_DESC = 'F350 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0239' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0240' ) THEN
        LMODEL_DESC = 'CROWN VICTORIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0241' ) THEN
        LMODEL_DESC = 'EXPLORER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0242' ) THEN
        LMODEL_DESC = 'WINDSTAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0243' ) THEN
        LMODEL_DESC = 'ASPIRE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0244' ) THEN
        LMODEL_DESC = 'CONTOUR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0245' ) THEN
        LMODEL_DESC = 'EXPEDITION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0246' ) THEN
        LMODEL_DESC = 'ESCORT ZX2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0247' ) THEN
        LMODEL_DESC = 'EV RANGER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0299' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0301' ) THEN
        LMODEL_DESC = 'PARISIENNE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0302' ) THEN
        LMODEL_DESC = 'CATALINA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0303' ) THEN
        LMODEL_DESC = 'FIERO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0304' ) THEN
        LMODEL_DESC = 'BONNEVILLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0305' ) THEN
        LMODEL_DESC = 'GRAND PRIX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0306' ) THEN
        LMODEL_DESC = 'FIREBIRD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0307' ) THEN
        LMODEL_DESC = 'SUNBIRD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0308' ) THEN
        LMODEL_DESC = 'PHOENIX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0311' ) THEN
        LMODEL_DESC = 'VENTURA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0312' ) THEN
        LMODEL_DESC = 'LEMANS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0313' ) THEN
        LMODEL_DESC = 'FULL SIZE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0314' ) THEN
        LMODEL_DESC = 'GRAND AM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0315' ) THEN
        LMODEL_DESC = 'ASTRE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0316' ) THEN
        LMODEL_DESC = 'J2000'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0317' ) THEN
        LMODEL_DESC = 'T1000'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0318' ) THEN
        LMODEL_DESC = '6000 LE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0319' ) THEN
        LMODEL_DESC = 'TRANS SPORT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0320' ) THEN
        LMODEL_DESC = 'SUNFIRE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0399' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0401' ) THEN
        LMODEL_DESC = 'SKYLARK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0402' ) THEN
        LMODEL_DESC = 'LESABRE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0403' ) THEN
        LMODEL_DESC = 'SOMERSET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0404' ) THEN
        LMODEL_DESC = 'ELECTRA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0405' ) THEN
        LMODEL_DESC = 'RIVIERA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0406' ) THEN
        LMODEL_DESC = 'CENTURY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0407' ) THEN
        LMODEL_DESC = 'PARK AVENUE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0408' ) THEN
        LMODEL_DESC = 'ROADMASTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0409' ) THEN
        LMODEL_DESC = 'ESTATE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0411' ) THEN
        LMODEL_DESC = 'SKYHAWK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0416' ) THEN
        LMODEL_DESC = 'OPEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0417' ) THEN
        LMODEL_DESC = 'REGAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0499' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0501' ) THEN
        LMODEL_DESC = 'VALIANT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0502' ) THEN
        LMODEL_DESC = 'COLT VISTA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0503' ) THEN
        LMODEL_DESC = 'CONQUEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0504' ) THEN
        LMODEL_DESC = 'SCAMP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0505' ) THEN
        LMODEL_DESC = 'CARAVELLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0506' ) THEN
        LMODEL_DESC = 'SUNDANCE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0507' ) THEN
        LMODEL_DESC = 'ACCLAIM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0510' ) THEN
        LMODEL_DESC = 'SAPPORO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0512' ) THEN
        LMODEL_DESC = 'FURY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0514' ) THEN
        LMODEL_DESC = 'VOLARE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0516' ) THEN
        LMODEL_DESC = 'HORIZON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0517' ) THEN
        LMODEL_DESC = 'CHAMP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0518' ) THEN
        LMODEL_DESC = 'RELIANT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0519' ) THEN
        LMODEL_DESC = 'ARROW PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0520' ) THEN
        LMODEL_DESC = 'CRICKET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0521' ) THEN
        LMODEL_DESC = 'FARGO PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0522' ) THEN
        LMODEL_DESC = 'TC3'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0523' ) THEN
        LMODEL_DESC = 'TRAILDUSTER MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0524' ) THEN
        LMODEL_DESC = 'VOYAGER VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0525' ) THEN
        LMODEL_DESC = 'LASER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0526' ) THEN
        LMODEL_DESC = 'COLT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0527' ) THEN
        LMODEL_DESC = 'NEON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0599' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0601' ) THEN
        LMODEL_DESC = 'CALAIS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0602' ) THEN
        LMODEL_DESC = 'DELTA 88'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0603' ) THEN
        LMODEL_DESC = '98'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0604' ) THEN
        LMODEL_DESC = 'TORONADO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0605' ) THEN
        LMODEL_DESC = 'CUTLASS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0606' ) THEN
        LMODEL_DESC = 'ROYALE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0607' ) THEN
        LMODEL_DESC = 'ACHIEVA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0610' ) THEN
        LMODEL_DESC = 'OMEGA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0611' ) THEN
        LMODEL_DESC = 'STARFIRE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0612' ) THEN
        LMODEL_DESC = 'LOYALE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0614' ) THEN
        LMODEL_DESC = 'CUSTOM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0615' ) THEN
        LMODEL_DESC = 'FIRENZA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0616' ) THEN
        LMODEL_DESC = 'AURORA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0617' ) THEN
        LMODEL_DESC = 'INTRIGUE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0618' ) THEN
        LMODEL_DESC = 'ALERO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0699' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0701' ) THEN
        LMODEL_DESC = 'DART'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0702' ) THEN
        LMODEL_DESC = 'CORONET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0703' ) THEN
        LMODEL_DESC = 'RAMPAGE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0704' ) THEN
        LMODEL_DESC = 'MONACO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0705' ) THEN
        LMODEL_DESC = 'CHALLENGER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0706' ) THEN
        LMODEL_DESC = 'CHARGER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0707' ) THEN
        LMODEL_DESC = 'COLT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0708' ) THEN
        LMODEL_DESC = '600'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0709' ) THEN
        LMODEL_DESC = 'OMNI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0710' ) THEN
        LMODEL_DESC = 'DIPLOMAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0711' ) THEN
        LMODEL_DESC = 'CARAVAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0712' ) THEN
        LMODEL_DESC = 'LANCER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0713' ) THEN
        LMODEL_DESC = 'DAYTONA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0714' ) THEN
        LMODEL_DESC = 'ASPEN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0715' ) THEN
        LMODEL_DESC = 'MAGNUM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0716' ) THEN
        LMODEL_DESC = 'MINIRAM VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0717' ) THEN
        LMODEL_DESC = 'ST. REGIS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0718' ) THEN
        LMODEL_DESC = 'MIRADA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0719' ) THEN
        LMODEL_DESC = 'ARIES'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0720' ) THEN
        LMODEL_DESC = 'SPORTSMAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0721' ) THEN
        LMODEL_DESC = 'SHADOW'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0722' ) THEN
        LMODEL_DESC = 'COLT PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0723' ) THEN
        LMODEL_DESC = 'DAKOTA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0724' ) THEN
        LMODEL_DESC = 'O24'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0725' ) THEN
        LMODEL_DESC = 'SPIRIT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0726' ) THEN
        LMODEL_DESC = 'RAMCHARGER MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0727' ) THEN
        LMODEL_DESC = 'CELESTE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0728' ) THEN
        LMODEL_DESC = 'RAM 50'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0729' ) THEN
        LMODEL_DESC = '400'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0730' ) THEN
        LMODEL_DESC = 'D-150'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0731' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0732' ) THEN
        LMODEL_DESC = 'DYNASTY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0733' ) THEN
        LMODEL_DESC = 'RAM WAGON VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0734' ) THEN
        LMODEL_DESC = 'INTREPID'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0735' ) THEN
        LMODEL_DESC = 'STEALTH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0736' ) THEN
        LMODEL_DESC = 'RAM 150'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0737' ) THEN
        LMODEL_DESC = 'RAM 250 VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0738' ) THEN
        LMODEL_DESC = 'NEON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0739' ) THEN
        LMODEL_DESC = 'AVENGER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0740' ) THEN
        LMODEL_DESC = 'STRATUS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0741' ) THEN
        LMODEL_DESC = 'GRAND CARAVAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0742' ) THEN
        LMODEL_DESC = 'DURANGO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0743' ) THEN
        LMODEL_DESC = 'RAM1500'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0744' ) THEN
        LMODEL_DESC = 'RAM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0799' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0801' ) THEN
        LMODEL_DESC = 'GOLF'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0802' ) THEN
        LMODEL_DESC = 'BEETLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0803' ) THEN
        LMODEL_DESC = 'FOX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0804' ) THEN
        LMODEL_DESC = 'DASHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0805' ) THEN
        LMODEL_DESC = 'POLO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0808' ) THEN
        LMODEL_DESC = 'THE THING'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0809' ) THEN
        LMODEL_DESC = 'RABBIT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0810' ) THEN
        LMODEL_DESC = 'SCIROCCO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0814' ) THEN
        LMODEL_DESC = 'JETTA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0815' ) THEN
        LMODEL_DESC = 'VANAGON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0816' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0817' ) THEN
        LMODEL_DESC = 'QUANTUM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0818' ) THEN
        LMODEL_DESC = 'PASSAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0819' ) THEN
        LMODEL_DESC = 'CORRADO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0820' ) THEN
        LMODEL_DESC = 'CABRIO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0821' ) THEN
        LMODEL_DESC = 'EUROVAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0899' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0901' ) THEN
        LMODEL_DESC = 'MONTEGO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0902' ) THEN
        LMODEL_DESC = 'TRACER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0903' ) THEN
        LMODEL_DESC = 'SABLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0904' ) THEN
        LMODEL_DESC = 'BOBCAT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0905' ) THEN
        LMODEL_DESC = 'TOPAZ'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0907' ) THEN
        LMODEL_DESC = 'COUGAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0908' ) THEN
        LMODEL_DESC = 'ZEPHYR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0909' ) THEN
        LMODEL_DESC = 'MARQUIS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0910' ) THEN
        LMODEL_DESC = 'COMET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0912' ) THEN
        LMODEL_DESC = 'MONARCH'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0913' ) THEN
        LMODEL_DESC = 'CAPRI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0915' ) THEN
        LMODEL_DESC = 'LYNX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0916' ) THEN
        LMODEL_DESC = 'LN7'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0917' ) THEN
        LMODEL_DESC = 'VILLAGER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0918' ) THEN
        LMODEL_DESC = 'MYSTIQUE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '0999' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1002' ) THEN
        LMODEL_DESC = 'SEVILLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1003' ) THEN
        LMODEL_DESC = 'FLEETWOOD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1004' ) THEN
        LMODEL_DESC = 'ELDORADO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1005' ) THEN
        LMODEL_DESC = 'BROUGHAM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1008' ) THEN
        LMODEL_DESC = 'DE VILLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1009' ) THEN
        LMODEL_DESC = 'CIMARRON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1010' ) THEN
        LMODEL_DESC = 'CONCOURSE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1099' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1101' ) THEN
        LMODEL_DESC = 'GREMLIN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1102' ) THEN
        LMODEL_DESC = 'HORNET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1103' ) THEN
        LMODEL_DESC = 'ALLIANCE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1106' ) THEN
        LMODEL_DESC = 'AMX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1109' ) THEN
        LMODEL_DESC = 'MATADOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1111' ) THEN
        LMODEL_DESC = 'CONCORD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1112' ) THEN
        LMODEL_DESC = 'PACER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1114' ) THEN
        LMODEL_DESC = 'SPIRIT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1199' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1201' ) THEN
        LMODEL_DESC = '100'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1202' ) THEN
        LMODEL_DESC = 'FOX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1203' ) THEN
        LMODEL_DESC = '200'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1204' ) THEN
        LMODEL_DESC = '5000'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1205' ) THEN
        LMODEL_DESC = '4000'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1206' ) THEN
        LMODEL_DESC = '80'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1207' ) THEN
        LMODEL_DESC = 'A4'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1208' ) THEN
        LMODEL_DESC = 'A6'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1209' ) THEN
        LMODEL_DESC = '90S'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1210' ) THEN
        LMODEL_DESC = 'A8'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1299' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1301' ) THEN
        LMODEL_DESC = 'CONTINENTAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1302' ) THEN
        LMODEL_DESC = 'MARK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1305' ) THEN
        LMODEL_DESC = 'VERSAILLES'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1306' ) THEN
        LMODEL_DESC = 'TOWN CAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1399' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1401' ) THEN
        LMODEL_DESC = '504'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1402' ) THEN
        LMODEL_DESC = '405'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1403' ) THEN
        LMODEL_DESC = '604'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1404' ) THEN
        LMODEL_DESC = '505'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1499' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1501' ) THEN
        LMODEL_DESC = 'PULSAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1502' ) THEN
        LMODEL_DESC = 'SENTRA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1503' ) THEN
        LMODEL_DESC = '300 ZX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1506' ) THEN
        LMODEL_DESC = '2000'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1507' ) THEN
        LMODEL_DESC = '610'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1508' ) THEN
        LMODEL_DESC = '210'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1510' ) THEN
        LMODEL_DESC = '260Z'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1511' ) THEN
        LMODEL_DESC = '280'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1512' ) THEN
        LMODEL_DESC = '510'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1513' ) THEN
        LMODEL_DESC = '200 SX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1515' ) THEN
        LMODEL_DESC = '310'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1516' ) THEN
        LMODEL_DESC = '810'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1517' ) THEN
        LMODEL_DESC = 'VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1518' ) THEN
        LMODEL_DESC = '1200'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1524' ) THEN
        LMODEL_DESC = 'F10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1525' ) THEN
        LMODEL_DESC = 'MAXIMA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1526' ) THEN
        LMODEL_DESC = '710'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1527' ) THEN
        LMODEL_DESC = 'STANZA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1528' ) THEN
        LMODEL_DESC = 'KING CAB PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1529' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1530' ) THEN
        LMODEL_DESC = '240 SX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1531' ) THEN
        LMODEL_DESC = 'AXXESS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1534' ) THEN
        LMODEL_DESC = 'PATHFINDER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1535' ) THEN
        LMODEL_DESC = 'ALTIMA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1536' ) THEN
        LMODEL_DESC = 'QUEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1537' ) THEN
        LMODEL_DESC = 'NX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1538' ) THEN
        LMODEL_DESC = 'FRONTIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1599' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1601' ) THEN
        LMODEL_DESC = 'LAND CRUISER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1602' ) THEN
        LMODEL_DESC = 'COROLLA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1603' ) THEN
        LMODEL_DESC = 'CAMRY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1604' ) THEN
        LMODEL_DESC = 'CORONA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1605' ) THEN
        LMODEL_DESC = 'MARK II'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1606' ) THEN
        LMODEL_DESC = 'CELICA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1607' ) THEN
        LMODEL_DESC = 'SUPRA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1608' ) THEN
        LMODEL_DESC = 'CRESSIDA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1609' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1610' ) THEN
        LMODEL_DESC = 'TERCEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1611' ) THEN
        LMODEL_DESC = 'VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1612' ) THEN
        LMODEL_DESC = 'STARLET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1613' ) THEN
        LMODEL_DESC = '4RUNNER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1614' ) THEN
        LMODEL_DESC = 'MR2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1615' ) THEN
        LMODEL_DESC = 'PREVIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1617' ) THEN
        LMODEL_DESC = 'PASEO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1618' ) THEN
        LMODEL_DESC = 'T100'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1619' ) THEN
        LMODEL_DESC = 'AVALON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1620' ) THEN
        LMODEL_DESC = 'TACOMA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1621' ) THEN
        LMODEL_DESC = 'RAV4'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1622' ) THEN
        LMODEL_DESC = 'SIENNA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1623' ) THEN
        LMODEL_DESC = 'CAMRY SOLARA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1699' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1701' ) THEN
        LMODEL_DESC = 'LE CAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1702' ) THEN
        LMODEL_DESC = '18'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1703' ) THEN
        LMODEL_DESC = '12'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1704' ) THEN
        LMODEL_DESC = '15'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1705' ) THEN
        LMODEL_DESC = '17'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1706' ) THEN
        LMODEL_DESC = '5'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1707' ) THEN
        LMODEL_DESC = 'FUEGO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1708' ) THEN
        LMODEL_DESC = 'ENCORE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1709' ) THEN
        LMODEL_DESC = 'ALLIANCE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1710' ) THEN
        LMODEL_DESC = 'SPORTSWAGON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1711' ) THEN
        LMODEL_DESC = 'MEDALLION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1799' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1801' ) THEN
        LMODEL_DESC = '808'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1802' ) THEN
        LMODEL_DESC = '323'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1803' ) THEN
        LMODEL_DESC = '929'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1804' ) THEN
        LMODEL_DESC = 'RX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1805' ) THEN
        LMODEL_DESC = 'GLC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1806' ) THEN
        LMODEL_DESC = 'MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1807' ) THEN
        LMODEL_DESC = '626'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1808' ) THEN
        LMODEL_DESC = 'MIATA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1809' ) THEN
        LMODEL_DESC = 'COSMO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1811' ) THEN
        LMODEL_DESC = 'MIZER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1812' ) THEN
        LMODEL_DESC = 'MX6'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1813' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1814' ) THEN
        LMODEL_DESC = 'B2000 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1815' ) THEN
        LMODEL_DESC = '323-PROTEGE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1816' ) THEN
        LMODEL_DESC = 'MX3'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1817' ) THEN
        LMODEL_DESC = 'MILLENIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1818' ) THEN
        LMODEL_DESC = 'MX5'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1899' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1902' ) THEN
        LMODEL_DESC = '128'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1903' ) THEN
        LMODEL_DESC = '124'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1905' ) THEN
        LMODEL_DESC = 'SPIDER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1906' ) THEN
        LMODEL_DESC = 'X19'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1908' ) THEN
        LMODEL_DESC = 'STRADA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1909' ) THEN
        LMODEL_DESC = 'BRAVA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1910' ) THEN
        LMODEL_DESC = '131'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1911' ) THEN
        LMODEL_DESC = '132'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '1999' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2004' ) THEN
        LMODEL_DESC = '164'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2006' ) THEN
        LMODEL_DESC = '240'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2007' ) THEN
        LMODEL_DESC = '260'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2010' ) THEN
        LMODEL_DESC = '244'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2011' ) THEN
        LMODEL_DESC = 'COUPE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2012' ) THEN
        LMODEL_DESC = 'DL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2013' ) THEN
        LMODEL_DESC = 'GLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2014' ) THEN
        LMODEL_DESC = 'GLT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2015' ) THEN
        LMODEL_DESC = '245'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2016' ) THEN
        LMODEL_DESC = '265'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2017' ) THEN
        LMODEL_DESC = '740 GLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2018' ) THEN
        LMODEL_DESC = '940'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2019' ) THEN
        LMODEL_DESC = '850'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2020' ) THEN
        LMODEL_DESC = '960'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2021' ) THEN
        LMODEL_DESC = 'S70'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2022' ) THEN
        LMODEL_DESC = 'S80'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2099' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2101' ) THEN
        LMODEL_DESC = 'NEWPORT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2103' ) THEN
        LMODEL_DESC = '300'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2104' ) THEN
        LMODEL_DESC = 'NEW YORKER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2105' ) THEN
        LMODEL_DESC = 'TOWN AND COUNTRY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2106' ) THEN
        LMODEL_DESC = 'IMPERIAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2107' ) THEN
        LMODEL_DESC = 'CORDOBA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2108' ) THEN
        LMODEL_DESC = 'LE BARON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2109' ) THEN
        LMODEL_DESC = 'FIFTH AVENUE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2110' ) THEN
        LMODEL_DESC = 'CONQUEST'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2111' ) THEN
        LMODEL_DESC = 'CIRRUS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2112' ) THEN
        LMODEL_DESC = 'CONCORDE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2113' ) THEN
        LMODEL_DESC = 'INTREPID'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2114' ) THEN
        LMODEL_DESC = 'SEBRING CONVERTIBLE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2199' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2201' ) THEN
        LMODEL_DESC = '400'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2202' ) THEN
        LMODEL_DESC = 'CENTAURI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2299' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2301' ) THEN
        LMODEL_DESC = 'CIVIC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2302' ) THEN
        LMODEL_DESC = 'ACCORD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2305' ) THEN
        LMODEL_DESC = 'PRELUDE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2306' ) THEN
        LMODEL_DESC = 'DEL SOL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2307' ) THEN
        LMODEL_DESC = 'ODYSSEY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2308' ) THEN
        LMODEL_DESC = 'PASSPORT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2309' ) THEN
        LMODEL_DESC = 'CRV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2399' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2401' ) THEN
        LMODEL_DESC = 'GV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2499' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2501' ) THEN
        LMODEL_DESC = 'MIDGET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2503' ) THEN
        LMODEL_DESC = 'MGB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2599' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2601' ) THEN
        LMODEL_DESC = 'XT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2602' ) THEN
        LMODEL_DESC = 'GL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2603' ) THEN
        LMODEL_DESC = 'DL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2604' ) THEN
        LMODEL_DESC = 'FE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2605' ) THEN
        LMODEL_DESC = 'GF'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2607' ) THEN
        LMODEL_DESC = 'BRAT MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2608' ) THEN
        LMODEL_DESC = 'GLF'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2609' ) THEN
        LMODEL_DESC = 'WAGON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2610' ) THEN
        LMODEL_DESC = 'JUSTY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2611' ) THEN
        LMODEL_DESC = 'LEGACY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2612' ) THEN
        LMODEL_DESC = 'LOYALE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2613' ) THEN
        LMODEL_DESC = 'IMPREZA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2614' ) THEN
        LMODEL_DESC = 'FORESTER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2699' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2701' ) THEN
        LMODEL_DESC = '320 I'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2702' ) THEN
        LMODEL_DESC = '528 I'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2703' ) THEN
        LMODEL_DESC = '633 CSI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2704' ) THEN
        LMODEL_DESC = '733 I'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2705' ) THEN
        LMODEL_DESC = '3.0 SI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2706' ) THEN
        LMODEL_DESC = '530 I'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2707' ) THEN
        LMODEL_DESC = '630 CSI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2708' ) THEN
        LMODEL_DESC = '2002'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2709' ) THEN
        LMODEL_DESC = '318'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2710' ) THEN
        LMODEL_DESC = '325 I'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2711' ) THEN
        LMODEL_DESC = '525I'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2712' ) THEN
        LMODEL_DESC = '540 I'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2799' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2801' ) THEN
        LMODEL_DESC = '240'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2802' ) THEN
        LMODEL_DESC = '300'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2803' ) THEN
        LMODEL_DESC = '190'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2806' ) THEN
        LMODEL_DESC = '280'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2809' ) THEN
        LMODEL_DESC = '450'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2812' ) THEN
        LMODEL_DESC = '6.9'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2814' ) THEN
        LMODEL_DESC = '230'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2815' ) THEN
        LMODEL_DESC = '380'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2816' ) THEN
        LMODEL_DESC = 'C220'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2817' ) THEN
        LMODEL_DESC = 'E420'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2818' ) THEN
        LMODEL_DESC = 'ML320'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2899' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2901' ) THEN
        LMODEL_DESC = 'CAR ELECTRIC'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '2999' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3001' ) THEN
        LMODEL_DESC = '99'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3002' ) THEN
        LMODEL_DESC = '900'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3003' ) THEN
        LMODEL_DESC = '9000'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3099' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3101' ) THEN
        LMODEL_DESC = 'SPITFIRE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3102' ) THEN
        LMODEL_DESC = 'TR7'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3199' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3201' ) THEN
        LMODEL_DESC = 'DEFORMABLE IMPACTOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3202' ) THEN
        LMODEL_DESC = 'CALSPAN RSV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3203' ) THEN
        LMODEL_DESC = 'LOAD CELL IMPACTOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3204' ) THEN
        LMODEL_DESC = 'MINICARS RSV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3206' ) THEN
        LMODEL_DESC = 'FLAT IMPACTOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3208' ) THEN
        LMODEL_DESC = 'TRUCK SIMULATOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3209' ) THEN
        LMODEL_DESC = 'CONTOURED IMPACTOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3210' ) THEN
        LMODEL_DESC = 'ROLLOVER CART'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3211' ) THEN
        LMODEL_DESC = 'SLED'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3299' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3301' ) THEN
        LMODEL_DESC = 'SAMURAI'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3302' ) THEN
        LMODEL_DESC = 'SIDEKICK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3303' ) THEN
        LMODEL_DESC = 'SWIFT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3304' ) THEN
        LMODEL_DESC = 'VITARA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3399' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3401' ) THEN
        LMODEL_DESC = 'PONY EXCEL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3402' ) THEN
        LMODEL_DESC = 'STELLAR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3403' ) THEN
        LMODEL_DESC = 'EXCEL GLS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3404' ) THEN
        LMODEL_DESC = 'SONATA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3405' ) THEN
        LMODEL_DESC = 'SCOUPE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3406' ) THEN
        LMODEL_DESC = 'ELANTRA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3407' ) THEN
        LMODEL_DESC = 'ACCENT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3408' ) THEN
        LMODEL_DESC = 'TIBURON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3499' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3501' ) THEN
        LMODEL_DESC = 'MOTOR HOME'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3599' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3601' ) THEN
        LMODEL_DESC = 'TAXICAB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3699' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3701' ) THEN
        LMODEL_DESC = 'MOTOR HOME'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3799' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3801' ) THEN
        LMODEL_DESC = 'COUPE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3899' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3901' ) THEN
        LMODEL_DESC = 'CHARADE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '3999' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4001' ) THEN
        LMODEL_DESC = 'JIMMY MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4002' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4003' ) THEN
        LMODEL_DESC = 'SPRINT MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4004' ) THEN
        LMODEL_DESC = 'SPORTVAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4005' ) THEN
        LMODEL_DESC = 'VANDURA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4006' ) THEN
        LMODEL_DESC = 'ASTRO TRUCK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4007' ) THEN
        LMODEL_DESC = 'S15 PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4008' ) THEN
        LMODEL_DESC = 'EV1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4099' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4101' ) THEN
        LMODEL_DESC = 'SCOUT MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4102' ) THEN
        LMODEL_DESC = 'SS MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4103' ) THEN
        LMODEL_DESC = 'TERRA MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4104' ) THEN
        LMODEL_DESC = 'TRAVELER MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4199' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4201' ) THEN
        LMODEL_DESC = 'I-MARK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4202' ) THEN
        LMODEL_DESC = 'PUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4203' ) THEN
        LMODEL_DESC = 'IMPULSE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4204' ) THEN
        LMODEL_DESC = 'TROOPER II'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4205' ) THEN
        LMODEL_DESC = 'SPACECAB'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4206' ) THEN
        LMODEL_DESC = 'AMIGO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4207' ) THEN
        LMODEL_DESC = 'STYLUS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4208' ) THEN
        LMODEL_DESC = 'RODEO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4209' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4299' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4401' ) THEN
        LMODEL_DESC = 'CHEROKEE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4402' ) THEN
        LMODEL_DESC = 'CJ'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4403' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4404' ) THEN
        LMODEL_DESC = 'TOWNSIDE MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4405' ) THEN
        LMODEL_DESC = 'COMANCHE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4406' ) THEN
        LMODEL_DESC = 'VJ'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4407' ) THEN
        LMODEL_DESC = 'WRANGLER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4408' ) THEN
        LMODEL_DESC = 'CHEROKEE LAREDO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4409' ) THEN
        LMODEL_DESC = 'GRAND CHEROKEE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4499' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4801' ) THEN
        LMODEL_DESC = 'MOTOR HOME'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '4899' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5301' ) THEN
        LMODEL_DESC = 'VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5399' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5401' ) THEN
        LMODEL_DESC = 'ELECTRICA 007'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5402' ) THEN
        LMODEL_DESC = 'COURIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5403' ) THEN
        LMODEL_DESC = 'ELECTRICA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5499' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5501' ) THEN
        LMODEL_DESC = 'DEFORMABLE IMPACTOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5599' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5601' ) THEN
        LMODEL_DESC = 'ELECTREK'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5699' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5801' ) THEN
        LMODEL_DESC = 'FAIRMONT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5802' ) THEN
        LMODEL_DESC = 'EVCORT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5899' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5901' ) THEN
        LMODEL_DESC = 'LEOPARD'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '5999' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6001' ) THEN
        LMODEL_DESC = 'TREKKER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6099' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6201' ) THEN
        LMODEL_DESC = 'PICKUP'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6202' ) THEN
        LMODEL_DESC = 'MONTERO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6203' ) THEN
        LMODEL_DESC = 'CORDIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6204' ) THEN
        LMODEL_DESC = 'TREDIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6205' ) THEN
        LMODEL_DESC = 'GALANT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6206' ) THEN
        LMODEL_DESC = 'STARION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6207' ) THEN
        LMODEL_DESC = 'MIRAGE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6208' ) THEN
        LMODEL_DESC = 'PRECIS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6209' ) THEN
        LMODEL_DESC = 'VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6210' ) THEN
        LMODEL_DESC = 'ECLIPSE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6211' ) THEN
        LMODEL_DESC = 'DIAMANTE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6212' ) THEN
        LMODEL_DESC = '3000 GT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6213' ) THEN
        LMODEL_DESC = 'MIGHTY MAX'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6299' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6301' ) THEN
        LMODEL_DESC = 'METRO'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6302' ) THEN
        LMODEL_DESC = 'PRIZM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6303' ) THEN
        LMODEL_DESC = 'STORM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6304' ) THEN
        LMODEL_DESC = 'TRACKER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6399' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6401' ) THEN
        LMODEL_DESC = 'ES250'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6402' ) THEN
        LMODEL_DESC = 'ES300'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6403' ) THEN
        LMODEL_DESC = 'SC300'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6404' ) THEN
        LMODEL_DESC = 'SC400'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6405' ) THEN
        LMODEL_DESC = 'LS400'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6406' ) THEN
        LMODEL_DESC = 'GS300'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6499' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6501' ) THEN
        LMODEL_DESC = 'REDDI BUS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6599' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6601' ) THEN
        LMODEL_DESC = 'CONVENTIONAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6602' ) THEN
        LMODEL_DESC = 'FORWARD CONTROL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6699' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6701' ) THEN
        LMODEL_DESC = 'CONVENTIONAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6702' ) THEN
        LMODEL_DESC = 'MIGHTY MITE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6799' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6801' ) THEN
        LMODEL_DESC = 'CONVENTIONAL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6802' ) THEN
        LMODEL_DESC = 'CADET'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6899' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6901' ) THEN
        LMODEL_DESC = 'SL1'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6902' ) THEN
        LMODEL_DESC = 'SL2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6903' ) THEN
        LMODEL_DESC = 'SC2'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '6999' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7001' ) THEN
        LMODEL_DESC = 'MPV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7002' ) THEN
        LMODEL_DESC = 'PREMIER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7003' ) THEN
        LMODEL_DESC = 'SUMMIT'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7004' ) THEN
        LMODEL_DESC = 'MEDALLION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7005' ) THEN
        LMODEL_DESC = 'TALON'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7006' ) THEN
        LMODEL_DESC = 'VISION'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7101' ) THEN
        LMODEL_DESC = 'VANDURA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7199' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7201' ) THEN
        LMODEL_DESC = 'G20'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7202' ) THEN
        LMODEL_DESC = 'M30'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7203' ) THEN
        LMODEL_DESC = 'Q45'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7204' ) THEN
        LMODEL_DESC = 'J30'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7299' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7301' ) THEN
        LMODEL_DESC = 'LEGEND'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7302' ) THEN
        LMODEL_DESC = 'INTEGRA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7303' ) THEN
        LMODEL_DESC = 'VIGOR'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7304' ) THEN
        LMODEL_DESC = '2.5 TL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7305' ) THEN
        LMODEL_DESC = '3.5 RL'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7399' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7401' ) THEN
        LMODEL_DESC = 'STEALTH SPORT VAN'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7499' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7501' ) THEN
        LMODEL_DESC = 'SUPER BANTAM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7599' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7601' ) THEN
        LMODEL_DESC = 'FORCE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7602' ) THEN
        LMODEL_DESC = 'E-10'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7699' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7701' ) THEN
        LMODEL_DESC = 'SEPHIA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7702' ) THEN
        LMODEL_DESC = 'SPORTAGE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7801' ) THEN
        LMODEL_DESC = 'ZEV'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7901' ) THEN
        LMODEL_DESC = 'CUSTOMS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '7999' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8001' ) THEN
        LMODEL_DESC = 'DISCOVERY'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8099' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8101' ) THEN
        LMODEL_DESC = 'TROPICA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8199' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8201' ) THEN
        LMODEL_DESC = 'COMMODORE ACCLAIM'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8299' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8301' ) THEN
        LMODEL_DESC = 'PLASCORE'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8399' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8401' ) THEN
        LMODEL_DESC = 'NUBIRA'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '8402' ) THEN
        LMODEL_DESC = 'LANOS'
        RETURN
      ELSE IF ( CODE(:CODE_L) .EQ. '9999' ) THEN
        LMODEL_DESC = 'OTHER'
        RETURN
      END IF

      RETURN

      END

! end of LMODEL_DESC()


!***********************************************************************
!
! Function:  LMODEL_CODE( DESC )
!
! Language:  Fortran
!
! Author:  aorndorf@duster.nhtsa.dot.gov
!
! Date Created:  Tue Aug  3 14:54:52 1999
!
! Description:
!  Lookup function for matching MODEL description to a code
!
!***********************************************************************

      CHARACTER*(*) FUNCTION LMODEL_CODE( DESC )

      ! Arguments ______________________________________________________
      CHARACTER*(*) DESC

      ! Includes _______________________________________________________
      INCLUDE 'uds_fxn.fi'

      ! Variables ______________________________________________________
      INTEGER DESC_L

      ! Initializations ________________________________________________
      LMODEL_CODE = ' '
      DESC_L = L_TRIM(DESC)

      IF ( DESC(:DESC_L) .EQ. '100' ) THEN
        LMODEL_CODE = '1201'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '12' ) THEN
        LMODEL_CODE = '1703'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '1200' ) THEN
        LMODEL_CODE = '1518'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '124' ) THEN
        LMODEL_CODE = '1903'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '128' ) THEN
        LMODEL_CODE = '1902'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '131' ) THEN
        LMODEL_CODE = '1910'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '132' ) THEN
        LMODEL_CODE = '1911'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '15' ) THEN
        LMODEL_CODE = '1704'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '164' ) THEN
        LMODEL_CODE = '2004'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '17' ) THEN
        LMODEL_CODE = '1705'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '18' ) THEN
        LMODEL_CODE = '1702'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '190' ) THEN
        LMODEL_CODE = '2803'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '2.5 TL' ) THEN
        LMODEL_CODE = '7304'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '200' ) THEN
        LMODEL_CODE = '1203'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '200 SX' ) THEN
        LMODEL_CODE = '1513'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '2000' ) THEN
        LMODEL_CODE = '1506'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '2002' ) THEN
        LMODEL_CODE = '2708'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '210' ) THEN
        LMODEL_CODE = '1508'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '230' ) THEN
        LMODEL_CODE = '2814'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '240' ) THEN
        LMODEL_CODE = '2006'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '240 SX' ) THEN
        LMODEL_CODE = '1530'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '244' ) THEN
        LMODEL_CODE = '2010'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '245' ) THEN
        LMODEL_CODE = '2015'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '260' ) THEN
        LMODEL_CODE = '2007'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '260Z' ) THEN
        LMODEL_CODE = '1510'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '265' ) THEN
        LMODEL_CODE = '2016'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '280' ) THEN
        LMODEL_CODE = '1511'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '3.0 SI' ) THEN
        LMODEL_CODE = '2705'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '3.5 RL' ) THEN
        LMODEL_CODE = '7305'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '300' ) THEN
        LMODEL_CODE = '2103'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '300 ZX' ) THEN
        LMODEL_CODE = '1503'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '3000 GT' ) THEN
        LMODEL_CODE = '6212'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '310' ) THEN
        LMODEL_CODE = '1515'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '318' ) THEN
        LMODEL_CODE = '2709'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '320 I' ) THEN
        LMODEL_CODE = '2701'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '323' ) THEN
        LMODEL_CODE = '1802'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '323-PROTEGE' ) THEN
        LMODEL_CODE = '1815'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '325 I' ) THEN
        LMODEL_CODE = '2710'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '380' ) THEN
        LMODEL_CODE = '2815'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '400' ) THEN
        LMODEL_CODE = '0729'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '4000' ) THEN
        LMODEL_CODE = '1205'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '405' ) THEN
        LMODEL_CODE = '1402'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '450' ) THEN
        LMODEL_CODE = '2809'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '4RUNNER' ) THEN
        LMODEL_CODE = '1613'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '5' ) THEN
        LMODEL_CODE = '1706'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '5000' ) THEN
        LMODEL_CODE = '1204'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '504' ) THEN
        LMODEL_CODE = '1401'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '505' ) THEN
        LMODEL_CODE = '1404'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '510' ) THEN
        LMODEL_CODE = '1512'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '525I' ) THEN
        LMODEL_CODE = '2711'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '528 I' ) THEN
        LMODEL_CODE = '2702'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '530 I' ) THEN
        LMODEL_CODE = '2706'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '540 I' ) THEN
        LMODEL_CODE = '2712'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '6.9' ) THEN
        LMODEL_CODE = '2812'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '600' ) THEN
        LMODEL_CODE = '0708'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '6000 LE' ) THEN
        LMODEL_CODE = '0318'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '604' ) THEN
        LMODEL_CODE = '1403'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '610' ) THEN
        LMODEL_CODE = '1507'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '626' ) THEN
        LMODEL_CODE = '1807'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '630 CSI' ) THEN
        LMODEL_CODE = '2707'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '633 CSI' ) THEN
        LMODEL_CODE = '2703'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '710' ) THEN
        LMODEL_CODE = '1526'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '733 I' ) THEN
        LMODEL_CODE = '2704'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '740 GLE' ) THEN
        LMODEL_CODE = '2017'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '80' ) THEN
        LMODEL_CODE = '1206'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '808' ) THEN
        LMODEL_CODE = '1801'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '810' ) THEN
        LMODEL_CODE = '1516'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '850' ) THEN
        LMODEL_CODE = '2019'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '900' ) THEN
        LMODEL_CODE = '3002'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '9000' ) THEN
        LMODEL_CODE = '3003'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '90S' ) THEN
        LMODEL_CODE = '1209'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '929' ) THEN
        LMODEL_CODE = '1803'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '940' ) THEN
        LMODEL_CODE = '2018'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '960' ) THEN
        LMODEL_CODE = '2020'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '98' ) THEN
        LMODEL_CODE = '0603'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. '99' ) THEN
        LMODEL_CODE = '3001'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'A4' ) THEN
        LMODEL_CODE = '1207'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'A6' ) THEN
        LMODEL_CODE = '1208'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'A8' ) THEN
        LMODEL_CODE = '1210'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ACCENT' ) THEN
        LMODEL_CODE = '3407'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ACCLAIM' ) THEN
        LMODEL_CODE = '0507'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ACCORD' ) THEN
        LMODEL_CODE = '2302'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ACHIEVA' ) THEN
        LMODEL_CODE = '0607'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AEROSTAR' ) THEN
        LMODEL_CODE = '0206'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ALERO' ) THEN
        LMODEL_CODE = '0618'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ALLIANCE' ) THEN
        LMODEL_CODE = '1103'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ALTIMA' ) THEN
        LMODEL_CODE = '1535'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AMIGO' ) THEN
        LMODEL_CODE = '4206'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AMX' ) THEN
        LMODEL_CODE = '1106'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ARIES' ) THEN
        LMODEL_CODE = '0719'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ARROW PICKUP' ) THEN
        LMODEL_CODE = '0519'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ASPEN' ) THEN
        LMODEL_CODE = '0714'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ASPIRE' ) THEN
        LMODEL_CODE = '0243'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ASTRE' ) THEN
        LMODEL_CODE = '0315'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ASTRO' ) THEN
        LMODEL_CODE = '0101'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ASTRO TRUCK' ) THEN
        LMODEL_CODE = '4006'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AURORA' ) THEN
        LMODEL_CODE = '0616'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AVALON' ) THEN
        LMODEL_CODE = '1619'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AVENGER' ) THEN
        LMODEL_CODE = '0739'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'AXXESS' ) THEN
        LMODEL_CODE = '1531'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'B2000 PICKUP' ) THEN
        LMODEL_CODE = '1814'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BEAUVILLE' ) THEN
        LMODEL_CODE = '0140'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BEETLE' ) THEN
        LMODEL_CODE = '0802'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BELAIR' ) THEN
        LMODEL_CODE = '0115'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BERETTA' ) THEN
        LMODEL_CODE = '0117'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BLAZER' ) THEN
        LMODEL_CODE = '0128'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BOBCAT' ) THEN
        LMODEL_CODE = '0904'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BONNEVILLE' ) THEN
        LMODEL_CODE = '0304'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BRAT MPV' ) THEN
        LMODEL_CODE = '2607'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BRAVA' ) THEN
        LMODEL_CODE = '1909'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BRONCO' ) THEN
        LMODEL_CODE = '0221'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BRONCO II' ) THEN
        LMODEL_CODE = '0222'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'BROUGHAM' ) THEN
        LMODEL_CODE = '1005'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C-1500' ) THEN
        LMODEL_CODE = '0134'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C10 PICKUP' ) THEN
        LMODEL_CODE = '0133'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C220' ) THEN
        LMODEL_CODE = '2816'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'C30 PICKUP' ) THEN
        LMODEL_CODE = '0125'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CABRIO' ) THEN
        LMODEL_CODE = '0820'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CADET' ) THEN
        LMODEL_CODE = '6802'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CALAIS' ) THEN
        LMODEL_CODE = '0601'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CALSPAN RSV' ) THEN
        LMODEL_CODE = '3202'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CAMARO' ) THEN
        LMODEL_CODE = '0108'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CAMRY' ) THEN
        LMODEL_CODE = '1603'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CAMRY SOLARA' ) THEN
        LMODEL_CODE = '1623'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CAPRI' ) THEN
        LMODEL_CODE = '0913'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CAPRICE' ) THEN
        LMODEL_CODE = '0107'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CAR ELECTRIC' ) THEN
        LMODEL_CODE = '2901'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CARAVAN' ) THEN
        LMODEL_CODE = '0711'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CARAVELLE' ) THEN
        LMODEL_CODE = '0505'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CATALINA' ) THEN
        LMODEL_CODE = '0302'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CAVALIER' ) THEN
        LMODEL_CODE = '0131'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CELEBRITY' ) THEN
        LMODEL_CODE = '0132'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CELESTE' ) THEN
        LMODEL_CODE = '0727'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CELICA' ) THEN
        LMODEL_CODE = '1606'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CENTAURI' ) THEN
        LMODEL_CODE = '2202'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CENTURY' ) THEN
        LMODEL_CODE = '0406'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHALLENGER' ) THEN
        LMODEL_CODE = '0705'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHAMP' ) THEN
        LMODEL_CODE = '0517'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHARADE' ) THEN
        LMODEL_CODE = '3901'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHARGER' ) THEN
        LMODEL_CODE = '0706'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEROKEE' ) THEN
        LMODEL_CODE = '4401'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEROKEE LAREDO' ) THEN
        LMODEL_CODE = '4408'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEVELLE' ) THEN
        LMODEL_CODE = '0102'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEVETTE' ) THEN
        LMODEL_CODE = '0113'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CHEVYII' ) THEN
        LMODEL_CODE = '0138'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CIMARRON' ) THEN
        LMODEL_CODE = '1009'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CIRRUS' ) THEN
        LMODEL_CODE = '2111'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CITATION' ) THEN
        LMODEL_CODE = '0123'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CIVIC' ) THEN
        LMODEL_CODE = '2301'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CJ' ) THEN
        LMODEL_CODE = '4402'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CLUBWAGON MPV' ) THEN
        LMODEL_CODE = '0232'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COLT' ) THEN
        LMODEL_CODE = '0526'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COLT PICKUP' ) THEN
        LMODEL_CODE = '0722'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COLT VISTA' ) THEN
        LMODEL_CODE = '0502'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COMANCHE' ) THEN
        LMODEL_CODE = '4405'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COMET' ) THEN
        LMODEL_CODE = '0910'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COMMODORE ACCLAIM' ) THEN
        LMODEL_CODE = '8201'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONCORD' ) THEN
        LMODEL_CODE = '1111'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONCORDE' ) THEN
        LMODEL_CODE = '2112'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONCOURSE' ) THEN
        LMODEL_CODE = '1010'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONQUEST' ) THEN
        LMODEL_CODE = '0503'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONTINENTAL' ) THEN
        LMODEL_CODE = '1301'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONTOUR' ) THEN
        LMODEL_CODE = '0244'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONTOURED IMPACTOR' ) THEN
        LMODEL_CODE = '3209'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CONVENTIONAL' ) THEN
        LMODEL_CODE = '6601'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CORDIA' ) THEN
        LMODEL_CODE = '6203'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CORDOBA' ) THEN
        LMODEL_CODE = '2107'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COROLLA' ) THEN
        LMODEL_CODE = '1602'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CORONA' ) THEN
        LMODEL_CODE = '1604'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CORONET' ) THEN
        LMODEL_CODE = '0702'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CORRADO' ) THEN
        LMODEL_CODE = '0819'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CORSICA' ) THEN
        LMODEL_CODE = '0116'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CORVETTE' ) THEN
        LMODEL_CODE = '0109'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COSMO' ) THEN
        LMODEL_CODE = '1809'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COUGAR' ) THEN
        LMODEL_CODE = '0907'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COUPE' ) THEN
        LMODEL_CODE = '2011'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'COURIER' ) THEN
        LMODEL_CODE = '0235'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CRESSIDA' ) THEN
        LMODEL_CODE = '1608'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CRICKET' ) THEN
        LMODEL_CODE = '0520'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CROWN VICTORIA' ) THEN
        LMODEL_CODE = '0240'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CRV' ) THEN
        LMODEL_CODE = '2309'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CUSTOM' ) THEN
        LMODEL_CODE = '0614'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CUSTOMS' ) THEN
        LMODEL_CODE = '7901'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'CUTLASS' ) THEN
        LMODEL_CODE = '0605'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'D-150' ) THEN
        LMODEL_CODE = '0730'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DAKOTA' ) THEN
        LMODEL_CODE = '0723'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DART' ) THEN
        LMODEL_CODE = '0701'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DASHER' ) THEN
        LMODEL_CODE = '0804'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DAYTONA' ) THEN
        LMODEL_CODE = '0713'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DE VILLE' ) THEN
        LMODEL_CODE = '1008'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DEFORMABLE IMPACTOR' ) THEN
        LMODEL_CODE = '3201'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DEL SOL' ) THEN
        LMODEL_CODE = '2306'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DELTA 88' ) THEN
        LMODEL_CODE = '0602'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DIAMANTE' ) THEN
        LMODEL_CODE = '6211'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DIPLOMAT' ) THEN
        LMODEL_CODE = '0710'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DISCOVERY' ) THEN
        LMODEL_CODE = '8001'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DL' ) THEN
        LMODEL_CODE = '2012'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DURANGO' ) THEN
        LMODEL_CODE = '0742'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'DYNASTY' ) THEN
        LMODEL_CODE = '0732'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'E-10' ) THEN
        LMODEL_CODE = '7602'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'E100 VAN' ) THEN
        LMODEL_CODE = '0224'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'E150 VAN' ) THEN
        LMODEL_CODE = '0225'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'E420' ) THEN
        LMODEL_CODE = '2817'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ECLIPSE' ) THEN
        LMODEL_CODE = '6210'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EL CAMINO' ) THEN
        LMODEL_CODE = '0120'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELANTRA' ) THEN
        LMODEL_CODE = '3406'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELDORADO' ) THEN
        LMODEL_CODE = '1004'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELECTRA' ) THEN
        LMODEL_CODE = '0404'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELECTREK' ) THEN
        LMODEL_CODE = '5601'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELECTRICA' ) THEN
        LMODEL_CODE = '5403'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELECTRICA 007' ) THEN
        LMODEL_CODE = '5401'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ELITE' ) THEN
        LMODEL_CODE = '0219'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ENCORE' ) THEN
        LMODEL_CODE = '1708'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ES250' ) THEN
        LMODEL_CODE = '6401'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ES300' ) THEN
        LMODEL_CODE = '6402'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ESCORT' ) THEN
        LMODEL_CODE = '0227'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ESCORT ZX2' ) THEN
        LMODEL_CODE = '0246'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ESTATE' ) THEN
        LMODEL_CODE = '0409'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EUROVAN' ) THEN
        LMODEL_CODE = '0821'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EV RANGER' ) THEN
        LMODEL_CODE = '0247'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EV1' ) THEN
        LMODEL_CODE = '4008'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EVCORT' ) THEN
        LMODEL_CODE = '5802'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EXCEL GLS' ) THEN
        LMODEL_CODE = '3403'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EXP' ) THEN
        LMODEL_CODE = '0233'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EXPEDITION' ) THEN
        LMODEL_CODE = '0245'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'EXPLORER' ) THEN
        LMODEL_CODE = '0241'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'F10' ) THEN
        LMODEL_CODE = '1524'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'F150 PICKUP' ) THEN
        LMODEL_CODE = '0236'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'F250 PICKUP' ) THEN
        LMODEL_CODE = '0237'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'F350 PICKUP' ) THEN
        LMODEL_CODE = '0238'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FAIRMONT' ) THEN
        LMODEL_CODE = '0217'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FARGO PICKUP' ) THEN
        LMODEL_CODE = '0521'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FE' ) THEN
        LMODEL_CODE = '2604'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FESTIVA' ) THEN
        LMODEL_CODE = '0211'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FIERO' ) THEN
        LMODEL_CODE = '0303'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FIESTA' ) THEN
        LMODEL_CODE = '0214'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FIFTH AVENUE' ) THEN
        LMODEL_CODE = '2109'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FIREBIRD' ) THEN
        LMODEL_CODE = '0306'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FIRENZA' ) THEN
        LMODEL_CODE = '0615'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLAT IMPACTOR' ) THEN
        LMODEL_CODE = '3206'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FLEETWOOD' ) THEN
        LMODEL_CODE = '1003'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FORCE' ) THEN
        LMODEL_CODE = '7601'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FORESTER' ) THEN
        LMODEL_CODE = '2614'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FORWARD CONTROL' ) THEN
        LMODEL_CODE = '6602'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FOX' ) THEN
        LMODEL_CODE = '0803'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FRONTIER' ) THEN
        LMODEL_CODE = '1538'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FUEGO' ) THEN
        LMODEL_CODE = '1707'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FULL SIZE' ) THEN
        LMODEL_CODE = '0119'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'FURY' ) THEN
        LMODEL_CODE = '0512'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'G-10' ) THEN
        LMODEL_CODE = '0126'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'G-30' ) THEN
        LMODEL_CODE = '0129'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'G20' ) THEN
        LMODEL_CODE = '7201'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GALANT' ) THEN
        LMODEL_CODE = '6205'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GALAXIE' ) THEN
        LMODEL_CODE = '0231'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GF' ) THEN
        LMODEL_CODE = '2605'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GL' ) THEN
        LMODEL_CODE = '2602'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GLC' ) THEN
        LMODEL_CODE = '1805'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GLE' ) THEN
        LMODEL_CODE = '2013'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GLF' ) THEN
        LMODEL_CODE = '2608'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GLT' ) THEN
        LMODEL_CODE = '2014'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GOLF' ) THEN
        LMODEL_CODE = '0801'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GRANADA' ) THEN
        LMODEL_CODE = '0220'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GRAND AM' ) THEN
        LMODEL_CODE = '0314'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GRAND CARAVAN' ) THEN
        LMODEL_CODE = '0741'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GRAND CHEROKEE' ) THEN
        LMODEL_CODE = '4409'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GRAND PRIX' ) THEN
        LMODEL_CODE = '0305'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GREMLIN' ) THEN
        LMODEL_CODE = '1101'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GS300' ) THEN
        LMODEL_CODE = '6406'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'GV' ) THEN
        LMODEL_CODE = '2401'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HORIZON' ) THEN
        LMODEL_CODE = '0516'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'HORNET' ) THEN
        LMODEL_CODE = '1102'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'I-MARK' ) THEN
        LMODEL_CODE = '4201'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IMPALA' ) THEN
        LMODEL_CODE = '0106'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IMPERIAL' ) THEN
        LMODEL_CODE = '2106'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IMPREZA' ) THEN
        LMODEL_CODE = '2613'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'IMPULSE' ) THEN
        LMODEL_CODE = '4203'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'INTEGRA' ) THEN
        LMODEL_CODE = '7302'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'INTREPID' ) THEN
        LMODEL_CODE = '0734'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'INTRIGUE' ) THEN
        LMODEL_CODE = '0617'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'J2000' ) THEN
        LMODEL_CODE = '0316'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'J30' ) THEN
        LMODEL_CODE = '7204'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'JETTA' ) THEN
        LMODEL_CODE = '0814'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'JIMMY MPV' ) THEN
        LMODEL_CODE = '4001'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'JUSTY' ) THEN
        LMODEL_CODE = '2610'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'K10 PICKUP' ) THEN
        LMODEL_CODE = '0135'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'K1500 PICKUP' ) THEN
        LMODEL_CODE = '0143'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'K20 PICKUP' ) THEN
        LMODEL_CODE = '0136'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'K2500 PICKUP' ) THEN
        LMODEL_CODE = '0145'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'KING CAB PICKUP' ) THEN
        LMODEL_CODE = '1528'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LANCER' ) THEN
        LMODEL_CODE = '0712'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LAND CRUISER' ) THEN
        LMODEL_CODE = '1601'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LANOS' ) THEN
        LMODEL_CODE = '8402'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LASER' ) THEN
        LMODEL_CODE = '0234'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LE BARON' ) THEN
        LMODEL_CODE = '2108'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LE CAR' ) THEN
        LMODEL_CODE = '1701'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEGACY' ) THEN
        LMODEL_CODE = '2611'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEGEND' ) THEN
        LMODEL_CODE = '7301'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEMANS' ) THEN
        LMODEL_CODE = '0312'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LEOPARD' ) THEN
        LMODEL_CODE = '5901'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LESABRE' ) THEN
        LMODEL_CODE = '0402'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LN7' ) THEN
        LMODEL_CODE = '0916'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOAD CELL IMPACTOR' ) THEN
        LMODEL_CODE = '3203'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LOYALE' ) THEN
        LMODEL_CODE = '0612'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LS400' ) THEN
        LMODEL_CODE = '6405'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LTD' ) THEN
        LMODEL_CODE = '0207'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LUMINA' ) THEN
        LMODEL_CODE = '0130'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LUV' ) THEN
        LMODEL_CODE = '0122'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'LYNX' ) THEN
        LMODEL_CODE = '0915'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'M30' ) THEN
        LMODEL_CODE = '7202'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MAGNUM' ) THEN
        LMODEL_CODE = '0715'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MALIBU' ) THEN
        LMODEL_CODE = '0114'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MARK' ) THEN
        LMODEL_CODE = '1302'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MARK II' ) THEN
        LMODEL_CODE = '1605'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MARQUIS' ) THEN
        LMODEL_CODE = '0909'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MATADOR' ) THEN
        LMODEL_CODE = '1109'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MAVERICK' ) THEN
        LMODEL_CODE = '0202'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MAXIMA' ) THEN
        LMODEL_CODE = '1525'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MEDALLION' ) THEN
        LMODEL_CODE = '1711'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MERKUR' ) THEN
        LMODEL_CODE = '0203'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'METRO' ) THEN
        LMODEL_CODE = '0144'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MGB' ) THEN
        LMODEL_CODE = '2503'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MIATA' ) THEN
        LMODEL_CODE = '1808'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MIDGET' ) THEN
        LMODEL_CODE = '2501'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MIGHTY MAX' ) THEN
        LMODEL_CODE = '6213'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MIGHTY MITE' ) THEN
        LMODEL_CODE = '6702'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MILLENIA' ) THEN
        LMODEL_CODE = '1817'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MINICARS RSV' ) THEN
        LMODEL_CODE = '3204'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MINIRAM VAN' ) THEN
        LMODEL_CODE = '0716'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MIRADA' ) THEN
        LMODEL_CODE = '0718'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MIRAGE' ) THEN
        LMODEL_CODE = '6207'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MIZER' ) THEN
        LMODEL_CODE = '1811'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ML320' ) THEN
        LMODEL_CODE = '2818'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MONACO' ) THEN
        LMODEL_CODE = '0704'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MONARCH' ) THEN
        LMODEL_CODE = '0912'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MONTE CARLO' ) THEN
        LMODEL_CODE = '0103'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MONTEGO' ) THEN
        LMODEL_CODE = '0901'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MONTERO' ) THEN
        LMODEL_CODE = '6202'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MONZA' ) THEN
        LMODEL_CODE = '0121'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MOTOR HOME' ) THEN
        LMODEL_CODE = '3501'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MPV' ) THEN
        LMODEL_CODE = '1806'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MR2' ) THEN
        LMODEL_CODE = '1614'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MUSTANG' ) THEN
        LMODEL_CODE = '0205'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MX3' ) THEN
        LMODEL_CODE = '1816'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MX5' ) THEN
        LMODEL_CODE = '1818'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MX6' ) THEN
        LMODEL_CODE = '1812'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'MYSTIQUE' ) THEN
        LMODEL_CODE = '0918'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NEON' ) THEN
        LMODEL_CODE = '0527'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NEW YORKER' ) THEN
        LMODEL_CODE = '2104'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NEWPORT' ) THEN
        LMODEL_CODE = '2101'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NOVA' ) THEN
        LMODEL_CODE = '0110'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NUBIRA' ) THEN
        LMODEL_CODE = '8401'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'NX' ) THEN
        LMODEL_CODE = '1537'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'O24' ) THEN
        LMODEL_CODE = '0724'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ODYSSEY' ) THEN
        LMODEL_CODE = '2307'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OMEGA' ) THEN
        LMODEL_CODE = '0610'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OMNI' ) THEN
        LMODEL_CODE = '0709'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OPEL' ) THEN
        LMODEL_CODE = '0416'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'OTHER' ) THEN
        LMODEL_CODE = '0199'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PACER' ) THEN
        LMODEL_CODE = '1112'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PARISIENNE' ) THEN
        LMODEL_CODE = '0301'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PARK AVENUE' ) THEN
        LMODEL_CODE = '0407'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PASEO' ) THEN
        LMODEL_CODE = '1617'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PASSAT' ) THEN
        LMODEL_CODE = '0818'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PASSPORT' ) THEN
        LMODEL_CODE = '2308'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PATHFINDER' ) THEN
        LMODEL_CODE = '1534'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PHOENIX' ) THEN
        LMODEL_CODE = '0308'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PICKUP' ) THEN
        LMODEL_CODE = '0137'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PINTO' ) THEN
        LMODEL_CODE = '0213'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PLASCORE' ) THEN
        LMODEL_CODE = '8301'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'POLO' ) THEN
        LMODEL_CODE = '0805'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PONY EXCEL' ) THEN
        LMODEL_CODE = '3401'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PRECIS' ) THEN
        LMODEL_CODE = '6208'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PRELUDE' ) THEN
        LMODEL_CODE = '2305'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PREMIER' ) THEN
        LMODEL_CODE = '7002'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PREVIA' ) THEN
        LMODEL_CODE = '1615'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PRIZM' ) THEN
        LMODEL_CODE = '6302'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PROBE' ) THEN
        LMODEL_CODE = '0212'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PULSAR' ) THEN
        LMODEL_CODE = '1501'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'PUP' ) THEN
        LMODEL_CODE = '4202'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'Q45' ) THEN
        LMODEL_CODE = '7203'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'QUANTUM' ) THEN
        LMODEL_CODE = '0817'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'QUEST' ) THEN
        LMODEL_CODE = '1536'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RABBIT' ) THEN
        LMODEL_CODE = '0809'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RAM' ) THEN
        LMODEL_CODE = '0744'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RAM 150' ) THEN
        LMODEL_CODE = '0736'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RAM 250 VAN' ) THEN
        LMODEL_CODE = '0737'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RAM 50' ) THEN
        LMODEL_CODE = '0728'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RAM WAGON VAN' ) THEN
        LMODEL_CODE = '0733'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RAM1500' ) THEN
        LMODEL_CODE = '0743'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RAMCHARGER MPV' ) THEN
        LMODEL_CODE = '0726'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RAMPAGE' ) THEN
        LMODEL_CODE = '0703'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RANCHERO' ) THEN
        LMODEL_CODE = '0215'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RANGER' ) THEN
        LMODEL_CODE = '0210'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RAV4' ) THEN
        LMODEL_CODE = '1621'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'REDDI BUS' ) THEN
        LMODEL_CODE = '6501'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'REGAL' ) THEN
        LMODEL_CODE = '0417'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RELIANT' ) THEN
        LMODEL_CODE = '0518'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RIVIERA' ) THEN
        LMODEL_CODE = '0405'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROADMASTER' ) THEN
        LMODEL_CODE = '0408'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RODEO' ) THEN
        LMODEL_CODE = '4208'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROLLOVER CART' ) THEN
        LMODEL_CODE = '3210'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ROYALE' ) THEN
        LMODEL_CODE = '0606'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'RX' ) THEN
        LMODEL_CODE = '1804'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'S-10' ) THEN
        LMODEL_CODE = '0124'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'S10 BLAZER' ) THEN
        LMODEL_CODE = '0139'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'S15 PICKUP' ) THEN
        LMODEL_CODE = '4007'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'S70' ) THEN
        LMODEL_CODE = '2021'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'S80' ) THEN
        LMODEL_CODE = '2022'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SABLE' ) THEN
        LMODEL_CODE = '0903'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SAMURAI' ) THEN
        LMODEL_CODE = '3301'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SAPPORO' ) THEN
        LMODEL_CODE = '0510'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SC2' ) THEN
        LMODEL_CODE = '6903'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SC300' ) THEN
        LMODEL_CODE = '6403'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SC400' ) THEN
        LMODEL_CODE = '6404'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SCAMP' ) THEN
        LMODEL_CODE = '0504'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SCIROCCO' ) THEN
        LMODEL_CODE = '0810'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SCOUPE' ) THEN
        LMODEL_CODE = '3405'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SCOUT MPV' ) THEN
        LMODEL_CODE = '4101'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEBRING CONVERTIBLE' ) THEN
        LMODEL_CODE = '2114'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SENTRA' ) THEN
        LMODEL_CODE = '1502'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEPHIA' ) THEN
        LMODEL_CODE = '7701'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SEVILLE' ) THEN
        LMODEL_CODE = '1002'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SHADOW' ) THEN
        LMODEL_CODE = '0721'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIDEKICK' ) THEN
        LMODEL_CODE = '3302'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SIENNA' ) THEN
        LMODEL_CODE = '1622'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SKYHAWK' ) THEN
        LMODEL_CODE = '0411'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SKYLARK' ) THEN
        LMODEL_CODE = '0401'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SL1' ) THEN
        LMODEL_CODE = '6901'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SL2' ) THEN
        LMODEL_CODE = '6902'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SLED' ) THEN
        LMODEL_CODE = '3211'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SOMERSET' ) THEN
        LMODEL_CODE = '0403'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SONATA' ) THEN
        LMODEL_CODE = '3404'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPACECAB' ) THEN
        LMODEL_CODE = '4205'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPECTRUM' ) THEN
        LMODEL_CODE = '0104'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPIDER' ) THEN
        LMODEL_CODE = '1905'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPIRIT' ) THEN
        LMODEL_CODE = '0725'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPITFIRE' ) THEN
        LMODEL_CODE = '3101'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPORTAGE' ) THEN
        LMODEL_CODE = '7702'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPORTSMAN' ) THEN
        LMODEL_CODE = '0720'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPORTSWAGON' ) THEN
        LMODEL_CODE = '1710'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPORTVAN' ) THEN
        LMODEL_CODE = '0127'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPRINT' ) THEN
        LMODEL_CODE = '0105'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SPRINT MPV' ) THEN
        LMODEL_CODE = '4003'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SS MPV' ) THEN
        LMODEL_CODE = '4102'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ST. REGIS' ) THEN
        LMODEL_CODE = '0717'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STANZA' ) THEN
        LMODEL_CODE = '1527'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STARFIRE' ) THEN
        LMODEL_CODE = '0611'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STARION' ) THEN
        LMODEL_CODE = '6206'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STARLET' ) THEN
        LMODEL_CODE = '1612'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEALTH' ) THEN
        LMODEL_CODE = '0735'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STEALTH SPORT VAN' ) THEN
        LMODEL_CODE = '7401'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STELLAR' ) THEN
        LMODEL_CODE = '3402'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STORM' ) THEN
        LMODEL_CODE = '6303'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STRADA' ) THEN
        LMODEL_CODE = '1908'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STRATUS' ) THEN
        LMODEL_CODE = '0740'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'STYLUS' ) THEN
        LMODEL_CODE = '4207'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUBURBAN' ) THEN
        LMODEL_CODE = '0111'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUMMIT' ) THEN
        LMODEL_CODE = '7003'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUNBIRD' ) THEN
        LMODEL_CODE = '0307'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUNDANCE' ) THEN
        LMODEL_CODE = '0506'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUNFIRE' ) THEN
        LMODEL_CODE = '0320'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUPER BANTAM' ) THEN
        LMODEL_CODE = '7501'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SUPRA' ) THEN
        LMODEL_CODE = '1607'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'SWIFT' ) THEN
        LMODEL_CODE = '3303'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'T100' ) THEN
        LMODEL_CODE = '1618'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'T1000' ) THEN
        LMODEL_CODE = '0317'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TACOMA' ) THEN
        LMODEL_CODE = '1620'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TAHOE' ) THEN
        LMODEL_CODE = '0141'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TALON' ) THEN
        LMODEL_CODE = '7005'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TAURUS' ) THEN
        LMODEL_CODE = '0208'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TAXICAB' ) THEN
        LMODEL_CODE = '3601'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TC3' ) THEN
        LMODEL_CODE = '0522'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TEMPO' ) THEN
        LMODEL_CODE = '0201'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TERCEL' ) THEN
        LMODEL_CODE = '1610'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TERRA MPV' ) THEN
        LMODEL_CODE = '4103'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'THE THING' ) THEN
        LMODEL_CODE = '0808'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'THUNDERBIRD' ) THEN
        LMODEL_CODE = '0209'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TIBURON' ) THEN
        LMODEL_CODE = '3408'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TOPAZ' ) THEN
        LMODEL_CODE = '0905'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TORINO' ) THEN
        LMODEL_CODE = '0204'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TORONADO' ) THEN
        LMODEL_CODE = '0604'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TOWN AND COUNTRY' ) THEN
        LMODEL_CODE = '2105'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TOWN CAR' ) THEN
        LMODEL_CODE = '1306'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TOWNSIDE MPV' ) THEN
        LMODEL_CODE = '4404'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TR7' ) THEN
        LMODEL_CODE = '3102'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRACER' ) THEN
        LMODEL_CODE = '0902'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRACKER' ) THEN
        LMODEL_CODE = '6304'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRAILDUSTER MPV' ) THEN
        LMODEL_CODE = '0523'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRANS SPORT' ) THEN
        LMODEL_CODE = '0319'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRAVELER MPV' ) THEN
        LMODEL_CODE = '4104'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TREDIA' ) THEN
        LMODEL_CODE = '6204'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TREKKER' ) THEN
        LMODEL_CODE = '6001'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TROOPER II' ) THEN
        LMODEL_CODE = '4204'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TROPICA' ) THEN
        LMODEL_CODE = '8101'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'TRUCK SIMULATOR' ) THEN
        LMODEL_CODE = '3208'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VALIANT' ) THEN
        LMODEL_CODE = '0501'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VAN' ) THEN
        LMODEL_CODE = '0223'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VANAGON' ) THEN
        LMODEL_CODE = '0815'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VANDURA' ) THEN
        LMODEL_CODE = '4005'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VEGA' ) THEN
        LMODEL_CODE = '0112'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VENTURA' ) THEN
        LMODEL_CODE = '0311'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VENTURE' ) THEN
        LMODEL_CODE = '0142'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VERSAILLES' ) THEN
        LMODEL_CODE = '1305'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VIGOR' ) THEN
        LMODEL_CODE = '7303'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VILLAGER' ) THEN
        LMODEL_CODE = '0917'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VISION' ) THEN
        LMODEL_CODE = '7006'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VITARA' ) THEN
        LMODEL_CODE = '3304'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VJ' ) THEN
        LMODEL_CODE = '4406'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VOLARE' ) THEN
        LMODEL_CODE = '0514'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'VOYAGER VAN' ) THEN
        LMODEL_CODE = '0524'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WAGON' ) THEN
        LMODEL_CODE = '2609'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WINDSTAR' ) THEN
        LMODEL_CODE = '0242'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'WRANGLER' ) THEN
        LMODEL_CODE = '4407'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'X19' ) THEN
        LMODEL_CODE = '1906'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'XT' ) THEN
        LMODEL_CODE = '2601'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ZEPHYR' ) THEN
        LMODEL_CODE = '0908'
        RETURN
      ELSE IF ( DESC(:DESC_L) .EQ. 'ZEV' ) THEN
        LMODEL_CODE = '7801'
        RETURN
      END IF

      RETURN

      END

! end of LMODEL_CODE()
