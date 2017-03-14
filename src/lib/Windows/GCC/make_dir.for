      INTEGER FUNCTION MAKE_DIR( DIR_OUT )

!***********************************************************************
!* Makes a Specified Directory
!*
!* Language: Fortran
!*
!* Platform: Windows/GCC
!*
!* Compiler: GFortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIR_OUT ! Output directory


      ! Variables ______________________________________________________

      INTEGER I1, IOS, IPOS, ISF, ISL, LD


      ! Functions ______________________________________________________

      INTEGER mkdir


      ! Create the directory
      LD = LEN_TRIM( DIR_OUT )
      IF ( DIR_OUT(1:1) .EQ. '/' ) THEN
        I1 = 2
      ELSE
        I1 = 1
      END IF
      IOS = 0
      DO WHILE ( ( IOS .EQ. 0 ) .AND. ( I1 .LT. LD ) ) ! Make next dir
        ISL = INDEX( DIR_OUT(I1:), '/' )
        IF ( ISL .EQ. 0 ) ISL = LD - I1 + 2
        ISF = INDEX( DIR_OUT(I1:), '/' )
        IF ( ISF .GT. 0 ) ISL = MIN( ISL, ISF )
        IPOS = I1 + ISL - 2
        IOS = mkdir( DIR_OUT(:IPOS) )
        I1 = IPOS + 2
      END DO
      MAKE_DIR = IOS

      RETURN
      END
