PROGRAM MAIN
IMPLICIT NONE

INTEGER, ALLOCATABLE :: ARR(:)
INTEGER :: FNO = 12, ERR = 0, LINES = 0, OUT = 0, ITEMP

OPEN(FNO, FILE="input.txt")

DO WHILE (ERR .EQ. 0)
    LINES = LINES + 1
    READ(FNO, '(I10)', IOSTAT=ERR, BLANK="ZERO") ITEMP
END DO

LINES = LINES - 1
ALLOCATE(ARR(LINES))
REWIND(FNO)

READ(FNO, '(I10)', BLANK="ZERO") ARR

CALL PART1(ARR, LINES, OUT)
PRINT '(I0)', OUT

CALL PART2(ARR, LINES, OUT)
PRINT '(I0)', OUT

CLOSE(FNO)
END PROGRAM

SUBROUTINE PART1(ARR, LINES, OUTPUT)
IMPLICIT NONE
INTEGER, INTENT(IN) :: LINES
INTEGER, DIMENSION(LINES), INTENT(IN) :: ARR
INTEGER, INTENT(OUT) :: OUTPUT

INTEGER :: ERR = 0, I, LNUM = 0, BNUM = 0, OUT = 0

DO I=1,LINES
    IF (ARR(I) .NE. 0) THEN
        LNUM = LNUM + ARR(I)
    ELSE
        IF (LNUM .GT. BNUM) THEN
            BNUM = LNUM
        END IF
        LNUM = 0
    END IF
END DO

OUTPUT = BNUM
END SUBROUTINE
        
SUBROUTINE PART2(ARR, LINES, OUTPUT)
IMPLICIT NONE
INTEGER, INTENT(IN) :: LINES
INTEGER, DIMENSION(LINES), INTENT(IN) :: ARR
INTEGER, INTENT(OUT) :: OUTPUT

INTEGER :: ERR = 0, I, LNUM = 0, OUT = 0
INTEGER, DIMENSION(3) :: TOPS

TOPS(3) = 0
TOPS(2) = 0
TOPS(1) = 0

DO I=1,LINES
    IF (ARR(I) .NE. 0) THEN
        LNUM = LNUM + ARR(I)
    ELSE
        IF (LNUM .GT. TOPS(3)) THEN
            TOPS(3) = LNUM
            IF (TOPS(3) .GT. TOPS(2)) THEN
                TOPS(3) = TOPS(2)
                TOPS(2) = LNUM
                IF (TOPS(2) .GT. TOPS(1)) THEN
                    TOPS(2) = TOPS(1)
                    TOPS(1) = LNUM
                END IF
            END IF
        END IF
        LNUM = 0
    END IF
END DO

OUTPUT = TOPS(1) + TOPS(2) + TOPS(3)
END SUBROUTINE
