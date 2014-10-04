      DOUBLE PRECISION FUNCTION LINTER( XTEST, X1, Y1, X2, Y2)
C function to linearly interpolate between two points
C arguements (xtest,x1,y1,x2,y2)
C function returns y value for xtest between
C two points (x1,y1) and (x2,y2)
      DOUBLE PRECISION XTEST, X1, Y1, X2, Y2

C linear coeffs
      DOUBLE PRECISION A, B

C end of decs

      A = (Y1-Y2)/(X1-X2)
      B = (Y2*X1-Y1*X2)/(X1-X2)
      LINTER = A*XTEST + B
      RETURN
      END
