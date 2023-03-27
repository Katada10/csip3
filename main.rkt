#lang racket

(define (readXYZ fileIn)
 (let ((sL (map (lambda s (string-split (car s)))
 (cdr (file->lines fileIn)))))
 (map (lambda (L)
 (map (lambda (s)
 (if (eqv? (string->number s) #f)
 s
(string->number s))) L)) sL)))


;Returns two vectors which are P2-P1 and P3-P1
(define (getVectors P1 P2 P3) (
                              ;Return a list composed of 2 vectors
           list (list (- (car P2) (car P1) ) (- (cadr P2) (cadr P1) ) (- (caddr P2) (caddr P1) ) ) ;Vector 1 = P2 - P1
                 (list (- (car P3) (car P1) ) (- (cadr P3) (cadr P1) ) (- (caddr P3) (caddr P1) ) ) ;Vector 2 = P3 - P1
  ))


;Returns the cross product of two vectors V1 and V2
(define (cross V1 V2) (

;The cross will be (b1c2-c1b2)i + (c1a2-a1c2)j + (a1b2-b1a2)k

                      list (- (*  (cadr V1)  (caddr V2) ) (* (caddr V1) (cadr V2) ));(b1c2-c1b2)i

                           (- (* (caddr V1) (car V2) ) (* (car V1) (caddr V2) ) )             ;(c1a2-a1c2)j

                           (- (* (car V1) (cadr V2) ) (* (cadr V1) (car V2) ) )               ;(a1b2-b1a2)k
))


;Calculates the "D" element of a plane equation by taking V as (A,B,C) and P as one of the points used to generate the plane equation
(define (calculateD V P) (
                          
            ; -((A * p1.getX()) + (B * p1.getY()) + (C * p1.getZ()))
             - 0 (+ (* (car V) (car P)) (* (cadr V) (cadr P)) (* (caddr V) (caddr P)) )
                          

             ))

;Returns a list of (A, B, C, D) representing a plane equation Ax + By + Cz + D = 0 from the 3 points
(define (plane P1 P2 P3) (

   let ((crossProduct (cross ( car(getVectors P1 P2 P3)) (cadr(getVectors P1 P2 P3)) ))) (

    ; -((A * p1.getX()) + (B * p1.getY()) + (C * p1.getZ()))            

      list (car crossProduct) (cadr crossProduct) (caddr crossProduct) (calculateD crossProduct P1)                                                                                    
                                                                                          
    )
                          
))

;Returns the square of a number
(define (square x) (* x x))

;Returns the normal vector of the plane
(define (planeVecMag plane) (


                             sqrt (+ (square (car plane)) (square (cadr plane)) (square (caddr plane)))


                             ))

;Returns absolute value of a number
(define (abs x) (

                 if (< x 0) (- x) x

                 ))



;Returns the dot product of a point with a plane
(define (pointDotPlane plane point) (

            abs (+ (* (car plane) (car point)) (* (cadr plane) (cadr point)) (* (caddr plane) (caddr point)) (cadddr plane))

                                     ))

;Caclulates the distance between a point and a plane
(define (distance Plane pt) (

/ (pointDotPlane Plane pt) (planeVecMag Plane)
                             ))

;Returns a list containing the distances from each point in pts to the plane
(define (getDistanceFromCloud plane pts) (

map (lambda (pt) (distance plane pt))  pts 

          ))


;Counts the number of points which are at a distance of <= eps from the plane
(define (support Plane Pts eps) (

let ((x 0) ) (

              map (lambda (dist) (

                          if (<= dist eps) (+ x 1) (+ x 0)

                             
                      ) )(getDistanceFromCloud Plane Pts)   


              )
                            
  ))


(define (planeRANSAC filename confidence percentage eps) (

    let ((Ps (readXYZ filename) ))
          
(

list (list-ref Ps (random (length Ps)))  (list-ref Ps (random (length Ps)))   (list-ref Ps (random (length Ps))) 
 
   ) 

                                                          
))