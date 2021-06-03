(in-package :cl-keyframes)

;;; There are two ways to use cl-keyframes. One is to create an alist of times to keyframes, then
;;; call timeline-alist-value on said alist. This requires traversing the alist each time you want
;;; to fetch keyframe values, which may not be performant with many keyframes. The second option is
;;; to use the timeline class, which lets you update the time incrementally (though only in the
;;; positive direction), and should be fast for small time increments.

(defclass timeline ()
  ((time :reader timeline-time)
   keyframes) ; car is the "previous" keyframe, or nil if we are before all keyframes
  )

(defclass keyframe ()
  ((time :accessor keyframe-time :initarg :time)
   (value :accessor keyframe-value :initarg :value)
   (in-easing :accessor keyframe-in-easing :initarg :in-easing)
   (out-easing :accessor keyframe-out-easing :initarg :out-easing)))

(defgeneric linearize (prev-out-easing next-in-easing progress)
  (:documentation "Given the out-easing of the last keyframe, the in-easing of the next keyframe, and
  how far we are between the frames (between 0 and 1), return a value between 0 and 1 of how we
  should interpolate linearly between the two keyframes values"))

(defgeneric interpolate (prev next progress)
  (:documentation "Interpolate linearly between prev and next using progress, between 0 and 1."))

(defmethod (setf timeline-time) (time (timeline timeline))
  (declare (number time))
  (with-slots ((timeline-time time) keyframes) timeline
    (assert (>= time timeline-time))
    (setf timeline-time time)
    (loop while (and (cdr keyframes) (>= time (keyframe-time (cadr keyframes))))
          do (setf keyframes (cdr keyframes)))))

(defmethod timeline-increment ((timeline timeline) delta-t)
  (setf (timeline-time timeline) (+ delta-t (timeline-time timeline))))

(defun time-value (prev-keyframe next-keyframe time)
  (cond
    ;; there is always at least one keyframe, so the cadr won't fail
    ((null prev-keyframe) (keyframe-value next-keyframe))
    ((null next-keyframe) (keyframe-value prev-keyframe))
    ((= (keyframe-time prev-keyframe) time) (keyframe-value prev-keyframe))
    (t
     (interpolate
      (keyframe-value prev-keyframe) (keyframe-value next-keyframe)
      (linearize (keyframe-out-easing prev-keyframe) (keyframe-in-easing next-keyframe)
                 (/ (- time (keyframe-time prev-keyframe))
                    (- (keyframe-time next-keyframe) (keyframe-time prev-keyframe))))))))

(defmethod timeline-value ((timeline timeline))
  (with-slots (time keyframes) timeline
    (time-value (car keyframes) (cadr keyframes) time)))

(defmethod initialize-instance :after ((timeline timeline) &key keyframes time)
  (with-slots ((timeline-time time) (timeline-keyframes keyframes)) timeline
    (let ((sorted-keyframes (sort (copy-list keyframes) #'< :key #'keyframe-time)))
      (setf timeline-time (1- (keyframe-time (car sorted-keyframes))))
      (setf timeline-keyframes (cons nil sorted-keyframes))
      (setf (timeline-time timeline) (or time (keyframe-time (car sorted-keyframes)))))))

(defmethod interpolate ((prev number) (next number) progress)
  (+ (* (- 1 progress) prev) (* progress next)))

;; (defgeneric linearize-one-side (easing progress)
;;   :documentation "Progress=0 means the halfway point, progress=1 is all the way")

;; (defmethod linearize (prev-out-easing next-in-easing progress)
;;   "Uses one sided easing if no specific two-sided method is defined"
;;   (if (>= progress 1/2)
;;       (+ 1/2 (* 2 (linearize-one-side next-in-easing (* 2 (- progress 1/2)))))
;;       (* 2 (linearize-one-side prev-out-easing (* 2 progress)))))

;; (defmethod linearize-one-side ((easing (eql 'linear)) progress)
;;   progress)

;; (defmethod linearize-one-side ((easing (eql 'sinusoidal)) progress)
;;   )

(defmacro defeasing (name default-other-easing symmetric-p progress-var
                     &rest bodyforms)
  (declare (symbol name default-other-easing progress-var)
           (boolean symmetric-p))
  (let ((prev-out-easing-sym (gensym "PREV-OUT-EASING"))
        (next-in-easing-sym (gensym "NEXT-IN-EASING"))
        (bodyforms (cons `(nil
                           (linearize ',name ',default-other-easing ,progress-var))
                         bodyforms)))
    `(progn
       ,@(loop for (other-easing . body) in bodyforms
               collect `(defmethod linearize ((,prev-out-easing-sym (eql ',name))
                                              (,next-in-easing-sym (eql ',other-easing))
                                              ,progress-var)
                          ,@body)
               when (and symmetric-p (not (eql name other-easing)))
                 collect `(defmethod linearize ((,prev-out-easing-sym (eql ',other-easing))
                                                (,next-in-easing-sym (eql ',name))
                                                ,progress-var)
                            (1- (linearize ,next-in-easing-sym ,prev-out-easing-sym
                                           (1- ,progress-var))))))))

(defeasing sine linear t progress
  (linear
   (1+ (sin (+ (/ pi -2) (* progress (/ pi 2)))))))

(defeasing linear linear t progress
  (linear
   progress))

(defmethod linearize ((prev-out-easing (eql 'step)) next-in-easing progress)
  1)

(defmethod linearize (prev-out-easing (next-in-easing (eql 'step)) progress)
  0)

(defmethod linearize ((prev-out-easing (eql 'hold)) next-in-easing progress)
  0)

(defmethod linearize (prev-out-easing (next-in-easing (eql 'hold)) progress)
  1)

