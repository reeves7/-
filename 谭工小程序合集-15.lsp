;;�û����Լ��޸Ľ���
(defun c:ggr ()
  (mai_make_panel
    (list
      (list "lispС����1"
      (list "       ���ඨ�������� CMC        " "CMC") ;(list  ��ť��ǩ  ���е�����)
      (list "       ���㻭Բ hyq       " "hyq")
      (list "       �Զ����������� lgg7     " "lgg7")
      (list "       ������ϵ���� 3tj          " "3tj")
      (list "        ��̬�ӳ� saa       " "saa")
      (list "       ���ƾ��ζԽ��� jxdx       " "jxdx")
      (list "           ����䶨������ twm      " "twm")
      (list "           �߶εȷ� XBR          " "XBR")
      (list "           ˫�»��� under2         " "under2")
      ) ;_ lispС����1
             (list "lispС����2"
      (list "�����������ƾ��ζԽ���" "draw_rectangle_diagonals") ;(list  ��ť��ǩ  ���е�����)
	(list "����ѡ�� qss" "qss") ;(list  ��ť��ǩ  ���е�����)
	(list "      ����ľ����С�㣩ldmf   " "ldmf") ;(list  ��ť��ǩ  ���е�����)
	(list "      ���ࡢ���ľ����С�㣩bdm   " "bdm") ;(list  ��ť��ǩ  ���е�����)
	(list "      ���ࡢ���ľ����С�㣩���װ�bdm3   " "bdm3") ;(list  ��ť��ǩ  ���е�����)
	(list "      ���ּӿ� bt  " "bt") ;(list  ��ť��ǩ  ���е�����)
	(list "      �ؽ����ͼ���߽� TCBJ  " "TCBJ") ;(list  ��ť��ǩ  ���е�����)
	(list "      ��ȫ��ͼ�� QBTC  " "QBTC ") ;(list  ��ť��ǩ  ���е�����)
	(list "      �رն������ڲ� GBTC  " "GBTC ") ;(list  ��ť��ǩ  ���е�����)
       ) ;_ ����listС����2
        (list "lispС����3"
      (list "ֻ��ʾ��ѡ�������ڲ�  ZXTC" "ZXTC") ;(list  ��ť��ǩ  ���е�����)
	       ) ;_ ����listС����3
    ) ;_ ����list
  ) ;_ ����mai_make_panel
  (princ)
) ;_ ����defun
;�����Ƕ��ඨ�����Զ����ظ���
(defun C:CMC ()
(setq A nil)
(setq OM (getvar "OSMODE"))
(setvar "OSMODE" 33)
(setq PNT1 (getpoint "\n�������:  "))
(setq PNT2 (getpoint "\n�����յ�: " PNT1))(terpri)
(setq ANG (angle PNT1 PNT2))
(setq JJ (getreal "\n ��������: "))
(setq SL (getreal "\n ��������: "))
(setvar "OSMODE" 0)
(setq A (ssget))
(setq INCR 0)
(setq TMS (FIX (+ 0.00001 SL)))
(repeat TMS
(setq INCR (+ INCR JJ))
(setq NEWPT (polar PNT1 ANG INCR))
(command "copy" A "" PNT1 NEWPT)
)
(setvar "OSMODE" OM)
(setq A nil)
)
;�����ǽ��㻭Բ
(vl-load-com)
(defun c:hyq(/ *acad *doc *error* acad acaddocument mspace odlst ptlist ss ssinters)
	(defun *error*(msg)
		(mapcar 'setvar '("cmdecho" "osmode" "peditaccept") odlst)
		(vlax-invoke-method *doc 'EndUndoMark)
		(princ msg)
	)
	(command "ucs" "w")
  (setq odlst (mapcar 'getvar '("cmdecho" "osmode" "peditaccept" "clayer")))
  (mapcar 'setvar '("cmdecho" "osmode" "peditaccept" ) '(0 0 1))
	(setq *acad  (vlax-get-acad-object) *doc   (vla-get-ActiveDocument *acad))
	(vlax-invoke-method *doc 'StartUndoMark)
	(progn 
		(defun ssinters        (ss / i num obj1 obj2 j interpts ptlist)
			(setq        i   0
				num (sslength ss)
			)
			(while (< i (1- num))
				(setq obj1 (ssname ss i)
					obj1 (vlax-ename->vla-object obj1)
					j    (1+ i)
				)
				(while (< j num)
					(setq obj2     (ssname ss j)
						obj2     (vlax-ename->vla-object obj2)
						interpts (vla-intersectwith
											 obj1
											 obj2
											 0
										 )
						interpts (vlax-variant-value interpts)
					)
					(if (> (vlax-safearray-get-u-bound interpts 1) 0)
						(progn
							(setq        interpts
								(vlax-safearray->list interpts)
							)
							(while (> (length interpts) 0)
								(setq ptlist (cons (list (car interpts)
																		 (cadr interpts)
																		 (caddr interpts)
																	 )
															 ptlist
														 )
								)
								(setq interpts (cdddr interpts))
							)
						)
					)
					(setq j (1+ j))
				)
				(setq i (1+ i))
			)
			ptlist
		)
	)
  (princ "\n��ѡ�����ߡ��������ߡ�ֱ�ߡ�Բ��Բ������Բ��")
  (if (not (setq ss (ssget '((0 . "*LINE,ARC,CIRCLE,ELLIPSE")))))
    (progn (princ "\n��ʾ��δѡȡͼԪ�������˳���\n") (exit))
  )
	(setq bj(getreal "\n������Բ�뾶<48>"))
	(if(not bj)(setq bj 48))
  (setq acad (vlax-get-acad-object))
  (setq acaddocument (vla-get-activedocument acad))
  (setq mspace (vla-get-modelspace acaddocument))
  (setq ptlist (ssinters ss))
	(foreach a ptlist
		(entmake (list (cons 0 "CIRCLE")(cons 10 a)(cons 40 bj) (cons 62 4)))
	)
  (mapcar 'setvar '("cmdecho" "osmode" "peditaccept" "clayer") odlst)
	(vlax-invoke-method *doc 'EndUndoMark)
	(princ)
)
;�������Զ�����������
(defun c:lgg7 ()
(setvar "cmdecho" 0)	
(setq en (entsel "ѡ�����ĳߴ��ע:"))	
(setq en_data (entget (car en)))	
(setq liangguige (assoc 1 en_data))
(setq liangchicun(cdr liangguige))
  (setq pos (vl-string-search "x" liangchicun));��300x800�����ﷵ�ؽ������3������ʼ����0��1��2��3����3���ҵ�x
  (setq liangkan(substr liangchicun 1 pos ));��������Ŀ�ȣ���300x800����ӵ�1λ����ʼ��ȡ�ַ�������ȡ�ĳ��ȵ���ֵ�պõ�������searchx����x����λ�õĴ�������
  (setq lianggao(substr liangchicun (+ pos 2) ));��������ĸ߶ȣ���300x800����ӵ�5λ����ʼ��ȡ�ַ�����3+2=5.
  (setq lianggao1(atof lianggao))
  (setq liangkan1(atof liangkan))
  (setq pa (getpoint "�����������Ͻǵ�: "))
  (setq pb (polar pa (* pi 1.5) lianggao1))
  (setq pc (polar pb 0 liangkan1))
  (setq pd (polar pa 0 liangkan1))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 pa)(cons 10 pb)(cons 10 pc)(cons 10 pd) (cons 62 4) (cons 70 1)))
;(command "rectangle" pa  pc )
  ;(command "pline" pa pb pc pd "c")
(prin1)
)
(prompt "*************** << C:CHGRAD >> *****************")
(prin1)

;�����ǻ�����ϵ����
(defun c:3tj (/ os pt1 pt2 pt3 pt4 ag0 ag len ent ct in k)
  (setq os (getvar "osmode")) 
  (while (and (setq pt1 (getpoint "\n�ǵ�1:"))
              (setq ag0  (getangle pt1 "�Ƕ�:"))
       (progn (princ "�Խǵ�:") (setq ct t))
       (progn
  (setq ct t k t ent nil)
         (while ct
    (setq in (grread 1))
    (cond
       (  (= 5 (car in))
                        (setq pt3 (cadr in))
                        (setq len (distance pt1 pt3) ag  (angle pt1 pt3) )
                        (setq pt2 (polar pt1 ag0 (* len (cos (- ag ag0)))))
                        (setq pt4 (polar pt1 (+ (/ pi 2.) ag0) (* len (sin (- ag ag0)))))
          (if ent (command "erase" ent ""))
          (setvar "osmode" 0)
          (command "pline" pt1 pt2 pt3 pt4 "c" )
          (setvar "osmode" os)
          (setq ent (entlast))
                     )
       (  (= 3 (car in))
          (setq pt3 (cadr in))
                        (setq len (distance pt1 pt3) ag  (angle pt1 pt3) )
                        (setq pt2 (polar pt1 ag0 (* len (cos (- ag ag0)))))
                        (setq pt4 (polar pt1 (+ (/ pi 2.) ag0) (* len (sin (- ag ag0)))))
          (if ent (command "erase" ent ""))
          (setvar "osmode" 0)
          (command "pline" pt1 pt2 pt3 pt4 "c" )
          (setvar "osmode" os)
          (setq ent (entlast))
          (setq ct nil)
       )
       (  (equal '(11 0) in)
          (if ent (command "erase" ent ""))
          (setq ct nil k nil)
       )
       (t)
    )  
         )
         k
       ) 
    ) 
       
  )
  (setvar "osmode" os)
  (princ)
)

;�����Ƕ�̬�ӳ�
(defun c:saa()
(command "Lengthen" "dy" "line" Pause))

;�������Զ����ƾ��ζԽ���
(defun c:jxdx(/ a b c)
(setq a (getpoint "\n��ѡ�����ڲ���һ��: "))
(command "boundary" a "")
(setq a (ssget "L")
a (ssname a 0)
a (entget a)
b (assoc 10 a)
)
(while b
(setq c (cons (cdr b) c)
a (cdr (member b a))
b (assoc 10 a)
)
)
(command "line" (nth 0 c) (nth 2 c) ""
"line" (nth 1 c) (nth 3 c) ""
)
)

;����������䶨������
(defun c:twm ( / en xfx pt1 pt2 zljl sl JJ)
(setvar 'cmdecho 0)
(setq en (car(entsel "\nѡ�����:")))
(setq xfx (car(apply 'mapcar(cons '- (bwh en)))))
(setq pt1 (getpoint "\nָ����һ��:"))
(setq pt2 (getpoint "\nָ���ڶ���:"))
(setq zljl (- (distance pt1 pt2) (abs xfx) ) )
(setq JJ (getreal "\n ��������:<300> "))
(if (not JJ)(setq JJ 300) )
(setq sl (fix(/ zljl JJ)  ))
(command "-array" en "" "r" 1 sl (/ zljl (float sl))  )
(setvar 'cmdecho 1)
(princ)
)


(defun bwh (en / p1 p2)
  (vla-getboundingbox (vlax-ename->vla-object en) 'p1 'p2)
  (setq p1 (vlax-safearray->list p1)
        p2 (vlax-safearray->list p2)
  )
  (list p1 p2)
)


;�������߶εȷ��ӳ���
(defun C:XBR (/ OS OE SS VSS N I D LEN E OBJ E)   
	 (princ "\n �ȷִ���߶�")   
	 (defun *ERROR* (MSG)        (setvar "osmode" OS)        (vl-cmdf ".undo" "e")        (setq *ERROR* OE)    )   
	 (vl-cmdf ".undo" "be")    
	 (princ "\n ѡ��Ҫ�ȷֵ����:")   
	  (setq  OE *ERROR*   OS (getvar "osmode")   SS (ssget '((0 . "LINE,*POLYLINE,SPLINE,ARC")))    )   
	   (setvar "osmode" 0)   
	    (or (setq N (getint "\n �����ȷ�<�趨��ȷ�/ֱ�ӻس�>:"))            (setq D (getdist "\n ָ���߶γ���:"))    ) 
	       (setq I 0)   
	        (if SS        (repeat (sslength SS)            
		        (setq E (ssname SS I))            
		        (setq   LEN (vlax-curve-getdistatparam E (vlax-curve-getendparam E))            )            
		        (if N   (setq D  (/ LEN N)              M  N   )   (setq M (1+ (fix (/ LEN D))))            ) 
		                   (repeat (1- M)   
			                   (if (setq PT (vlax-curve-getpointatdist E D))      (vl-cmdf ".break" (list E PT) PT)   )  
			                    (setq E (entlast))            )           
		                    (setq I (1+ I))        )    )    
	        (setvar "osmode" OS)  
	          (vl-cmdf ".undo" "e")  
	           (princ) )

;������˫�»����ӳ���
(defun c:under2 ( )
    (LM:strikethrough:single 
       '(
            (-0.8  0.05)
            (-1.0  0.05)
        )
    )
)
;; Strikthrough Text: Single Selection  -  Lee Mac
;; Prompts the user to select a single text, mtext or attribute object and evaluates the
;; strikethrough function with the supplied parameter list.
;; l - [lst] List of ((<Spacing Factor> <Width Factor>) ... )

(defun LM:strikethrough:single ( l / *error* e )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (while
        (progn
            (setvar 'errno 0)
            (setq e (nentselp "\nSelect text, mtext or attribute: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null e) nil)
                (   (and (= 2 (length e))
                         (wcmatch (cdr (assoc 0 (entget (car e)))) "TEXT,MTEXT,ATTRIB")
                    )
                    (LM:strikethrough (car e) l)
                )
                (   (princ "\nInvalid object selected."))
            )
        )
    )
    (princ)
)

;; Strikthrough Text: Multiple Selection  -  Lee Mac
;; Prompts the user for a selection of text, mtext or attributed blocks and evaluates the
;; strikethrough function with the supplied parameter list.
;; l - [lst] List of ((<Spacing Factor> <Width Factor>) ... )

(defun LM:strikethrough:selection ( l / *error* a e i s x )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if (setq s (ssget '((-4 . "<OR") (0 . "TEXT,MTEXT") (-4 . "<AND") (0 . "INSERT") (66 . 1) (-4 . "AND>") (-4 . "OR>"))))
        (repeat (setq i (sslength s))
            (setq i (1- i)
                  e (ssname s i)
            )
            (if (= "INSERT" (cdr (assoc 0 (entget e))))
                (progn
                    (setq a (entnext e)
                          x (entget  a)
                    )
                    (while (= "ATTRIB" (cdr (assoc 0 x)))
                        (LM:strikethrough a l)
                        (setq a (entnext a)
                              x (entget  a)
                        )
                    )
                )
                (LM:strikethrough e l)
            )
        )
    )
    (princ)
)

;; Strikethrough Text  -  Lee Mac
;; Generates polylines through the supplied text object, with spacing & width given by the supplied parameter list.
;; ent - [ent] Text or MText entity
;; par - [lst] List of ((<Spacing Factor> <Width Factor>) ... ) for each polyline
;; Returns: [lst] List of created polyline entities

(defun LM:strikethrough ( ent par / ang enx hgt lst md1 md2 rtn )
    (if (setq lst (LM:textbox (setq enx (entget ent))))
        (progn
            (setq hgt (cdr (assoc 40 enx))
                  md1 (mid   (car  lst) (last  lst))
                  md2 (mid   (cadr lst) (caddr lst))
                  ang (angle (car  lst) (last  lst))
            )
            (foreach itm par
                (setq rtn
                    (cons
                        (entmakex
                            (append
                               '(   (000 . "LWPOLYLINE")
                                    (100 . "AcDbEntity")
                                    (100 . "AcDbPolyline")
                                    (090 . 2)
                                    (070 . 0)
                                )
                                (LM:defaultprops enx)
                                (list
                                    (cons  043 (* (cadr itm) hgt))
                                    (cons  038 (caddar lst))
                                    (cons  010 (polar md1 ang (* (car itm) hgt)))
                                    (cons  010 (polar md2 ang (* (car itm) hgt)))
                                    (assoc 210 enx)
                                )
                            )
                        )
                        rtn
                    )
                )
            )
        )
    )
    rtn
)

;; Midpoint  -  Lee Mac
;; Returns the midpoint of two points

(defun mid ( a b )
    (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) a b)
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups
 
(defun LM:defaultprops ( enx )
    (mapcar '(lambda ( x ) (cond ((assoc (car x) enx)) ( x )))
       '(
            (006 . "BYLAYER")
            (008 . "0")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Text Box  -  Lee Mac (based on code by gile)
;; Returns the bounding box of a text, mtext, or attribute entity (in OCS)
;; enx - [lst] Text, MText or Attribute DXF data list

(defun LM:textbox ( enx / bpt hgt jus lst ocs org rot wid )
    (cond
        (   (and (= "ATTRIB" (cdr (assoc 000 enx)))
                 (= "Embedded Object" (cdr (assoc 101 enx)))
            )
            (LM:textbox (cons '(000 . "MTEXT") (member '(101 . "Embedded Object") enx)))
        )
        (   (cond
                (   (wcmatch  (cdr (assoc 000 enx)) "ATTRIB,TEXT")
                    (setq bpt (cdr (assoc 010 enx))
                          rot (cdr (assoc 050 enx))
                          lst (textbox enx)
                          lst (list (car lst) (list (caadr lst) (cadar lst)) (cadr lst) (list (caar lst) (cadadr lst)))
                    )
                )
                (   (= "MTEXT" (cdr (assoc 000 enx)))
                    (setq ocs  (cdr (assoc 210 enx))
                          bpt  (trans (cdr (assoc 010 enx)) 0 ocs)
                          rot  (angle '(0.0 0.0) (trans (cdr (assoc 011 enx)) 0 ocs))
                          wid  (cdr (assoc 042 enx))
                          hgt  (cdr (assoc 043 enx))
                          jus  (cdr (assoc 071 enx))
                          org  (list (cond ((member jus '(2 5 8)) (/ wid -2.0)) ((member jus '(3 6 9)) (- wid))      (0.0))
                                     (cond ((member jus '(1 2 3)) (- hgt))      ((member jus '(4 5 6)) (/ hgt -2.0)) (0.0))
                               )
                          lst  (list org (mapcar '+ org (list wid 0)) (mapcar '+ org (list wid hgt)) (mapcar '+ org (list 0 hgt)))
                    )
                )
            )
            (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) bpt)) lst))
                (list
                    (list (cos rot) (sin (- rot)) 0.0)
                    (list (sin rot) (cos rot)     0.0)
                   '(0.0 0.0 1.0)
                )
            )
        )
    )
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

(princ)



  
;�����ǿ����������ƾ��ζԽ���

;*********************************************************************************************************************************************************************

;	komondormrex, feb 2023

;*********************************************************************************************************************************************************************

(defun c:draw_rectangle_diagonals (/ ename_index ename_sset object)
	(vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
	(repeat (sslength (setq ename_index -1
							ignore_empty_sset (while (null (setq ename_sset (vl-catch-all-apply 'ssget (list '((0 . "lwpolyline") (-4 . "&=") (70 . 1) (90 . 4))))
														   )
													  )
											   )
							ename_sset (cond
												(
													(vl-catch-all-error-p ename_sset)
														(princ "\nCommand cancelled")
														(ssadd)
												)
												(
													t
														ename_sset
												)
									   )
					)
			)
				(setq object (vlax-ename->vla-object (ssname ename_sset (setq ename_index (1+ ename_index)))))
				(if
					(and
						 (= "AcDbPolyline" (vla-get-objectname object))
						 (equal (distance (vlax-curve-getpointatparam object 0) (vlax-curve-getpointatparam object 2))
								(distance (vlax-curve-getpointatparam object 1) (vlax-curve-getpointatparam object 3))
								1e-8
						 )
						 (equal (angle (vlax-curve-getpointatparam object 0) (vlax-curve-getpointatparam object 1))
								(angle (vlax-curve-getpointatparam object 3) (vlax-curve-getpointatparam object 2))
								1e-8
						 )
						 (equal (angle (vlax-curve-getpointatparam object 1) (vlax-curve-getpointatparam object 2))
								(angle (vlax-curve-getpointatparam object 0) (vlax-curve-getpointatparam object 3))
								1e-8
						 )
						 (zerop (apply '+ (mapcar 'abs (mapcar 'cdr (vl-remove-if-not '(lambda (group) (= 42 (car group))) (entget (ssname ename_sset ename_index)))))))
					)
						(progn
							(vla-addline (vla-get-block (vla-get-activelayout (vla-get-activedocument (vlax-get-acad-object))))
										 (vlax-3d-point (vlax-curve-getpointatparam object 0))
										 (vlax-3d-point (vlax-curve-getpointatparam object 2))
							)
							(vla-addline (vla-get-block (vla-get-activelayout (vla-get-activedocument (vlax-get-acad-object))))
										 (vlax-3d-point (vlax-curve-getpointatparam object 1))
										 (vlax-3d-point (vlax-curve-getpointatparam object 3))
							)
						)
				)
				(princ)
	)
	(vla-endaundomark (vla-get-activedocument (vlax-get-acad-object)))
	(princ)
)

;*********************************************************************************************************************************************************************

;*********************************************************************************************************************************************************************





;�����ǿ���ѡ��

;;----------------------------------------------------------------------
;;��̳��ַ:http://www.xdcad.net/forum/showthread.php?threadid=564783
;;����������wkai
;;������ƣ�С��
;;�����Դ�������KLDC
;;���߶Ա������ṩ�κ�ʹ���ϵı�֤��
;;�����򲻵�������ҵĿ�ģ����ƴ����뱣��������Ϣ
;;----------------------------------------------------------------------
(defun c:qss (/	     slent   f	     dcl_name	     lst2    lst1
	     color   tmp     index_value     flag    entl    lst3
	     lst4    code    ktmp    klst    lst4    hand    fjflt
	     filter  kl_pre
	     ;;ԭΪȫ�ֱ���������ʵ���ѡʱ���ϴι��˱����ѡ������ss_saved_lst����
	     ss	     fjlst   ssl     attdis  strtmp
	    )
  ;;ss_saved_lst��Ϊȫ�ֱ��������ϴ�ѡ��ı���ֵ����ʽ��(hand fjflt filter kl_pre)
  (setq	attdis "Y"
	kl_pre (last ss_saved_lst)
	slent  "N"
  )
  (while (= slent "N")
    (initget "N")
    (setq
      slent (entsel
	      (strcat "\n��ѡ������ʵ��(N-�رտ�������ʾ,��ǰ״̬��"
		      (if (= "Y" attdis)
			"��"
			"�ر�"
		      )
		      "):"
	      )
	    )
    )
    (if	(= slent "N")
      (setq attdis "N")
      (setq slent (car slent))
    )
  )
  ;;while
  (if slent
;;;----------------------1
    (progn
;;;-----------------------1
      (setq fjflt  nil
	    filter nil
	    entl   (entget slent)
      )
      (setq lst2
	     '(
	       ("ͨ��"
		((0 "ʵ������")
		 (6 "ʵ������")
		 (8 "����ͼ��")
		 (48 "���ͱ���")
		 (62
		  "ʵ����ɫ"
		  ((256 "���")
		   (0 "���")
		   (1 "��ɫ")
		   (2 "��ɫ")
		   (3 "��ɫ")
		   (4 "��ɫ")
		   (5 "��ɫ")
		   (6 "��ɫ")
		   (7 "�ڰ�")
		  )
		 )
		 (370 "ʵ���߿�")
		)
	       )
	       ("ARC"
		((-4 "Բ��")
		 (10 "Բ������")
		 (40 "Բ���뾶")
		 (39 "ʵ����")
		 (50 "���Ƕ�")
		 (51 "�յ�Ƕ�")
		)
		("FJ" ("FJ1" "Բ������" (len slent)))
	       )
	       ("CIRCLE"
		((-4 "Բ��")
		 (10 "Բ������")
		 (40 "Բ�ΰ뾶")
		 (39 "ʵ����")
		)
	       )
	       ("SOLID" ((-4 "SOLID") (39 "ʵ����")))
	       ("POINT"
		((-4 "��")
		 (10 "���λ��")
		 (39 "ʵ����")
		 (50 "��ת�Ƕ�")
		)
	       )
	       ("LINE"
		((-4 "ֱ�߶�")
		 (10 "�������")
		 (11 "�յ�����")
		 (39 "ʵ����")
		)
		("FJ"
		 ("FJ1" "�߶γ���" (len slent))
		 ("FJ2"
		  "�߶νǶ�"
		  (REM
		   (ATOF (ANGTOS (ANGLE (DXF 10 slent) (DXF 11 slent))))
		   180
		  )
		 )
		)
	       )
	       ("ELLIPSE"
		((-4 "��Բ")
		 (10 "��Բ����")
		 (11 "����˵�")
		 (40 "�������")
		 (41 "��ʼ����")
		 (42 "��������")
		)
	       )
	       ("INSERT"
		((-4 "ͼ��")
		 (10 "ͼ��λ��")
		 (2 "ͼ������")
		 (41 "X �����")
		 (42 "Y �����")
		 (43 "Z �����")
		 (50 "��ת�Ƕ�")
		)
	       )
					;("FJ" ("FJ1" "���Ա�־" (car (attstr slent))) ("FJ2" "������ֵ" (cadr (attstr slent)))))
	       ("LWPOLYLINE"
		((-4 "�������")
		 (38 "���߱��")
		 (43 "�̶����")
		 (90 "�������")
		 (39 "���ߺ��")
		 (70 "�Ƿ�պ�" ((0 "���պ�") (1 "�պ�")))
		)
		("FJ" ("FJ1" "���߳���" (len slent)))
	       )
	       ("POLYLINE"
		((-4 "�ض�����")
		 (70 "�Ƿ�պ�" ((0 "���պ�") (1 "�պ�")))
		)
		("FJ" ("FJ1" "���߳���" (len slent)))
	       )
	       ("HATCH"
		((-4 "ͼ�����")
		 (2 "���ͼ��")
		 (41 "������")
		 (52 "���Ƕ�")
		 (71 "�߽����" ((0 "������") (1 "����")))
		 (76 "ͼ������" ((0 "�û�����") (1 "Ԥ����") (2 "�Զ���")))
		)
	       )
	       ("TEXT"
		((-4 "����")
		 (1 "��������")
		 (7 "������ʽ")
		 (10 "����λ��")
		 (40 "���ָ߶�")
		 (41 "���ϵ��")
		 (50 "��ת�Ƕ�")
		 (51 "��б�Ƕ�")
		 (71
		  "���־���"
		  ((0 "Ĭ��") (2 "���ַ���") (4 "���ֵ���") (6 "������"))
		 )
		 (72
		  "ˮƽ����"
		  ((0 "�����")
		   (1 "���ж���")
		   (2 "�Ҷ���")
		   (3 "����")
		   (4 "�м�")
		   (5 "���")
		  )
		 )
		 (73
		  "��ֱ����"
		  ((0 "���߶���")
		   (1 "�׶˶���")
		   (2 "���ж���")
		   (3 "���˶���")
		  )
		 )
		)
		("FJ" ("FJ1" "������ֵ" (ATOF (DXF 1 slent))))
	       )
	       ("ATTDEF"
		((-4 "���Զ���")
		 (2 "���Ա��")
		 (7 "������ʽ")
		 (10 "����λ��")
		 (40 "���ָ߶�")
		 (50 "��ת�Ƕ�")
		 (51 "��б�Ƕ�")
		 (71
		  "���־���"
		  ((0 "Ĭ��") (2 "���ַ���") (4 "���ֵ���") (6 "������"))
		 )
		 (72
		  "ˮƽ����"
		  ((0 "�����")
		   (1 "���ж���")
		   (2 "�Ҷ���")
		   (3 "����")
		   (4 "�м�")
		   (5 "���")
		  )
		 )
		 (73
		  "��ֱ����"
		  ((0 "���߶���")
		   (1 "�׶˶���")
		   (2 "���ж���")
		   (3 "���˶���")
		  )
		 )
		)
		("FJ" ("FJ1" "�����ֵ" (ATOF (DXF 2 slent))))
	       )
	       ("MTEXT"
		((-4 "��������")
		 (10 "����λ��")
		 (1 "��������")
		 (7 "������ʽ")
		 (40 "���ָ߶�")
		 (50 "��ת�Ƕ�")
		)
	       )
	       ("SPLINE"
		((-4 "��������")
		 (70 "���߱�־")
		 (71 "���߽���")
		 (72 "�ڵ�����")
		 (73 "���Ƶ���")
		 (74 "��ϵ���")
		 (42 "�ڵ㹫��")
		 (43 "�ص㹫��")
		 (44 "��Ϲ���")
		)
		("FJ" ("FJ1" "���߳���" (len slent)))
	       )
	       ("DIMENSION"
		((-4 "�ߴ��ע")
		 (1 "��ע����")
		 (42 "����ֵ")
		 (3 "��ע��ʽ")
		 (70
		  "��ע����"
		  ((32 "ˮƽ��ֱ")
		   (33 "�����ע")
		   (34 "�Ƕȱ�ע")
		   (35 "ֱ����ע")
		   (36 "�뾶��ע")
		   (37 "����Ƕ�")
		   (38 "�����ע")
		  )
		 )
		)
	       )
	      )
      )
      ;;setq lst2 
      (if (and (= attdis "Y") (= "INSERT" (dxf 0 slent)))
	(kldc_1)
      )
      ;;�Կ�ʵ�壬�������Թ��˱�slent��lst2��Ϊȫ�ֱ�������
      (setq lst3 (car (dxf "ͨ��" lst2))
	    lst5 (dxf (dxf 0 entl) lst2)
	    lst4 (car lst5)
	    lst5 (cadr lst5)
      )
      (foreach tmp lst3
	(if (and (not (dxf (car tmp) entl)) (/= (car tmp) 62))
	  (setq lst3 (vl-remove tmp lst3))
	)
      )
      ;;foreach 
      (setq dcl_name (strcat (getenv "temp") "\\sel" ".dcl")
	    f	     (OPEN dcl_name "w")
      )
      (write-line "sl:dialog{label=\"�ҵ�ѡ����--By С��\";" f)
      (write-line ":column{" f)
      (write-line ":boxed_column{label=\"��������\";" f)
      (write-line ":boxed_column{label=\"ͨ��\";" f)
      (foreach tmp lst3
	(write-line ":row{fixed_width=true;" f)
	(write-line
	  (strcat ":toggle{key=\""
		  (itoa (car tmp))
		  "\";label=\""
		  (cadr tmp)
		  "\";width=12;}"
	  )
	  f
	)
	(write-line
	  (strcat ":popup_list{edit_width=5;key=\"pop"
		  (itoa (car tmp))
		  "\";}"
	  )
	  f
	)
	(setq
	  ktmp (list (strcat "pop" (itoa (car tmp))) (itoa (car tmp)))
	)
	(if (/= 62 (car tmp))
	  (progn
	    (setq
	      ktmp (write f
			  ktmp
			  (car tmp)
			  (vl-princ-to-string (dxf (car tmp) entl))
			  "txt"
			  "16"
		   )
	    )
	    (if	(= 48 (car tmp))
	      (setq ktmp (write f ktmp 48 "�ݲ�" "txta" "7"))
	    )
	    ;;if 
	  )
	  ;;progn 
	  (progn
	    (setq color (dxf 62 entl))
	    (if	(not color)
	      (setq color 256)
	    )
	    (setq ktmp (write f ktmp 62 (itoa color) "txt" "16"))
	    (write-line
	      (strcat ":edit_box{value=\""
		      (vl-princ-to-string (car (dxf color (caddr tmp))))
		      "\";edit_width=7 ;allow_accept=true;}"
	      )
	      f
	    )
	  )				;progn 
	)
	;;if 
	(write-line "}" f)
	(setq klst (cons (reverse ktmp) klst))
      )
      ;;foreach 
      (write-line "}" f)
      (write-line
	(strcat	":boxed_column{label=\""
		(vl-princ-to-string (car (dxf -4 lst4)))
		"\";"
	)
	f
      )
      (setq lst4 (cdr lst4))
      ;;ȥ��ǰ���-4���� 
      (foreach tmp lst4
	(setq code (car tmp)
	      ktmp nil
	)
	(if (dxf code entl)
	  (progn
	    (write-line ":row{fixed_width=true;" f)
	    (setq ktmp (list (strcat "pop" (itoa code)) (itoa code)))
	    (write-line
	      (strcat ":toggle{key=\""
		      (itoa code)
		      "\";label=\""
		      (vl-princ-to-string (cadr tmp))
		      "\";width=12;}"
	      )
	      f
	    )
	    (write-line
	      (strcat ":popup_list{edit_width=5;key=\"pop"
		      (itoa code)
		      "\";}"
	      )
	      f
	    )
	    (cond
	      ((or (= code 10) (= code 11))
	       (setq ktmp
		      (write f
			     ktmp
			     code
			     (vl-princ-to-string (car (dxf code entl)))
			     "txt_x"
			     "6.5"
		      )
	       )
	       (setq ktmp
		      (write f
			     ktmp
			     code
			     (vl-princ-to-string (cadr (dxf code entl)))
			     "txt_y"
			     "6"
		      )
	       )
	       (setq ktmp
		      (write f
			     ktmp
			     code
			     (vl-princ-to-string (caddr (dxf code entl)))
			     "txt_z"
			     "7"
		      )
	       )
	      )
	      ((member code
		       '(1 2 3 7 90 38 39 40 41	42 43 44 50 51 52 70 71
			 72 73 74 76)
	       )
	       (setq strtmp (vl-princ-to-string (dxf code entl)))
	       (if (= code 1)
		 (foreach tmp '("\r\n" "\\P" "\\")
		   (while (vl-string-search tmp strtmp)
		     (setq strtmp (vl-string-subst " " tmp strtmp))
		   )
		 )
		 ;;foreach ;;;;����acad2005�е�mtext�еĻ��з�(shift+enter)���¶Ի�������
	       )
	       ;;end if code=1
	       (setq ktmp (write f ktmp code strtmp "txt" "16"))
					;ԭstrtmp=(vl-princ-to-string (dxf code entl))
	       (cond
		 ((member code '(38 39 40 41 42 43 44 50 51 52))
		  (setq ktmp (write f ktmp code "�ݲ�" "txta" "7"))
		 )
		 ((member code '(70 71 72 73 74 76))
		  (if
		    (car (dxf (dxf code entl) (cadr (dxf code lst4))))
		     (write-line
		       (strcat
			 ":edit_box{value=\""
			 (vl-princ-to-string
			   (car
			     (dxf (dxf code entl) (cadr (dxf code lst4)))
			   )
			 )
			 "\";edit_width=7;allow_accept=true;}"
		       )
		       f
		     )
		  )			;if 
		 )
	       )
	       ;;cond 
	      )
	    )
	    ;;cond 
	    (write-line "}" f)
	  )
	)
	;;progn & if 
	(if ktmp
	  (setq klst (cons (reverse ktmp) klst))
	)
      )
      ;;foreach 
      (write-line "}" f)
      (if lst5
	(progn (setq lst5 (cdr lst5))
	       ;;ȥ��lst5��һ��Ԫ��"FJ" 
	       (write-line ":boxed_column{label=\"���ӹ���\";" f)
	       (foreach	tmp lst5
		 (write-line ":row{fixed_width=true;" f)
		 (write-line
		   (strcat ":toggle{key=\""
			   (car tmp)
			   "\";label=\""
			   (cadr tmp)
			   "\";width=12;}"
		   )
		   f
		 )
		 (write-line
		   (strcat ":popup_list{edit_width=5;key=\"pop"
			   (car tmp)
			   "\";}"
		   )
		   f
		 )
		 (setq ktmp (list (strcat "pop" (car tmp)) (car tmp)))
		 (setq ktmp (write f
				   ktmp
				   (car tmp)
				   (vl-princ-to-string (eval (caddr tmp)))
				   "txt"
				   "16"
			    )
		 )
		 (setq ktmp (write f ktmp (car tmp) "�ݲ�" "txta" "7"))
		 (setq fjlst (cons (reverse ktmp) fjlst))
		 ;;fjlst�Ǹ��ӹ��������ı����� 
		 (write-line "}" f)
	       )
	       ;;foreach 
	       (write-line "}" f)
	)
      )					;if lst5 
      (write-line "}:row{:boxed_radio_row{label=\"���˷�Χ\";" f)
      (write-line
	":radio_button{label=\"��ѡ\";key=\"hand\";value=\"1\";}"
	f
      )
      (write-line ":radio_button{label=\"Ԥѡ\";key=\"pre\";}" f)
      (write-line ":radio_button{label=\"ȫͼ\";key=\"all\";}" f)
      (write-line "}}:row{ok_cancel;}}}" f)
      (close f)
      (setq klst (reverse klst))
      (setq index_value (load_dialog dcl_name)) ;_����dcl�ļ� 
      (new_dialog "sl" index_value) ;_��ʼ�¶Ի��� 
      (foreach tmp klst
	;;klstΪ�����������ʼ���б���������ʼֵ 
	;;�磺'(("0" "pop0" ("txt0" "INSERT")) ("8" "pop8" ("txt8" "_��������")) ("62" "pop62" ("txt62" "256")) 
	;;("10" "pop10" ("txt_x10" "3431.58") ("txt_y10" "-17355.0") ("txt_z10" "0.0")) ("2" "pop2" ("txt2" "RXF008")) 
	;;("41" "pop41" ("txt41" "-64.0") ("txta41" "��?.. 
	(cond ((member (car tmp) '("0" "1" "2" "3" "6" "7" "8"))
	       (show_list (cadr tmp) '("=" "<>"))
	      )
	      ((member (car tmp)
		       '("10"	"11"   "38"   "39"   "40"   "41"
			 "42"	"43"   "44"   "48"   "50"   "51"
			 "52"
			)
	       )
	       (show_list (cadr tmp) '("=" "<" ">" "<=" ">=" "<>"))
	      )
	      ((member (car tmp)
		       '("62" "70" "71" "72" "73" "74" "76" "90")
	       )
	       (show_list (cadr tmp)
			  '("=" "<" ">" "<=" ">=" "<>" "&" "&=")
	       )
	      )
	)
	;;cond 
      )
      ;;foreach ��ʾ����ѡ����Ϣ 
      (if fjlst
	(foreach tmp fjlst
	  (show_list (cadr tmp) '("=" "<" ">" "<=" ">=" "<>"))
	)
      )
;;;;end if fjlst;��ʾ���ӹ�������ѡ����Ϣ 
;;;;fjlst�Ǹ��ӹ��������ı�����,�磺'(("FJ3" "popFJ3" ("txtFJ3" "0.0") ("txtaFJ3" "�ݲ�")) ("FJ2" "popFJ2" ("txtFJ2" "0.0") 
      ;;("txtaFJ2" "�ݲ�")) ("FJ1" "popFJ1" ("txtFJ1" "0.0") ("txtaFJ1" "�ݲ�"))) 
      (if kl_pre
	(foreach tmp (cdr kl_pre)
	  (if (= (dxf 0 entl) (car kl_pre))
	    (set_tile (car tmp) "1")
	    (if	(member (car tmp) '("0" "6" "8" "48" "62" "370"))
	      (set_tile (car tmp) "1")
	    )
	    ;;end if 
	  )
	  ;;end if 
	)
	;;foreach 
      )
      ;;���ϴ�ѡ�еĸ�ѡ����Ϊѡ��״̬ 
      (action_tile "accept" "(get_filter) (done_dialog 1)")
      (setq flag (start_dialog))
      (unload_dialog index_value)
    )
    ;;end progn ;;;-----------------------1
    (setq hand	 (car ss_saved_lst)
	  fjflt	 (cadr ss_saved_lst)
	  filter (caddr ss_saved_lst)
    )
    ;;setq
  )
  ;;end if;;;;--------------------------1
  (if filter
    (progn (princ "\nʹ�ù�������")
	   (prin1 filter)
	   (cond ((= hand "1") (setq ss (ssget filter)))
		 ((= hand "2") (setq ss (ssget "p" filter)))
		 ((= hand "3") (setq ss (ssget "x" filter)))
	   )
	   ;;cond 
    )
  )
  ;;end if filter 
  (if (and (setq ssl (chsget ss)) fjflt)
    (foreach slent ssl
      (if (not (eval fjflt))
	(setq ss (ssdel slent ss))
      )
    )
  )
  ;;end if
  (setq ss_saved_lst (list hand fjflt filter kl_pre))
  ;;������ȫ�ֱ���
  (if ss
    (progn
      (princ (strcat "\n��ѡ����" (itoa (sslength ss)) "��ʵ�塣")
      )
      (if (= 0 (getvar "cmdactive"))
	(command "select" ss "" "pselect" ss "")
      )
    )
    ;;progn
    (princ "\n��ѡ����0��ʵ�塣")
  )
  ;;if
  ss
)
;;defun 
(defun get_filter (/ tmp pop txt txt1 rc txt2 txt3 pop_1 pop_2 pop_3)
  (cond	((= "1" (get_tile "hand")) (setq hand "1"))
	((= "1" (get_tile "pre")) (setq hand "2"))
	((= "1" (get_tile "all")) (setq hand "3"))
  )
  ;;cond
  (foreach tmp klst
    (if	(/= "1" (get_tile (car tmp)))
      (setq klst (vl-remove tmp klst))
    )
  )
  (foreach tmp fjlst
    (if	(/= "1" (get_tile (car tmp)))
      (setq fjlst (vl-remove tmp fjlst))
    )
  )
  (setq kl_pre (append (list (dxf 0 entl)) klst fjlst))
  ;;���ӹ���ѡ�е����´�ʹ��Ҳ��Ϊȱʡѡ�� 
  (foreach tmp klst
    (setq pop (get_tile (cadr tmp)))
    (cond
      ((member (car tmp) '("0" "1" "2" "3" "6" "7" "8"))
       (setq txt  (get_tile (caaddr tmp))
	     txt1 (cadr (caddr tmp))
       )
       (if (= txt txt1)
	 (setq txt (dxf (read (car tmp)) entl))
       )
       ;;���(car tmp)��Ӧ��ֵδ���û��޸Ĺ���ȡ��ԭ����ֵ 
       (cond ((= pop "0")		;(setq txt (get_tile (caaddr tmp)) 
	      (setq filter (append (cons '(-4 . "<OR")
					 (cons (cons (read (car tmp)) txt)
					       '((-4 . "OR>"))
					 )
				   )
				   filter
			   )
	      )
	      ;;setq 
	     )
	     ((= pop "1")		; (setq txt (get_tile (caaddr tmp)) 
	      (setq filter (append (cons '(-4 . "<NOT")
					 (cons (cons (read (car tmp)) txt)
					       '((-4 . "NOT>"))
					 )
				   )
				   filter
			   )
	      )
	      ;;setq 
	     )
       )
       ;;cond 
      )
      ;;end member 
      ((member (car tmp)
	       '("62" "70" "71" "72" "73" "74" "76" "90")
       )
       (setq txt    (get_tile (caaddr tmp))
	     filter (append
		      (cons (cons -4
				  (nth (read pop)
				       '("=" "<" ">" "<=" ">=" "<>" "&" "&=")
				  )
			    )
			    (list (cons (read (car tmp)) (read txt)))
		      )
		      filter
		    )
		    ;;append 
       )
       ;;setq 
      )
      ((member (car tmp)
	       '("38" "39" "40" "41" "42" "43" "44" "48" "50" "51" "52")
       )
       (setq txt  (get_tile (caaddr tmp))
	     txt1 (cadr (caddr tmp))
	     rc	  (read (get_tile (car (last tmp))))
       )
       ;;setq 
       (if (/= txt txt1)
	 (setq txt (atof txt))
	 (setq txt (dxf (read (car tmp)) entl))
       )
       ;;���(car tmp)��Ӧ��ֵδ���û��޸Ĺ���ȡ��ԭ����ʵ����ֵ 
       (if (and	(or (= (type rc) 'REAL) (= (type rc) 'INT))
		(= pop "0")
	   )
	 ;;����������ݲ��Ϊ��ֵ�ͣ���������Ϊ"="ʱҪ�����ݲ� 
	 (setq filter
		(append
		  ;;�����ݲ� 
		  (cons	'(-4 . "<=")
			(list (cons (read (car tmp)) (+ txt (abs rc))))
		  )
		  (cons	'(-4 . ">=")
			(list (cons (read (car tmp)) (- txt (abs rc))))
		  )
		  filter
		)
		;;append 
	 )
	 ;;setq 
	 (setq
	   filter (append		;�������ݲ� 
		    (cons
		      (cons
			-4
			(nth (read pop) '("=" "<" ">" "<=" ">=" "<>"))
		      )
		      (list (cons (read (car tmp)) txt))
		    )
		    filter
		  )
		  ;;append 
	 )
	 ;;setq 
       )
       ;;end of if �ݲ� 
      )
      ((member (car tmp) '("10" "11"))
       (setq txt1  (get_tile (caaddr tmp))
	     txt2  (get_tile (car (cadddr tmp)))
	     txt3  (get_tile (car (last tmp)))
	     pop_1 (nth (read pop) '("=" "<" ">" "<=" ">=" "<>"))
	     pop_2 pop_1
	     pop_3 pop_1
       )
       ;;setq
       (if (= txt1 "")
	 (setq pop_1 "*")
       )
       (if (= txt2 "")
	 (setq pop_2 "*")
       )
       (if (= txt3 "")
	 (setq pop_3 "*")
       )
       (if (/= txt1 (cadr (caddr tmp)))
	 (setq txt1 (atof txt1))
	 (setq txt1 (car (dxf (read (car tmp)) entl)))
       )
       ;;��������Ӧ��ֵδ���û��޸Ĺ���ȡ��ԭ����ʵ����ֵ 
       (if (/= txt2 (cadr (cadddr tmp)))
	 (setq txt2 (atof txt2))
	 (setq txt2 (cadr (dxf (read (car tmp)) entl)))
       )
       (if (/= txt3 (cadr (last tmp)))
	 (setq txt3 (atof txt3))
	 (setq txt3 (caddr (dxf (read (car tmp)) entl)))
       )
       (setq filter
	      (append
		(cons
		  (cons -4 (strcat pop_1 "," pop_2 "," pop_3))
		  (list (cons (read (car tmp)) (list txt1 txt2 txt3)))
		)
		filter
	      )
       )
      )
      ;;end of member (car tmp) '("10" "11") 
    )
    ;;cond 
  )
  ;;foreach tmp klst 
  (if fjlst
    (progn
      (if (null filter)
	(setq filter (list (assoc 0 entl)))
      )
      ;;�����ѡ�еĸ�����������filter��Ϊ����ʵ������ 
      (setq fjflt '(and))
      (foreach tmp fjlst
	(setq pop  (get_tile (cadr tmp))
	      txt  (get_tile (caaddr tmp))
	      txt1 (cadr (caddr tmp))
	      rc   (read (get_tile (car (last tmp))))
	)
	;;setq 
	(if (/= txt txt1)
	  (if (/= "INSERT" (dxf 0 slent))
	    (setq txt (atof txt))
	  )				;ͼ��ʵ��ĸ��ӹ���Ϊ�ַ��ͣ�����Ϊ��ֵ��
	  (setq txt (eval (cadr (dxf (car tmp) lst5))))
	)
	;;���(car tmp)��Ӧ��ֵδ���û��޸Ĺ���ȡ��ԭ����ʵ����ֵ 
	(if (and (or (= (type rc) 'REAL) (= (type rc) 'INT))
		 (= pop "0")
	    )
	  ;;����������ݲ��Ϊ��ֵ�ͣ���������Ϊ"="ʱҪ�����ݲ� 
	  (setq	fjflt
		 (append ;;�����ݲ� 
			 fjflt
			 (list (list 'and
				     (list '<=
					   (cadr (dxf (car tmp) lst5))
					   (+ txt (abs rc))
				     )
				     (list '>=
					   (cadr (dxf (car tmp) lst5))
					   (- txt (abs rc))
				     )
			       )
			 )
		 )
		 ;;append 
	  )
	  ;;setq 
	  (setq	fjflt
		 (append		;�������ݲ� 
		   fjflt
		   (list
		     (list
		       (read
			 (nth (read pop) '("=" "<" ">" "<=" ">=" "<>"))
		       )
		       (cadr (dxf (car tmp) lst5))
		       txt
		     )
		   )
		 )
		 ;;append 
	  )
	  ;;setq 
	)
	;;end of if �ݲ� 
      )
      ;;foreach fjlst 
    )
  )
  ;;end if fjlst 
)
;;defun 
(defun show_list (key lst)
  (start_list key)
  (mapcar 'add_list lst)
  (end_list)
)
;;defun 
(defun write (f ktmp code value txt width / tmp)
  (setq tmp (strcat txt (vl-princ-to-string code)))
  (write-line
    (strcat ":edit_box{value=\"" value
	    "\";key=\""		 tmp
	    "\";edit_width="	 width
	    ";allow_accept=true;}"
	   )
    f
  )
  (setq ktmp (cons (list tmp value) ktmp))
)
;;defun 
(defun dxf (i ent)
  (if (= (type ent) 'ENAME)
    (setq ent (entget ent))
  )
  (cdr (assoc i ent))
)
;;defun
(defun chsget (c01 / c02 c03 c04 c05)
  (if c01
    (progn
      (setq c02	0
	    c03	(sslength c01)
      )
      (while (< c02 c03)
	(setq c04 (ssname c01 c02)
	      c02 (1+ c02)
	)
	(setq c05 (cons c04 c05))
      )					;end of while 
    )					;end of progn 
  )					;end of if 
  c05
)					;end of defun
(defun len (ent)
  (if (= (type ent) 'ENAME)
    (setq ent (vlax-ename->vla-object ent))
  )
  (if
    (wcmatch
      (vla-get-ObjectName ent)
      "AcDbPolyline,AcDbEllipse,AcDbCircle,AcDbArc,AcDbLine,AcDb2dPolyline,AcDbSpline"
    )
     (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
  )
  ;;if
)
;;defun
(defun VxGetAtts (Obj)
  (if (= (type Obj) 'ENAME)
    (setq Obj (vlax-ename->vla-object Obj))
  )
  (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
    (mapcar
      '(lambda (Att)
	 (cons (vla-get-TagString Att)
	       (vla-get-TextString Att)
	 )
       )
      (vlax-invoke Obj "GetAttributes")
    )
  )
)
(defun KLDC_1 (/ attl alen lval ltag aflst aa cc a11 a12 a13)
  (setq attl (VxGetAtts slent))
  (if attl
    (progn
      (setq alen (length attl))
      (while (> alen 0)
	(setq a11   (list (cons 'nth (cons (- alen 1) '((VxGetAtts slent)))))
	      a12   (cons 'if (list '(VxGetAtts slent) (cons 'cdr a11)))
	      a13   (cons 'if (list '(VxGetAtts slent) (cons 'car a11)))
	      lval  (list (strcat "FJ" (rtos (* 2 alen) 2 0))
			  '"������ֵ"
			  a12
		    )
	      ltag  (list (strcat "FJ" (rtos (- (* 2 alen) 1) 2 0))
			  '"���Ա�־"
			  a13
		    )
	      aflst (append (list lval ltag) aflst)
	      alen  (1- alen)
	)
      )
      ;;end while
      (setq aa	 (assoc "INSERT" lst2)
	    cc	 (list (car aa) (cadr aa) (append '(FJ) (reverse aflst)))
	    lst2 (subst cc aa lst2)
      )
    )
    ;;progn
  )
  ;;if
)
;;end defun
(if (not (member "acopm.arx" (arx)))
  (arxload "acopm.arx")
)
(princ)













;����������ľ����С�㣩
(defun c:ldmf()
(setvar "cmdecho" 0)	
  (setq pz (getpoint "�����������е�: "))
  (setq pa (polar pz (* pi 1) 1000))
  (setq pb (polar pa (* pi 1.5) 100))
  (setq pc (polar pb 0 2000))
  (setq pd (polar pz 0 1000))
  (command "rectangle" pa pc) ;rectangle ������ž��εĶԽ������
 ; (command "pline" pa pb pc pd "c")
  ;��ִ�����ܶ�,��Ϊ�ĸ��߶λ��ƣ����Գ���ʱ��ٴ�
(prin1)
)
(prompt "̷�� ����ľ��С���Զ�����")
(prin1)


;���������ࡢ���ľ����С�㣩
(defun c:bdm()
(setvar "cmdecho" 0)	
;�����ǰ����ľ����С�㣩
  (setq za (getpoint "�����������󽻵�: "))
  ;(setq pa (getpoint "�����������ҽ���: "))
  (setq lj (getpoint "�����������·��ǵ㣺"))
  ;(setq ljz (getpoint "�����������·��ǵ㣺"))
  (setq pa (list (car lj) (cadr za)) ljz (list (car za) (cadr lj)))
  (setq pb (polar pa (* pi 0) 50))
  (setq pc (polar pb (* pi 1.5) 100))
  (setq pd (polar pc 0 -50))
  (setq ss (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 pa)(cons 10 pb)(cons 10 pc)(cons 10 pd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) ss)
  
  ;���涨������ľ�������߶εĶ˵�
  (setq laa(polar pa (* pi 0) 30))
   (setq lab(polar laa (* pi 1.5) 20))
   (setq lba(polar pa (* pi 0) 10))
   (setq lbb(polar lba (* pi 1.5) 80))
    (setq lca(polar pa (* pi 0) 35))
   (setq lcb(polar lca (* pi 1.5) 42))
   (setq lda(polar pa (* pi 0) 26))
   (setq ldb(polar lda (* pi 1.5) 70))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 lab)(cons 11 lbb) (cons 62 4)))
   (ssadd (entlast) ss)
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 lcb)(cons 11 ldb) (cons 62 4)))
   (ssadd (entlast) ss)
   	
   ;;�������ɿ�
(command "_.-group"  "c" "*"  "" ss "")

;�����ǰ����ľ����С�㣩
 (setq zb (polar za (* pi 1.5) 100))
  (setq zc (polar zb (* pi 1) 50))
  (setq zd (polar zc (* pi 0.5) 100))
   (setq zz (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 za)(cons 10 zb)(cons 10 zc)(cons 10 zd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) zz)

  ;���涨������ľ�������߶εĶ˵�
(setq za1(polar za (* pi 1) 50))
  (setq laaz(polar za1 (* pi 0) 30))
   (setq labz(polar laaz (* pi 1.5) 20))
   (setq lbaz(polar za1 (* pi 0) 10))
   (setq lbbz(polar lbaz (* pi 1.5) 80))
    (setq lcaz(polar za1 (* pi 0) 35))
   (setq lcbz(polar lcaz (* pi 1.5) 42))
   (setq ldaz(polar za1 (* pi 0) 26))
   (setq ldbz(polar ldaz (* pi 1.5) 70))
  
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 labz)(cons 11 lbbz) (cons 62 4)))
   (ssadd (entlast) zz)
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 lcbz)(cons 11 ldbz) (cons 62 4)))
   (ssadd (entlast) zz)
   (command "_.-group"  "c" "*"  "" zz "")
  
   ;���涨���׸ֹܶ˵�
   (setq gg1(polar pa (* pi 1.5) 100))
   (setq gg2 (polar gg1 (* pi 0) 1000))
   (setq gg3 (polar gg1 (* pi 1.5) 48))
   (setq gg4 (polar gg3 (* pi 0) 1000))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 gg1)(cons 11 gg2) (cons 62 4)))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 gg3)(cons 11 gg4) (cons 62 4)))
   ;���涨�����Ҳ�ľ���ǵ�
      (setq cla(polar pa (* pi 1.5) 148))
   (setq clb (polar cla (* pi 0) 100))
  (setq clc (polar lj (* pi 0) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 cla)(cons 10 clb)(cons 10 clc)(cons 10 lj) (cons 62 4) (cons 70 1)))
;���涨�������ľ���ǵ�
 (setq claz(polar za (* pi 1.5) 148))
     (setq clcz (polar ljz (* pi 1) 100))
     (setq cldz (polar claz (* pi 1) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 claz)(cons 10 ljz)(cons 10 clcz)(cons 10 cldz) (cons 62 4) (cons 70 1)))

;���¶������׵�һ��ľ���ǵ�
   (setq la (polar lj (* pi 1) 1000))
  (setq lb (polar la (* pi 1.5) 100))
  (setq lc (polar lb 0 2000))
  (setq ld (polar lj 0 1000))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 la)(cons 10 lb)(cons 10 lc)(cons 10 ld) (cons 62 4) (cons 70 1)))
  ;(command "rectangle" la lc) ;rectangle ������ž��εĶԽ������

      ;(command "rectangle" pa pc) ;rectangle ������ž��εĶԽ������
 ; (command "pline" pa pb pc pd "c")
  ;��ִ�����ܶ�,��Ϊ�ĸ��߶λ��ƣ����Գ���ʱ��ٴ�
  ;(command "line" lab lbb "")
  ;(command "line" lcb ldb "")
  
 ;(command "line" gg1 gg2 "")
  ; (command "line" gg3 gg4 "")
   ;(command "rectangle" clb lj) 
(prin1)
)
(prompt "̷�� �����ľ��С�� ����Զ�����")
(prin1)

;���������ࡢ���ľ����С�㣩�����װ汾��
(defun c:bdm3()
(setvar "cmdecho" 0)	
;�����ǰ����ľ����С�㣩
  (setq za (getpoint "�����������󽻵�: "))
  ;(setq pa (getpoint "�����������ҽ���: "))
  (setq lj (getpoint "�����������·��ǵ㣺"))
  ;(setq ljz (getpoint "�����������·��ǵ㣺"))
  (setq xza2 (getpoint "��������һ�������󽻵�"))
  (setq pa (list (car lj) (cadr za)) ljz (list (car za) (cadr lj)))
  (setq pb (polar pa (* pi 0) 50))
  (setq pc (polar pb (* pi 1.5) 100))
  (setq pd (polar pc 0 -50))
  (setq ss (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 pa)(cons 10 pb)(cons 10 pc)(cons 10 pd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) ss)
  
  ;���涨������ľ�������߶εĶ˵�
  (setq laa(polar pa (* pi 0) 30))
   (setq lab(polar laa (* pi 1.5) 20))
   (setq lba(polar pa (* pi 0) 10))
   (setq lbb(polar lba (* pi 1.5) 80))
    (setq lca(polar pa (* pi 0) 35))
   (setq lcb(polar lca (* pi 1.5) 42))
   (setq lda(polar pa (* pi 0) 26))
   (setq ldb(polar lda (* pi 1.5) 70))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 lab)(cons 11 lbb) (cons 62 4)))
   (ssadd (entlast) ss)
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 lcb)(cons 11 ldb) (cons 62 4)))
   (ssadd (entlast) ss)
   	
   ;;�������ɿ�
(command "_.-group"  "c" "*"  "" ss "")

;�������ɳ�����ľ��
;(setq ss (car(entsel "\nѡ�����:")))
;(setq xza2 (getpoint "��������һ�������󽻵�"))
(setq zljl  (distance pa xza2) )
(setq JJ 300)
(setq sl (fix(/ zljl JJ)  ))
(command "-array" ss "" "r" 1 sl (/ zljl (float sl))  )
 (setq gg1(polar pa (* pi 1.5) 100))
  (setq gg2 (polar xza2 (* pi 1.5) 100))
   (setq gg3 (polar gg1 (* pi 1.5) 48))
   (setq gg4 (polar gg2 (* pi 1.5) 48))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 gg1)(cons 11 gg2) (cons 62 4)))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 gg3)(cons 11 gg4) (cons 62 4)))

   ;�����ǰ����ľ����С�㣩
 (setq zb (polar za (* pi 1.5) 100))
  (setq zc (polar zb (* pi 1) 50))
  (setq zd (polar zc (* pi 0.5) 100))
   (setq zz (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 za)(cons 10 zb)(cons 10 zc)(cons 10 zd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) zz)

  ;���涨������ľ�������߶εĶ˵�
(setq za1(polar za (* pi 1) 50))
  (setq laaz(polar za1 (* pi 0) 30))
   (setq labz(polar laaz (* pi 1.5) 20))
   (setq lbaz(polar za1 (* pi 0) 10))
   (setq lbbz(polar lbaz (* pi 1.5) 80))
    (setq lcaz(polar za1 (* pi 0) 35))
   (setq lcbz(polar lcaz (* pi 1.5) 42))
   (setq ldaz(polar za1 (* pi 0) 26))
   (setq ldbz(polar ldaz (* pi 1.5) 70))
  
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 labz)(cons 11 lbbz) (cons 62 4)))
   (ssadd (entlast) zz)
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 lcbz)(cons 11 ldbz) (cons 62 4)))
   (ssadd (entlast) zz)
   (command "_.-group"  "c" "*"  "" zz "")

 ;���涨�����Ҳ�ľ���ǵ�
      (setq cla(polar pa (* pi 1.5) 148))
   (setq clb (polar cla (* pi 0) 100))
  (setq clc (polar lj (* pi 0) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 cla)(cons 10 clb)(cons 10 clc)(cons 10 lj) (cons 62 4) (cons 70 1)))
;���涨�������ľ���ǵ�
 (setq claz(polar za (* pi 1.5) 148))
     (setq clcz (polar ljz (* pi 1) 100))
     (setq cldz (polar claz (* pi 1) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 claz)(cons 10 ljz)(cons 10 clcz)(cons 10 cldz) (cons 62 4) (cons 70 1)))

;���¶������׵�һ��ľ���ǵ�
    (setq la (list( -  ( /  ( + (car lj) (car za) ) 2)  1000)(cadr lj)))
  (setq lb (polar la (* pi 1.5) 100))
  (setq lc (polar lb 0 2000))
  (setq ld (polar la 0 2000))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 la)(cons 10 lb)(cons 10 lc)(cons 10 ld) (cons 62 2) (cons 70 1)))

   
(setvar 'cmdecho 1)
(princ)
)







;���������ּӿ����

(defun c:bt ( / *error* def enx idx lst off sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (or (not (setq def (getenv "LMac\\boxtext-off")))
            (not (setq def (distof def 2)))
        )
        (setenv "LMac\\boxtext-off" (rtos (setq def 0.35) 2 2))
    )
    (initget 4)
    (if (setq off (getreal (strcat "\nSpecify offset factor <" (rtos def 2 2) ">: ")))
        (setenv "LMac\\boxtext-off" (rtos off 2 2))
        (setq off def)
    )
    
    (LM:startundo (LM:acdoc))
    (if (setq sel (LM:ssget "\nSelect text or mtext <exit>: " '(((0 . "TEXT,MTEXT")))))
        (repeat (setq idx (sslength sel))
            (setq enx (entget (ssname sel (setq idx (1- idx))))
                  lst (text-box-off enx (* off (cdr (assoc 40 enx))))
            )
            (entmake
                (append
                   '(
                        (000 . "LWPOLYLINE")
                        (100 . "AcDbEntity")
                        (100 . "AcDbPolyline")
                        (090 . 4)
                        (070 . 1)
                    )
                    (LM:defaultprops enx)
                    (list (cons  038 (caddar lst)))
                    (mapcar '(lambda ( x ) (cons 10 x)) lst)
                    (list (assoc 210 enx))
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups

(defun LM:defaultprops ( enx )
    (mapcar '(lambda ( x ) (cond ((assoc (car x) enx)) ( x )))
       '(
            (006 . "BYLAYER")
            (008 . "0")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Text Box  -  gile / Lee Mac
;; Returns an OCS point list describing a rectangular frame surrounding
;; the supplied text or mtext entity with optional offset
;; enx - [lst] Text or MText DXF data list
;; off - [rea] offset (may be zero)

(defun text-box-off ( enx off / bpt hgt jus lst ocs org rot wid )
    (cond
        (   (= "TEXT" (cdr (assoc 00 enx)))
            (setq bpt (cdr (assoc 10 enx))
                  rot (cdr (assoc 50 enx))
                  lst (textbox enx)
                  lst
                (list
                    (list (- (caar  lst) off) (- (cadar  lst) off)) (list (+ (caadr lst) off) (- (cadar  lst) off))
                    (list (+ (caadr lst) off) (+ (cadadr lst) off)) (list (- (caar  lst) off) (+ (cadadr lst) off))
                )
            )
        )
        (   (= "MTEXT" (cdr (assoc 00 enx)))
            (setq ocs  (cdr (assoc 210 enx))
                  bpt  (trans (cdr (assoc 10 enx)) 0 ocs)
                  rot  (angle '(0.0 0.0) (trans (cdr (assoc 11 enx)) 0 ocs))
                  wid  (cdr (assoc 42 enx))
                  hgt  (cdr (assoc 43 enx))
                  jus  (cdr (assoc 71 enx))
                  org  (list (cond ((member jus '(2 5 8)) (/ wid -2.0)) ((member jus '(3 6 9)) (- wid))      (0.0))
                             (cond ((member jus '(1 2 3)) (- hgt))      ((member jus '(4 5 6)) (/ hgt -2.0)) (0.0))
                       )
                  lst
                (list
                    (list (- (car org) off)     (- (cadr org) off))     (list (+ (car org) wid off) (- (cadr org) off))
                    (list (+ (car org) wid off) (+ (cadr org) hgt off)) (list (- (car org) off)     (+ (cadr org) hgt off))
                )
            )
        )
    )
    (if lst
        (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) bpt)) lst))
            (list
                (list (cos rot) (sin (- rot)) 0.0)
                (list (sin rot) (cos rot)     0.0)
               '(0.0 0.0 1.0)
            )
        )
    )
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: BoxText.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"bt\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;




;�������ؽ����߽����
(defun C:TCBJ(/ sg i s1) 
  (princ "\n��������߿���ѡ�����ͼ����")
  (setq sg (ssget '((0 . "HATCH"))) i -1)
  (while (setq s1 (ssname sg (setq i (1+ i))))
    (command "-hatchedit" s1 "b" "p" "y")
   )
  (princ)
)


;�����Ǵ�ȫ��ͼ�����
(defun c:QBTC ()
(command "layer" "on" "*" "")
(princ)
)
;�����ǹرն������ڲ�
(defun c:GBTC (/ ss c en lay)
  (if (setq ss (ssget))
    (progn;;;�ر�
      (setq c 0)
      (while (< c (sslength ss))
        (setq en (ssname ss c))
        (setq lay (cdr (assoc 8 (entget en))))
        (if (not (member lay laylst))
          (setq laylst (cons lay laylst))
        )
        (if (= lay (getvar "clayer"))
          (command "-layer" "off" lay "y" "")
          (command "-layer" "off" lay "")
        )
        (setq c (+ 1 c))
      )
    )
    (progn;;;����
      (setq c 0)
      (repeat (length laylst)
        (command "-layer" "on" (nth c laylst) "")
        (setq c (1+ c))
      )
    )
  )
 (princ)
)

;������;ֻ��ʾ��ѡ�������ڲ�
(DEFUN C:ZXTC (/ ES EN EL A)
 (princ "��ѡ�����δ��ѡ�ж������ڵĲ㽫���ر�")
 (setq ES (ssget) A 0 EN "" EL nil FL nil)
 (while (/= EN nil)
 (setq EN (ssname ES A) EL (cons EN EL) A (1+ A)))
 (setq EL (cdr EL) FL (cdr (assoc ' 8 (entget (car EL)))) EL (cdr EL))
 (repeat (- A 2)
 (setq EN (cdr (assoc ' 8 (entget (car EL))))
  FL (strcat EN "," FL) EL (cdr EL)) )
 (command "LAYER" "off" "*" "y" "on" (eval FL) "")
(princ)
)




























  



  


;���¼�����������


;���¼�����������

(defun mai_make_panel (menulist / m n nn nnn menuname val_lst tt order_lst)

  (defun add_dclrow (dstr)
    (if  dstr
      (setq val_lst (cons dstr val_lst))
    ) ;_ ����if
  ) ;_ ����defun

  (add_dclrow "curbutton : button{width=10;vertical_margin=none;vertical_margin=none;}")
  (add_dclrow
    "curpanel : dialog{label=\"̷������С���򹤾���\";alignment=centered;vertical_margin=none;horizontal_margin=none;"
  ) ;_ ����add_dclrow
  (add_dclrow ":row{")
  (setq  len    (apply 'max (mapcar 'length menulist))
  order_lst (apply 'append (mapcar 'cdr menulist))
  n    0
  m    0
  ) ;_ ����setq
  (foreach nn menulist
    (add_dclrow
      (strcat " : boxed_column{label=\"" (car nn) "\";vertical_margin=none; horizontal_margin=none;")
    ) ;����б��
    (foreach nnn (cdr nn)
      (setq n  (1+ n)
      tt (car nnn)
      tt (if tt
     tt
     ""
         ) ;_ ����if
      ) ;_ ����setq
      (add_dclrow (strcat " : curbutton{key=\"but" (vl-princ-to-string n) "\";label=\"" tt "\";}"))
    ) ;����б���еİ�ť
    (repeat (- len (length nn))
      (setq m (1+ m))
      (add_dclrow (strcat " : curbutton{key=\"butno" (vl-princ-to-string m) "\";}"))
    ) ;���в����б���еİ�ť
    (add_dclrow "}")
  ) ;_ ����foreach

  (add_dclrow
    "}:button{label=\"�ر�\";key=\"cancel\";is_cancel=true;width=10;fixed_width=true;alignment=centered;}}"
  )


  (setq menuname (vl-filename-mktemp "temp_pannel.dcl"))
  (setq nn (open menuname "w"))
  (foreach n (reverse val_lst) (write-line n nn))
  (close nn)
  (setq nnn (load_dialog menuname))
  (vl-file-delete menuname)

  (if (not (new_dialog "curpanel" nnn))
    (exit)
  ) ;_ ����if
  (setq n 0)
  (repeat m (mode_tile (strcat "butno" (vl-princ-to-string m)) 1) (setq m (1- m))) ;���ò����б���еİ�ť
  (foreach nn menulist
    (foreach nnn (cdr nn)
      (setq n  (1+ n)
      tt (car nnn) ;tt order_lst
      tt (if tt
     tt
     ""
         ) ;_ ����if
      ) ;_ ����setq
      (if (= tt "")
  (mode_tile (strcat "but" (vl-princ-to-string n)) 1)
  (action_tile
    (strcat "but" (vl-princ-to-string n))
    (strcat "(done_dialog " (vl-princ-to-string n) ")")
  ) ;_ ����action_tile
      ) ;_ ����if
    ) ;_ ����foreach
  ) ;_ ����foreach

  (setq nn (start_dialog))
  (unload_dialog nnn)


  (if (> nn 0)
    (progn (setq tt (cadr (nth (1- nn) order_lst))) ;ȡ�����ť��ֵ(����ܹؼ�)
     (if (= (eval (read (strcat "(type c:" tt ")"))) 'SUBR)
       (progn (princ "\n")
        (eval (read (strcat "(c:" tt ")")))
       ) ;_ ����progn
       (progn
         (princ "\n")
         (vla-SendCommand (vla-get-ActiveDocument(vlax-get-acad-object)) (strcat tt "\n"))
       ) ;_ ����progn
     ) ;_ ����if
    ) ;_ ����progn
  ) ;_ ����if
) ;_ ����defun