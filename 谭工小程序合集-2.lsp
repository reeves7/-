;;�û����Լ��޸Ľ���
(defun c:xxx ()
  (mai_make_panel
    (list
      (list "lispС����1"
      (list "���ඨ��������" "CMC") ;(list  ��ť��ǩ  ���е�����)
      (list "���㻭Բ" "hyq")
      (list "�Զ�����������" "lgg7")
      (list "������ϵ����" "3tj")
      (list "��̬�ӳ�" "saa")
      (list "���ƾ��ζԽ���" "jxdx")
      (list "����䶨������" "twm")
      (list "�߶εȷ�" "XBR")
      ) ;_ ����list
      (list "ı�����2"
      (list " �� ����" "mini_c_column")
      (list " �� ����" "mini_l_column")
      (list " �� ����" "mini_t_column")
      (list " �� ����" "mini_O_column")
      (list " �� ����" "mini_r_column")
      (list "���߲���" "mini_axis_column")
      (list "�� �� ��" "mini_fill_column")
      (list "ǽ�����" "mini_fill_wall")
      (list "תPKPM GS��" "mini_pline_to_beam")
      (list "���߱���" "mini_pline_to_column")
      ) ;_ ����list
       (list "ı�����3"
      (list " �� ����" "mini_c_column")
      (list " �� ����" "mini_l_column")
      (list " �� ����" "mini_t_column")
      (list " �� ����" "mini_O_column")
      (list " �� ����" "mini_r_column")
      (list "���߲���" "mini_axis_column")
      (list "�� �� ��" "mini_fill_column")
      (list "ǽ�����" "mini_fill_wall")
      (list "תPKPM GS��" "mini_pline_to_beam")
      (list "���߱���" "mini_pline_to_column")
      ) ;_ ����list
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
	(setq bj(getreal "\n������Բ�뾶<60>"))
	(if(not bj)(setq bj 60))
  (setq acad (vlax-get-acad-object))
  (setq acaddocument (vla-get-activedocument acad))
  (setq mspace (vla-get-modelspace acaddocument))
  (setq ptlist (ssinters ss))
	(foreach a ptlist
		(command "circle" a bj)
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
  (command "pline" pa pb pc pd "c")
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
(setq JJ (getreal "\n ��������: "))
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