;;�û����Լ��޸Ľ���
(defun c:ffe ()
  (mai_make_panel
    (list
      (list "lispС����1"
      (list "       ����ѡ��ͼԪ��ͼ��        " "SO") ;(list  ��ť��ǩ  ���е�����)
      (list "       ��ȫ��ͼ��       " "QBTC")

      ) ;_ lispС����1
             (list "lispС����2"
      (list "  ���ࡢ���ľ����С�㣩�����װ汾��"    "erer") ;(list  ��ť��ǩ  ���е�����)
	
       ) ;_ ����listС����2
    ) ;_ ����list
  ) ;_ ����mai_make_panel
  (princ)
) ;_ ����defun
;�����Ǳ���ѡ��ͼԪ��ͼ�� 
(vl-load-com) ; ����ActiveX֧��

(defun c:SO (/ ent layname acadDoc layers layerObj)
  ;; ������ - ��ʾѡ��ͼԪ����ͼ��
  (setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object))) ; ��ȡ��ǰ�ĵ�
  
  ;; ѡ��ͼԪ
  (if (setq ent (car (entsel "\nѡ��Ҫ��ʾ��ͼ��ͼԪ: ")))
    (progn
      ;; ��ȡͼԪ����ͼ��
      (setq layname (cdr (assoc 8 (entget ent))))
      
      ;; ��ȡͼ�㼯��
      (setq layers (vla-get-Layers acadDoc))
      
      ;; ��������ͼ��
      (vlax-for layerObj layers
        (if (= (vla-get-Name layerObj) layname)
          (vla-put-LayerOn layerObj :vlax-true) ; ��Ŀ��ͼ��
          (vla-put-LayerOn layerObj :vlax-false) ; �ر�����ͼ��
        )
      )
      
      ;; ˢ����ʾ
      (vla-Regen acadDoc acActiveViewport) 
      (princ (strcat "\n����������ͼ�㣬����ʾ <" layname "> ͼ��"))
    )
    (princ "\nδѡ����ЧͼԪ")
  )
  (princ)
)




;�����Ǵ�ȫ��ͼ��
(defun c:QBTC ()
(command "layer" "on" "*" "")
(princ)
)

;���������ࡢ���ľ����С�㣩�����װ汾��
(defun c:erer()
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