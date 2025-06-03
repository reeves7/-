;;用户可自己修改界面
(defun c:ffe ()
  (mai_make_panel
    (list
      (list "lisp小程序1"
      (list "       保留选择图元的图层        " "SO") ;(list  按钮标签  运行的命令)
      (list "       打开全部图层       " "QBTC")

      ) ;_ lisp小程序1
             (list "lisp小程序2"
      (list "  梁侧、板底木方（小楞）（进阶版本）"    "erer") ;(list  按钮标签  运行的命令)
	
       ) ;_ 结束list小程序2
    ) ;_ 结束list
  ) ;_ 结束mai_make_panel
  (princ)
) ;_ 结束defun
;以下是保留选择图元的图层 
(vl-load-com) ; 加载ActiveX支持

(defun c:SO (/ ent layname acadDoc layers layerObj)
  ;; 主程序 - 显示选中图元所在图层
  (setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object))) ; 获取当前文档
  
  ;; 选择图元
  (if (setq ent (car (entsel "\n选择要显示的图层图元: ")))
    (progn
      ;; 获取图元所属图层
      (setq layname (cdr (assoc 8 (entget ent))))
      
      ;; 获取图层集合
      (setq layers (vla-get-Layers acadDoc))
      
      ;; 遍历所有图层
      (vlax-for layerObj layers
        (if (= (vla-get-Name layerObj) layname)
          (vla-put-LayerOn layerObj :vlax-true) ; 打开目标图层
          (vla-put-LayerOn layerObj :vlax-false) ; 关闭其他图层
        )
      )
      
      ;; 刷新显示
      (vla-Regen acadDoc acActiveViewport) 
      (princ (strcat "\n已隐藏其他图层，仅显示 <" layname "> 图层"))
    )
    (princ "\n未选择有效图元")
  )
  (princ)
)




;以下是打开全部图层
(defun c:QBTC ()
(command "layer" "on" "*" "")
(princ)
)

;以下是梁侧、板底木方（小楞）（进阶版本）
(defun c:erer()
(setvar "cmdecho" 0)	
;以下是板底右木方（小楞）
  (setq za (getpoint "请输入梁板左交点: "))
  ;(setq pa (getpoint "请输入梁板右交点: "))
  (setq lj (getpoint "请输入梁右下方角点："))
  ;(setq ljz (getpoint "请输入梁左下方角点："))
  (setq xza2 (getpoint "请输入下一个梁板左交点"))
  (setq pa (list (car lj) (cadr za)) ljz (list (car za) (cadr lj)))
  (setq pb (polar pa (* pi 0) 50))
  (setq pc (polar pb (* pi 1.5) 100))
  (setq pd (polar pc 0 -50))
  (setq ss (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 pa)(cons 10 pb)(cons 10 pc)(cons 10 pd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) ss)
  
  ;下面定义板底右木方框内线段的端点
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
   	
   ;;以下生成块
(command "_.-group"  "c" "*"  "" ss "")

;以下生成长向板底木方
;(setq ss (car(entsel "\n选择对象:")))
;(setq xza2 (getpoint "请输入下一个梁板左交点"))
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

   ;以下是板底左木方（小楞）
 (setq zb (polar za (* pi 1.5) 100))
  (setq zc (polar zb (* pi 1) 50))
  (setq zd (polar zc (* pi 0.5) 100))
   (setq zz (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 za)(cons 10 zb)(cons 10 zc)(cons 10 zd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) zz)

  ;下面定义板底左木方框内线段的端点
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

 ;下面定义梁右侧木方角点
      (setq cla(polar pa (* pi 1.5) 148))
   (setq clb (polar cla (* pi 0) 100))
  (setq clc (polar lj (* pi 0) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 cla)(cons 10 clb)(cons 10 clc)(cons 10 lj) (cons 62 4) (cons 70 1)))
;下面定义梁左侧木方角点
 (setq claz(polar za (* pi 1.5) 148))
     (setq clcz (polar ljz (* pi 1) 100))
     (setq cldz (polar claz (* pi 1) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 claz)(cons 10 ljz)(cons 10 clcz)(cons 10 cldz) (cons 62 4) (cons 70 1)))

;以下定义梁底第一层木方角点
   (setq la (list( -  ( /  ( + (car lj) (car za) ) 2)  1000)(cadr lj)))
  (setq lb (polar la (* pi 1.5) 100))
  (setq lc (polar lb 0 2000))
  (setq ld (polar la 0 2000))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 la)(cons 10 lb)(cons 10 lc)(cons 10 ld) (cons 62 2) (cons 70 1)))

   
(setvar 'cmdecho 1)
(princ)
)
































  



  


;往下继续是主程序


;往下继续是主程序

(defun mai_make_panel (menulist / m n nn nnn menuname val_lst tt order_lst)

  (defun add_dclrow (dstr)
    (if  dstr
      (setq val_lst (cons dstr val_lst))
    ) ;_ 结束if
  ) ;_ 结束defun

  (add_dclrow "curbutton : button{width=10;vertical_margin=none;vertical_margin=none;}")
  (add_dclrow
    "curpanel : dialog{label=\"谭工技研小程序工具箱\";alignment=centered;vertical_margin=none;horizontal_margin=none;"
  ) ;_ 结束add_dclrow
  (add_dclrow ":row{")
  (setq  len    (apply 'max (mapcar 'length menulist))
  order_lst (apply 'append (mapcar 'cdr menulist))
  n    0
  m    0
  ) ;_ 结束setq
  (foreach nn menulist
    (add_dclrow
      (strcat " : boxed_column{label=\"" (car nn) "\";vertical_margin=none; horizontal_margin=none;")
    ) ;添加列表框
    (foreach nnn (cdr nn)
      (setq n  (1+ n)
      tt (car nnn)
      tt (if tt
     tt
     ""
         ) ;_ 结束if
      ) ;_ 结束setq
      (add_dclrow (strcat " : curbutton{key=\"but" (vl-princ-to-string n) "\";label=\"" tt "\";}"))
    ) ;添加列表框中的按钮
    (repeat (- len (length nn))
      (setq m (1+ m))
      (add_dclrow (strcat " : curbutton{key=\"butno" (vl-princ-to-string m) "\";}"))
    ) ;按列补齐列表框中的按钮
    (add_dclrow "}")
  ) ;_ 结束foreach

  (add_dclrow
    "}:button{label=\"关闭\";key=\"cancel\";is_cancel=true;width=10;fixed_width=true;alignment=centered;}}"
  )


  (setq menuname (vl-filename-mktemp "temp_pannel.dcl"))
  (setq nn (open menuname "w"))
  (foreach n (reverse val_lst) (write-line n nn))
  (close nn)
  (setq nnn (load_dialog menuname))
  (vl-file-delete menuname)

  (if (not (new_dialog "curpanel" nnn))
    (exit)
  ) ;_ 结束if
  (setq n 0)
  (repeat m (mode_tile (strcat "butno" (vl-princ-to-string m)) 1) (setq m (1- m))) ;禁用补齐列表框中的按钮
  (foreach nn menulist
    (foreach nnn (cdr nn)
      (setq n  (1+ n)
      tt (car nnn) ;tt order_lst
      tt (if tt
     tt
     ""
         ) ;_ 结束if
      ) ;_ 结束setq
      (if (= tt "")
  (mode_tile (strcat "but" (vl-princ-to-string n)) 1)
  (action_tile
    (strcat "but" (vl-princ-to-string n))
    (strcat "(done_dialog " (vl-princ-to-string n) ")")
  ) ;_ 结束action_tile
      ) ;_ 结束if
    ) ;_ 结束foreach
  ) ;_ 结束foreach

  (setq nn (start_dialog))
  (unload_dialog nnn)


  (if (> nn 0)
    (progn (setq tt (cadr (nth (1- nn) order_lst))) ;取点击按钮的值(这个很关键)
     (if (= (eval (read (strcat "(type c:" tt ")"))) 'SUBR)
       (progn (princ "\n")
        (eval (read (strcat "(c:" tt ")")))
       ) ;_ 结束progn
       (progn
         (princ "\n")
         (vla-SendCommand (vla-get-ActiveDocument(vlax-get-acad-object)) (strcat tt "\n"))
       ) ;_ 结束progn
     ) ;_ 结束if
    ) ;_ 结束progn
  ) ;_ 结束if
) ;_ 结束defun