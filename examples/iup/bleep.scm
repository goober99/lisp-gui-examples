(import iup)

(define dlg
  (dialog
    title: "Bleep"
    (vbox
      alignment: 'ACENTER
      gap: 25
      margin: '25x25
      (valuator
        min: 20
        max: 20000
        value: 440
        expand: 'Yes)
      (hbox
        alignment: 'ACENTER
        gap: 25
        margin: '0x0
        (button title: '<)
        (hbox
          alignment: 'ACENTER
          gap: 5
          (textbox 'spin: 'Yes)
          (label "Hz"))
        (button title: '>))
      (hbox
        alignment: 'ACENTER
        gap: 25
        margin: '0x0
        (hbox
          alignment: 'ACENTER
          gap: 5
          (textbox 'spin: 'Yes)
          (label "ms"))
        (button title: 'Play)
        (hbox
          alignment: 'ACENTER
          gap: 5
          (label "♪")
          (listbox #:1 'A #:2 'B #:3 'C #:4 'D #:5 'E #:6 'F #:7 'G value: 1 dropdown: 'Yes))))))

(show dlg)
(main-loop)
(destroy! dlg)
(exit 0)
