(import iup)

(define dlg
  (dialog
    (vbox
      (valuator)
      (hbox
        (button title: '<)
        (textbox 'spin: 'Yes)
        (label "Hz")
        (button title: '>)
        gap: 10
        alignment: 'ACENTER
        margin: '15x15)
      (hbox
        (textbox 'spin: 'Yes)
        (label "ms")
        (button title: 'Play)
        (label "♪")
        (listbox #:1 'A #:2 'B #:3 'C #:4 'D #:5 'E #:6 'F #:7 'G value: 1 dropdown: 'Yes)
        gap: 10
        alignment: 'ACENTER
        margin: '15x15)
      gap: 10
      alignment: 'ACENTER
      margin: '15x15)
    title: 'Bleep))

(show dlg)
(main-loop)
(destroy! dlg)
(exit 0)
