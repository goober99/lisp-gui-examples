import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.2
import EQL5 1.0

ColumnLayout {
  id: root
  objectName: "root"

  property real frequency: 440

  Slider {
    id: frequencySlider
    from: minPosition
    value: Lisp.call("frequency->position", root.frequency)
    to: maxPosition
    // https://doc.qt.io/qt-5/qml-qtquick-controls2-slider.html#moved-signal
    // onMoved requires QtQuick.Controls 2.2 or greater
    // onMoved instead of onValueChanged so that frequency will only be updated
    // when interactively moved avoiding binding loops.
    onMoved: root.frequency = Lisp.call("position->frequency", value)
    Layout.fillWidth: true
    Layout.margins: 25
  }

  RowLayout {
    spacing: 25
    Layout.alignment: Qt.AlignHCenter
    Layout.margins: 25

    Button {
      text: "<"
      onClicked: Lisp.call("decrease-octave")
    }

    RowLayout {
      SpinBox {
        id: frequencyField
        editable: true
        from: minFrequency * 100
        value: Math.round(root.frequency * 100)
        to: maxFrequency * 100
        stepSize: 100
        onValueChanged: root.frequency = value / 100

        property int decimals: 2
        property real realValue: value / 100

        validator: DoubleValidator {
          bottom: Math.min(frequencyField.from, frequencyField.to)
          top: Math.max(frequencyField.from, frequencyField.to)
        }

        textFromValue: function(value, locale) {
          return Number(value / 100).toLocaleString(locale, 'f', decimals)
        }

        valueFromText: function(text, locale) {
          return Math.round(Number.fromLocaleString(locale, text) * 100)
        }
      }
      Label { text: "Hz" }
    }

    Button {
      text: ">"
      onClicked: Lisp.call("increase-octave")
    }
  }

  RowLayout {
    spacing: 25
    Layout.margins: 25

    RowLayout {
      SpinBox {
        id: durationField
        editable:true
        from: 1
        value: 200
        to: 600000
      }
      Label { text: "ms" }
    }

    Button {
      text: "Play"
      onClicked: Lisp.call("generate-tone", frequency, durationField.value)
    }

    RowLayout {
      Label { text: "♪" }
      ComboBox {
        textRole: "note"
        // Notes -> frequency (middle A-G [A4-G4])
        // http://pages.mtu.edu/~suits/notefreqs.html
        model: [
          { note: "A", freq: 440.00 },
          { note: "B", freq: 493.88 },
          { note: "C", freq: 261.63 },
          { note: "D", freq: 293.66 },
          { note: "E", freq: 329.63 },
          { note: "F", freq: 349.23 },
          { note: "G", freq: 392.00 }
        ]
        onActivated: root.frequency = model[index]["freq"]
      }
    }
  }
}
