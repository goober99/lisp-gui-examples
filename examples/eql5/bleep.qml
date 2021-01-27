import QtQuick 2.0
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.2
import EQL5 1.0

ColumnLayout {
  id: root

  property real frequency: 440

  function setFrequency(freq) {
    frequencySlider.value = freq
    frequencyField.value = freq
  }

  Slider {
    id: frequencySlider
    from: 0
    value: Lisp.call("frequency->position", 440)
    to: 2000
    //onValueChanged: setFrequency(value)
    onValueChanged: {
      //console.log("CHANGING SLIDER TO " + value)
      root.frequency = value
    }
    Layout.fillWidth: true
    Layout.margins: 25
  }

  RowLayout {
    spacing: 25
    Layout.alignment: Qt.AlignHCenter
    Layout.margins: 25

    Button {
      text: "<"
    }

    RowLayout {
      /*
      SpinBox {
        id: frequencyField
        from: 20
        //value: frequencySlider.value
        value: Lisp.call("position->frequency", root.frequency)
        to: 20000
        editable: true
        //onValueChanged: setFrequency(value)
        onValueChanged: {
          console.log("CHANGING SPINBOX TO " + value)
          root.frequency = value
        }
      }
      */
      SpinBox {
        id: frequencyField
        from: 20 * 100
        //value: 440 * 100
        value: Lisp.call("position->frequency", frequencySlider.value) * 100
        to: 20000 * 100
        stepSize: 100
        editable: true

        property int decimals: 2
        property real realValue: value / 100

        validator: DoubleValidator {
          bottom: Math.min(frequencyField.from, frequencyField.to)
          top: Math.max(frequencyField.from, frequencyField.to)
        }

        textFromValue: function(value, locale) {
          return Number(value / 100).toLocaleString(locale, 'f', frequencyField.decimals)
        }

        valueFromText: function(text, locale) {
          return Number.fromLocaleString(locale, text) * 100
        }
      }
      Label { text: "Hz" }
    }

    Button {
      text: ">"
    }
  }

  RowLayout {
    spacing: 25
    Layout.margins: 25

    RowLayout {
      SpinBox {
        from: 1
        value: 200
        to: 600000
        editable: true
      }
      Label { text: "ms" }
    }

    Button {
      text: "Play"
    }

    RowLayout {
      Label { text: "♪" }
      ComboBox {
        model: ["A", "B", "C", "D", "E", "F", "G"]
      }
    }
  }
}
