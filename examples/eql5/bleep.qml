import QtQuick 2.0
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.2

ColumnLayout {
  id: root

  property real frequency: 440

  function setFrequency(freq) {
    frequencySlider.value = freq
    frequencyField.value = freq
  }

  Slider {
    id: frequencySlider
    from: 20
    value: root.frequency
    to: 20000
    //onValueChanged: setFrequency(value)
    onValueChanged: {
      console.log("CHANGING SLIDER TO " + value)
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
      SpinBox {
        id: frequencyField
        from: 20
        //value: frequencySlider.value
        value: root.frequency
        to: 20000
        editable: true
        //onValueChanged: setFrequency(value)
        onValueChanged: {
          console.log("CHANGING SPINBOX TO " + value)
          root.frequency = value
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
