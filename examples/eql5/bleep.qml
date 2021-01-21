import QtQuick 2.0
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.2

ColumnLayout {
  id: root

  function setFrequency(freq) {
    frequencySlider.value = freq
    frequencyField.value = freq
  }

  Slider {
    id: frequencySlider
    from: 20
    value: 440
    to: 20000
    onValueChanged: setFrequency(value)
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
    SpinBox {
      id: frequencyField
      from: 20
      value: 440
      to: 20000
      editable: true
      onValueChanged: setFrequency(value)
    }
    Button {
      text: ">"
    }
  }

  RowLayout {
    spacing: 25
    Layout.margins: 25

    SpinBox {
      from: 1
      value: 200
      to: 600000
      editable: true
    }
    Button {
      text: "Play"
    }
    ComboBox {
      model: ["A", "B", "C", "D", "E", "F", "G"]
    }
  }
}
