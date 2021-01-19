import QtQuick 2.0
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.0

ColumnLayout {
  spacing: 25

  Slider {
    from: 20
    value: 440
    to: 20000
  }

  RowLayout {
    spacing: 25

    Button {
      text: "<"
    }
    SpinBox {
      from: 20
      value: 440
      to: 20000
      editable: true
    }
    Button {
      text: ">"
    }
  }

  RowLayout {
    spacing: 25

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
